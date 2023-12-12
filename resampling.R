# resampling

# randomly allocate nbets in different bookmakers under different constraints and evaluating returns 

# data from football-data.co.uk

rm(list = ls())

library(reshape2) 
library(fitdistrplus)
library(tidyverse)

options(scipen = 999)

# 1 - Data ----

football_odds <- readRDS(file = "./data-raw/football-data.RDS")

# Odds Abbreviations for 1x2 in Football-data.co.uk
odds_abbs <- list(Bet365 = c("B365H", "B365D", "B365A"), Bwin = c("BWH", "BWD", "BWA"),
                  Interwetten = c("IWH", "IWD", "IWA"), WilliamHill = c("WHH", "WHD", "WHA"), 
                  Pinnacle = c("PSH", "PSD", "PSA"), VC = c("VCH", "VCD", "VCA")) 

# Convert columns with odds to numeric
football_odds <- football_odds %>% mutate_at(vars(unlist(odds_abbs)), as.numeric)

# count non NA odds of different bookmakers
n_obs <- colSums(!is.na(football_odds[, unlist(map(odds_abbs, 1))])) %>% 
  sort(decreasing = T)

# 2 - Resampling ----

# remove margin from odds
# power method finding the value of k, such as: sum(prop^k) = 1
remove_overround <- function(bookmaker_odds) {
  
  n <- length(bookmaker_odds)
  overround_props <- 1/bookmaker_odds

  # If NA, or any odds < 1, or margin < 0
  if (n < 2 || any(is.na(overround_props)) || any(overround_props > 1) || sum(overround_props) < 1) 
    return (rep(NA, n))
  
  sum_prop <- function(bias) {
    props <- overround_props^(1/bias)
    return (sum(props) - 1)
  }
  
  bias_ <- tryCatch(uniroot(sum_prop, c(0.5, 1))$root, 
                    error = function(e) {warning(conditionMessage(e)); NA})
  
  fair_props <- overround_props^(1/bias_) %>% {./sum(.)}
  
  return (fair_props)
  
}

# simulate full time result
simulate_ftr <- function(probabilities) {
  
  ftr <- c("H", "D", "A")
  
  mc_result <- function(cum_props) {
    p <- runif(1)
    
    # winner from simulation
    y <- cum_props - p
    index <- which(y >= 0)[1]
    
    return (ftr[index])
  }

  ftr_sim <- map(probabilities, cumsum) %>%
    map_chr(mc_result) 
  
  return(ftr_sim)
}

# partitioning the odds space to 0.01 intervals of overround props
# @params: 
# nbets: Number of bets to allocate
# fair_odds: T if to bet on fair odds
# sim_results: T to simulate Full time Result, F to use actual results
simulate_bet <- function(data, bookmaker, nbets = 100, fair_odds = F, sim_results = F, nsims = 5000) {
  
  params <- as.list(environment())[2:5]
  
  odds <- odds_abbs[[bookmaker]]
  
  data <- na.omit(data[, c(odds, "FTR")]) %>%
    filter(if_all(all_of(odds), ~. > 1)) %>%
    setNames(c("H", "D", "A", "FTR")) 

  fair_props <- mapply(c, data[["H"]], data[["D"]], data[["A"]], SIMPLIFY = F) %>%
    map(remove_overround) 
  
  # simulate full time result
  if (sim_results) data$FTR <- simulate_ftr(fair_props)
  
  # if fair_odds replace bookmaker's odds with fair odds 
  if (fair_odds) {
    data <- data %>%
      mutate(H = 1/sapply(fair_props, `[[`, 1), 
             D = 1/sapply(fair_props, `[[`, 2), 
             A = 1/sapply(fair_props, `[[`, 3)) 
  }
  
  data <- data  %>%
    reshape2::melt(id.vars = "FTR", variable.name = "selection", value.name = "odds") %>%
    mutate(success = (FTR == selection), 
           PnL = success*odds, 
           odds_bin = cut(1/odds, breaks = c(c(0, 0.04), seq(0.05, 0.9, 0.01), c(0.91, 1)))) %>%
    group_by(odds_bin) %>%
    filter(n() > 20)
  
  # random sample, n bets from every bin with replacement & summarize
  sampling <- function(df) {
    # data already grouped by bin
    df %>% 
      slice_sample(n = nbets, replace = T) %>%
      summarise(odds = mean(odds),
                returns = mean(PnL), 
                success = sum(success))}
  
  # sampling nsims times
  sim <- replicate(nsims, sampling(data), simplify = F) %>% 
    bind_rows() %>% bind_cols(params) 
  
  return (sim)
}

set.seed(1)

bootstrap <- data.frame(nbets = seq(1, 101, 10),
                        bookmaker = c("Bet365")) %>%
  pmap(simulate_bet, data = football_odds, sim_results = F, fair_odds = F, nsims = 1000, .progress = T) %>%
  bind_rows() 

bootstrap %>% filter(nbets == 101) %>%
  ggplot(aes(x = 1/odds, y = returns)) + 
  geom_point(alpha = 0.1) + geom_smooth(size = 1.5)

# 3 fit distribution|analyze ----

statistics <- function(df) {
  
  if (length(df$success) == 0) return(NULL)
  
  # fit a binomial distribution to successes 
  fit <- tryCatch(fitdist(df$success, distr = "binom", fix.arg=list(size = unique(df$nbets)), start=list(prob=0.5), method = "mle"), 
                  error = function(e) {warning(conditionMessage(e)); list(NA)})
  
  # returns_mean: actual mean returns from nsims simulations
  # p_binom: probability of successful bet from binomial fit
  # returns_mean_binom: mean returns as calculated by binomial fit.
  # returns = p*odds -> E(returns) = odds*E(p)
  # returns_se_binom: standard error of the mean returns
  # relationship between SD and SE: returns_se = returns_sd/sqrt(nsims)
  # min_success: minimum number of correct bets to achieve profit
  df %>%
    group_by(bookmaker, odds_bin, nbets) %>%
    summarize(odds = mean(odds),
              returns_mean = mean(returns),
              returns_sd = sd(returns),
              p_binom = fit$estimate,
              p_sd = fit$sd,
              returns_mean_binom = odds*p_binom, 
              returns_se_binom = odds*p_sd, 
              .groups = "drop") %>%
    mutate(min_success = ceiling(nbets/odds),
           p_profit = pbinom(min_success - 1, nbets, p_binom, lower.tail = F)) 
}

params <- bootstrap %>%
  split(f = list(.$bookmaker, .$odds_bin, .$nbets)) %>%
  lapply(statistics) %>%
  bind_rows() 

params %>% 
  ggplot(aes(x = 1/odds, y = p_profit, color = as.factor(nbets))) +
  geom_point(alpha = 0.5) + geom_smooth(method="lm", se = F) +
  facet_wrap(bookmaker ~.) +
  ggtitle("Profit Probability ~ f(overround probability)") 

params %>% filter(nbets > 1) %>%
  ggplot(aes(x = 1/odds, y = returns_se_binom, color = as.factor(nbets))) +
  geom_point(alpha = 0.5) + 
  facet_wrap(bookmaker ~.) +
  ggtitle("Standard error of mean returns ~ f(overround probability|nbets)") 
