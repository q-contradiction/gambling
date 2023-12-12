# Odds Comparison between different bookmakers in Football, identifying inefficiencies in the market

# Reproducing results & methodology from: football-data.co.uk/blog/wisdom_of_the_crowd.php (Joseph Buchdahl) in R
# and extending the analysis

# data from football-data.co.uk


rm(list = ls())

library(rvest) #read_html
library(data.table) 
library(reshape2) 
library(implied) #https://opisthokonta.net/?p=1797
library(fitdistrplus)
library(tidyverse)

options(scipen = 999)

# 1 - Load Football data ----

football_odds <- readRDS(file = "./data-raw/football-data.rds")

# Market/avg,max & Betbrain/avg,max are the same variables
# Substitute where avg/max market odds are NA with Betbrain odds
cols <- list(c("MaxH", "BbMxH"), c("MaxD", "BbMxD"), c("MaxA", "BbMxA"),
             c("AvgH", "BbAvH"), c("AvgD", "BbAvD"), c("AvgA", "BbAvA"),
             c("Max>2.5", "BbMx>2.5"), c("Max<2.5", "BbMx<2.5"),
             c("Avg>2.5", "BbAv>2.5"), c("Avg<2.5", "BbAv<2.5"))

for (v in cols)  {
  replace_indexes <- is.na(football_odds[[v[1]]])
  football_odds[[v[1]]][replace_indexes] <- football_odds[[v[2]]][replace_indexes]
}

# Remove BetBrain odds
football_odds <- football_odds %>% .[setdiff(names(.), unlist(map(cols, 2)))]

# Odds Abbreviations for 1x2 in Football-data.co.uk. C at the end of bookmaker's name indicates Closing odds
odds_1X2 <- list(Bet365 = c("B365H", "B365D", "B365A"), Bet365C = c("B365CH", "B365CD", "B365CA"),
                 Bwin = c("BWH", "BWD", "BWA"), BwinC = c("BWCH", "BWCD", "BWCA"), 
                 Interwetten =  c("IWH", "IWD", "IWA"), InterwettenC = c("IWCH", "IWCD", "IWCA"),
                 Pinnacle = c("PSH", "PSD", "PSA"), PinnacleC = c("PSCH", "PSCD", "PSCA"),
                 WilliamHill = c("WHH", "WHD", "WHA"), WilliamHillC = c("WHCH", "WHCD", "WHCA"),
                 VC = c("VCH", "VCD", "VCA"), VCC = c("VCCH", "VCCD", "VCCA"),
                 MarketMax = c("MaxH", "MaxD", "MaxA"), MarketMaxC = c("MaxCH", "MaxCD", "MaxCA"),
                 MarketAvg = c("AvgH", "AvgD", "AvgA"),  MarketAvgC = c("AvgCH", "AvgCD", "AvgCA"),
                 Sportingbet =  c("SBH", "SBD", "SBA"),
                 BlueSquare =  c("BSH", "BSD", "BSA"), 
                 Gamebookers =  c("GBH", "GBD", "GBA"), 
                 Ladbrokes = c("LBH", "LBD", "LBA"),
                 StanJames = c("SJH", "SJD", "SJA")) 


odds_OU <- list(Bet365 = c("B365>2.5", "B365<2.5"), Bet365C = c("B365C>2.5", "B365C<2.5"), 
                Pinnacle = c("P>2.5", "P<2.5"), PinnacleC = c("PC>2.5", "PC<2.5"),
                MarketMax = c("Max>2.5", "Max<2.5"), MarketMaxC = c("MaxC>2.5", "MaxC<2.5"),
                MarketAvg = c("Avg>2.5", "Avg<2.5"), MarketAvgC = c("AvgC>2.5", "AvgC<2.5"))


odds_abbs <- list('1X2' = odds_1X2, OU = odds_OU)

# Convert columns with odds to numeric
football_odds <- football_odds %>% mutate_at(vars(unlist(odds_abbs)), as.numeric)

# count non NA odds of different bookmakers
n_obs <- colSums(!is.na(football_odds[, unlist(c(map(odds_1X2, 1),map(odds_OU, 1)))])) %>% 
  sort(decreasing = T)

# Unique match identifier & OU result (Full time Line)
football_odds <- football_odds %>%
  mutate(ID = seq(1, nrow(.), 1), 
         FTL = ifelse(FTHG + FTAG > 2, "O", "U"))

# 2 - Build Odds Comparison Table ----

fair_probabilities <- function(data, odds, market, desc) {
  
  # filter NA or invalid odds (overround > 1, all odds > 1)
  valid_overround <- rowSums(1/data[, odds], na.rm = T) > 1
  
  data <- data[valid_overround, ] %>%
    filter(across(all_of(odds), ~. > 1))
  
  fair_props <- implied_probabilities(data[, odds], method = "power") 
  
  is_ok <- which(!fair_props[["problematic"]])  
  fair_props <- fair_props %>% .[["probabilities"]] %>% as.data.frame()
  
  fair_props <- if (market == "OU") 
    setNames(fair_props, paste(desc, c("PO", "PU"), sep = "_")) 
  else 
    setNames(fair_props, paste(desc, c("PH", "PD", "PA"), sep = "_")) 
  
  data <- bind_cols(data[is_ok, ], fair_props[is_ok, ]) %>% na.omit()
  
  return (data)
}

# long format | custom melt
transform_data <- function(data, market) {
  
  # Create separate tables for {Home, Draw, Away} | {Over, Under}
  dfList <- list()
  
  if (market == "1X2") {
    event = c("H", "D", "A")
    for (i in 1:3) {
      dfList[[i]] <- data %>%
        dplyr::select(ID, ends_with(event[i]), FTR) %>% 
        mutate(Event = event[i])}
    
  } else {
    #first set Over Columns, 2nd Under
    cols <- list(c(5, 1, 3, 7, 9, 11, 13, 6), 
                 c(5, 2, 4, 8, 10, 12, 14, 6))
    event = c("O", "U")
    
    for (i in 1:2) {
      dfList[[i]] <- data[cols[[i]]] %>%
        mutate(Event = event[i])
    }
  }
  
  bets <- dfList %>% 
    lapply(setNames, c("ID", "Odds_ref", "Odds_bet", "Prop_ref", 
                       "Prop_bet",  "EV", "PnL", "Res",  "Event")) %>%
    bind_rows() %>% na.omit() %>%
    arrange(ID) 
  
  return(bets)
  
}

# Creates a long table of odds comparing prices from bookie_ref and bookie_bet
compare_odds <- function(data, bookie_ref, bookie_bet, market) {
  
  print(paste(bookie_ref, bookie_bet))
  
  odds_ref <- odds_abbs[[market]][[bookie_ref]]
  odds_bet <- odds_abbs[[market]][[bookie_bet]]
  
  res <- if (market == "1X2") "FTR" else "FTL"
  
  if (is.null(odds_ref) || is.null(odds_bet) || bookie_ref == bookie_bet) return (NULL)
  
  data <- data[, c(odds_ref, odds_bet, "ID", res)] %>% na.omit()
  
  if (nrow(data) == 0) return (NULL)
  
  data <- data %>% 
    fair_probabilities(odds_ref, market, "ref") %>%
    fair_probabilities(odds_bet, market, "bet")
  
  # features:
  # {PH, PD, PA} = Props for {Home, Draw, Away} as estimated using bookmaker_ref odds
  # {EVH, EVD, EVA} = Expected Values 
  # {PnLH, PnLD, PnLA} = Profit & Loss for Betting {Home, Draw, Away}
  # Same for O/U
  if (market == "1X2") {
    data <- data %>% 
      mutate(EVH = ref_PH*get(odds_bet[1]) - 1,
             EVD = ref_PD*get(odds_bet[2]) - 1,
             EVA = ref_PA*get(odds_bet[3]) - 1,
             
             PnLH = -1 + ifelse(FTR == "H", get(odds_bet[1]), 0),
             PnLD = -1 + ifelse(FTR == "D", get(odds_bet[2]), 0),
             PnLA = -1 + ifelse(FTR == "A", get(odds_bet[3]), 0))
  } else {
    data <- data %>% 
      mutate(EVO = ref_PO*get(odds_bet[1]) - 1,
             EVU = ref_PU*get(odds_bet[2]) - 1,
             
             PnLO = -1 + ifelse(FTL == "O", get(odds_bet[1]), 0),
             PnLU = -1 + ifelse(FTL == "U", get(odds_bet[2]), 0))
  }
  
  
  bets <- transform_data(data, market) %>%
    mutate(bookie_ref = bookie_ref,
           bookie_bet = bookie_bet)
  
  return (bets)
}


# All the valid possible combinations to compare one bookmaker against the other
# Includes also instances where the bookmaker is compared to its closing line
# 420 combinations, ~ 5 sec to run each
combinations <- data.frame(bookie_ref = rep(names(odds_1X2), each = 2*length(names(odds_1X2))),
                           bookie_bet = rep(names(odds_1X2), each = 2),
                           market = c("1X2", "OU")) %>%
  filter(bookie_bet != bookie_ref) %>%
  filter(!(market == "OU" & (!(bookie_bet %in% names(odds_OU)) | !(bookie_ref %in% names(odds_OU))))) %>%
  filter(!grepl("Max", bookie_ref))


odds_comparison <- combinations %>%
  pmap(compare_odds, data = football_odds, .progress = T) %>% 
  bind_rows()


# Cached table
# odds_comparison <- readRDS("./odds_comparison.rds")

# 3 - Analyze table ----

analyze_odds <- function(comparison_table, bookies_ref, bookies_bet) {
  
  # PnL = f(EV) for all bets using bookie ref to estimate true probability of an event
  # and bookie bet to bet on the event
  EV_comparison <- comparison_table %>% 
    filter(bookie_ref %in% bookies_ref, bookie_bet %in% bookies_bet) %>%
    filter(between(EV, -0.25, 0.25)) %>%
    group_by(bookie_ref, bookie_bet) %>%
    mutate(EV_int = cut(EV, breaks=c(quantile(EV, probs = seq(0, 1, by = 0.025))), include.lowest = T)) %>%
    ungroup() %>%
    group_by(bookie_ref, bookie_bet, EV_int) %>%
    summarize(n = n(),
              EV = mean(EV), 
              PnL = mean(PnL))
  
  # A table of correlations between PnL and EV
  # Correlation of 1, means that any Profits & Losses are completely explained by EV
  EV_cors <- EV_comparison %>%
    group_by(bookie_ref, bookie_bet) %>%
    summarize(obs = sum(n),
              cor = cor(PnL, EV))
  
  
  pl_EV <- ggplot(EV_comparison, aes(x = EV, y = PnL, color = bookie_bet)) + geom_point(alpha = 0.5) + 
    geom_smooth(method = "lm", se = F) + 
    geom_abline(slope = 1, intercept = 0) + 
    geom_segment(aes(x = -0.25, xend = 0, y = 0, yend = 0), linetype = "dashed", color = "black") +
    geom_segment(aes(x = 0, xend = 0, y = -0.3, yend = 0), linetype = "dashed", color = "black") +
    facet_wrap(bookie_ref ~., labeller = label_both) +
    labs(title = "Odds Comparison, PnL ~ f(EV)", 
         x = "Expected Value", y = "Profit & Loss")
  print(pl_EV)
  
  
  # value bets defined as positive EV, cumulative profits & EV
  value_bets <- comparison_table %>% 
    filter(bookie_ref %in% bookies_ref, bookie_bet %in% bookies_bet) %>%
    filter(EV > 0) %>%
    group_split(bookie_ref, bookie_bet) %>%
    lapply(function(df) {
      df %>% transmute(NVBets = as.numeric(row.names(.)),
                       EV_total = cumsum(EV),
                       PnL_total = cumsum(PnL),
                       bookie_ref = bookie_ref, 
                       bookie_bet = bookie_bet)}) %>%
    rbindlist() %>%
    melt(measure.vars = c("PnL_total", "EV_total"))
  
  
  pl_returns <- ggplot(value_bets, aes(x = NVBets, y = value, color = bookie_bet, linetype = variable)) + 
    geom_smooth(method = "lm", formula = y ~ 0 + x) + 
    labs(title = "Cumulative ProfitnLoss and EV ~ f(Number of Value Bets)", 
         x = "Value Bets", y = "PnL|EV") +
    facet_wrap(bookie_ref ~., scales = "free", labeller = label_both) 
  print(pl_returns)
  
  return(EV_cors)
  
}

lines <- c("Bet365", "WilliamHill", "Bwin", "Pinnacle", "Interwetten", "VC", "MarketMax")
closing_lines <- c("Bet365C", "InterwettenC", "PinnacleC", "MarketAvgC", "WilliamHillC")

# 'opening' to 'opening'
cors_o2o <- analyze_odds(odds_comparison, lines, lines)
# closing to 'opening' 
cors_c2o <- analyze_odds(odds_comparison, closing_lines, lines)

cors <- rbind(cors_o2o, cors_c2o) %>% 
  group_by(bookie_ref) %>%
  summarize(cor_mean = weighted.mean(cor, obs)) %>%
  dplyr::arrange(desc(cor_mean))
print(cors)

# The efficiency of Pinnacle's Closing price is unparalleled and can be treated
# as a true estimator of the true probability of an event

# Also Pinnacle's pre-closing line is very efficient, and sometimes we can use it
# to profit in other bookies

# Distribution reshaping - opening & closing EV compared to Pinnacle's closing price
odds_comparison %>% 
  filter(bookie_ref == "PinnacleC") %>%
  filter(bookie_bet %in% names(odds_1X2)[c(1, 2, 3, 4, 5, 6, 9, 10)]) %>% 
  ggplot(aes(x = EV, y =..density.., color = bookie_bet)) +
  geom_histogram(binwidth = 0.005) +
  geom_density() + 
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  facet_wrap(bookie_bet ~. , ncol = 2) +
  coord_cartesian(xlim = c(-0.25, 0.25)) + guides(color = "none") +
  labs(title = "Expected Value distribution using Pinnacle's closing Prices",
       subtitle = "Before and at Closing odds for different bookmakers")

# Distribution is not normal, they are negatively skewed
# Try mirrored Gumbel or Dagum instead
params_normal <- odds_comparison %>%
  filter(bookie_ref == "PinnacleC") %>%
  filter(bookie_bet %in% names(odds_1X2)[c(1, 2, 3, 4, 5, 6, 9, 10)]) %>% 
  split(f = .$bookie_bet) %>%
  lapply(function(df) {params <- fitdist(df$EV, distr = "norm", method = "mle")$estimate}) %>%
  bind_rows(.id = "Bookmaker")


# Odds drift
H2H <- odds_comparison %>%
  filter( (bookie_ref == "Bet365C" & bookie_bet == "Bet365") |
            (bookie_ref == "PinnacleC" & bookie_bet == "Pinnacle") |
            (bookie_ref == "BwinC" & bookie_bet == "Bwin") |
            (bookie_ref == "InterwettenC" & bookie_bet == "Interwetten") |
            (bookie_ref == "WilliamHillC" & bookie_bet == "WilliamHill"))

# All of the prices that have some EV shorten their prices, 
# Whereas those that they don't have any value either do not change (big peak at r = 1)
# or move towards both directions
H2H %>% 
  ggplot(aes(x = Prop_bet/Prop_ref, color = EV > 0)) +
  geom_histogram(aes(y = ..density..), position = "identity", binwidth = 0.01) +
  facet_wrap(. ~ bookie_bet, labeller = label_both, nrow = 2) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(x = "Opening Probability/Closing Probability", 
       title = "Opening to closing probability for positive and negative EV") +
  theme_minimal() +
  coord_cartesian(xlim = c(0.8, 1.2)) 


# Where can we spot value bets
# Percentage of H(ome), D(raw), A(way), O(ver), U(nder) that have value
# Draw few value bets, Under higher percentage than Over
odds_comparison %>% 
  filter(bookie_ref == "PinnacleC") %>%
  filter(bookie_bet %in% lines) %>% 
  mutate(market = ifelse(Event %in% c("O", "U"), "OU", "1X2")) %>%
  ggplot(aes(x = EV > 0,  y = ..prop.., group = Event, fill = Event)) +
  geom_bar(position = "dodge", color = "black", alpha = 0.6) +
  scale_x_discrete(labels=c('Negative', "Positive")) +
  facet_wrap(bookie_bet ~.) +
  labs(title = "Perncetage of Value Bets in {H, D, A, O, U}", 
       x = "Expected Value") +
  theme_minimal()

# Using Deep Learning we could identify any systematic modelling biases in pre-closing odds

# 4 - Entropy ----

# KL divergence (https://www.perfectlynormal.co.uk/blog-kl-divergence)
# ?the KL divergence represents the amount you can win from the casino by exploiting the difference between the true probabilities 
# P and the house's false beliefs Q. The closer and Q are, the harder it is to profit from your extra knowledge.?


