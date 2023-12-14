Roulette Strategy
================
Thanos Livanis
13/12/2023

**Roulette Simulation**  

*Properties of Roulette Spinning*  
- Profit & Loss distribution  
- Probability to make a profit  
- Longshots & margin applied

Simulation of *Gambler’s Ruin* problem under different betting
strategies

## Roulette

### PnL Distribution

``` r
rm(list = ls())

library(tidyverse)
library(gridExtra)
library(ggthemr)

options(scipen = 999)


# types of roulette bets with different Payouts
roulette <- tribble(
  ~bet,       ~n,   ~payout,
  "Straight",  1,    35,  
  "Split",     2,    17,
  "Street",    3,    11,
  "Corner",    4,    8,
  "Six Line",  6,    5,
  "Column",    12,   2,
  "Red",       18,   1)

roulette <- bind_rows(roulette %>% mutate(type = "European"), 
                      roulette %>% mutate(type = "American")) %>%
  relocate(type, .before = bet) %>%
  mutate(prop = ifelse(type == "European", n/37, n/38), 
         EV = prop*(payout + 1) - 1)
```

``` r
spin_wheel <- function(roulette_type, bet_type, nspins){
  
  params <- as.list(environment())
  
  id <- which(roulette$type == roulette_type & roulette$bet == bet_type)
  prop <- roulette$prop[id]
  payout <- roulette$payout[id]
  
  spins = rbinom(nspins, 1, prop)
  
  list(PnL = sum(ifelse(spins, payout, -1))) %>% bind_cols(params)
}


returns <- expand.grid(roulette_type = c("European", "American"),
                       bet_type = unique(roulette$bet),
                       trials = 1000) %>%
  pmap(function(roulette_type, bet_type, trials) {
    replicate(5000, spin_wheel(roulette_type, bet_type, trials), simplify = F) %>% bind_rows()}) %>% bind_rows()
```

``` r
ggthemr("flat")

pl_density <- returns %>% filter(roulette_type == "European") %>%
  ggplot(aes(x = PnL, color = bet_type)) +
  geom_density(adjust = 1.5, show.legend = F) + 
  coord_cartesian(xlim = c(-500, 500)) 

pl_box <- returns %>% filter(roulette_type == "European") %>%
  ggplot(aes(x = bet_type, y = PnL, fill = bet_type)) +
  geom_boxplot() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


grid.arrange(pl_density, pl_box, ncol=2, top = "Profit & Loss distribution after 1000 European Roulette Spins")
```

![](roulette_strategy_files/figure-gfm/pnl_distribution-1.png)<!-- -->

### Profit Probability and Margin

**Theoretical probability to make a profit \~ Bet, Spins**

If we spin the Roulette N times and win M bets:
*P**n**L* = *M* \* *p**a**y**o**u**t* − (*N*−*M*). Thus:
$M > \\frac{N}{payout + 1}$  
*P**r**o**f**i**t**P**r**o**b**a**b**i**l**i**t**y* = *P*(*X*≥⌈*M*⌉) = pbinom(⌊*M*⌋,*S**p**i**n**s*,*π*,*l**o**w**e**r*.*t**a**i**l*=*F*)

``` r
prop_profit <- function(roulette_type, bet_type, nspins) {
  
  params <- as.list(environment())
  
  id <- which(roulette$type == roulette_type & roulette$bet == bet_type)
  prop <- roulette$prop[id]
  payout <- roulette$payout[id]
  
  list(p_profit = pbinom(floor(nspins/(payout + 1)), nspins, prop, lower.tail = F)) %>%
    bind_cols(params)
  
}

ggthemr("flat")

expand.grid(bet_type = unique(roulette$bet),
            roulette_type = c("European", "American"),
            nspins = seq(100, 2000, 200)) %>%
  pmap_dfr(prop_profit) %>%
  ggplot(aes(x = nspins, y = p_profit, color = bet_type, shape = roulette_type)) +
  geom_point() + 
  facet_wrap(roulette_type ~.) +
  guides(color = guide_legend(title = "Bet"), shape = "none") +
  labs(x = "Spins", y = "Profit Probability", title = "Theoritical Probability to make a profit")
```

![](roulette_strategy_files/figure-gfm/profit-1.png)<!-- -->

Suppose that we bet on a single number (Straight Bet) in a European
Roulette. The decimal odds are 36, fair odds are 37, and the fair
probability is 1/37. The rest of the numbers constitute a *compound
event*, with decimal odds, fair odds and fair probability 1, 37/36 and
36/37 respectively. The margin M is 1/36.

Thus fair odds of {37/36, 37} are mapped to roulette odds {1, 36},
$rouletteOdds > \\frac{fairOdds}{1 + M}$ 

**Contrary to Sports betting there isn’t any longshot bias.** Typically
in Sports betting a margin of 1/36 applied to fair probabilities of
(1/37, 36/37), would result in odds of {1.016, 22.63} using a method
called odds ratio. 

Let’s visualize how the margin is applied to a probability event. The
margin of an Event i with odds of o and probability p is:
$M_i = \\frac{1}{o} - p$. We will use the [implied
package](https://opisthokonta.net/?p=1797) to convert fair probabilities
to bookmaker’s odds by applying sequentially a margin of 1/36 and 2/36,
as in the case of European and American Roulette respectively.

``` r
ggthemr("flat")

library(implied)
sport_odds <- function(margin = 1/36) {
  
  # Generate grid of fair probabilities - all possible combination with step 0.1
  # can't allocate enough memory if events > 3| consider increasing step
  fair_props <- do.call(expand.grid, replicate(2, seq(0.02, 0.98, 0.04), simplify = FALSE)) %>% 
    filter(rowSums(.) == 1) %>% setNames(paste0("P", 1:2))
  
  bookmakers_odds  <- implied_odds(fair_props, "or", margin) %>% .[["odds"]] %>%
    as.data.frame() %>% setNames(paste0("O", 1:2))
  
  odds <- bind_cols(fair_props, bookmakers_odds) %>% select(P1, O1) %>%
    filter(P1 <= 0.5) %>% mutate(M = margin)  
  
  return (odds)
}

# method jsd is not supported with 'implied_odds'
# additive method seems to produce identical results with wpo, excluding both
odds <- map_dfr(list(1/36, 2/36), sport_odds) 

# Applied Margin
ggplot() +
  geom_point(data = roulette, aes(x = prop, y = 1/(payout + 1) - prop, color = bet, shape = type)) + 
  geom_point(data = odds, aes(x = P1, y = 1/O1 - P1), color = "black", shape = 3) +
  geom_abline(intercept = 0, slope = 1/36, linetype = 1) + 
  geom_abline(intercept = 0, slope = 2/36, linetype = 2) + 
  scale_shape_manual(values = c("American" = 2, "European" = 16, "Sports" = 3)) +
  labs(x = "Fair Probabillity", y = "Applied Margin", title = "Applied Margin in Roulette & Sports Betting")
```

![](roulette_strategy_files/figure-gfm/applied_margin-1.png)<!-- -->

Thus there is a much higher chance to profit from long shots due to
higher standard deviation returns, but obviously we can suffer heavier
losses.

## II. Gamber’s Ruin in Roulette

We will use a variation of the classic Gambler’s Ruin problem under
different betting strategies. In the classic scenario, a gambler starts
with an initial capital and on each game, the gambler wins $1 with
probability p or loses $1 with probability q = 1 − p. The gambler will
stop playing if either N dollars are accumulated or all money has been
lost.  

In our case the Gambler starts with an initial Capital & stops until he
is ruined or reaches his target. He has the option to bet on an American
or European Roulette and can choose to bet on a Straight, Split, Red
etc. There are three betting strategies he can choose from:

**1. Default Strategy**  
The gambler makes a constant bet, say $1  

**2. Martingale**  
The gambler starting with $1, tries to cover all previous loses. The
strategy is typical for even odds, where we double our initial stake,
but can be extended to uneven odds as well.

**3. Bold Strategy**  
The gambler bets his entire capital.

All betting strategies are limited by the current bank (we can’t bet
more than we have), we can’t bet more than the Casino Limit, and most
importantly we never bet more money than we need to reach our target. 
Thus the bold strategy doesn’t necessarily mean we always put all money.
If our current bank is 80, our target is 100 and we bet on red, we will
place 20$
