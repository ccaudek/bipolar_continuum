# Compute the correlation between CS and UCS of Trait Self-Compassion


library(tidyverse)
library(here)
library(brms)


trait_sc <- rio::import(
  here::here(
    "data", "quest", "piel", "quest_scales", "scs_scores.csv"
  )
)

trait_sc <- trait_sc |> 
  mutate(
    CS = self_kindness + common_humanity + mindfulness,
    UCS = self_judgment + isolation + over_identification
  )

trait_sc$CS <- scale(trait_sc$CS) |> as.numeric()
trait_sc$UCS <- scale(trait_sc$UCS) |> as.numeric()


cor(trait_sc$CS, trait_sc$UCS)

plot(trait_sc$CS, trait_sc$UCS)

f1 <-
  brm(data = trait_sc, 
      family = student(),
      backend = "cmdstanr",
      UCS ~ 0 + CS,
      prior = c(
                prior(normal(0, 1), class = b),
                prior(normal(0, 1), class = sigma)),
      chains = 4, cores = 4, 
      seed = 1)

pp_check(f1)

summary(f1, prob=0.89)
