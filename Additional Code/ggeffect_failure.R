# making reproducible example for https://github.com/strengejacke/ggeffects/issues/222
library(geepack)
library(tidyverse)
library(mice)
library(ggeffects)
library(lme4)

df <- data.frame(id = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 , 1, 
                        1, 1, 1, 1, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 
                        6, 6, 6, 6, 6, 6),
                 y = c(1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0,
                       1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0,
                       1, 1, 1, 1, 1, 1),
                 exp = sample(c(0,1), size = 50, replace = TRUE),
                 em = c(59, NA, 60, 50, 57, 56, 52, 50, 49, 50, 57, 57, 51, 58, 60, 57,
                        57, 59, NA, 52, 60, 49, 59, 59, 60, 58, 60, NA, 60, 58, 47, 54,
                        59, 60, 59, 54, 60, NA, 60, 58, 59, 59, 60, 58, 57, NA, 60, 58,
                        60, 58))

imp <- mice(df, m = 5, seed = 10)

# this version works
mod <- geeglm(y ~ exp*em, 
              data = complete(imp, 1),
              id = id,
              family = binomial,
              corstr = "exchangeable")

ggpredict(mod, "exp")

# this version does not work
mods <- complete(imp, "all") %>%
  map(~ geeglm(y ~ exp*em, 
               data = .x,
               id = id,
               family = binomial,
               corstr = "exchangeable"))


ggpredict(mods$`1`, c("exp", "em"))

# this verison does not work either
mods1 <- with(imp, geeglm(y ~ exp*em,
                          id = id,
                          family = binomial,
                          corstr = "exchangeable"))

ggpredict(mods1$analyses[[1]], "exp")

# mods version does not work with pool_predictions either
mods %>%
  map(~ ggpredict(.x, "exp")) %>%
  pool_predictions()

# it does, however, work with glmer in lme4 despite some warnings
glmm_mod <- complete(imp, "all") %>%
  map(~ glmer(formula = y ~ exp*em + (1 | id),
              family = binomial,
              data = .x))

# pool predictions
glmm_mod %>%
  map(~ ggpredict(.x, "exp")) %>%
  pool_predictions()
