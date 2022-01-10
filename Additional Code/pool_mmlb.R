# call relevant libraries
library(tidyverse)
library(mice)

# create function for pooling mm function

# first need to create tidy dataframe with 6 colums: term, estimate, std.error, statistic, p.value, and nobs (degrees of freedom) from MMLogit class (from GEE version)
# rows is the number of rows in the combined dataset
fitlist.mm <- function(object, rows) {
  # first get summary
  sum <- summary(object)
  # now make tidy version
  sum_tidy <- as_tibble(sum$mean.table) %>%
    add_column(term = rownames(sum$mean.table), .before = 1) %>%
    mutate(nobs = rows) %>%
    rename(estimate = Estimate,
           std.error = `Model SE`,
           statistic = `Chi Square`,
           p.value = `Pr(>Chi)`)
  sum_tidy
}

# now make glanced list with rows n.clusters, max.cluster.size, alpha,  and nobs
glanced.mm <- function(object, rows) {
  data.frame(n.clusters = unname(object$control["n_subj"]),
             max.cluster.size = unname(object$control["max_n_visit"]),
             alpha = unname(object$alpha),
             nobs = rows)
}

# barnard rubin function pulled directly from mice code https://rdrr.io/cran/mice/src/R/barnard.rubin.R
barnard.rubin <- function(m, b, t, dfcom = 999999) {
  lambda <- (1 + 1 / m) * b / t
  lambda[lambda < 1e-04] <- 1e-04
  dfold <- (m - 1) / lambda^2
  dfobs <- (dfcom + 1) / (dfcom + 3) * dfcom * (1 - lambda)
  dfold * dfobs / (dfold + dfobs)
}

pool.mm <- function(object, rows) {
  call <- paste0("pool(object = ")
  # based on pool.fitlist from https://rdrr.io/cran/mice/src/R/pool.R
  p <- map_df(object, ~ fitlist.mm(.x, rows))
  
  # we don't want group_by to change the order of the terms
  p <- p %>% mutate(term = factor(term, levels = unique(term)))
  
  pooled <- p %>%
    group_by(term) %>%
    summarise(
      m = n(),
      qbar = mean(estimate),
      ubar = mean(std.error^2),
      b = var(estimate),
      t = ubar + (1 + 1 / m) * b,
      dfcom = rows,
      df = barnard.rubin(m, b, t, dfcom),
      riv = (1 + 1 / m) * b / ubar,
      lambda = (1 + 1 / m) * b / t,
      fmi = (riv + 2 / (df + 3)) / (riv + 1)
    )
  pooled <- data.frame(pooled)
  names(pooled)[names(pooled) == "qbar"] <- "estimate"
  
  # based on https://rdrr.io/cran/mice/src/R/get.df.R
  g <- map_df(object, ~ glanced.mm(.x, rows))
  
  # finally need make mipo object to work with mice summary function
  mipo.mm <- list(m = pooled$m[1],
                  pooled = pooled,
                  glanced = g)
  class(mipo.mm) <- c("mipo", "data.frame")
  mipo.mm
}
