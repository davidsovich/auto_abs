# ---- Description --------------------------------------------------------------------------------

# This program estimates the cross-sectional regressions for the elasticity study. 

# ---- Preliminaries ------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(ggplot2)
library(rdrobust)
library(broom)
library(tidyr)
library(lfe)

# Load functions
source("./assist/functions.R")
source("./assist/regressions.R")

# Load constants
source("./assist/elast_constants.R")

# ---- Load data ----------------------------------------------------------------------------------

# Load loans
elast_df = get(load(
  file = "../data/elast/elast_df.Rda"
))

# Load discontinuities
disc_df = get(load(
  file = "../data/elast/disc_df.Rda"
))

# ---- Wrangle data -------------------------------------------------------------------------------

# Append discontinuities
elast_df = elast_df %>%
  inner_join(
    y = disc_df %>%
      select(
        name,
        threshold,
        disc_id
      ),
    by = c("name" = "name")
  ) %>%
  mutate(
    dist = cscore - threshold
  )

# Restrict distance (keep replicates)
elast_df = elast_df %>%
  filter(
    abs(dist) <= max(thresh_values(disc_df))
  ) %>%
  mutate(
    treated = as.numeric(dist >= 0)
  )

# ---- Cross-sectional cuts -----------------------------------------------------------------------

# Pre-determined cuts
elast_df = elast_df %>%
  mutate(
    income_cut = as.numeric(ntile(income, 2) == 1),
    cscore_cut = as.numeric(ntile(cscore, 2) == 1),
    ltv_cut = as.numeric(ntile(ltv, 2) == 2)
  )

# ---- Cross-sectional parametric models ----------------------------------------------------------

# Declare storage
base_df = NULL

# Income model (h = 10)
base_df[[1]] = frd_cs(
  df = elast_df,
  y = "delinq_90_24",
  v = "rate", 
  x = "dist",
  d = "treated",
  cs = "income_cut",
  p = p_choice(),
  h = 10,
  fe = fe2(),
  cluster = clusters1()
)

# Income model (h = 15)
base_df[[2]] = frd_cs(
  df = elast_df,
  y = "delinq_90_24",
  v = "rate", 
  x = "dist",
  d = "treated",
  cs = "income_cut",
  p = p_choice(),
  h = 15,
  fe = fe2(),
  cluster = clusters1()
)

# Income model (h = 19)
base_df[[3]] = frd_cs(
  df = elast_df,
  y = "delinq_90_24",
  v = "rate", 
  x = "dist",
  d = "treated",
  cs = "income_cut",
  p = p_choice(),
  h = h_choice(disc_df),
  fe = fe2(),
  cluster = clusters1()
)

# Credit score model (h = 10)
base_df[[4]] = frd_cs(
  df = elast_df,
  y = "delinq_90_24",
  v = "rate", 
  x = "dist",
  d = "treated",
  cs = "cscore_cut",
  p = p_choice(),
  h = 10,
  fe = fe2(),
  cluster = clusters1()
)

# Credit score model (h = 15)
base_df[[5]] = frd_cs(
  df = elast_df,
  y = "delinq_90_24",
  v = "rate", 
  x = "dist",
  d = "treated",
  cs = "cscore_cut",
  p = p_choice(),
  h = 15,
  fe = fe2(),
  cluster = clusters1()
)

# Credit score model (h = 19)
base_df[[6]] = frd_cs(
  df = elast_df,
  y = "delinq_90_24",
  v = "rate", 
  x = "dist",
  d = "treated",
  cs = "cscore_cut",
  p = p_choice(),
  h = h_choice(disc_df),
  fe = fe2(),
  cluster = clusters1()
)

# LTV model (h = 10)
base_df[[7]] = frd_cs(
  df = elast_df,
  y = "delinq_90_24",
  v = "rate", 
  x = "dist",
  d = "treated",
  cs = "ltv_cut",
  p = p_choice(),
  h = 10,
  fe = fe2(),
  cluster = clusters1()
)

# LTV model (h = 15)
base_df[[8]] = frd_cs(
  df = elast_df,
  y = "delinq_90_24",
  v = "rate", 
  x = "dist",
  d = "treated",
  cs = "ltv_cut",
  p = p_choice(),
  h = 15,
  fe = fe2(),
  cluster = clusters1()
)

# LTV model (h = 19)
base_df[[9]] = frd_cs(
  df = elast_df,
  y = "delinq_90_24",
  v = "rate", 
  x = "dist",
  d = "treated",
  cs = "ltv_cut",
  p = p_choice(),
  h = h_choice(disc_df),
  fe = fe2(),
  cluster = clusters1()
)

# F-tests of equality
F_df = tibble::tibble(
  F_stat = c(
    unlist(lapply(
      base_df[1:3],
      FUN = function(x) {
        car::linearHypothesis(
          x,
          "`rate(fit)` + `I(income_cut * rate)(fit)` = 0"
        )$`Pr(>Chisq)`[2]
      }
    )),
    unlist(lapply(
      base_df[4:6],
      FUN = function(x) {
        car::linearHypothesis(
          x,
          "`rate(fit)` + `I(cscore_cut * rate)(fit)` = 0"
        )$`Pr(>Chisq)`[2]
      }
    )),
    unlist(lapply(
      base_df[7:9],
      FUN = function(x) {
        car::linearHypothesis(
          x,
          "`rate(fit)` + `I(ltv_cut * rate)(fit)` = 0"
        )$`Pr(>Chisq)`[2]
      }
    ))
  ),
  Cut = c(rep("income", 3), rep("cscore", 3), rep("ltv", 3)) 
)

# Export results
write_csv(
  F_df,
  path = "../output/tables/elast/F_channel_iv.csv"
)

# Regression table
base_df = reg_table(
  base_df,
  round_val = 5,
  manual_var_names = rep(c(10, 15, 20), 3)
)

# Export results
write_csv(
  base_df,
  path = "../output/tables/elast/base_channel_iv.csv"
)




