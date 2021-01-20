# ---- Description --------------------------------------------------------------------------------

# This program estimates the instrumental variables regressions for the elasticity study. 

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

# ---- Baseline parametric models -----------------------------------------------------------------  

# Declare storage
base_df = NULL

# Baseline model (h = 10)
base_df[[1]] = frd(
  df = elast_df,
  y = "delinq_90_24",
  v = "rate", 
  x = "dist",
  d = "treated",
  p = p_choice(),
  h = 10,
  fe = fe1(),
  cluster = clusters1()
)

# Alternative model (h = 10)
base_df[[2]] = frd(
  df = elast_df,
  y = "delinq_90_24",
  v = "rate", 
  x = "dist",
  d = "treated",
  p = p_choice(),
  h = 10,
  fe = fe2(),
  cluster = clusters1()
)

# Baseline model (h = 15)
base_df[[3]] = frd(
  df = elast_df,
  y = "delinq_90_24",
  v = "rate", 
  x = "dist",
  d = "treated",
  p = p_choice(),
  h = 15,
  fe = fe1(),
  cluster = clusters1()
)


# Alternative model (h = 15)
base_df[[4]] = frd(
  df = elast_df,
  y = "delinq_90_24",
  v = "rate", 
  x = "dist",
  d = "treated",
  p = p_choice(),
  h = 15,
  fe = fe2(),
  cluster = clusters1()
)

# Baseline model (h = 19)
base_df[[5]] = frd(
  df = elast_df,
  y = "delinq_90_24",
  v = "rate", 
  x = "dist",
  d = "treated",
  p = p_choice(),
  h = h_choice(disc_df),
  fe = fe1(),
  cluster = clusters1()
)

# Alternative model (h = 19)
base_df[[6]] = frd(
  df = elast_df,
  y = "delinq_90_24",
  v = "rate", 
  x = "dist",
  d = "treated",
  p = p_choice(),
  h = h_choice(disc_df),
  fe = fe2(),
  cluster = clusters1()
)


# Regression table
base_df = reg_table(
  base_df,
  round_val = 5,
  manual_var_names = rep(c(10, 15, 19), 2)
)

# Export results
write_csv(
  base_df,
  path = "../output/tables/elast/base_param_iv.csv"
)

# ---- Robust parametric models -------------------------------------------------------------------

# Estimate models
robust_df = frd_matrix(
  df = elast_df,
  y = "delinq_90_24",
  v = "rate",
  x = "dist",
  d = "treated",
  p_vec = c(1:4),
  h_vec = thresh_values(disc_df),
  fe = fe2(),
  cluster = clusters1()
)

# Wrangle data
robust_df = robust_df %>%
  select(
    -se
  ) %>%
  gather(
    coef,
    t_stat,
    key = "stat",
    value = "value"
  ) %>%
  spread(
    key = polynomial,
    value = value
  )

# Export results
write_csv(
  robust_df,
  path = "../output/tables/elast/robust_param_iv_24.csv"
)

# ---- Parametric models with control variables ---------------------------------------------------  

# Declare storage
base_df = NULL

# Default model (h = 10)
base_df[[1]] = frd(
  df = elast_df,
  y = "delinq_90_24",
  v = "rate", 
  x = "dist",
  d = "treated",
  p = p_choice(),
  h = 10,
  fe = fe2(),
  cluster = clusters1(),
  ctrls = c("income")
)

# Default model (h = 10)
base_df[[2]] = frd(
  df = elast_df,
  y = "delinq_90_24",
  v = "rate", 
  x = "dist",
  d = "treated",
  p = p_choice(),
  h = 10,
  fe = fe2(),
  cluster = clusters1(),
  ctrls = c("income", "value", "new_vehicle")
)

# Default model (h = 15)
base_df[[3]] = frd(
  df = elast_df,
  y = "delinq_90_24",
  v = "rate", 
  x = "dist",
  d = "treated",
  p = p_choice(),
  h = 15,
  fe = fe2(),
  cluster = clusters1(),
  ctrls = c("income")
)

# Default model (h = 15)
base_df[[4]] = frd(
  df = elast_df,
  y = "delinq_90_24",
  v = "rate", 
  x = "dist",
  d = "treated",
  p = p_choice(),
  h = 15,
  fe = fe2(),
  cluster = clusters1(),
  ctrls = c("income", "value", "new_vehicle")
)

# Default model (h = 19)
base_df[[5]] = frd(
  df = elast_df,
  y = "delinq_90_24",
  v = "rate", 
  x = "dist",
  d = "treated",
  p = p_choice(),
  h = h_choice(disc_df),
  fe = fe2(),
  cluster = clusters1(),
  ctrls = c("income")
)

# Default model (h = 19)
base_df[[6]] = frd(
  df = elast_df,
  y = "delinq_90_24",
  v = "rate", 
  x = "dist",
  d = "treated",
  p = p_choice(),
  h = h_choice(disc_df),
  fe = fe2(),
  cluster = clusters1(),
  ctrls = c("income", "value", "new_vehicle")
)

# Regression table
base_df = reg_table(
  base_df,
  round_val = 5,
  manual_var_names = c(10, 10, 15, 15, 19, 19)
)

# Export results
write_csv(
  base_df,
  path = "../output/tables/elast/controls_param_iv.csv"
)


