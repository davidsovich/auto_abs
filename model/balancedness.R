# ---- Description --------------------------------------------------------------------------------

# This program estimates the covariate balance regressions for the elasticity study. 

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

# ---- Parametric plots of covariate balance ------------------------------------------------------

# Aggregate data
cov_df = elast_df %>%
  group_by(
    dist
  ) %>%
  filter(
    n() >= 50
  ) %>%
  summarise(
    Amount = mean(amount, na.rm = TRUE),
    #Payment = mean(scheduled_pmt, na.rm = TRUE),
    Months = mean(months, na.rm = TRUE),
    LTV = mean(ltv, na.rm = TRUE),
    Value = mean(value, na.rm = TRUE),
    New = mean(new_vehicle, na.rm = TRUE),
    ModelYear = mean(model_year, na.rm = TRUE),
    Warehouse = mean(whouse, na.rm = TRUE),
    Income = mean(income, na.rm = TRUE),
    Uwrite = mean(uwrite, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  gather(
    -dist,
    key = "variable",
    value = "value"
  ) %>% 
  arrange(
    variable,
    dist
  )

# Plot the result
gg1 = srd_plot(
  df = cov_df,
  y = "value",
  x = "dist",
  p_l = 2,
  p_r = 2,
  xlab = "Centered credit score",
  ylab = ""
) + 
  facet_wrap(
    variable ~ .,
    scales = "free"
  )

# Export plot
cairo_pdf(
  filename = "../output/figures/elast/param_balanced.pdf",
  onefile = TRUE,
  width = 16,
  height = 9
)
print(gg1)
dev.off()

# ---- Baseline parametric models -----------------------------------------------------------------  

# Declare storage
base_df = NULL

# Income
base_df[[1]] = srd(
  df = elast_df,
  y = "income",
  x = "dist",
  d = "treated",
  p = p_choice(),
  h = h_choice(disc_df),
  fe = fe2(),
  cluster = clusters1()
)

# Loan amount
base_df[[2]] = srd(
  df = elast_df,
  y = "amount",
  x = "dist",
  d = "treated",
  p = p_choice(),
  h = h_choice(disc_df),
  fe = fe2(),
  cluster = clusters1()
)

# Co-sign
base_df[[3]] = srd(
  df = elast_df,
  y = "cosign",
  x = "dist",
  d = "treated",
  p = p_choice(),
  h = h_choice(disc_df),
  fe = fe2(),
  cluster = clusters1()
)

# Loan months
base_df[[4]] = srd(
  df = elast_df,
  y = "months",
  x = "dist",
  d = "treated",
  p = p_choice(),
  h = h_choice(disc_df),
  fe = fe2(),
  cluster = clusters1()
)

# LTV
base_df[[5]] = srd(
  df = elast_df,
  y = "ltv",
  x = "dist",
  d = "treated",
  p = p_choice(),
  h = h_choice(disc_df),
  fe = fe2(),
  cluster = clusters1()
)

# Vehicle value
base_df[[6]] = srd(
  df = elast_df,
  y = "value",
  x = "dist",
  d = "treated",
  p = p_choice(),
  h = h_choice(disc_df),
  fe = fe2(),
  cluster = clusters1()
)

# New vehicle
base_df[[7]] = srd(
  df = elast_df,
  y = "new_vehicle",
  x = "dist",
  d = "treated",
  p = p_choice(),
  h = h_choice(disc_df),
  fe = fe2(),
  cluster = clusters1()
)

# Model year
base_df[[8]] = srd(
  df = elast_df,
  y = "model_year",
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
  manual_var_names = rep(19, 8)
)

# Export results
write_csv(
  base_df,
  path = "../output/tables/elast/base_param_balanced.csv"
)