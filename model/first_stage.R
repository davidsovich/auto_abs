# ---- Description --------------------------------------------------------------------------------

# This program estimates the first-stage regressions for the elasticity study. 

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

# ---- Parametric plot of discontinuity -----------------------------------------------------------

# Aggregate data
rate_df = elast_df %>%
  group_by(
    dist
  ) %>%
  filter(
    n() >= 50
  ) %>%
  summarise(
    mean =  100 * mean(rate, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  arrange(
    dist
  )

# Plot the result
gg1 = srd_plot(
  df = rate_df,
  y = "mean",
  x = "dist",
  p_l = 2,
  p_r = 2,
  xlab = "Centered credit score",
  ylab = "Interest rate * 100"
)

# Export plot
cairo_pdf(
  filename = "../output/figures/elast/param_discont.pdf",
  onefile = TRUE
)
gg1
dev.off()

# ---- Parametric plot of each discontinuity ------------------------------------------------------

# Aggregate data
rate_df = elast_df %>%
  mutate(
    disc_name = paste0(name, " ", threshold)
  ) %>% 
  group_by(
    disc_name,
    dist
  ) %>%
  filter(
    n() >= 50
  ) %>%
  summarise(
    mean =  100 * mean(rate, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  arrange(
    dist
  )

# Plot the result
gg2 = srd_plot(
  df = rate_df,
  y = "mean",
  x = "dist",
  p_l = 2,
  p_r = 2,
  xlab = "Centered credit score",
  ylab = "Interest rate * 100"
) + 
  facet_wrap(
    disc_name ~ .,
    scales = "free"
  )

# Export plot
cairo_pdf(
  filename = "../output/figures/elast/param_discont_lender.pdf",
  width = 16,
  height = 9,
  onefile = TRUE
)
gg2
dev.off()

# ---- Baseline parametric models -----------------------------------------------------------------  

# Declare storage
base_df = NULL

# Baseline model (h = 10)
base_df[[1]] = srd(
  df = elast_df,
  y = "rate",
  x = "dist",
  d = "treated",
  p = p_choice(),
  h = 10,
  fe = fe1(),
  cluster = clusters1()
)

# Alternate model (h = 10)
base_df[[2]] = srd(
  df = elast_df,
  y = "rate",
  x = "dist",
  d = "treated",
  p = p_choice(),
  h = 10,
  fe = fe2(),
  cluster = clusters1()
)

# Baseline model (h = 15)
base_df[[3]] = srd(
  df = elast_df,
  y = "rate",
  x = "dist",
  d = "treated",
  p = p_choice(),
  h = 15,
  fe = fe1(),
  cluster = clusters1()
)

# Alternate model (h = 15)
base_df[[4]] = srd(
  df = elast_df,
  y = "rate",
  x = "dist",
  d = "treated",
  p = p_choice(),
  h = 15,
  fe = fe2(),
  cluster = clusters1()
)

# Baseline model (h = 19)
base_df[[5]] = srd(
  df = elast_df,
  y = "rate",
  x = "dist",
  d = "treated",
  p = p_choice(),
  h = h_choice(disc_df),
  fe = fe1(),
  cluster = clusters1()
)

# Alternate model (h = 19)
base_df[[6]] = srd(
  df = elast_df,
  y = "rate",
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
  manual_var_names = c(10, 10, 15, 15, 19, 19),
  round_val = 5
)

# Export results
write_csv(
  base_df,
  path = "../output/tables/elast/base_param_first.csv"
)

# ---- Robust parametric models -------------------------------------------------------------------

# Estimate models
robust_df = srd_matrix(
  df = elast_df,
  y = "rate",
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
  path = "../output/tables/elast/robust_param_first.csv"
)






