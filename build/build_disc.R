# ---- Description --------------------------------------------------------------------------------

# This program identifies within-lender interest rate discontinuities for the elasticity study. 

# ---- Preliminaries ------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(ggplot2)
library(broom)
library(tidyr)
library(lfe)

# Load functions
source("./assist/functions.R")

# Load constants
source("./assist/elast_constants.R")

# ---- Load data ----------------------------------------------------------------------------------

# Load data
elast_df = get(load(
  file = "../data/elast/elast_df.Rda"
))

# ---- Wrangle data -------------------------------------------------------------------------------

# Restrict credit scores
elast_df = elast_df %>%
  filter(
    cscore >= 500
  ) %>%
  mutate(
    cscore_bin = floor(cscore / 1) * 1
  ) 

# ---- Aggregate data -----------------------------------------------------------------------------

# Aggregate data
rate_df = elast_df %>%
  group_by(
    originator,
    name,
    cscore_bin
  ) %>%
  filter(
    n() >= 50
  ) %>%
  summarise(
    mean = 100 * mean(rate, na.rm = TRUE),
    sd = 100 * sd(rate, na.rm = TRUE),
    se = 100 * sd(rate, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup() %>%
  arrange(
    originator,
    name,
    cscore_bin
  )

# ---- Data-driven discontinuities ----------------------------------------------------------------

# Differences in means
disc_df = rate_df %>%
  group_by(
    originator,
    name
  ) %>%
  filter(
    cscore_bin == (lag(cscore_bin, 1) + 1)
  ) %>% 
  mutate(
    diff = mean - lag(mean, 1),
    se_diff = sqrt(se^2 + lag(se, 1)^2)
  ) %>% 
  ungroup()

# Leads and lags
disc_df = disc_df %>%
  group_by(
    originator,
    name
  ) %>%
  mutate(
    lag_diff = lag(diff, 1),
    lag_se = lag(se_diff, 1),
    lead_diff = lead(diff, 1),
    lead_se = lead(se_diff, 1)
  ) %>%
  ungroup()

# Test statistics
disc_df = disc_df %>%
  mutate(
    t_diff = (diff - min_disc()) / se_diff,
    lag_t =  (lag_diff - min_smooth()) / lag_se,
    lead_t = (lead_diff - min_smooth()) / lead_se
  )

# Discontinuities
disc_df = disc_df %>%
  filter(
    t_diff < max_t_stat()#,
    #lag_t > qnorm(0.05),
    #lead_t > qnorm(0.05),
  )

# Remove one-off lenders
disc_df = disc_df %>%
  filter(
    originator != "SC",
    originator != "ESB",
    originator != "Fifth Third Bank"
  )

# Append identifiers
disc_df = disc_df %>%
  arrange(
    originator,
    cscore_bin
  ) %>% 
  mutate(
    threshold = cscore_bin,
    disc_id = row_number()
  )

# Export data
save(
  disc_df,
  file = "../data/elast/disc_df.Rda"
)

# ---- Alternative data-driven discontinuities ----------------------------------------------------

# Data driven RDDs



