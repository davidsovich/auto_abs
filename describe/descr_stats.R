# ---- Description --------------------------------------------------------------------------------

# This program describes the loans for the elasticity study. 

# ---- Preliminaries ------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(ggplot2)
library(broom)
library(tidyr)
library(lfe)

# Load functions
source("./assist/functions.R")
source("./assist/descr_stats.R")

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

# Scale variables
elast_df = elast_df %>%
  mutate(
    rate = 100 * rate
  )

# ---- Descriptive statistics ---------------------------------------------------------------------

# Wide window (h = 19)
{

  # Calculate statistics 
  descr_df = descr_stats_tc(
    df = elast_df,
    features = c(
      "amount",
      "rate",
      "scheduled_pmt",
      "months",
      "ltv",
      "value",
      "model_year",
      "new_vehicle",
      "whouse", 
      "cscore",
      "income",
      "cosign",
      "uwrite",
      "delinq_90_12",
      "delinq_90_18",
      "delinq_90_24",
      "delinq_90_30",
      "delinq_90_36"
    ),
    cluster = clusters1()
  )
  
  # Export results
  write_csv(
    descr_df,
    path = "../output/tables/elast/descr_20.csv"
  )

}

# Narrow window (h = 5)
{
  
  # Calculate statistics 
  descr_df = descr_stats_tc(
    df = filter(elast_df, abs(dist) <= 5),
    features = c(
      "amount",
      "rate",
      "scheduled_pmt",
      "months",
      "ltv",
      "value",
      "model_year",
      "new_vehicle",
      "whouse", 
      "cscore",
      "income",
      "cosign",
      "uwrite",
      "delinq_90_12",
      "delinq_90_18",
      "delinq_90_24",
      "delinq_90_30",
      "delinq_90_36"
    ),
    cluster = clusters1()
  )
  
  # Export results
  write_csv(
    descr_df,
    path = "../output/tables/elast/descr_5.csv"
  )
  
}

