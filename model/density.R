# ---- Description --------------------------------------------------------------------------------

# This program performs the density tests for the elasticity study. 

# ---- Preliminaries ------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(ggplot2)
library(rddensity)
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

# ---- Parametric plot of density -----------------------------------------------------------------

# Aggregate data
den_df = elast_df %>%
  group_by(
    dist
  ) %>%
  filter(
    n() >= 50
  ) %>%
  summarise(
    num_obs = n()
  ) %>%
  ungroup() %>%
  mutate(
    pct_obs = 100 * (num_obs / sum(num_obs))
  ) %>% 
  arrange(
    dist
  )

# Plot the result
gg1 = srd_plot(
  df = den_df,
  y = "pct_obs",
  x = "dist",
  p_l = 2,
  p_r = 2,
  xlab = "Centered credit score",
  ylab = "Fraction of loans"
) + 
  ylim(
    c(0, 5)
  )

# Export plot
cairo_pdf(
  filename = "../output/figures/elast/param_density.pdf",
  onefile = TRUE
)
print(gg1)
dev.off()

# --- Tests of manipulation in the forcing variable -----------------------------------------------

# Cattaneo et al. (2020) test
c_test = rddensity(
  X = elast_df$dist,
  massPoints = FALSE
)

# McCrary (2008) test
m_test = DCdensity(
  runvar = elast_df$dist
)

# Frandsen (2017) test





