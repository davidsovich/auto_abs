# ---- Description --------------------------------------------------------------------------------

# This program plots the covariates for the elasticity study. 

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

# Load loans
elast_df = get(load(
  file = "../data/elast/elast_df.Rda"
))

# Load discontinuities
disc_df = get(load(
  file = "../data/elast/disc_df.Rda"
))

# ---- Aggregate data -----------------------------------------------------------------------------

# Aggregated data
cov_df = elast_df %>%
  filter(
    cscore >= 500
  ) %>%
  mutate(
    cscore_bin = floor(cscore / 1) * 1
  ) %>%
  group_by(
    originator,
    name,
    cscore_bin
  ) %>%
  filter(
    n() >= 50
  ) %>%
  summarise(
    Amount = mean(amount, na.rm = TRUE),
    Payment = mean(scheduled_pmt, na.rm = TRUE),
    Months = mean(months, na.rm = TRUE),
    LTV = mean(ltv, na.rm = TRUE),
    Value = mean(value, na.rm = TRUE),
    New = mean(new_vehicle, na.rm = TRUE),
    Warehouse = mean(whouse, na.rm = TRUE),
    Income = mean(income, na.rm = TRUE),
    CoSign = mean(cosign, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(
    originator,
    name,
    cscore_bin
  )

# Restrict the lenders
cov_df = cov_df %>%
  filter(
    originator %in% unique(disc_df$originator)
  ) %>%
  gather(
    Amount,
    Payment,
    Months,
    LTV,
    Value, 
    New,
    Warehouse,
    Income,
    CoSign,
    key = "variable",
    value = "value"
  )

# ---- Plot the covariates ------------------------------------------------------------------------

# Loop over covariates
for(x in unique(cov_df$variable)) {

  # Plot the data
  gg1 = ggplot(
    data = filter(cov_df, variable == x)
  ) +
    geom_point(
      aes(
        x = cscore_bin,
        y = value
      )
    ) + 
    theme_bw() + 
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1, 
        size = 12
      ),
      axis.text.y = element_text(
        size = 12
      ),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = NA, color = "black"),
      strip.text.x = element_text(
        size = 14
      )
    ) + 
    labs(
      x = "",
      y = ""
    ) +
    geom_vline(
      data = disc_df,
      mapping = aes(xintercept = cscore_bin),
      color = "black",
      linetype = "dashed"
    ) +
    facet_wrap(
      name ~ .,
      scales = "free",
    )
  
  # Export results
  cairo_pdf(
    filename = paste0("../output/figures/elast/lender_", x, "_discont.pdf"),
    width = 16,
    height = 9,
    onefile = TRUE
  )
  print(gg1)
  dev.off()

}