# ---- Description --------------------------------------------------------------------------------

# This program plots the density for the elasticity study. 

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

# ---- Five-point aggregation of data -------------------------------------------------------------

# Aggregated data
den5_df = elast_df %>%
  filter(
    cscore >= 500
  ) %>%
  mutate(
    cscore_bin = floor(cscore / 5) * 5
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
    num_obs = n()
  ) %>%
  ungroup() %>%
  group_by(
    originator,
    name
  ) %>%
  mutate(
    pct_obs = 100 * (num_obs / sum(num_obs))
  ) %>%
  ungroup() %>%
  arrange(
    originator,
    name,
    cscore_bin
  )
  
# Restrict the lenders
den5_df = den5_df %>%
  filter(
    originator %in% unique(disc_df$originator)
  )

# ---- Ten-point aggregation of data --------------------------------------------------------------

# Aggregated data
den10_df = elast_df %>%
  filter(
    cscore >= 500
  ) %>%
  mutate(
    cscore_bin = floor(cscore / 10) * 10
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
    num_obs = n()
  ) %>%
  ungroup() %>%
  group_by(
    originator,
    name
  ) %>%
  mutate(
    pct_obs = 100 * (num_obs / sum(num_obs))
  ) %>%
  ungroup() %>%
  arrange(
    originator,
    name,
    cscore_bin
  )

# Restrict the lenders
den10_df = den10_df %>%
  filter(
    originator %in% unique(disc_df$originator)
  )

# ---- Plot the five-point histogram --------------------------------------------------------------

# Plot the data
gg1 = ggplot(
  data = den5_df
) +
  geom_bar(
    aes(
      x = cscore_bin,
      y = num_obs
    ),
    stat = "identity",
    color = "black", 
    fill = "gray"
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
    nrow = 2,
    ncol = 3
  )

# Export results
cairo_pdf(
  filename = "../output/figures/elast/lender_density_five_point.pdf",
  width = 16,
  height = 9,
  onefile = TRUE
)
print(gg1)
dev.off()

# ---- Plot the ten-point histogram ---------------------------------------------------------------

# Plot the data
gg2 = ggplot(
  data = den5_df
) +
  geom_bar(
    aes(
      x = cscore_bin,
      y = num_obs
    ),
    stat = "identity",
    color = "black", 
    fill = "gray"
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
    nrow = 2,
    ncol = 3
  )

# Export results
cairo_pdf(
  filename = "../output/figures/elast/lender_density_ten_point.pdf",
  width = 16,
  height = 9,
  onefile = TRUE
)
print(gg2)
dev.off()


