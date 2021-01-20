# ---- Description --------------------------------------------------------------------------------

# This program plots the default rates for the elasticity study. 

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
rate_df = elast_df %>%
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
    mean_12 = 100 * mean(delinq_90_12, na.rm = TRUE),
    mean_24 = 100 * mean(delinq_90_24, na.rm = TRUE),
    mean_36 = 100 * mean(delinq_90_36, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(
    originator,
    name,
    cscore_bin
  )

# Restrict loan issuers
rate_df = rate_df %>%
  filter(
    originator %in% unique(disc_df$originator)
  )

# ---- Plot the one-year default rates ------------------------------------------------------------

# Plot the data
gg1 = ggplot(
  data = rate_df
) +
  geom_point(
    aes(
      x = cscore_bin,
      y = mean_12
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
    nrow = 2,
    ncol = 3
  )

# Export results
cairo_pdf(
  filename = "../output/figures/elast/lender_default_12mo.pdf",
  width = 16,
  height = 9,
  onefile = TRUE
)
print(gg1)
dev.off()

# ---- Plot the two-year default rates ------------------------------------------------------------

# Plot the data
gg2 = ggplot(
  data = rate_df
) +
  geom_point(
    aes(
      x = cscore_bin,
      y = mean_24
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
    nrow = 2,
    ncol = 3
  )

# Export results
cairo_pdf(
  filename = "../output/figures/elast/lender_default_24mo.pdf",
  width = 16,
  height = 9,
  onefile = TRUE
)
print(gg2)
dev.off()

# ---- Plot the three-year default rates ----------------------------------------------------------

# Plot the data
gg3 = ggplot(
  data = rate_df
) +
  geom_point(
    aes(
      x = cscore_bin,
      y = mean_24
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
    nrow = 2,
    ncol = 3
  )

# Export results
cairo_pdf(
  filename = "../output/figures/elast/lender_default_36mo.pdf",
  width = 16,
  height = 9,
  onefile = TRUE
)
print(gg3)
dev.off()

