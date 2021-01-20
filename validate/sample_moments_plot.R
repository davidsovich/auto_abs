# ---- Description --------------------------------------------------------------------------------

# This program plots the means and standard deviations of 2019 Q4 loans.  

# ---- Preliminaries ------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(ggplot2)
library(readxl)
library(tidyr)
library(lfe)

# Load functions
source("./assist/functions.R")

# Load constants
source("./assist/constants.R")

# ---- Load data ----------------------------------------------------------------------------------

# Load data
plot_df = read_excel(
  path = "../data/validation/mean_sd.xlsx"
)

# ---- Plot the data ------------------------------------------------------------------------------

# Plot the data
gg1 = ggplot(
  data = plot_df
) +
  geom_bar(
    aes(
      x = stat,
      y = value,
      fill = series
    ),
    position = "dodge",
    stat = "identity",
    color = "black"
  ) + 
  scale_fill_manual(
    values = c("white", "darkgray")
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
    ),
    legend.position = "top"
  ) + 
  labs(
    x = "",
    y = ""
  ) +
  facet_wrap(
    variable ~ .,
    scales = "free",
    nrow = 2,
    ncol = 4
  )

# Export results
cairo_pdf(
  filename = "../output/figures/elast/sample_moments.pdf",
  width = 16,
  height = 9,
  onefile = TRUE
)
gg1
dev.off()