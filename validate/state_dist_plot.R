# ---- Description --------------------------------------------------------------------------------

# This program plots the distribution of loans across states. 

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
  path = "../data/validation/state_dist.xlsx"
)

# ---- Plot state distribution --------------------------------------------------------------------

# Plot the data
gg1 = ggplot(
  data = plot_df
) + 
  geom_bar(
    aes(
      x = state,
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
      size = 12,
      angle = 90
    ),
    axis.text.y = element_text(
      size = 12
    ),
    axis.title.y = element_text(
      size = 12
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  ) + 
  labs(
    x = "",
    y = "Fraction of borrowers"
  )

# Export figure
cairo_pdf(
  filename = "../output/figures/validation/state_dist.pdf",
  width = 16,
  height = 9,
  onefile = TRUE
)
gg1
dev.off()
