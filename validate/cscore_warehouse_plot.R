# ---- Description --------------------------------------------------------------------------------

# This program plots the average of warehouse months across original credit scores.  

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
  path = "../data/validation/cscore_warehouse.xlsx"
)

# ---- Plot the data ------------------------------------------------------------------------------

# Plot data
gg_cw = ggplot(
  data = plot_df 
) + 
  geom_bar(
    aes(
      x = cscore,
      y = value
    ),
    position = "dodge",
    stat = "identity",
    fill = "darkgray",
    color = "black"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      size = 12,
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(
      size = 12
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  ) + 
  labs(
    x = "Credit score",
    y = "Average warehoused months"
  ) +
  ylim(
    0,
    50
  )

# ---- Export the plot ----------------------------------------------------------------------------

# Export figure
cairo_pdf(
  filename = "../output/figures/validation/cscore_warehouse_plot.pdf",
)
gg_cw
dev.off()

# Export object
save(
  gg_cw,
  file = "../output/objects/validation/gg_cw.Rda"
)



