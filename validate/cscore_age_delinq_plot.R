# ---- Description --------------------------------------------------------------------------------

# This program plots the percent of 2019 Q4 balances 90+ DPD across original credit scores and 
# 2019 Q4 loan age. 

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
  path = "../data/validation/cscore_age_delinq.xlsx"
)

# ---- Plot the raw data --------------------------------------------------------------------------

# Plot data
gg_cad = ggplot(
  data = plot_df 
) + 
  geom_bar(
    aes(
      x = cscore,
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
    x = element_blank(),
    y = element_blank()
  ) +
  facet_wrap(
    . ~ age,
    nrow = 3,
    ncol = 2,
    scales = "free"
  )

# ---- Export the raw plot ------------------------------------------------------------------------

# Standard plot
cairo_pdf(
  filename = "../output/figures/validation/cscore_age_delinq_plot.pdf",
)
gg_cad
dev.off()

# Wide plot
cairo_pdf(
  filename = "../output/figures/validation/cscore_age_delinq_wide_plot.pdf",
  width = 16,
  height = 9
)
gg_cad
dev.off()

# Export object
save(
  gg_cad,
  file = "../output/objects/validation/gg_cad.Rda"
)
