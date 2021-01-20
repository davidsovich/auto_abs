# ---- Description --------------------------------------------------------------------------------

# This program plots the percent of 2019 Q4 balances across 2019 Q4 loan age. 

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
  path = "../data/validation/age_balance.xlsx"
)

# ---- Plot the data ------------------------------------------------------------------------------

# Plot data
gg_ab = ggplot(
  data = plot_df 
) + 
  geom_bar(
    aes(
      x = age,
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
    x = "Loan age",
    y = "Fraction of balances"
  )

# ---- Export the plot ----------------------------------------------------------------------------

# Export figure
cairo_pdf(
  filename = "../output/figures/validation/age_balance_plot.pdf",
)
gg_ab
dev.off()

# Export object
save(
  gg_ab,
  file = "../output/objects/validation/gg_ab.Rda"
)
