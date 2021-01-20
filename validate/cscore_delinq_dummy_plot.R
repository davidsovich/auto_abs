# ---- Description --------------------------------------------------------------------------------

# This program plots the percent of 2019 Q4 balances 90+ DPD across original credit scores.  

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
  path = "../data/validation/cscore_delinq_dummy.xlsx"
)

# ---- Plot the data ------------------------------------------------------------------------------

# Plot data
gg_cdd = ggplot(
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
    x = "Credit score",
    y = "Fraction of loans 90+ DPD"
  )

# ---- Export the plot ----------------------------------------------------------------------------

# Export figure
cairo_pdf(
  filename = "../output/figures/validation/cscore_delinq_dummy_plot.pdf",
)
gg_cdd
dev.off()

# Export object
save(
  gg_cd,
  file = "../output/objects/validation/gg_cdd.Rda"
)



