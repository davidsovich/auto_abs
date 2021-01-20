# ---- Description --------------------------------------------------------------------------------

# This program plots the distribution of credit scores. 

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
  path = "../data/validation/cscore_dist.xlsx"
)

# ---- Plot credit score distribution -------------------------------------------------------------

# Plot the data
gg1 = ggplot(
  data = plot_df
) + 
  geom_bar(
    aes(
      x = cscore,
      y = value,
      fill = series,
      color = series
    ),
    stat = "identity", 
    position = "identity",
    alpha = 0.5
  ) +
  scale_fill_manual(
    values = c("white", "darkgray")
  ) + 
  scale_color_manual(
    values = c("black", "black")
  ) + 
  theme_bw() +
  theme(
    axis.text.x = element_text(
      size = 12
    ),
    axis.text.y = element_text(
      size = 12,
      angle = 45
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  ) + 
  labs(
    x = "Credit score",
    y = "Fraction of borrowers"
  )

# Export figure
cairo_pdf(
  filename = "../output/figures/validation/cscore_dist.pdf",
)
gg1
dev.off()

# ---- Plot alternate credit score distribution ---------------------------------------------------

# Plot the data
gg2 = ggplot(
  data = filter(plot_df, series == "Reg AB II")
) + 
  geom_histogram(
    aes(
      x = cscore,
      y = value
    ),
    stat = "identity",
    position = "identity",
    color = "darkgray",
    fill = "darkgray"
  ) + 
  geom_histogram(
    data = filter(plot_df, series == "Equifax"),
    aes(
      x = cscore,
      y = value
    ),
    stat = "identity",
    position = "identity",
    color = "black",
    fill = "white",
    alpha = 0
  ) + 
  scale_fill_manual(
    name = "",
    values = c("darkgray", "white")
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      size = 12
    ),
    axis.text.y = element_text(
      size = 12,
      angle = 45
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  ) + 
  labs(
    x = "Credit score",
    y = "Fraction of borrowers"
  )
  
# Export figure
cairo_pdf(
  filename = "../output/figures/validation/cscore_dist_alternate.pdf",
)
gg2
dev.off()


