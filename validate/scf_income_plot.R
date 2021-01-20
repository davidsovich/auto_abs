# ---- Description --------------------------------------------------------------------------------

# This program plots the income distribution in the Reg AB II and the SCF.  

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
plot_df = read_csv(
  file = "../data/validation/scf_income.csv",
  col_types = "didd"
)

# ---- Wrangle data -------------------------------------------------------------------------------

# Wrangle data
plot_df = plot_df %>%
  filter(
    income <= 250000
  ) %>%
  mutate(
    wgt = ifelse(
      is.na(wgt),
      1,
      wgt
    ),
    auto = ifelse(
      is.na(X437) | X437 == 1,
      1, 
      0
    ),
    series = case_when(
      user_data == 1 ~ "Reg AB II",
      TRUE           ~ "SCF"
    )
  )

# ---- Plot income distribution conditional on loan -----------------------------------------------

# Plot the data
gg_inc = ggplot(
  data = filter(plot_df, user_data == 1)
) + 
  geom_histogram(
    aes(
      x = income,
      y = ..density..
    ),
    position = "identity",
    color = "darkgray",
    fill = "darkgray",
    bins = 48
  ) + 
  geom_histogram(
    data = filter(plot_df, user_data == 0, auto == 1),
    aes(
      x = income,
      y = ..density..,
      weight = wgt
    ),
    position = "identity",
    color = "black",
    fill = "white",
    bins = 48,
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
    x = "Income",
    y = "Density"
  )

# ---- Export the plot ----------------------------------------------------------------------------

# Export figure
cairo_pdf(
  filename = "../output/figures/validation/scf_income_plot.pdf",
)
gg_inc
dev.off()

# Export object
save(
  gg_inc,
  file = "../output/objects/validation/gg_inc.Rda"
)

# ---- Plot alternate income distribution conditional on loan -------------------------------------

# Plot the data
gg_ainc = ggplot(
  data = filter(plot_df, auto == 1)
) + 
  geom_histogram(
    aes(
      x = income,
      y = ..density..,
      fill = series,
      color = series,
      weight = wgt
    ),
    position = "identity",
    bins = 48,
    alpha = 0.5
  ) +
  scale_fill_manual(
    values = c("darkgray", "white")
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
    x = "Income",
    y = "Density"
  )

# ---- Export the plot ----------------------------------------------------------------------------

# Export figure
cairo_pdf(
  filename = "../output/figures/validation/scf_alternate_income_plot.pdf",
)
gg_ainc
dev.off()

# Export object
save(
  gg_ainc,
  file = "../output/objects/validation/gg_ainc.Rda"
)

# ---- Plot unconditional income distribution -----------------------------------------------------

# Plot the data
gg_uinc = ggplot(
  data = filter(plot_df, user_data == 1)
) + 
  geom_histogram(
    aes(
      x = income,
      y = ..density..
    ),
    position = "identity",
    color = "darkgray",
    fill = "darkgray",
    bins = 48
  ) + 
  geom_histogram(
    data = filter(plot_df, user_data == 0),
    aes(
      x = income,
      y = ..density..,
      weight = wgt
    ),
    position = "identity",
    color = "black",
    fill = "white",
    bins = 48,
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
    x = "Income",
    y = "Density"
  )

# ---- Export the plot ----------------------------------------------------------------------------

# Export figure
cairo_pdf(
  filename = "../output/figures/validation/scf_income_unconditional_plot.pdf",
)
gg_uinc
dev.off()

# Export object
save(
  gg_uinc,
  file = "../output/objects/validation/gg_uinc.Rda"
)

# ---- Plot unconditional alternate income distribution -------------------------------------------

# Plot the data
gg_uainc = ggplot(
  data = plot_df
) + 
  geom_histogram(
    aes(
      x = income,
      y = ..density..,
      fill = series,
      color = series,
      weight = wgt
    ),
    position = "identity",
    bins = 48,
    alpha = 0.5
  ) +
  scale_fill_manual(
    values = c("darkgray", "white")
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
    x = "Income",
    y = "Density"
  )

# ---- Export the plot ----------------------------------------------------------------------------

# Export figure
cairo_pdf(
  filename = "../output/figures/validation/scf_alternate_income_unconditional_plot.pdf",
)
gg_uainc
dev.off()

# Export object
save(
  gg_uainc,
  file = "../output/objects/validation/gg_uainc.Rda"
)

