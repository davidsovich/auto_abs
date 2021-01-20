# ---- Description --------------------------------------------------------------------------------

# This program plots the within-lender interest rate discontinuities for the elasticity study. 

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
    mean = 100 * mean(rate, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  arrange(
    originator,
    name,
    cscore_bin
  )

# ---- Plot the discontinuities -------------------------------------------------------------------

# Plot the data
gg1 = ggplot(
  data = rate_df %>%
    filter(
      originator %in% unique(disc_df$originator)
    )
) +
  geom_point(
    aes(
      x = cscore_bin,
      y = mean
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
  filename = "../output/figures/elast/lender_discont.pdf",
  width = 16,
  height = 9,
  onefile = TRUE
)
gg1
dev.off()

# ---- Plot the lack of discontinuities -----------------------------------------------------------

# Plot the data
gg2 = ggplot(
  data = rate_df %>%
    filter(
      !(originator %in% unique(disc_df$originator)),
      originator != "ESB",
      originator != "Fifth Third Bank"
    )
) +
  geom_point(
    aes(
      x = cscore_bin,
      y = mean
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
  facet_wrap(
    name ~ .,
    scales = "free",
    nrow = 3,
    ncol = 3
  )

# Export results
cairo_pdf(
  filename = "../output/figures/elast/lender_no_discont.pdf",
  width = 16,
  height = 9,
  onefile = TRUE
)
gg2
dev.off()

# ---- Plot the number of discontinuities ---------------------------------------------------------

# Plot data
plot_df = lender_names() %>%
  filter(
    ticker != "Ally Bank",
    ticker != "Harley Davidson",
    ticker != "Fifth Third"
  ) %>%
  left_join(
    y = disc_df %>%
      group_by(
        originator
      ) %>%
      summarise(
        num_obs = n()
      ) %>%
      ungroup(),
    by = c("ticker" = "originator")
  ) %>%
  mutate(
    num_obs = replace_na(num_obs, 0)
  ) %>%
  arrange(
    ticker
  ) %>%
  mutate(
    t_index = row_number()
  )

# Plot the data
gg3 = ggplot(
  data = plot_df
) +
  geom_bar(
    aes(
      x = t_index,
      y = num_obs
    ),
    stat = "identity",
    color = "black",
    fill = "gray"
  ) + 
  scale_x_continuous(
    breaks = plot_df$t_index,
    labels = plot_df$name
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
    axis.title.x = element_blank()
  ) + 
  labs(
    y = "Number of discontinuities"
  )

# Export results
cairo_pdf(
  filename = "../output/figures/elast/lender_num_discont.pdf",
  width = 16,
  height = 9,
  onefile = TRUE
)
gg3
dev.off()

# ---- Plot the magnitude of discontinuities ------------------------------------------------------

# Plot data
plot_df = lender_names() %>%
  filter(
    ticker != "Ally Bank",
    ticker != "Harley Davidson",
    ticker != "Fifth Third"
  ) %>%
  left_join(
    y = disc_df %>%
      group_by(
        originator
      ) %>%
      summarise(
        mean = mean(diff)
      ) %>%
      ungroup(),
    by = c("ticker" = "originator")
  ) %>%
  mutate(
    mean = replace_na(mean, 0)
  ) %>%
  arrange(
    ticker
  ) %>%
  mutate(
    t_index = row_number()
  )

# Plot the data
gg4 = ggplot(
  data = plot_df
) +
  geom_bar(
    aes(
      x = t_index,
      y = abs(mean)
    ),
    stat = "identity",
    color = "black",
    fill = "gray"
  ) + 
  scale_x_continuous(
    breaks = plot_df$t_index,
    labels = plot_df$name
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
    axis.title.x = element_blank()
  ) + 
  labs(
    y = "Average size of discontinuities (percentage points)"
  )

# Export results
cairo_pdf(
  filename = "../output/figures/elast/lender_size_discont.pdf",
  width = 16,
  height = 9,
  onefile = TRUE
)
gg4
dev.off()

# ---- Plot distribution of size of discontinuities -----------------------------------------------

# Plot data
plot_df = tibble(
  rate_bin = seq(0, 3, by = 0.5)
) %>%
  left_join(
    disc_df %>%
      mutate(
        rate_bin = floor(abs(diff) / 0.50) * 0.50
      ) %>%
      group_by(
        rate_bin
      ) %>%
      summarise(
        num_obs = n()
      ) %>%
      ungroup(),
    by = c("rate_bin" = "rate_bin")
  ) %>%
  mutate(
    num_obs = replace_na(num_obs, 0)
  ) %>%
  mutate(
    t_index = row_number(),
    rate_label = paste0("[", rate_bin, ", ", rate_bin + 0.50, ")")
  )
    
# Plot the data
gg5 = ggplot(
  data = plot_df
) +
  geom_bar(
    aes(
      x = t_index,
      y = num_obs
    ),
    stat = "identity",
    color = "black",
    fill = "gray"
  ) + 
  scale_x_continuous(
    breaks = plot_df$t_index,
    labels = plot_df$rate_label
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
    panel.grid.minor = element_blank()
  ) + 
  labs(
    y = "Number of disconinuities",
    x = "Change in interest rate (percent)"
  )

# Export results
cairo_pdf(
  filename = "../output/figures/elast/disc_size_dist.pdf"
)
gg5
dev.off()


