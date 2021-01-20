# ---- Description --------------------------------------------------------------------------------

# This program plots the number of loans and percent of income verifications per lender in the raw 
# data.  

# ---- Preliminaries ------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(ggplot2)

# Load functions
source("./assist/functions.R")

# Load constants
source("./assist/elast_constants.R")

# ---- Load data ----------------------------------------------------------------------------------

# Load data
elast_df = read_csv(
  file = "../data-raw/Screening_Project/Analysis_Sample_2020_09_27.csv",
  guess_max = 100000
)

# ---- Wrangle the data ---------------------------------------------------------------------------

# Wrangle the data
elast_df = elast_df %>%
  select(
    loan_id,
    originatorName,
    obligorIncomeVerificationLevelCode,
    obligorEmploymentVerificationCode
  ) %>%
  rename(
    originator = originatorName,
    i_verif_code = obligorIncomeVerificationLevelCode,
    e_verif_code = obligorEmploymentVerificationCode
  ) %>%
  mutate(
    i_verif = as.numeric(i_verif_code %in% c(3, 4, 5)),
    e_verif = as.numeric(e_verif_code %in% c(3, 4, 5))
  )

# ---- Plot observations across lenders -----------------------------------------------------------

# Plot data
plot_df = elast_df %>%
  group_by(
    originator
  ) %>%
  summarise(
    num_obs = n()
  ) %>%
  ungroup() %>%
  mutate(
    num_obs = num_obs / 10^6
  ) %>%
  arrange(
    desc(num_obs)
  ) %>%
  mutate(
    t_index = row_number()
  ) %>% 
  inner_join(
    y = lender_names(),
    by = c("originator" = "ticker")
  )

# Plot observations
gg1 = ggplot(
  data = plot_df
) + geom_bar(
  aes(
    x = t_index,
    y = num_obs,
    fill = type
  ),
  position = "dodge",
  stat = "identity",
  color = "black"
  #fill = "white"
) + 
  scale_x_continuous(
    breaks = plot_df$t_index,
    labels = plot_df$name
  ) + 
  scale_fill_grey() + 
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
    axis.title.x = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  ) + 
  labs(
    y = "Number of observations (millions)"
  )

# Export plot
cairo_pdf(
  filename = "../output/figures/validation/lender_number_loans.pdf",
  #width = 16,
  #height = 9,
  onefile = TRUE
)
gg1
dev.off()

# ---- Plot income verifications across lenders ---------------------------------------------------

# Plot data
plot_df = elast_df %>%
  filter(
    !is.na(i_verif)
  ) %>% 
  group_by(
    originator
  ) %>%
  summarise(
    num_obs = n(),
    pct_obs = 100 * sum(i_verif) / n()
  ) %>%
  ungroup() %>%
  mutate(
    num_obs = num_obs / 10^6
  ) %>%
  arrange(
    desc(num_obs)
  ) %>%
  mutate(
    t_index = row_number()
  ) %>% 
  inner_join(
    y = lender_names(),
    by = c("originator" = "ticker")
  )

# Plot observations
gg2 = ggplot(
  data = plot_df
) + geom_bar(
  aes(
    x = t_index,
    y = pct_obs,
    fill = type
  ),
  position = "dodge",
  stat = "identity",
  color = "black"
  #fill = "white"
) + 
  scale_x_continuous(
    breaks = plot_df$t_index,
    labels = plot_df$name
  ) + 
  scale_fill_grey() + 
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
    axis.title.x = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  ) + 
  labs(
    y = "Percent verified"
  )

# Export plot
cairo_pdf(
  filename = "../output/figures/validation/lender_verified.pdf",
  #width = 16,
  #height = 9,
  onefile = TRUE
)
gg2
dev.off()