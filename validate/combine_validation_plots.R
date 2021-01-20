# ---- Description --------------------------------------------------------------------------------

# This program combines the validation plots.  

# ---- Preliminaries ------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(ggplot2)
library(tidyr)
library(lfe)

# Load functions
source("./assist/functions.R")

# Load constants
source("./assist/constants.R")

# ---- Load data ----------------------------------------------------------------------------------

# Credit balance
gg_cb = get(load(
  file = "../output/objects/validation/gg_cb.Rda"
))

# Credit loan amounts
gg_cl = get(load(
  file = "../output/objects/validation/gg_cl.Rda"
))

# Credit delinquency
gg_cd = get(load(
  file = "../output/objects/validation/gg_cd.Rda"
))

# Credit delinquency dummy
gg_cdd = get(load(
  file = "../output/objects/validation/gg_cd.Rda"
))

# Credit warehouse
gg_cw = get(load(
  file = "../output/objects/validation/gg_cw.Rda"
))

# Age balance
gg_ab = get(load(
  file = "../output/objects/validation/gg_ab.Rda"
))

# Age loan amounts
gg_al = get(load(
  file = "../output/objects/validation/gg_al.Rda"
))

# Age delinquency
gg_ad = get(load(
  file = "../output/objects/validation/gg_ad.Rda"
))

# Age delinquency dummy
gg_ad = get(load(
  file = "../output/objects/validation/gg_add.Rda"
))

# Age warehouse
gg_aw = get(load(
  file = "../output/objects/validation/gg_aw.Rda"
))

# ---- Combine credit score plots -----------------------------------------------------------------

# Combine plots
gg_cc = ggpubr::ggarrange(
  gg_cb + 
    labs(
      x = element_blank()
    ),
  gg_cl + 
    labs(
      x = element_blank()
    ),
  gg_cd + 
    labs(
      x = element_blank()
    ),
  gg_cw + 
    labs(
      x = element_blank()
    ),
  nrow = 2,
  ncol = 2,
  labels = c(
    "A",
    "B",
    "C",
    "D"
  ),
  common.legend = TRUE
)

# ---- Export credit score plot -------------------------------------------------------------------

# Standard plot
cairo_pdf(
  filename = "../output/figures/validation/cscore_combined_static_plots.pdf"
)
gg_cc
dev.off()

# Wide plot
cairo_pdf(
  filename = "../output/figures/validation/cscore_combined_static_wide_plots.pdf",
  width = 16,
  height = 9
)
gg_cc
dev.off()

# ---- Combine loan age plots ---------------------------------------------------------------------

# Combine plots
gg_ac = ggpubr::ggarrange(
  gg_ab + 
    labs(
      x = element_blank()
    ),
  gg_al + 
    labs(
      x = element_blank()
    ),
  gg_ad + 
    labs(
      x = element_blank()
    ),
  gg_aw + 
    labs(
      x = element_blank()
    ),
  nrow = 2,
  ncol = 2,
  labels = c(
    "A",
    "B",
    "C",
    "D"
  ),
  common.legend = TRUE
)

# ---- Export credit score plot -------------------------------------------------------------------

# Standard plot
cairo_pdf(
  filename = "../output/figures/validation/age_combined_static_plots.pdf"
)
gg_ac
dev.off()

# Wide plot
cairo_pdf(
  filename = "../output/figures/validation/age_combined_static_wide_plots.pdf",
  width = 16,
  height = 9
)
gg_ac
dev.off()







