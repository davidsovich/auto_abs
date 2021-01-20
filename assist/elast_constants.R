# ---- Description --------------------------------------------------------------------------------

# This program defines constants for the elasticity analysis.

# ---- Data-driven discontinuity limits -----------------------------------------------------------

min_disc = function() {
  -0.25
  #-0.50
}

min_smooth = function() {
  -0.05
}

max_t_stat = function() {
  qnorm(0.001)
}

# ---- Threshold values ---------------------------------------------------------------------------

thresh_max = function(disc_df) {
  disc_df %>%
    dplyr::arrange(
      originator,
      cscore_bin
    ) %>%
    dplyr::group_by(
      originator
    ) %>%
    dplyr::mutate(
      diff_cscore = cscore_bin - dplyr::lag(cscore_bin, 1)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(
      min_diff = min(diff_cscore, na.rm = TRUE)
    ) %>%
    as.numeric()
}

thresh_values = function(disc_df) {
  max_val = thresh_max(disc_df) -1
  temp_vals = seq(5, max_val, by = 5)
  if(max_val %in% temp_vals) {
    temp_vals
  } else {
    c(temp_vals, max_val)
  }
}

# --- Baseline model choices ----------------------------------------------------------------------

p_choice = function() {
  2
}

h_choice = function(disc_df) {
  max(thresh_values(disc_df))
}



# --- Fixed effects choices -----------------------------------------------------------------------

fe1 = function() {
  c("disc_id")
}

fe2 = function() {
  c(
    "disc_id",
    "state_time_fe"
  )
}

# --- Cluster choices -----------------------------------------------------------------------------

clusters1 = function() {
  c("disc_id")
}

clusters2 = function() {
  c(
    "disc_id",
    "cscore"
  )
}

