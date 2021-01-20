# ---- Description --------------------------------------------------------------------------------

# This program builds the loan data for the elasticity study. 

# ---- Preliminaries ------------------------------------------------------------------------------

# Load libraries
library(tidyverse)

# Load functions
source("./assist/functions.R")
source("./assist/descr_stats.R")

# Load constants
source("./assist/elast_constants.R")

# ---- Load data ----------------------------------------------------------------------------------

# Load data
elast_df = read_csv(
  file = "../data-raw/Screening_Project/Analysis_Sample_2020_09_27.csv",
  guess_max = 100000
)

# ---- Subset variables ---------------------------------------------------------------------------

# Subset variables
elast_df = elast_df %>%
  select(
    loan_id,
    # origination
    originatorName,
    originationDate,
    originalLoanAmount,
    originalLoanTerm,
    originalInterestRatePercentage,
    underwritingIndicator,
    subvented,
    warehouse,
    reportingPeriodScheduledPaymentAmount,
    # loan
    vehicleManufacturerName,
    vehicleNewUsedCode,
    vehicleModelYear,
    vehicleTypeCode,
    vehicleValueAmount,
    # obligor
    obligorCreditScore,
    obligorIncomeVerificationLevelCode,
    obligorEmploymentVerificationCode,
    paymentToIncomePercentage,
    coObligorIndicator,
    obligorGeographicLocation,
    Income,
    # delinquency
    dpd60_12,
    dpd90_12,
    dpd60_18,
    dpd90_18,
    dpd60_24,
    dpd90_24,
    dpd60_30,
    dpd90_30,
    dpd60_36,
    dpd90_36,
    severe_derog60_12,
    severe_derog90_12,
    severe_derog60_18,
    severe_derog90_18,
    severe_derog60_24,
    severe_derog90_24,
    severe_derog60_30,
    severe_derog90_30,
    severe_derog60_36,
    severe_derog90_36,
    # filters
    interestCalculationTypeCode,
    originalInterestRateTypeCode,
    paymentTypeCode,
    servicingAdvanceMethodCode
  )

# Rename variables
elast_df = elast_df %>%
  rename(
    # origination
    originator = originatorName,
    orig_date = originationDate,
    amount = originalLoanAmount,
    months = originalLoanTerm,
    rate = originalInterestRatePercentage,
    uwrite = underwritingIndicator,
    whouse = warehouse,
    scheduled_pmt = reportingPeriodScheduledPaymentAmount,
    # loan
    manufacturer = vehicleManufacturerName,
    new_used_code = vehicleNewUsedCode,
    model_year = vehicleModelYear,
    vehicle_type = vehicleTypeCode,
    value = vehicleValueAmount,
    # obligor
    cscore = obligorCreditScore,
    income = Income, 
    i_verif_code = obligorIncomeVerificationLevelCode,
    e_verif_code = obligorEmploymentVerificationCode,
    pmi = paymentToIncomePercentage,
    cosign = coObligorIndicator,
    state = obligorGeographicLocation,
    # delinquency
    delinq_60_12 = severe_derog60_12,
    delinq_90_12 = severe_derog90_12,
    delinq_60_18 = severe_derog60_18,
    delinq_90_18 = severe_derog90_18,
    delinq_60_24 = severe_derog60_24,
    delinq_90_24 = severe_derog90_24,
    delinq_60_30 = severe_derog60_30,
    delinq_90_30 = severe_derog90_30,
    delinq_60_36 = severe_derog60_36,
    delinq_90_36 = severe_derog90_36
  )

# ---- Wrangle data -------------------------------------------------------------------------------

# Date changes
elast_df = elast_df %>%
  mutate(
    orig_date = yyyymm_dashes(orig_date)
  )

# Rate bounds
elast_df = elast_df %>%
  mutate(
    rate = ifelse(rate > 0.50, rate / 100, rate),
    pmi = ifelse(pmi > 0.50, pmi / 100, pmi)
  )

# Income bounds
elast_df = elast_df %>%
  mutate(
    income = cap_vals(income, 2500, 250000)
  )

# Other variables
elast_df = elast_df %>%
  mutate(
    ltv = amount / value,
    new_vehicle = as.numeric(new_used_code == 1),
    i_verif = as.numeric(i_verif_code %in% c(3, 4, 5)),
    e_verif = as.numeric(e_verif_code %in% c(3, 4, 5)),
    rate_sub = as.numeric(subvented %in% c("1", "1,2", "2,1", "1,98")),
    cash_sub = as.numeric(subvented %in% c("2", "1,2", "2,1", "2,98"))
  )

# ---- Clean data ---------------------------------------------------------------------------------

# Baseline filters
elast_df = elast_df %>%
  filter(
    interestCalculationTypeCode != 98,
    !is.na(interestCalculationTypeCode),
    originalInterestRateTypeCode != 98,
    !is.na(originalInterestRateTypeCode),
    paymentTypeCode != 98,
    !is.na(paymentTypeCode),
    subvented != 98,
    !is.na(subvented),
    servicingAdvanceMethodCode != 98,
    !is.na(servicingAdvanceMethodCode)
  ) %>%
  select(
    -interestCalculationTypeCode,
    -originalInterestRateTypeCode,
    -paymentTypeCode,
    -servicingAdvanceMethodCode
  )

# Data quality
elast_df = elast_df %>%
  filter(
    value > 0,
    ltv < 2,
    amount < 250000,
    cscore >= 280,
    cscore <= 850,
    rate <= 0.30,
    pmi <= 0.30,
    state %in% us_states()
  )

# ---- Append lender names ------------------------------------------------------------------------

# Append names
elast_df = elast_df %>%
  inner_join(
    y = lender_names(),
    by = c("originator" = "ticker")
  )

# ---- Descriptive statistics ---------------------------------------------------------------------

# Wide window 
descr_df = descr_stats(
  df = elast_df,
  features = c(
    "amount",
    "rate",
    "scheduled_pmt",
    "months",
    "ltv",
    "value",
    "model_year",
    "new_vehicle",
    "whouse",
    "cscore",
    "income",
    "pmi", 
    "cosign",
    "uwrite",
    "rate_sub",
    "cash_sub",
    "i_verif",
    "e_verif"
  )
)

# Export results
write_csv(
  descr_df,
  path = "../output/tables/validation/descr_stats.csv"
)