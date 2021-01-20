# ---- Description --------------------------------------------------------------------------------

# This program defines constants for the analysis. 

# ---- Local loan tables --------------------------------------------------------------------------

first_loan_tbl = function() {
  "../data-raw/LoanLevelData/First_Loan_With_Unique_IDs_2020_06.csv"
}

del_loan_tbl = function(horizon) {
  if(horizon == 12) {
    "../data-raw/Delinquency_Tables/Delin_12_With_NewUniqueID.csv"
  } else if (horizon == 18) {
    "../data-raw/Delinquency_Tables/Delin_18_With_NewUniqueID.csv"
  } else if (horizon == 24) {
    "../data-raw/Delinquency_Tables/Delin_24_With_NewUniqueID.csv"
  } else if (horizon == 30) {
    "../data-raw/Delinquency_Tables/Delin_30_With_NewUniqueID.csv"
  } else if (horizon == 36) {
    "../data-raw/Delinquency_Tables/Delin_36_With_NewUniqueID.csv"
  }
}

# ---- Local lease tables -------------------------------------------------------------------------

first_lease_tbl = function() {
  "../data-raw/LeaseLevelData/FirstLease_2020_06.csv"
}

# ---- Local mapping tables -----------------------------------------------------------------------

issuer_mapping_tbl = function() {
  "../data-raw/LoanLevelData/LinkingTable_CIK_IssuingEntities.csv"
}

issuer_date_tbl = function() {
  "../data-raw/Relational_Tables/issuing_entity_table.csv"
}

sponsor_mapping_tbl = function() {
  "../data-raw/LoanLevelData/IssuingEntity_Depositor_Sponser_Final.csv"
}



# ---- BigQuery loan tables -----------------------------------------------------------------------

# ---- BigQuery lease tables ----------------------------------------------------------------------

# ---- Raw loan variables -------------------------------------------------------------------------

loan_id_vars = function(clean = TRUE) {
  if(clean == TRUE) {
    c(
      "loan_id",
      "assetNumber",
      "reportingPeriodBeginningDate",
      "reportingPeriodEndingDate"
    )
  } else {
    c(
      lo_newid_vars(clean = TRUE),
      "assetTypeNumber",
      "assetAddedIndicator"
    )
  }
}

loan_orig_vars = function(clean = TRUE) {
  if(clean == TRUE) {
    c(
      "originatorName",
      "originationDate",
      "originalLoanAmount",
      "originalLoanTerm",
      "originalInterestRatePercentage", 
      "underwritingIndicator",
      "paymentTypeCode", 
      "subvented"
    )
  } else {
    c(
      lo_orig_vars(clean = TRUE),
      "loanMaturityDate",
      "interestCalculationTypeCode",
      "originalInterestRateTypeCode",
      "originalInterestOnlyTermNumber",
      "originalFirstPaymentDate",
      "gracePeriodNumber"
    )
  }
}

loan_vehic_vars = function() {
  c(
    "vehicleManufacturerName", 
    "vehicleModelName", 
    "vehicleNewUsedCode", 
    "vehicleModelYear", 
    "vehicleTypeCode",
    "vehicleValueAmount", 
    "vehicleValueSourceCode"
  )
}

loan_oblig_vars = function() {
  c(
    "obligorCreditScoreType", 
    "obligorCreditScore", 
    "obligorIncomeVerificationLevelCode",
    "obligorEmploymentVerificationCode",
    "coObligorIndicator",
    "paymentToIncomePercentage",
    "obligorGeographicLocation"
  )
}

loan_payment_vars = function(clean = TRUE) {
  if(clean == TRUE) {
    c(
      "remainingTermToMaturityNumber", 
      "reportingPeriodBeginningLoanBalanceAmount",
      "reportingPeriodActualEndBalanceAmount",
      "reportingPeriodScheduledPaymentAmount",
      "totalActualAmountPaid", 
      "zeroBalanceEffectiveDate",
      "zeroBalanceCode"
    )
  } else {
    c(
      loan_payment_vars(clean = TRUE),
      "scheduledInterestAmount",
      "scheduledPrincipalAmount",
      "otherPrincipalAdjustmentAmount",
      "actualInterestCollectedAmount",
      "actualPrincipalCollectedAmount",
      "actualOtherCollectedAmount",
      "reportingPeriodInterestRatePercentage",
      "nextInterestRatePercentage",
      "nextReportingPeriodPaymentAmountDue", 
      "interestPaidThroughDate"
    )
  }
}

loan_mod_vars = function() {
  c(
    "reportingPeriodModificationIndicator", 
    "modificationTypeCode",
    "paymentExtendedNumber"
  )
}

loan_delinq_vars = function() {
  c(
    "currentDelinquencyStatus",
    "chargedoffPrincipalAmount",
    "recoveredAmount", 
    "repossessedIndicator",
    "repossessedProceedsAmount"
  )
}

loan_demand_vars = function() {
  c(
    "assetSubjectDemandIndicator",
    "RepurchaseAmount"
  )
}

loan_service_vars = function() {
  c(
    "servicingAdvanceMethodCode",
    "servicingFeePercentage",
    "servicingFlatFeeAmount",
    "otherServicerFeeRetainedByServicer",
    "otherAssessedUncollectedServicerFeeAmount",
    "primaryLoanServicerName",
    "mostRecentServicingTransferReceivedDate"
  )
}

# ---- Raw lease variables ------------------------------------------------------------------------

lease_newid_vars = function(clean = TRUE) {
  if(clean == TRUE) {
    c(
      "lease_id",
      "assetNumber",
      "reportingPeriodBeginDate",
      "reportingPeriodEndDate",
      "cik_issuer"
    )
  } else {
    c(
      lo_newid_vars(clean = TRUE),
      "assetTypeNumber",
      "assetAddedIndicator"
    )
  }
}

lease_orig_vars = function(clean = TRUE) {
  if(clean == TRUE) {
    c(
      "originatorName",
      "originationDate",
      "acquisitionCost",
      "originalLeaseTermNumber",
      "underwritingIndicator",
      "paymentTypeCode",
      "subvented"
    )
  } else {
    c(
      lo_orig_vars(clean = TRUE),
      "scheduledTerminationDate",
      "originalFirstPaymentDate",
      "gracePeriod" 
    )
  }
}

lease_vehic_vars = function() {
  c(
    "vehicleManufacturerName", 
    "vehicleModelName", 
    "vehicleNewUsedCode", 
    "vehicleModelYear", 
    "vehicleTypeCode",
    "vehicleValueAmount", 
    "vehicleValueSourceCode",
    "baseResidualValue",
    "baseResidualSourceCode",
    "contractResidualValue"
  )
}

lease_oblig_vars = function() {
  c(
    "lesseeCreditScoreType", 
    "lesseeCreditScore", 
    "lesseeIncomeVerificationLevelCode",
    "lesseeEmploymentVerificationCode",
    "coLesseePresentIndicator",
    "paymentToIncomePercentage",
    "lesseeGeographicLocation"
  )
}

lease_payment_vars = function(clean = TRUE) {
  if(clean == TRUE) {
    c(
      "remainingTermNumber", 
      "reportingPeriodSecuritizationValueAmount",
      "securitizationDiscountRate",
      "reportingPeriodEndingActualBalanceAmount", 
      "totalActualAmountPaid",
      "reportingPeriodEndActualSecuritizationAmount",
      "zeroBalanceEffectiveDate",
      "zeroBalanceCode" 
    )
  } else {
    c(
      loan_payment_vars(clean = TRUE),
      "nextReportingPeriodPaymentAmountDue",
      "reportingPeriodScheduledPaymentAmount",
      "actualOtherCollectedAmount",
      "paidThroughDate" 
    )
  }
}

lease_mod_vars = function() {
  c(
    "reportingPeriodModificationIndicator", 
    "modificationTypeCode",
    "leaseExtended"
  )
}

lease_delinq_vars = function() {
  c(
    "currentDelinquencyStatus",
    "chargedOffAmount", 
    "terminationIndicator",
    "excessFeeAmount",
    "liquidationProceedsAmount"
  )
}

lease_demand_vars = function() {
  c(
    "assetSubjectDemandIndicator",
    "RepurchaseAmount"
  )
}

lease_service_vars = function() {
  c(
    "servicingAdvanceMethodCode",
    "servicingFeePercentage",
    "servicingFlatFeeAmount",
    "otherLeaseLevelServicingFeesRetainedAmount",
    "otherAssessedUncollectedServicerFeeAmount",
    "servicerAdvancedAmount", 
    "primaryLeaseServicerName"
  )
}

# ---- Discontinuity lenders ----------------------------------------------------------------------

dverify_lenders = function(limited = TRUE) {
  if(limited == TRUE) {
    c(
      "CBS",
      "Ford Credit",
      "GM FINANCIAL",
      "HCA",
      "SC",
      "WORLD OMNI FINANCIAL CORP"
    )
  } else {
    c(
      dverify_lenders(limited = TRUE),
      "Mechanics Bank",
      "TMCC",
      "VW Credit"
    )
  }
}

# ---- Rule-of-thumb lenders ----------------------------------------------------------------------

rule_lenders = function() {
  c(
    "BMW Bank of North America",
    "ESB",
    "Fifth Third Bank", 
    "HCA",
    "Mechanics Bank",
    "VW Credit"
  )
}

rule_points = function(lender = "all") {
  if(lender == "BMW Bank of North America") {
    c(640, 675)
  } else if (lender == "ESB") {
    c(740, 700)
  } else if (lender == "Fifth Third Bank") {
    c(690)
  } else if (lender == "HCA") {
    c(620, 640, 660, 680, 700)
  } else if (lender == "Mechanics Bank") {
    c(620, 640, 660) 
  } else if (lender == "VW Credit") {
    c(620, 640, 660, 680, 700) 
  } else {
    sort(unique(c(
      rule_points(lender = "BMW Bank of North America"),
      rule_points(lender = "ESB"),
      rule_points(lender = "Fifth Third Bank"),
      rule_points(lender = "HCA"),
      rule_points(lender = "Mechanics Bank"),
      rule_points(lender = "VW Credit")
    )))
  }
}

    
    
