# 03 | Dashboard Intelligence Functions
# Clinical Safety Intelligence Dashboard
# Developed by Vincent Kometsi

library(tidyverse)
library(scales)

create_safety_interpretation <- function(subjects, adverse_events, labs, exposure) {
  
  total_subjects <- nrow(subjects)
  total_ae <- nrow(adverse_events)
  serious_ae <- sum(adverse_events$seriousness == "Serious", na.rm = TRUE)
  severe_ae <- sum(adverse_events$severity == "Severe", na.rm = TRUE)
  abnormal_labs <- sum(labs$abnormal_flag != "Normal", na.rm = TRUE)
  mean_compliance <- mean(exposure$compliance_rate, na.rm = TRUE)
  discontinuation_rate <- mean(subjects$discontinued == 1, na.rm = TRUE)
  
  ae_rate <- total_ae / total_subjects
  serious_ae_rate <- serious_ae / total_subjects
  severe_ae_rate <- severe_ae / total_subjects
  abnormal_lab_rate <- abnormal_labs / nrow(labs)
  
  safety_level <- case_when(
    serious_ae_rate >= 0.15 | discontinuation_rate >= 0.20 ~ "Elevated Safety Concern",
    serious_ae_rate >= 0.08 | abnormal_lab_rate >= 0.25 ~ "Moderate Safety Concern",
    TRUE ~ "Stable Safety Profile"
  )
  
  interpretation <- case_when(
    safety_level == "Elevated Safety Concern" ~ paste0(
      "The filtered study population shows an elevated safety profile requiring closer clinical review. ",
      "The serious adverse event rate is ", percent(serious_ae_rate, accuracy = 0.1),
      ", while the discontinuation rate is ", percent(discontinuation_rate, accuracy = 0.1),
      ". These patterns suggest that patient-level and site-level monitoring should be prioritised."
    ),
    
    safety_level == "Moderate Safety Concern" ~ paste0(
      "The filtered study population shows moderate safety signals. ",
      "The adverse event burden is observable, with an AE rate of ",
      round(ae_rate, 2), " events per subject and a laboratory abnormality rate of ",
      percent(abnormal_lab_rate, accuracy = 0.1),
      ". Continued safety surveillance is recommended."
    ),
    
    TRUE ~ paste0(
      "The filtered study population shows a generally stable safety profile. ",
      "Serious adverse events remain limited at ",
      percent(serious_ae_rate, accuracy = 0.1),
      ", with mean treatment compliance of ",
      percent(mean_compliance, accuracy = 0.1),
      ". Routine monitoring should continue."
    )
  )
  
  tibble(
    safety_level = safety_level,
    total_subjects = total_subjects,
    adverse_event_rate = round(ae_rate, 3),
    serious_ae_rate = round(serious_ae_rate, 3),
    severe_ae_rate = round(severe_ae_rate, 3),
    abnormal_lab_rate = round(abnormal_lab_rate, 3),
    mean_compliance = round(mean_compliance, 3),
    discontinuation_rate = round(discontinuation_rate, 3),
    interpretation = interpretation
  )
}

create_high_risk_patient_table <- function(patient_profile, top_n = 15) {
  
  patient_profile %>%
    arrange(desc(patient_risk_score)) %>%
    select(
      subject_id,
      site_id,
      treatment_arm,
      age,
      sex,
      discontinued,
      total_ae,
      serious_ae,
      severe_ae,
      abnormal_labs,
      grade_2_or_higher_labs,
      patient_risk_score,
      patient_risk_category
    ) %>%
    slice_head(n = top_n)
}

create_site_escalation_table <- function(site_risk_enhanced, top_n = 10) {
  
  site_risk_enhanced %>%
    arrange(desc(site_risk_score)) %>%
    select(
      risk_rank,
      site_id,
      enrolled_subjects,
      ae_rate,
      serious_ae_rate,
      missed_visit_rate,
      out_of_window_rate,
      discontinuation_rate,
      site_risk_score,
      site_risk_category,
      key_risk_driver
    ) %>%
    slice_head(n = top_n)
}

create_signal_summary <- function(adverse_events, labs) {
  
  ae_signals <- adverse_events %>%
    count(ae_term, seriousness, severity, name = "n") %>%
    arrange(desc(n)) %>%
    slice_head(n = 10)
  
  lab_signals <- labs %>%
    filter(abnormal_flag != "Normal") %>%
    count(lab_test, abnormal_flag, toxicity_grade, name = "n") %>%
    arrange(desc(n)) %>%
    slice_head(n = 10)
  
  list(
    ae_signals = ae_signals,
    lab_signals = lab_signals
  )
}