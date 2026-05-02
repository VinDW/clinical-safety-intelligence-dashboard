# 01 | Synthetic Trial Data Construction
# Clinical Safety Intelligence Dashboard
# Developed by Vincent Kometsi
# Generates synthetic datasets for safety monitoring, lab analytics, and Shiny visualisation.

library(tidyverse)
library(lubridate)

set.seed(2026)

n_subjects <- 500
n_sites <- 20

study_start <- as.Date("2024-01-01")
study_end   <- as.Date("2025-12-31")

sites <- paste0("SITE-", stringr::str_pad(1:n_sites, width = 2, pad = "0"))

treatment_arms <- c("Placebo", "Active Treatment")

sex_levels <- c("Male", "Female")

race_levels <- c(
  "African",
  "White",
  "Indian/Asian",
  "Coloured",
  "Other"
)

ae_terms <- c(
  "Headache",
  "Nausea",
  "Fatigue",
  "Dizziness",
  "Vomiting",
  "Rash",
  "Diarrhoea",
  "Hypertension",
  "Anaemia",
  "Elevated ALT",
  "Elevated AST",
  "Neutropenia",
  "Chest Pain",
  "Shortness of Breath",
  "Insomnia"
)

ae_system_organs <- c(
  "Nervous system disorders",
  "Gastrointestinal disorders",
  "General disorders",
  "Skin disorders",
  "Cardiac disorders",
  "Respiratory disorders",
  "Investigations",
  "Blood disorders"
)

lab_reference <- tibble(
  lab_test = c(
    "ALT",
    "AST",
    "Haemoglobin",
    "Platelets",
    "Neutrophils",
    "Creatinine"
  ),
  unit = c(
    "U/L",
    "U/L",
    "g/dL",
    "10^9/L",
    "10^9/L",
    "umol/L"
  ),
  lower_limit = c(0, 0, 12, 150, 2.0, 45),
  upper_limit = c(40, 40, 17, 400, 7.5, 110),
  mean_value = c(25, 24, 14, 250, 4.2, 80),
  sd_value = c(10, 9, 1.5, 60, 1.2, 18)
)

visit_schedule <- tibble(
  visit_number = 1:5,
  visit_name = c(
    "Screening",
    "Baseline",
    "Week 4",
    "Week 8",
    "Week 12"
  ),
  planned_day = c(-14, 0, 28, 56, 84)
)

subjects <- tibble(
  subject_id = paste0("SUBJ-", stringr::str_pad(1:n_subjects, width = 4, pad = "0")),
  site_id = sample(sites, n_subjects, replace = TRUE),
  treatment_arm = sample(
    treatment_arms,
    n_subjects,
    replace = TRUE,
    prob = c(0.50, 0.50)
  ),
  sex = sample(
    sex_levels,
    n_subjects,
    replace = TRUE,
    prob = c(0.48, 0.52)
  ),
  race = sample(
    race_levels,
    n_subjects,
    replace = TRUE,
    prob = c(0.65, 0.12, 0.08, 0.10, 0.05)
  ),
  age = round(rnorm(n_subjects, mean = 54, sd = 13)),
  weight_kg = round(rnorm(n_subjects, mean = 78, sd = 16), 1),
  bmi = round(rnorm(n_subjects, mean = 27, sd = 5), 1),
  enrolment_date = sample(
    seq(study_start, study_end - 120, by = "day"),
    n_subjects,
    replace = TRUE
  )
)

subjects <- subjects %>%
  mutate(
    age = pmin(pmax(age, 18), 85),
    bmi = pmin(pmax(bmi, 16), 45),
    discontinued = rbinom(
      n_subjects,
      size = 1,
      prob = if_else(treatment_arm == "Active Treatment", 0.12, 0.08)
    ),
    discontinuation_reason = case_when(
      discontinued == 0 ~ "Completed / Ongoing",
      discontinued == 1 ~ sample(
        c(
          "Adverse Event",
          "Lost to Follow-up",
          "Withdrawal by Subject",
          "Protocol Deviation"
        ),
        n_subjects,
        replace = TRUE,
        prob = c(0.35, 0.25, 0.25, 0.15)
      )
    )
  )

visits <- subjects %>%
  select(subject_id, site_id, treatment_arm, enrolment_date) %>%
  tidyr::crossing(visit_schedule) %>%
  mutate(
    expected_visit_date = enrolment_date + planned_day,
    visit_jitter = round(rnorm(n(), mean = 0, sd = 4)),
    actual_visit_date = expected_visit_date + visit_jitter,
    visit_completed = rbinom(
      n(),
      size = 1,
      prob = case_when(
        visit_number <= 2 ~ 0.98,
        visit_number == 3 ~ 0.94,
        visit_number == 4 ~ 0.90,
        TRUE ~ 0.86
      )
    ),
    days_from_planned = as.numeric(actual_visit_date - expected_visit_date),
    visit_window_status = case_when(
      visit_completed == 0 ~ "Missed",
      abs(days_from_planned) <= 7 ~ "Within Window",
      abs(days_from_planned) > 7 ~ "Out of Window"
    )
  ) %>%
  mutate(
    actual_visit_date = if_else(
      visit_completed == 1,
      actual_visit_date,
      as.Date(NA)
    ),
    days_from_planned = if_else(
      visit_completed == 1,
      days_from_planned,
      NA_real_
    )
  )

adverse_events <- subjects %>%
  rowwise() %>%
  mutate(
    expected_ae_count = case_when(
      treatment_arm == "Active Treatment" ~ 1.30,
      treatment_arm == "Placebo" ~ 0.90
    ),
    n_ae = rpois(1, lambda = expected_ae_count)
  ) %>%
  ungroup() %>%
  filter(n_ae > 0) %>%
  tidyr::uncount(n_ae) %>%
  group_by(subject_id) %>%
  mutate(ae_sequence = row_number()) %>%
  ungroup() %>%
  mutate(
    ae_id = paste0(
      subject_id,
      "-AE-",
      stringr::str_pad(ae_sequence, width = 2, pad = "0")
    ),
    ae_term = sample(ae_terms, n(), replace = TRUE),
    system_organ_class = sample(ae_system_organs, n(), replace = TRUE),
    ae_start_day = sample(1:120, n(), replace = TRUE),
    ae_start_date = enrolment_date + ae_start_day,
    severity = sample(
      c("Mild", "Moderate", "Severe"),
      n(),
      replace = TRUE,
      prob = c(0.55, 0.35, 0.10)
    ),
    seriousness = rbinom(
      n(),
      size = 1,
      prob = if_else(severity == "Severe", 0.20, 0.03)
    ),
    seriousness = if_else(seriousness == 1, "Serious", "Non-serious"),
    relationship = sample(
      c("Not Related", "Unlikely", "Possible", "Probable", "Definite"),
      n(),
      replace = TRUE,
      prob = c(0.30, 0.25, 0.25, 0.15, 0.05)
    ),
    outcome = sample(
      c("Recovered", "Recovering", "Not Recovered", "Unknown"),
      n(),
      replace = TRUE,
      prob = c(0.55, 0.25, 0.15, 0.05)
    )
  ) %>%
  select(
    ae_id,
    subject_id,
    site_id,
    treatment_arm,
    ae_sequence,
    ae_term,
    system_organ_class,
    ae_start_date,
    ae_start_day,
    severity,
    seriousness,
    relationship,
    outcome
  )

labs <- subjects %>%
  select(subject_id, site_id, treatment_arm, enrolment_date) %>%
  tidyr::crossing(visit_schedule) %>%
  tidyr::crossing(lab_reference) %>%
  mutate(
    lab_date = enrolment_date + planned_day + round(rnorm(n(), mean = 0, sd = 3)),
    treatment_shift = case_when(
      treatment_arm == "Active Treatment" &
        lab_test %in% c("ALT", "AST") &
        visit_number >= 3 ~ 8,
      treatment_arm == "Active Treatment" &
        lab_test == "Neutrophils" &
        visit_number >= 3 ~ -0.5,
      TRUE ~ 0
    ),
    value = rnorm(
      n(),
      mean = mean_value + treatment_shift,
      sd = sd_value
    ),
    value = case_when(
      lab_test %in% c("ALT", "AST", "Creatinine") ~ pmax(value, 1),
      lab_test %in% c("Haemoglobin", "Platelets", "Neutrophils") ~ pmax(value, 0.1),
      TRUE ~ value
    ),
    value = round(value, 2),
    abnormal_flag = case_when(
      value < lower_limit ~ "Low",
      value > upper_limit ~ "High",
      TRUE ~ "Normal"
    ),
    toxicity_grade = case_when(
      abnormal_flag == "Normal" ~ 0,
      lab_test %in% c("ALT", "AST") &
        value > upper_limit &
        value <= 3 * upper_limit ~ 1,
      lab_test %in% c("ALT", "AST") &
        value > 3 * upper_limit &
        value <= 5 * upper_limit ~ 2,
      lab_test %in% c("ALT", "AST") &
        value > 5 * upper_limit ~ 3,
      lab_test == "Neutrophils" &
        value < lower_limit &
        value >= 1.5 ~ 1,
      lab_test == "Neutrophils" &
        value < 1.5 &
        value >= 1.0 ~ 2,
      lab_test == "Neutrophils" &
        value < 1.0 ~ 3,
      abnormal_flag != "Normal" ~ 1,
      TRUE ~ 0
    )
  ) %>%
  select(
    subject_id,
    site_id,
    treatment_arm,
    visit_number,
    visit_name,
    lab_date,
    lab_test,
    value,
    unit,
    lower_limit,
    upper_limit,
    abnormal_flag,
    toxicity_grade
  )

exposure <- subjects %>%
  mutate(
    first_dose_date = enrolment_date,
    planned_treatment_days = 84,
    actual_treatment_days = case_when(
      discontinued == 1 ~ sample(14:80, n(), replace = TRUE),
      discontinued == 0 ~ planned_treatment_days
    ),
    last_dose_date = first_dose_date + actual_treatment_days,
    dose_mg = if_else(treatment_arm == "Active Treatment", 100, 0),
    compliance_rate = round(rnorm(n(), mean = 0.93, sd = 0.08), 2),
    compliance_rate = pmin(pmax(compliance_rate, 0.55), 1.00),
    compliance_category = case_when(
      compliance_rate >= 0.90 ~ "High",
      compliance_rate >= 0.75 ~ "Moderate",
      TRUE ~ "Low"
    )
  ) %>%
  select(
    subject_id,
    site_id,
    treatment_arm,
    first_dose_date,
    last_dose_date,
    planned_treatment_days,
    actual_treatment_days,
    dose_mg,
    compliance_rate,
    compliance_category,
    discontinued,
    discontinuation_reason
  )

site_risk <- subjects %>%
  group_by(site_id) %>%
  summarise(
    enrolled_subjects = n(),
    discontinuation_rate = mean(discontinued),
    .groups = "drop"
  ) %>%
  left_join(
    adverse_events %>%
      group_by(site_id) %>%
      summarise(
        total_ae = n(),
        serious_ae = sum(seriousness == "Serious"),
        .groups = "drop"
      ),
    by = "site_id"
  ) %>%
  left_join(
    visits %>%
      group_by(site_id) %>%
      summarise(
        missed_visit_rate = mean(visit_completed == 0),
        out_of_window_rate = mean(visit_window_status == "Out of Window"),
        .groups = "drop"
      ),
    by = "site_id"
  ) %>%
  mutate(
    total_ae = replace_na(total_ae, 0),
    serious_ae = replace_na(serious_ae, 0),
    ae_rate = total_ae / enrolled_subjects,
    serious_ae_rate = serious_ae / enrolled_subjects,
    site_risk_score =
      30 * discontinuation_rate +
      25 * missed_visit_rate +
      20 * out_of_window_rate +
      15 * serious_ae_rate +
      10 * ae_rate,
    site_risk_score = round(site_risk_score, 2),
    site_risk_category = case_when(
      site_risk_score >= quantile(site_risk_score, 0.75) ~ "High Risk",
      site_risk_score >= quantile(site_risk_score, 0.40) ~ "Medium Risk",
      TRUE ~ "Low Risk"
    )
  )

if (!dir.exists("data")) {
  dir.create("data")
}

write_csv(subjects, "data/simulated_subjects.csv")
write_csv(visits, "data/simulated_visits.csv")
write_csv(adverse_events, "data/simulated_adverse_events.csv")
write_csv(labs, "data/simulated_labs.csv")
write_csv(exposure, "data/simulated_exposure.csv")
write_csv(site_risk, "data/simulated_site_risk.csv")

cat("Synthetic clinical trial datasets generated successfully.\n")
cat("Files saved in the data folder.\n\n")

cat("Dataset summary:\n")
cat("Subjects:", nrow(subjects), "\n")
cat("Visits:", nrow(visits), "\n")
cat("Adverse events:", nrow(adverse_events), "\n")
cat("Laboratory records:", nrow(labs), "\n")
cat("Exposure records:", nrow(exposure), "\n")
cat("Site risk records:", nrow(site_risk), "\n")