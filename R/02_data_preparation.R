# 02 | Dashboard Data Preparation
# Clinical Safety Intelligence Dashboard
# Developed by Vincent Kometsi
# Prepares synthetic clinical trial data for safety analytics and Shiny visualisation.

library(tidyverse)
library(lubridate)
library(janitor)

subjects <- read_csv("data/simulated_subjects.csv", show_col_types = FALSE)
visits <- read_csv("data/simulated_visits.csv", show_col_types = FALSE)
adverse_events <- read_csv("data/simulated_adverse_events.csv", show_col_types = FALSE)
labs <- read_csv("data/simulated_labs.csv", show_col_types = FALSE)
exposure <- read_csv("data/simulated_exposure.csv", show_col_types = FALSE)
site_risk <- read_csv("data/simulated_site_risk.csv", show_col_types = FALSE)

subjects <- subjects %>%
  clean_names() %>%
  mutate(
    enrolment_date = as.Date(enrolment_date),
    treatment_arm = factor(treatment_arm),
    sex = factor(sex),
    race = factor(race),
    discontinued = factor(
      discontinued,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

visits <- visits %>%
  clean_names() %>%
  mutate(
    enrolment_date = as.Date(enrolment_date),
    expected_visit_date = as.Date(expected_visit_date),
    actual_visit_date = as.Date(actual_visit_date),
    treatment_arm = factor(treatment_arm),
    visit_completed = factor(
      visit_completed,
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    visit_window_status = factor(
      visit_window_status,
      levels = c("Within Window", "Out of Window", "Missed")
    )
  )

adverse_events <- adverse_events %>%
  clean_names() %>%
  mutate(
    ae_start_date = as.Date(ae_start_date),
    treatment_arm = factor(treatment_arm),
    severity = factor(
      severity,
      levels = c("Mild", "Moderate", "Severe")
    ),
    seriousness = factor(
      seriousness,
      levels = c("Non-serious", "Serious")
    ),
    relationship = factor(
      relationship,
      levels = c("Not Related", "Unlikely", "Possible", "Probable", "Definite")
    )
  )

labs <- labs %>%
  clean_names() %>%
  mutate(
    lab_date = as.Date(lab_date),
    treatment_arm = factor(treatment_arm),
    abnormal_flag = factor(
      abnormal_flag,
      levels = c("Normal", "Low", "High")
    ),
    toxicity_grade = factor(
      toxicity_grade,
      levels = c(0, 1, 2, 3),
      labels = c("Grade 0", "Grade 1", "Grade 2", "Grade 3")
    )
  )

exposure <- exposure %>%
  clean_names() %>%
  mutate(
    first_dose_date = as.Date(first_dose_date),
    last_dose_date = as.Date(last_dose_date),
    treatment_arm = factor(treatment_arm),
    compliance_category = factor(
      compliance_category,
      levels = c("High", "Moderate", "Low")
    ),
    discontinued = factor(
      discontinued,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

site_risk <- site_risk %>%
  clean_names() %>%
  mutate(
    site_risk_category = factor(
      site_risk_category,
      levels = c("Low Risk", "Medium Risk", "High Risk")
    )
  )

study_kpis <- tibble(
  metric = c(
    "Total Subjects",
    "Clinical Sites",
    "Treatment Arms",
    "Total Adverse Events",
    "Serious Adverse Events",
    "Laboratory Records",
    "Mean Compliance Rate",
    "Overall Discontinuation Rate"
  ),
  value = c(
    nrow(subjects),
    n_distinct(subjects$site_id),
    n_distinct(subjects$treatment_arm),
    nrow(adverse_events),
    sum(adverse_events$seriousness == "Serious"),
    nrow(labs),
    round(mean(exposure$compliance_rate, na.rm = TRUE), 3),
    round(mean(subjects$discontinued == "Yes"), 3)
  )
)

enrolment_summary <- subjects %>%
  mutate(enrolment_month = floor_date(enrolment_date, unit = "month")) %>%
  count(enrolment_month, treatment_arm, name = "subjects_enrolled") %>%
  group_by(treatment_arm) %>%
  arrange(enrolment_month) %>%
  mutate(cumulative_enrolment = cumsum(subjects_enrolled)) %>%
  ungroup()

treatment_summary <- subjects %>%
  count(treatment_arm, name = "n_subjects") %>%
  mutate(
    percentage = round(100 * n_subjects / sum(n_subjects), 1)
  )

site_enrolment_summary <- subjects %>%
  count(site_id, treatment_arm, name = "n_subjects") %>%
  group_by(site_id) %>%
  mutate(site_total = sum(n_subjects)) %>%
  ungroup()

ae_summary_by_arm <- adverse_events %>%
  group_by(treatment_arm) %>%
  summarise(
    total_ae = n(),
    serious_ae = sum(seriousness == "Serious"),
    severe_ae = sum(severity == "Severe"),
    related_ae = sum(relationship %in% c("Possible", "Probable", "Definite")),
    unique_subjects_with_ae = n_distinct(subject_id),
    .groups = "drop"
  ) %>%
  left_join(
    subjects %>%
      count(treatment_arm, name = "n_subjects"),
    by = "treatment_arm"
  ) %>%
  mutate(
    ae_rate_per_subject = round(total_ae / n_subjects, 3),
    serious_ae_rate = round(serious_ae / n_subjects, 3),
    subject_ae_incidence = round(unique_subjects_with_ae / n_subjects, 3)
  )

ae_term_frequency <- adverse_events %>%
  count(ae_term, severity, name = "n") %>%
  group_by(ae_term) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  arrange(desc(total))

ae_soc_summary <- adverse_events %>%
  count(system_organ_class, treatment_arm, name = "n") %>%
  group_by(treatment_arm) %>%
  mutate(percentage = round(100 * n / sum(n), 1)) %>%
  ungroup()

ae_timeline <- adverse_events %>%
  mutate(ae_month = floor_date(ae_start_date, unit = "month")) %>%
  count(ae_month, treatment_arm, seriousness, name = "n_ae") %>%
  group_by(treatment_arm, seriousness) %>%
  arrange(ae_month) %>%
  mutate(cumulative_ae = cumsum(n_ae)) %>%
  ungroup()

lab_summary <- labs %>%
  group_by(lab_test, treatment_arm, visit_name) %>%
  summarise(
    mean_value = round(mean(value, na.rm = TRUE), 2),
    median_value = round(median(value, na.rm = TRUE), 2),
    sd_value = round(sd(value, na.rm = TRUE), 2),
    abnormal_count = sum(abnormal_flag != "Normal"),
    grade_2_or_higher = sum(toxicity_grade %in% c("Grade 2", "Grade 3")),
    n_records = n(),
    .groups = "drop"
  ) %>%
  mutate(
    abnormal_rate = round(abnormal_count / n_records, 3),
    grade_2_or_higher_rate = round(grade_2_or_higher / n_records, 3)
  )

lab_abnormality_summary <- labs %>%
  count(lab_test, abnormal_flag, treatment_arm, name = "n") %>%
  group_by(lab_test, treatment_arm) %>%
  mutate(percentage = round(100 * n / sum(n), 1)) %>%
  ungroup()

toxicity_summary <- labs %>%
  count(lab_test, toxicity_grade, treatment_arm, name = "n") %>%
  group_by(lab_test, treatment_arm) %>%
  mutate(percentage = round(100 * n / sum(n), 1)) %>%
  ungroup()

visit_summary <- visits %>%
  group_by(visit_name, treatment_arm) %>%
  summarise(
    expected_visits = n(),
    completed_visits = sum(visit_completed == "Yes"),
    missed_visits = sum(visit_completed == "No"),
    out_of_window_visits = sum(visit_window_status == "Out of Window"),
    .groups = "drop"
  ) %>%
  mutate(
    completion_rate = round(completed_visits / expected_visits, 3),
    missed_rate = round(missed_visits / expected_visits, 3),
    out_of_window_rate = round(out_of_window_visits / expected_visits, 3)
  )

exposure_summary <- exposure %>%
  group_by(treatment_arm) %>%
  summarise(
    n_subjects = n(),
    mean_treatment_days = round(mean(actual_treatment_days), 1),
    median_treatment_days = round(median(actual_treatment_days), 1),
    mean_compliance = round(mean(compliance_rate), 3),
    low_compliance_subjects = sum(compliance_category == "Low"),
    discontinued_subjects = sum(discontinued == "Yes"),
    .groups = "drop"
  ) %>%
  mutate(
    low_compliance_rate = round(low_compliance_subjects / n_subjects, 3),
    discontinuation_rate = round(discontinued_subjects / n_subjects, 3)
  )

patient_profile <- subjects %>%
  left_join(
    adverse_events %>%
      group_by(subject_id) %>%
      summarise(
        total_ae = n(),
        serious_ae = sum(seriousness == "Serious"),
        severe_ae = sum(severity == "Severe"),
        related_ae = sum(relationship %in% c("Possible", "Probable", "Definite")),
        .groups = "drop"
      ),
    by = "subject_id"
  ) %>%
  left_join(
    labs %>%
      group_by(subject_id) %>%
      summarise(
        abnormal_labs = sum(abnormal_flag != "Normal"),
        grade_2_or_higher_labs = sum(toxicity_grade %in% c("Grade 2", "Grade 3")),
        .groups = "drop"
      ),
    by = "subject_id"
  ) %>%
  left_join(
    exposure %>%
      select(subject_id, actual_treatment_days, compliance_rate, compliance_category),
    by = "subject_id"
  ) %>%
  mutate(
    total_ae = replace_na(total_ae, 0),
    serious_ae = replace_na(serious_ae, 0),
    severe_ae = replace_na(severe_ae, 0),
    related_ae = replace_na(related_ae, 0),
    abnormal_labs = replace_na(abnormal_labs, 0),
    grade_2_or_higher_labs = replace_na(grade_2_or_higher_labs, 0),
    patient_risk_score =
      20 * serious_ae +
      15 * severe_ae +
      10 * related_ae +
      8 * grade_2_or_higher_labs +
      5 * abnormal_labs +
      if_else(discontinued == "Yes", 15, 0),
    patient_risk_category = case_when(
      patient_risk_score >= quantile(patient_risk_score, 0.80) ~ "High Risk",
      patient_risk_score >= quantile(patient_risk_score, 0.50) ~ "Medium Risk",
      TRUE ~ "Low Risk"
    )
  )

site_risk_enhanced <- site_risk %>%
  mutate(
    risk_rank = dense_rank(desc(site_risk_score)),
    key_risk_driver = case_when(
      missed_visit_rate >= quantile(missed_visit_rate, 0.75) ~ "High missed-visit rate",
      out_of_window_rate >= quantile(out_of_window_rate, 0.75) ~ "Visit-window deviations",
      serious_ae_rate >= quantile(serious_ae_rate, 0.75) ~ "High serious AE rate",
      discontinuation_rate >= quantile(discontinuation_rate, 0.75) ~ "High discontinuation rate",
      TRUE ~ "General monitoring"
    )
  )

if (!dir.exists("data/processed")) {
  dir.create("data/processed", recursive = TRUE)
}

write_csv(study_kpis, "data/processed/study_kpis.csv")
write_csv(enrolment_summary, "data/processed/enrolment_summary.csv")
write_csv(treatment_summary, "data/processed/treatment_summary.csv")
write_csv(site_enrolment_summary, "data/processed/site_enrolment_summary.csv")
write_csv(ae_summary_by_arm, "data/processed/ae_summary_by_arm.csv")
write_csv(ae_term_frequency, "data/processed/ae_term_frequency.csv")
write_csv(ae_soc_summary, "data/processed/ae_soc_summary.csv")
write_csv(ae_timeline, "data/processed/ae_timeline.csv")
write_csv(lab_summary, "data/processed/lab_summary.csv")
write_csv(lab_abnormality_summary, "data/processed/lab_abnormality_summary.csv")
write_csv(toxicity_summary, "data/processed/toxicity_summary.csv")
write_csv(visit_summary, "data/processed/visit_summary.csv")
write_csv(exposure_summary, "data/processed/exposure_summary.csv")
write_csv(patient_profile, "data/processed/patient_profile.csv")
write_csv(site_risk_enhanced, "data/processed/site_risk_enhanced.csv")

cat("Dashboard-ready datasets created successfully.\n")
cat("Files saved in data/processed folder.\n\n")

cat("Processed dataset summary:\n")
cat("Study KPI records:", nrow(study_kpis), "\n")
cat("Enrolment summary records:", nrow(enrolment_summary), "\n")
cat("AE summary by arm records:", nrow(ae_summary_by_arm), "\n")
cat("Lab summary records:", nrow(lab_summary), "\n")
cat("Patient profile records:", nrow(patient_profile), "\n")
cat("Enhanced site risk records:", nrow(site_risk_enhanced), "\n")