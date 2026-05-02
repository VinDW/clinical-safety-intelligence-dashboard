# Clinical Safety Intelligence Dashboard

An interactive R Shiny dashboard for clinical trial safety monitoring, adverse event analytics, laboratory surveillance, patient risk profiling, and site-level risk review.

## Project Overview

This project presents a clinical trial safety intelligence dashboard built using R and Shiny. The dashboard simulates a clinical trial environment and transforms safety-related data into an interactive decision-support tool for clinical monitoring and risk-based review.

The project focuses on real-time visual analytics for trial oversight, including adverse event monitoring, laboratory abnormality tracking, treatment exposure review, patient-level risk scoring, and site-level risk escalation.

The project uses synthetic clinical trial data only. No real patient data is included.

## Key Features

- Interactive R Shiny dashboard
- Synthetic clinical trial data generation
- Adverse event monitoring
- Serious adverse event tracking
- Laboratory abnormality surveillance
- Patient-level safety risk scoring
- Site-level risk ranking
- Automated clinical safety interpretation
- Executive safety review module
- Treatment arm and clinical site filters
- Patient profile exploration
- Interactive Plotly visualisations
- Dynamic data tables
- Downloadable filtered reports

- ## Dashboard Preview

### Executive Review

![Executive Review](screenshots/Executive%20Review.png)

### Study Overview

![Study Overview](screenshots/Study%20Overview.png)

### Adverse Events

![Adverse Events](screenshots/Adverse%20Events.png)

### Laboratory Monitoring

![Laboratory Monitoring](screenshots/Laboratory%20Moni.png)

### Patient Profiles

![Patient Profiles](screenshots/Patient%20Profiles.png)

### Site Risk

![Site Risk](screenshots/Site%20Risk.png)

## Dashboard Modules

### Executive Review

Provides an automated safety interpretation based on the selected filters. This module summarises the overall safety classification, serious adverse event rate, laboratory abnormality rate, high-risk patients, site escalation watchlist, adverse event signals, and laboratory safety signals.

### Study Overview

Summarises the filtered trial population, treatment allocation, clinical sites, adverse event burden, serious adverse events, treatment compliance, discontinuation rate, and enrolment trends.

### Adverse Events

Allows users to explore adverse events by treatment arm, clinical site, severity, seriousness, event term, and event timing.

### Laboratory Monitoring

Tracks laboratory values across visits and treatment arms. The module provides mean laboratory trends, abnormality profiles, laboratory summaries, and detailed laboratory listings.

### Patient Profiles

Provides subject-level review of patient risk scores, safety category, laboratory history, and adverse event history.

### Site Risk

Ranks clinical sites using a composite risk score and highlights key risk drivers such as missed visits, serious adverse event rate, out-of-window visits, and discontinuation rate.

### Downloads

Allows users to export filtered subject data, adverse event data, laboratory data, and site-risk tables.

## Project Structure

```text
clinical-safety-intelligence-dashboard/
│
├── app.R
├── README.md
│
├── R/
│   ├── 01_generate_simulated_data.R
│   ├── 02_data_preparation.R
│   └── 03_dashboard_functions.R
│
├── data/
│   ├── simulated_subjects.csv
│   ├── simulated_visits.csv
│   ├── simulated_adverse_events.csv
│   ├── simulated_labs.csv
│   ├── simulated_exposure.csv
│   ├── simulated_site_risk.csv
│   │
│   └── processed/
│       ├── study_kpis.csv
│       ├── enrolment_summary.csv
│       ├── treatment_summary.csv
│       ├── ae_summary_by_arm.csv
│       ├── ae_term_frequency.csv
│       ├── lab_summary.csv
│       ├── visit_summary.csv
│       ├── exposure_summary.csv
│       ├── patient_profile.csv
│       └── site_risk_enhanced.csv
```

## Tools Used

This project was developed using:

- R
- Shiny
- shinydashboard
- tidyverse
- plotly
- DT
- scales
- lubridate
- janitor

## How to Run the Dashboard

Install the required R packages:

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "tidyverse",
  "plotly",
  "DT",
  "scales",
  "lubridate",
  "janitor"
))
```

Run the data generation script:

```r
source("R/01_generate_simulated_data.R")
```

Run the data preparation script:

```r
source("R/02_data_preparation.R")
```

Launch the dashboard:

```r
shiny::runApp()
```

## Data Description

The project uses synthetic clinical trial datasets generated for demonstration and portfolio purposes.

The simulated data includes:

- Subject demographics and treatment allocation
- Visit-level data
- Adverse event records
- Laboratory test results
- Treatment exposure and compliance data
- Site-level monitoring data

## Risk Scoring Approach

The dashboard includes patient-level and site-level risk scoring.

Patient risk scoring considers:

- Serious adverse events
- Severe adverse events
- Abnormal laboratory results
- Grade 2 or higher laboratory findings
- Treatment discontinuation

Site risk scoring considers:

- Adverse event rates
- Serious adverse event rates
- Missed visit rates
- Out-of-window visit rates
- Discontinuation rates

The scoring framework is designed for demonstration purposes and can be extended for real-world clinical monitoring workflows.

## Skills Demonstrated

This project demonstrates practical skills in:

- R programming
- Shiny dashboard development
- Clinical trial analytics
- Data simulation
- Data preparation
- Risk scoring
- Exploratory data analysis
- Interactive visual analytics
- Dashboard design
- Reproducible project structuring

## Author

Vincent Kometsi
