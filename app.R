# Clinical Safety Intelligence Dashboard
# Developed by Vincent Kometsi

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(scales)

source("R/03_dashboard_functions.R")

subjects <- read_csv("data/simulated_subjects.csv", show_col_types = FALSE)
adverse_events <- read_csv("data/simulated_adverse_events.csv", show_col_types = FALSE)
labs <- read_csv("data/simulated_labs.csv", show_col_types = FALSE)
visits <- read_csv("data/simulated_visits.csv", show_col_types = FALSE)
exposure <- read_csv("data/simulated_exposure.csv", show_col_types = FALSE)

patient_profile <- read_csv("data/processed/patient_profile.csv", show_col_types = FALSE)
site_risk_enhanced <- read_csv("data/processed/site_risk_enhanced.csv", show_col_types = FALSE)

subjects <- subjects %>%
  mutate(enrolment_date = as.Date(enrolment_date))

adverse_events <- adverse_events %>%
  mutate(ae_start_date = as.Date(ae_start_date))

labs <- labs %>%
  mutate(lab_date = as.Date(lab_date))

visits <- visits %>%
  mutate(
    enrolment_date = as.Date(enrolment_date),
    expected_visit_date = as.Date(expected_visit_date),
    actual_visit_date = as.Date(actual_visit_date)
  )

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = "Clinical Safety Intelligence"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Executive Review", tabName = "executive", icon = icon("medkit")),
      menuItem("Study Overview", tabName = "overview", icon = icon("line-chart")),
      menuItem("Adverse Events", tabName = "ae", icon = icon("exclamation-triangle")),
      menuItem("Laboratory Monitoring", tabName = "labs", icon = icon("flask")),
      menuItem("Patient Profiles", tabName = "patients", icon = icon("user")),
      menuItem("Site Risk", tabName = "site_risk", icon = icon("hospital-o")),
      menuItem("Downloads", tabName = "downloads", icon = icon("download")),
      
      hr(),
      
      selectInput(
        inputId = "filter_arm",
        label = "Treatment Arm",
        choices = c("All", sort(unique(subjects$treatment_arm))),
        selected = "All"
      ),
      
      selectInput(
        inputId = "filter_site",
        label = "Clinical Site",
        choices = c("All", sort(unique(subjects$site_id))),
        selected = "All"
      )
    )
  ),
  
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
        .small-box { border-radius: 14px; }
        .box { border-radius: 12px; }
        .content-wrapper { background-color: #f4f6f9; }
        .main-header .logo { font-weight: bold; }
        .box-title { font-weight: bold; }
      "))
    ),
    
    tabItems(
      
      tabItem(
        tabName = "executive",
        
        fluidRow(
          box(
            title = "Automated Clinical Safety Interpretation",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            uiOutput("safety_interpretation_text")
          )
        ),
        
        fluidRow(
          valueBoxOutput("executive_safety_level"),
          valueBoxOutput("executive_sae_rate"),
          valueBoxOutput("executive_lab_abnormality")
        ),
        
        fluidRow(
          box(
            title = "Top High-Risk Patients for Clinical Review",
            width = 12,
            status = "danger",
            solidHeader = TRUE,
            DTOutput("high_risk_patient_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Site Escalation Watchlist",
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            DTOutput("site_escalation_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Top Adverse Event Safety Signals",
            width = 6,
            status = "danger",
            solidHeader = TRUE,
            DTOutput("ae_signal_table")
          ),
          box(
            title = "Top Laboratory Safety Signals",
            width = 6,
            status = "warning",
            solidHeader = TRUE,
            DTOutput("lab_signal_table")
          )
        )
      ),
      
      tabItem(
        tabName = "overview",
        
        fluidRow(
          valueBoxOutput("total_subjects"),
          valueBoxOutput("total_sites"),
          valueBoxOutput("total_ae")
        ),
        
        fluidRow(
          valueBoxOutput("serious_ae_box"),
          valueBoxOutput("mean_compliance_box"),
          valueBoxOutput("discontinuation_box")
        ),
        
        fluidRow(
          box(
            title = "Cumulative Enrolment by Treatment Arm",
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("enrolment_plot")
          ),
          box(
            title = "Treatment Allocation",
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("treatment_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "Filtered Subject Table",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            DTOutput("subject_table")
          )
        )
      ),
      
      tabItem(
        tabName = "ae",
        
        fluidRow(
          box(
            title = "Adverse Event Filters",
            width = 12,
            status = "danger",
            solidHeader = TRUE,
            selectInput(
              inputId = "filter_severity",
              label = "Severity",
              choices = c("All", sort(unique(adverse_events$severity))),
              selected = "All"
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Adverse Event Summary by Treatment Arm",
            width = 12,
            status = "danger",
            solidHeader = TRUE,
            DTOutput("ae_summary_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Most Frequent Adverse Events",
            width = 6,
            status = "danger",
            solidHeader = TRUE,
            plotlyOutput("ae_term_plot")
          ),
          box(
            title = "Adverse Events Over Time",
            width = 6,
            status = "danger",
            solidHeader = TRUE,
            plotlyOutput("ae_timeline_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "Filtered Adverse Event Listing",
            width = 12,
            status = "danger",
            solidHeader = TRUE,
            DTOutput("ae_listing_table")
          )
        )
      ),
      
      tabItem(
        tabName = "labs",
        
        fluidRow(
          box(
            title = "Laboratory Filter",
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            selectInput(
              inputId = "filter_lab",
              label = "Laboratory Test",
              choices = c("All", sort(unique(labs$lab_test))),
              selected = "All"
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Mean Laboratory Values by Visit",
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("lab_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "Laboratory Abnormality Profile",
            width = 6,
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("lab_abnormality_plot")
          ),
          box(
            title = "Laboratory Summary Table",
            width = 6,
            status = "warning",
            solidHeader = TRUE,
            DTOutput("lab_summary_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Filtered Laboratory Listing",
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            DTOutput("lab_listing_table")
          )
        )
      ),
      
      tabItem(
        tabName = "patients",
        
        fluidRow(
          box(
            title = "Select Patient",
            width = 4,
            status = "info",
            solidHeader = TRUE,
            uiOutput("patient_selector_ui")
          ),
          valueBoxOutput("patient_risk_score"),
          valueBoxOutput("patient_risk_category")
        ),
        
        fluidRow(
          box(
            title = "Patient Profile",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            DTOutput("patient_profile_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Patient Laboratory History",
            width = 6,
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("patient_lab_plot")
          ),
          box(
            title = "Patient Adverse Event History",
            width = 6,
            status = "info",
            solidHeader = TRUE,
            DTOutput("patient_ae_table")
          )
        )
      ),
      
      tabItem(
        tabName = "site_risk",
        
        fluidRow(
          valueBoxOutput("high_risk_sites_box"),
          valueBoxOutput("medium_risk_sites_box"),
          valueBoxOutput("low_risk_sites_box")
        ),
        
        fluidRow(
          box(
            title = "Site Risk Score Ranking",
            width = 7,
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("site_risk_plot")
          ),
          box(
            title = "Site Risk Matrix",
            width = 5,
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("site_risk_matrix")
          )
        ),
        
        fluidRow(
          box(
            title = "Site-Level Risk Monitoring Table",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            DTOutput("site_risk_table")
          )
        )
      ),
      
      tabItem(
        tabName = "downloads",
        
        fluidRow(
          box(
            title = "Export Dashboard Data",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            p("Download filtered dashboard datasets for review, reporting, and portfolio demonstration."),
            br(),
            downloadButton("download_subjects", "Download Filtered Subjects"),
            br(), br(),
            downloadButton("download_ae", "Download Filtered Adverse Events"),
            br(), br(),
            downloadButton("download_labs", "Download Filtered Laboratory Data"),
            br(), br(),
            downloadButton("download_site_risk", "Download Site Risk Table")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_subjects <- reactive({
    data <- subjects
    
    if (input$filter_arm != "All") {
      data <- data %>% filter(treatment_arm == input$filter_arm)
    }
    
    if (input$filter_site != "All") {
      data <- data %>% filter(site_id == input$filter_site)
    }
    
    data
  })
  
  filtered_ae <- reactive({
    data <- adverse_events
    
    if (input$filter_arm != "All") {
      data <- data %>% filter(treatment_arm == input$filter_arm)
    }
    
    if (input$filter_site != "All") {
      data <- data %>% filter(site_id == input$filter_site)
    }
    
    if (!is.null(input$filter_severity) && input$filter_severity != "All") {
      data <- data %>% filter(severity == input$filter_severity)
    }
    
    data
  })
  
  filtered_labs <- reactive({
    data <- labs
    
    if (input$filter_arm != "All") {
      data <- data %>% filter(treatment_arm == input$filter_arm)
    }
    
    if (input$filter_site != "All") {
      data <- data %>% filter(site_id == input$filter_site)
    }
    
    if (!is.null(input$filter_lab) && input$filter_lab != "All") {
      data <- data %>% filter(lab_test == input$filter_lab)
    }
    
    data
  })
  
  filtered_exposure <- reactive({
    data <- exposure
    
    if (input$filter_arm != "All") {
      data <- data %>% filter(treatment_arm == input$filter_arm)
    }
    
    if (input$filter_site != "All") {
      data <- data %>% filter(site_id == input$filter_site)
    }
    
    data
  })
  
  filtered_patient_profile <- reactive({
    data <- patient_profile
    
    if (input$filter_arm != "All") {
      data <- data %>% filter(treatment_arm == input$filter_arm)
    }
    
    if (input$filter_site != "All") {
      data <- data %>% filter(site_id == input$filter_site)
    }
    
    data
  })
  
  filtered_site_risk <- reactive({
    data <- site_risk_enhanced
    
    if (input$filter_site != "All") {
      data <- data %>% filter(site_id == input$filter_site)
    }
    
    data
  })
  
  executive_interpretation <- reactive({
    create_safety_interpretation(
      subjects = filtered_subjects(),
      adverse_events = filtered_ae(),
      labs = filtered_labs(),
      exposure = filtered_exposure()
    )
  })
  
  output$safety_interpretation_text <- renderUI({
    x <- executive_interpretation()
    
    HTML(
      paste0(
        "<h3 style='margin-top:0; font-weight:700;'>", x$safety_level, "</h3>",
        "<p style='font-size:16px; line-height:1.7;'>", x$interpretation, "</p>"
      )
    )
  })
  
  output$executive_safety_level <- renderValueBox({
    level <- executive_interpretation()$safety_level
    
    colour <- case_when(
      level == "Elevated Safety Concern" ~ "red",
      level == "Moderate Safety Concern" ~ "yellow",
      TRUE ~ "green"
    )
    
    valueBox(
      value = level,
      subtitle = "Overall Safety Classification",
      icon = icon("shield"),
      color = colour
    )
  })
  
  output$executive_sae_rate <- renderValueBox({
    valueBox(
      value = percent(executive_interpretation()$serious_ae_rate, accuracy = 0.1),
      subtitle = "Serious AE Rate",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$executive_lab_abnormality <- renderValueBox({
    valueBox(
      value = percent(executive_interpretation()$abnormal_lab_rate, accuracy = 0.1),
      subtitle = "Laboratory Abnormality Rate",
      icon = icon("flask"),
      color = "yellow"
    )
  })
  
  output$high_risk_patient_table <- renderDT({
    data <- create_high_risk_patient_table(
      patient_profile = filtered_patient_profile(),
      top_n = 15
    )
    
    datatable(
      data,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$site_escalation_table <- renderDT({
    data <- create_site_escalation_table(
      site_risk_enhanced = filtered_site_risk(),
      top_n = 10
    )
    
    datatable(
      data,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$ae_signal_table <- renderDT({
    signals <- create_signal_summary(
      adverse_events = filtered_ae(),
      labs = filtered_labs()
    )
    
    datatable(
      signals$ae_signals,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$lab_signal_table <- renderDT({
    signals <- create_signal_summary(
      adverse_events = filtered_ae(),
      labs = filtered_labs()
    )
    
    datatable(
      signals$lab_signals,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$total_subjects <- renderValueBox({
    valueBox(
      value = nrow(filtered_subjects()),
      subtitle = "Filtered Subjects",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$total_sites <- renderValueBox({
    valueBox(
      value = n_distinct(filtered_subjects()$site_id),
      subtitle = "Filtered Clinical Sites",
      icon = icon("hospital-o"),
      color = "green"
    )
  })
  
  output$total_ae <- renderValueBox({
    valueBox(
      value = nrow(filtered_ae()),
      subtitle = "Filtered Adverse Events",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$serious_ae_box <- renderValueBox({
    valueBox(
      value = sum(filtered_ae()$seriousness == "Serious", na.rm = TRUE),
      subtitle = "Serious Adverse Events",
      icon = icon("warning"),
      color = "red"
    )
  })
  
  output$mean_compliance_box <- renderValueBox({
    valueBox(
      value = percent(mean(filtered_exposure()$compliance_rate, na.rm = TRUE), accuracy = 0.1),
      subtitle = "Mean Compliance Rate",
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  output$discontinuation_box <- renderValueBox({
    valueBox(
      value = percent(mean(filtered_subjects()$discontinued == 1, na.rm = TRUE), accuracy = 0.1),
      subtitle = "Discontinuation Rate",
      icon = icon("sign-out"),
      color = "yellow"
    )
  })
  
  output$enrolment_plot <- renderPlotly({
    data <- filtered_subjects() %>%
      mutate(enrolment_month = floor_date(enrolment_date, unit = "month")) %>%
      count(enrolment_month, treatment_arm, name = "subjects_enrolled") %>%
      group_by(treatment_arm) %>%
      arrange(enrolment_month) %>%
      mutate(cumulative_enrolment = cumsum(subjects_enrolled)) %>%
      ungroup()
    
    validate(need(nrow(data) > 0, "No enrolment records available for the selected filters."))
    
    p <- data %>%
      ggplot(aes(
        x = enrolment_month,
        y = cumulative_enrolment,
        colour = treatment_arm
      )) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      labs(
        x = "Enrolment Month",
        y = "Cumulative Enrolment",
        colour = "Treatment Arm"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$treatment_plot <- renderPlotly({
    data <- filtered_subjects() %>%
      count(treatment_arm, name = "n_subjects")
    
    validate(need(nrow(data) > 0, "No treatment records available for the selected filters."))
    
    p <- data %>%
      ggplot(aes(
        x = treatment_arm,
        y = n_subjects,
        fill = treatment_arm
      )) +
      geom_col() +
      labs(
        x = "Treatment Arm",
        y = "Number of Subjects"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$subject_table <- renderDT({
    datatable(
      filtered_subjects(),
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$ae_summary_table <- renderDT({
    data <- filtered_ae() %>%
      group_by(treatment_arm) %>%
      summarise(
        total_ae = n(),
        serious_ae = sum(seriousness == "Serious", na.rm = TRUE),
        severe_ae = sum(severity == "Severe", na.rm = TRUE),
        unique_subjects_with_ae = n_distinct(subject_id),
        .groups = "drop"
      )
    
    datatable(data, rownames = FALSE, options = list(pageLength = 5))
  })
  
  output$ae_term_plot <- renderPlotly({
    data <- filtered_ae() %>%
      count(ae_term, name = "total") %>%
      arrange(desc(total)) %>%
      slice_head(n = 10)
    
    validate(need(nrow(data) > 0, "No adverse event records available for the selected filters."))
    
    p <- data %>%
      ggplot(aes(
        x = reorder(ae_term, total),
        y = total
      )) +
      geom_col(fill = "firebrick") +
      coord_flip() +
      labs(
        x = "Adverse Event Term",
        y = "Frequency"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$ae_timeline_plot <- renderPlotly({
    data <- filtered_ae() %>%
      mutate(ae_month = floor_date(ae_start_date, unit = "month")) %>%
      count(ae_month, seriousness, name = "n_ae")
    
    validate(need(nrow(data) > 0, "No adverse event timeline available for the selected filters."))
    
    p <- data %>%
      ggplot(aes(
        x = ae_month,
        y = n_ae,
        colour = seriousness
      )) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2) +
      labs(
        x = "Month",
        y = "Number of Adverse Events",
        colour = "Seriousness"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$ae_listing_table <- renderDT({
    datatable(
      filtered_ae(),
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$lab_plot <- renderPlotly({
    data <- filtered_labs() %>%
      group_by(lab_test, treatment_arm, visit_name, visit_number) %>%
      summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")
    
    validate(need(nrow(data) > 0, "No laboratory records available for the selected filters."))
    
    visit_lookup <- data %>%
      distinct(visit_number, visit_name) %>%
      arrange(visit_number)
    
    p <- data %>%
      ggplot(aes(
        x = visit_number,
        y = mean_value,
        group = treatment_arm,
        colour = treatment_arm
      )) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2) +
      facet_wrap(~ lab_test, scales = "free_y") +
      scale_x_continuous(
        breaks = visit_lookup$visit_number,
        labels = visit_lookup$visit_name
      ) +
      labs(
        x = "Visit",
        y = "Mean Laboratory Value",
        colour = "Treatment Arm"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
    
    ggplotly(p)
  })
  
  output$lab_abnormality_plot <- renderPlotly({
    data <- filtered_labs() %>%
      count(lab_test, abnormal_flag, name = "n")
    
    validate(need(nrow(data) > 0, "No laboratory abnormality records available."))
    
    p <- data %>%
      ggplot(aes(
        x = lab_test,
        y = n,
        fill = abnormal_flag
      )) +
      geom_col(position = "stack") +
      labs(
        x = "Laboratory Test",
        y = "Number of Records",
        fill = "Flag"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
    
    ggplotly(p)
  })
  
  output$lab_summary_table <- renderDT({
    data <- filtered_labs() %>%
      group_by(lab_test, treatment_arm, visit_name) %>%
      summarise(
        mean_value = round(mean(value, na.rm = TRUE), 2),
        median_value = round(median(value, na.rm = TRUE), 2),
        abnormal_count = sum(abnormal_flag != "Normal", na.rm = TRUE),
        n_records = n(),
        abnormal_rate = round(abnormal_count / n_records, 3),
        .groups = "drop"
      )
    
    datatable(data, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$lab_listing_table <- renderDT({
    datatable(
      filtered_labs(),
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$patient_selector_ui <- renderUI({
    choices <- sort(filtered_patient_profile()$subject_id)
    
    validate(need(length(choices) > 0, "No patients available for the selected filters."))
    
    selectInput(
      inputId = "selected_patient",
      label = "Subject ID",
      choices = choices,
      selected = choices[1]
    )
  })
  
  selected_patient_data <- reactive({
    req(input$selected_patient)
    
    filtered_patient_profile() %>%
      filter(subject_id == input$selected_patient)
  })
  
  output$patient_risk_score <- renderValueBox({
    valueBox(
      value = selected_patient_data()$patient_risk_score,
      subtitle = "Patient Risk Score",
      icon = icon("heartbeat"),
      color = "yellow"
    )
  })
  
  output$patient_risk_category <- renderValueBox({
    risk_category <- selected_patient_data()$patient_risk_category
    
    colour <- case_when(
      risk_category == "High Risk" ~ "red",
      risk_category == "Medium Risk" ~ "yellow",
      TRUE ~ "green"
    )
    
    valueBox(
      value = risk_category,
      subtitle = "Patient Risk Category",
      icon = icon("shield"),
      color = colour
    )
  })
  
  output$patient_profile_table <- renderDT({
    datatable(
      selected_patient_data(),
      rownames = FALSE,
      options = list(scrollX = TRUE)
    )
  })
  
  output$patient_lab_plot <- renderPlotly({
    req(input$selected_patient)
    
    data <- labs %>%
      filter(subject_id == input$selected_patient)
    
    validate(need(nrow(data) > 0, "No laboratory history available for this patient."))
    
    p <- data %>%
      ggplot(aes(
        x = visit_number,
        y = value,
        group = lab_test,
        colour = lab_test
      )) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2) +
      facet_wrap(~ lab_test, scales = "free_y") +
      labs(
        x = "Visit Number",
        y = "Laboratory Value",
        colour = "Lab Test"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$patient_ae_table <- renderDT({
    req(input$selected_patient)
    
    data <- adverse_events %>%
      filter(subject_id == input$selected_patient)
    
    datatable(
      data,
      rownames = FALSE,
      options = list(pageLength = 5, scrollX = TRUE)
    )
  })
  
  output$high_risk_sites_box <- renderValueBox({
    valueBox(
      value = sum(site_risk_enhanced$site_risk_category == "High Risk", na.rm = TRUE),
      subtitle = "High Risk Sites",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$medium_risk_sites_box <- renderValueBox({
    valueBox(
      value = sum(site_risk_enhanced$site_risk_category == "Medium Risk", na.rm = TRUE),
      subtitle = "Medium Risk Sites",
      icon = icon("warning"),
      color = "yellow"
    )
  })
  
  output$low_risk_sites_box <- renderValueBox({
    valueBox(
      value = sum(site_risk_enhanced$site_risk_category == "Low Risk", na.rm = TRUE),
      subtitle = "Low Risk Sites",
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  output$site_risk_plot <- renderPlotly({
    data <- filtered_site_risk()
    
    validate(need(nrow(data) > 0, "No site-risk records available for the selected filters."))
    
    p <- data %>%
      ggplot(aes(
        x = reorder(site_id, site_risk_score),
        y = site_risk_score,
        fill = site_risk_category
      )) +
      geom_col() +
      coord_flip() +
      labs(
        x = "Site ID",
        y = "Risk Score",
        fill = "Risk Category"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$site_risk_matrix <- renderPlotly({
    data <- filtered_site_risk()
    
    validate(need(nrow(data) > 0, "No site-risk matrix available for the selected filters."))
    
    p <- data %>%
      ggplot(aes(
        x = missed_visit_rate,
        y = serious_ae_rate,
        text = paste(
          "Site:", site_id,
          "<br>Risk Score:", site_risk_score,
          "<br>Risk Category:", site_risk_category,
          "<br>Driver:", key_risk_driver
        )
      )) +
      geom_point(aes(size = site_risk_score, colour = site_risk_category), alpha = 0.8) +
      labs(
        x = "Missed Visit Rate",
        y = "Serious AE Rate",
        colour = "Risk Category",
        size = "Risk Score"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$site_risk_table <- renderDT({
    datatable(
      filtered_site_risk(),
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$download_subjects <- downloadHandler(
    filename = function() {
      paste0("filtered_subjects_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(filtered_subjects(), file)
    }
  )
  
  output$download_ae <- downloadHandler(
    filename = function() {
      paste0("filtered_adverse_events_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(filtered_ae(), file)
    }
  )
  
  output$download_labs <- downloadHandler(
    filename = function() {
      paste0("filtered_laboratory_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(filtered_labs(), file)
    }
  )
  
  output$download_site_risk <- downloadHandler(
    filename = function() {
      paste0("site_risk_table_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(filtered_site_risk(), file)
    }
  )
}

shinyApp(ui = ui, server = server)