if (!require("shiny")) install.packages("shiny")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("shinythemes")) install.packages("shinythemes")
if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse", "dplyr", "shiny", "shinythemes", "patchwork", "ggplot2", "scales")

library(shiny)

setwd("/Users/Thea2/Desktop/proj25") # add personal directory

indexfm = read.csv("index_items_prepared_comma.csv", header=TRUE)

zip_to_section_simple = c(
  
  "Foreword" = "0 Foreword", 
  "White House Office" = "1.1 White House Office", 
  "Department of Energy and Related Commissions" = "3.03 Energy and Related Commissions",
  "Department of Health and Human Services" = "3.05 Health and Human Services",
  "Department of Education" = "3.02 Education",
  "Agency for International Development" = "2.6 International Development",
  "Department of Defense" = "2.1 Defense",
  "Department of Homeland Security" = "2.2 Homeland Security",
  "Intelligence Community" = "2.4 Intelligence Community",
  "Department of Labor and Related Agencies" = "3.09 Labor and Related Agencies",
  "Department of State" = "2.3 State",
  "Department of Justice" = "3.08 Justice",
  "Department of Commerce" = "4.1 Commerce",
  "Environmental Protection Agency" = "3.04 Environmental Protection",
  "Department of the Treasury" = "4.2 Treasury",
  "Executive Office of the President of the United States" = "1.2 Executive Office",
  "Department of the Interior" = "3.07 Interior",
  "Department of Transportation" = "3.10 Transportation",
  "Department of Agriculture" = "3.1 Agriculture",
  "Central Personnel Agencies: Managing the Bureaucracy" = "1.3 Central Personnel Agencies",
  "Media Agencies" = "2.5 Media",
  "Department of Veterans Affairs" = "3.11 Veterans Affairs",
  "Department of Housing and Urban Development" = "3.06 Housing and Urban Dev.",
  "Section 3: The General Welfare" = "3 The General Welfare",
  "Section 4: The Economy" = "4 The Economy",
  "Section 2: The Common Defense" = "2 The Common Defense",
  "Section 1: Taking the Reins of Government" = "1 Taking the Reins of Government",
  "Export-Import Bank" = "4.3 Export-Import Bank",
  "Federal Reserve" = "4.4 Federal Reserve",
  "Small Business Administration" = "4.5 Small Business",
  "Trade" = "4.6 Trade",
  "Section 5: Independent Regulatory Agencies" = "5 Independent Regulatory Agencies",
  "Federal Communications Commission" = "Communications Commission",
  "Financial Regulatory Agencies" = "Financial Agencies",
  "Federal Election Commission" = "Election Commission",
  "Federal Trade Commission" = "Trade Commission"
)

indexfm = indexfm %>%
  mutate(section_simple = factor(zip_to_section_simple[as.character(section)]))

indexfm$section = as.factor(indexfm$section)
indexfm$section_simple = as.factor(indexfm$section_simple)
indexfm$reference_type = as.factor(indexfm$reference_type)

# Example themes list (replace with your actual data)
themes <- list(
  "Tax" = c("Tax", "Tax revenue", "Taxpayer",
              "Sales tax", "Tax policy", "Tax rate",
              "Tax deduction", "Taxation in the United States",
              "Tax and spend", "Corporate tax", "Transfer tax", "Income tax in the United States"),
  
  "LGBT" = c("Gender", "Transgender", "Gender identity", "Gender transition",
             "Sex–gender distinction", "Non-binary gender", "Gender sensitivity",
             "Gender-based activities", "Gender ideology", "Gender equality",
             "Sexual characteristics", "Gender studies", "Gender dysphoria",
             "Sexual dimorphism", "Sex", "Gender mainstreaming",
             "Sexual Orientation and Gender Identity", "Gender-affirming surgery",
             "Sexism"),
  
  "Environment" = c("Climate Change", "Sustainability")
)

# Filtered instance categories
valid_instances <- c("human", "academic discipline")
instance_filtered <- indexfm %>%
  filter(instance_of %in% valid_instances) %>%
  distinct(instance_of, title)

ui <- fluidPage(
  titlePanel("Project 2025 Index"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "search_mode",
        label = "Choose Search Mode",
        choices = c("Themes", "Instances", "All"),
        selected = "Themes"
      ),
      uiOutput("dynamic_selector"),
      checkboxGroupInput(
        inputId = "title_select",
        label = "Select Titles",
        choices = NULL # Dynamically populated
      )
    ),
    mainPanel(
      h3("Selected Titles"),
      verbatimTextOutput("selected_titles") # For debugging and display
    )
  )
)


server <- function(input, output, session) {
  # Dynamically render the selector based on search_mode
  output$dynamic_selector <- renderUI({
    if (input$search_mode == "Themes") {
      selectInput(
        inputId = "theme_select",
        label = "Select Theme",
        choices = names(themes),
        selected = NULL
      )
    } else if (input$search_mode == "Instances") {
      selectInput(
        inputId = "instance_select",
        label = "Select Instance Category",
        choices = valid_instances,
        selected = NULL
      )
    } else if (input$search_mode == "All") {
      selectizeInput(
        inputId = "title_select",
        label = "Search Titles",
        choices = unique(indexfm$title),
        multiple = TRUE
      )
    }
  })
  
  # Update titles for Themes
  observeEvent(input$theme_select, {
    if (!is.null(input$theme_select)) {
      selected_theme_titles <- themes[[input$theme_select]]
      updateCheckboxGroupInput(
        session,
        inputId = "title_select",
        label = "Select Titles in Theme",
        choices = selected_theme_titles,
        selected = selected_theme_titles # Preselect all for themes
      )
    }
  })
  
  # Update titles for Instances
  observeEvent(input$instance_select, {
    if (!is.null(input$instance_select)) {
      filtered_titles <- instance_filtered %>%
        filter(instance_of == input$instance_select) %>%
        pull(title)
      updateCheckboxGroupInput(
        session,
        inputId = "title_select",
        label = "Select Titles in Instance",
        choices = filtered_titles,
        selected = filtered_titles # Preselect all for instances
      )
    }
  })
  
  # Debugging/Output for Selected Titles
  output$selected_titles <- renderPrint({
    input$title_select
  })
}


shinyApp(ui, server)
