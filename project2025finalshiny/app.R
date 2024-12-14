if(!require("shinydashboard")) install.packages("shinydashboards")
if (!require("pacman")) install.packages("pacman")

pacman::p_load("tidyverse", "dplyr", "shiny", "shinythemes", "patchwork", "ggplot2", "scales")

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
indexfm$reference_type = as.factor(indexfm$reference_type)

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Project 2025 Index"),
  dashboardSidebar(
    sidebarMenu(
      # Sidebar tabs
      menuItem("General Visualizations", tabName = "general", icon = icon("chart-bar")),
      menuItem("Sentiment Data", tabName = "sentiment", icon = icon("smile")),
      menuItem("API Definitions", tabName = "api", icon = icon("info-circle")),
      
      # Confirm selection button
      actionButton(inputId = "confirm_button", label = "Confirm Selection"),
      
      # Selectize Input for multiple titles
      selectizeInput(
        inputId = "title_select",
        label = "Select Titles",
        choices = NULL, # Choices will be updated dynamically
        multiple = TRUE, # Enable multiple selections
        options = list(placeholder = "Type to search titles...")
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      # General Visualizations Tab
      tabItem(tabName = "general",
              h2("General Visualizations"),
              plotOutput("heatmap"),
              plotOutput("bar_chart")
      ),
      # Sentiment Data Tab (placeholder)
      tabItem(tabName = "sentiment",
              h3("Selected Titles"),
              verbatimTextOutput("selected_titles")
      ),
      # API Definitions Tab
      tabItem(tabName = "api",
              h2("API Definitions"),
              textOutput("api_info")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Dynamically update the title choices
  observe({
    # Simulate dynamically loading choices (replace with actual data)
    updateSelectizeInput(session, "title_select", choices = unique(indexfm$title), server = TRUE)
  })
  
  # Placeholder for selected titles
  selected_titles <- reactiveVal(NULL)
  
  # Update selected titles when the Confirm button is pressed
  observeEvent(input$confirm_button, {
    selected_titles(input$title_select) # Store selected titles in the reactive value
    print(selected_titles()) # For debugging - shows the selected titles in the console
  })
  
  # General Visualizations
  
  output$heatmap <- renderPlot({
    titles <- selected_titles()  # Get the confirmed titles
    
    # If no titles are selected, return an empty plot or message
    if (is.null(titles) || length(titles) == 0) {
      plot(1, type = "n", xlab = "", ylab = "", main = "Please select titles and click Confirm")
      return()
    }
    
    # Filter the data to include only the selected titles
    filtered_data <- indexfm %>%
      filter(title %in% titles) %>%
      count(title, section)  # Count occurrences of each title in each section
    
    # Ensure all sections are included, even if they have no instances
    all_combinations <- expand.grid(
      title = titles,
      section = unique(indexfm$section)
    )
    
    # Join with the counted data to ensure all combinations are present, filling missing counts with 0
    complete_data <- all_combinations %>%
      left_join(filtered_data, by = c("title", "section")) %>%
      mutate(n = ifelse(is.na(n), 0, n))  # Replace NA with 0
    
    # Limit title length to 10 characters for display
    #complete_data$title <- substr(complete_data$title, 1, 15)
    
    # Sort by frequency (most frequent titles first)
    filtered_data <- filtered_data %>%
      arrange(desc(n))
    
    # Create a heatmap using ggplot2
    ggplot(complete_data, aes(x = section, y = title, fill = n)) +
      geom_tile(width = 1, height = 1, color = "black") +
      scale_fill_gradient(low = "white", high = "red") +
      labs(title = "Heatmap of Title Occurrences by Section", x = "Section", y = "Title", fill = "Count") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate section names for better readability
        #panel.grid = element_blank(),  # Remove grid lines
        #axis.ticks = element_blank(),  # Remove axis ticks
        plot.margin = margin(0, 0, 0, 0)  # Remove any extra margin around the plot
      )
    
  })
  
  
  # General Visualizations - Stacked Bar Chart
  output$bar_chart <- renderPlot({
    titles <- selected_titles()  # Get the confirmed titles
    
    # If no titles are selected, return an empty plot or message
    if (is.null(titles) || length(titles) == 0) {
      plot(1, type = "n", xlab = "", ylab = "", main = "Please select titles and click Confirm")
      return()
    }
    
    # Filter the data to include only the selected titles
    filtered_data <- indexfm %>%
      filter(title %in% titles) %>%
      count(title, reference_type)  # Count occurrences of each title by reference_type
    
    # Limit title length to 15 characters for display
    #filtered_data$title <- substr(filtered_data$title, 1, 15)
    
    # Sort by frequency (count of occurrences)
    filtered_data <- filtered_data %>%
      arrange(desc(n))
    
    # Create the stacked bar chart
    ggplot(filtered_data, aes(x = reorder(title, -n), y = n, fill = reference_type)) +
      geom_bar(stat = "identity", position = "stack") +  # Create a stacked bar chart
      labs(title = "Stacked Bar Chart of Title Frequency by Reference Type", x = "Title", y = "Frequency", fill = "Reference Type") +
      coord_flip() +
      theme_minimal() +
      scale_fill_manual(values = c("Statement" = "blue", "Critique" = "red", "Recommendation" = "green", "Other" = "gray")) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate title names for better readability
        plot.margin = margin(0, 0, 0, 0)  # Remove any extra margin around the plot
      )
  })
  
  # API Definitions
  output$api_info <- renderText({
    "This section will provide definitions and details for the API."
  })
}

shinyApp(ui, server)
