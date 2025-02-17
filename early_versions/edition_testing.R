library(shiny)
library(bs4Dash)
library(dplyr)
#library(DT)
library(text2vec)
library(tidytext)

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  "tidyverse", "dplyr", "shiny", "bs4Dash", "patchwork", 
  "ggplot2", "scales"
)

# Load data (replace with your actual data loading method)
load_project_data <- function() {
  # Placeholder for data loading
  # Assuming a CSV with columns: title, section, page, summary, wikipedia_id, instance_of
  setwd("/Users/Thea2/Desktop/proj25") # Replace with your directory
  indexfm <- read.csv("index_items_prepared_comma.csv", header = TRUE)
  
  # Preprocess data
  data <- data %>%
    mutate(
      is_unique = n() == 1,  # Identify hapax legomena
      summary_tokens = map(summary, ~ str_split(., "\\s+")[[1]])
    )
  
  return(data)
}

# Compute similarity matrix (example implementation)
compute_similarity_matrix <- function(data) {
  # Create document-term matrix
  vectorizer <- data$summary %>%
    itoken() %>%
    create_vocabulary() %>%
    vocab_vectorizer()
  
  # Compute TF-IDF and cosine similarity
  tfidf_matrix <- data$summary %>%
    itoken() %>%
    create_dtm(vectorizer) %>%
    transform_tfidf()
  
  similarity_matrix <- tfidf_matrix %>%
    sim2(method = "cosine")
  
  return(similarity_matrix)
}

# UI Definition
ui <- bs4DashPage(
  navbar = bs4DashNavbar(
    title = "Project 2025 Index Explorer",
    status = "primary"
  ),
  sidebar = bs4DashSidebar(
    skin = "light",
    # Sidebar content
    sidebarMenu(
      menuItem("Topic Search", tabName = "search", icon = icon("search")),
      menuItem("Unique Topics", tabName = "unique", icon = icon("star")),
      menuItem("Topic Networks", tabName = "network", icon = icon("project-diagram"))
    ),
    
    # Search filters
    selectizeInput(
      "selected_topics", 
      "Select Topics", 
      choices = NULL,  # Will be populated dynamically
      multiple = TRUE
    ),
    sliderInput(
      "page_range", 
      "Page Range", 
      min = 1, 
      max = 1000,  # Adjust based on your max page number
      value = c(1, 1000)
    ),
    checkboxGroupInput(
      "sections", 
      "Sections",
      choices = NULL  # Will be populated dynamically
    )
  ),
  body = bs4DashBody(
    tabItems(
      # Topic Search Tab
      tabItem(
        tabName = "search",
        fluidRow(
          box(
            title = "Topic Instances",
            DTOutput("topic_instances")
          ),
          box(
            title = "Topic Distribution",
            plotlyOutput("topic_distribution")
          )
        )
      ),
      
      # Unique Topics Tab
      tabItem(
        tabName = "unique",
        fluidRow(
          box(
            title = "Rare and Unique Topics",
            DTOutput("unique_topics")
          )
        )
      ),
      
      # Topic Network Tab
      tabItem(
        tabName = "network",
        fluidRow(
          box(
            title = "Topic Relationships",
            visNetworkOutput("topic_network")
          )
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  # Load data reactively
  project_data <- reactive({
    load_project_data()
  })
  
  # Compute similarity matrix
  similarity_matrix <- reactive({
    compute_similarity_matrix(project_data())
  })
  
  # Dynamic input population
  observe({
    updateSelectizeInput(
      session, 
      "selected_topics",
      choices = unique(project_data()$title),
      server = TRUE
    )
    
    updateCheckboxGroupInput(
      session,
      "sections",
      choices = unique(project_data()$section)
    )
  })
  
  # Filtered Data
  filtered_data <- reactive({
    req(input$selected_topics)
    
    project_data() %>%
      filter(
        title %in% input$selected_topics,
        between(page, input$page_range[1], input$page_range[2]),
        section %in% input$sections
      )
  })
  
  # Topic Instances Table
  output$topic_instances <- renderDT({
    filtered_data() %>%
      select(title, section, page, summary) %>%
      datatable(
        options = list(pageLength = 10),
        rownames = FALSE
      )
  })
  
  # Unique Topics Table
  output$unique_topics <- renderDT({
    project_data() %>%
      filter(is_unique) %>%
      select(title, section, page, summary) %>%
      datatable(
        options = list(pageLength = 10),
        rownames = FALSE
      )
  })
  
  # Topic Distribution (placeholder)
  output$topic_distribution <- renderPlotly({
    # Implement topic distribution visualization
  })
  
  # Topic Network (placeholder)
  output$topic_network <- renderVisNetwork({
    # Implement topic network visualization
  })
}

# Run the application 
shinyApp(ui, server)