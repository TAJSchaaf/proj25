if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  "tidyverse", "dplyr", "shiny", "bs4Dash", "patchwork", 
  "ggplot2", "scales"
)

setwd("/Users/Thea2/Desktop/proj25") # Replace with your directory
indexfm <- read.csv("index_items_prepared_comma.csv", header = TRUE)

indexfm$section <- as.factor(indexfm$section)
indexfm$reference_type <- as.factor(indexfm$reference_type)

ui <- bs4DashPage(
  title = "Project 2025 Index",
  header = bs4DashNavbar(),
  sidebar = bs4DashSidebar(
    skin = "dark",
    title = "Menu",
    collapsed = TRUE,
    bs4SidebarMenu(
      bs4SidebarMenuItem(
        "General Visualizations", tabName = "general", icon = icon("chart-bar")
      ),
      bs4SidebarMenuItem(
        "Result Edition", tabName = "edition", icon = icon("book")
      ),
      bs4SidebarMenuItem(
        "Sentiment Data", tabName = "sentiment", icon = icon("smile")
      ),
      bs4SidebarMenuItem(
        "API Definitions", tabName = "api", icon = icon("info-circle")
      ),
      actionButton(inputId = "confirm_button", label = "Confirm Selection"),
      selectizeInput(
        inputId = "title_select",
        label = "Select Titles",
        choices = NULL,
        multiple = TRUE,
        options = list(placeholder = "Type to search titles...")
      )
    )
  ),
  body = bs4DashBody(
    bs4TabItems(
      # General Visualizations Tab
      bs4TabItem(
        tabName = "general",
        h2("General Visualizations"),
        plotOutput("heatmap"),
        plotOutput("bar_chart")
      ),
      # Edition Tab
      bs4TabItem(
        tabName = "edition",
        h2("Edition"),
        uiOutput("accordion")
      ),
      # Sentiment Data Tab
      bs4TabItem(
        tabName = "sentiment",
        h3("Selected Titles"),
        verbatimTextOutput("selected_titles")
      ),
      # API Definitions Tab
      bs4TabItem(
        tabName = "api",
        h2("API Definitions"),
        textOutput("api_info")
      )
    )
  )
)

server <- function(input, output, session) {
  # Dynamically update the title choices
  observe({
    updateSelectizeInput(session, "title_select", choices = unique(indexfm$title), server = TRUE)
  })
  
  # Reactive value for selected titles
  selected_titles <- reactiveVal(NULL)
  
  # Update selected titles when the Confirm button is pressed
  observeEvent(input$confirm_button, {
    selected_titles(input$title_select)
  })
  
  # Heatmap Visualization
  output$heatmap <- renderPlot({
    titles <- selected_titles()
    if (is.null(titles) || length(titles) == 0) {
      plot(1, type = "n", xlab = "", ylab = "", main = "Please select titles and click Confirm")
      return()
    }
    
    filtered_data <- indexfm %>%
      filter(title %in% titles) %>%
      count(title, section)
    
    all_combinations <- expand.grid(
      title = titles,
      section = unique(indexfm$section)
    )
    
    complete_data <- all_combinations %>%
      left_join(filtered_data, by = c("title", "section")) %>%
      mutate(n = ifelse(is.na(n), 0, n))
    
    ggplot(complete_data, aes(x = section, y = title, fill = n)) +
      geom_tile(color = "black") +
      scale_fill_gradient(low = "white", high = "red") +
      labs(title = "Heatmap of Title Occurrences by Section", x = "Section", y = "Title", fill = "Count") +
      theme_minimal()
  })
  
  # Stacked Bar Chart Visualization
  output$bar_chart <- renderPlot({
    titles <- selected_titles()
    if (is.null(titles) || length(titles) == 0) {
      plot(1, type = "n", xlab = "", ylab = "", main = "Please select titles and click Confirm")
      return()
    }
    
    filtered_data <- indexfm %>%
      filter(title %in% titles) %>%
      count(title, reference_type)
    
    ggplot(filtered_data, aes(x = reorder(title, -n), y = n, fill = reference_type)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(
        title = "Stacked Bar Chart of Title Frequency by Reference Type",
        x = "Title", y = "Frequency", fill = "Reference Type"
      ) +
      coord_flip() +
      theme_minimal()
  })
  
  # Editions Tab - Accordion
  output$accordion <- renderUI({
    titles <- selected_titles()
    
    # If no titles are selected, display a message
    if (is.null(titles) || length(titles) == 0) {
      return(h4("Please select topics and click Confirm to view their references."))
    }
    
    # Filter the data to include only the selected titles
    filtered_data <- indexfm %>%
      filter(title %in% titles) %>%
      arrange(page)  # Sort by page number
    
    # Group by section and filter to include only sections with selected topics
    sections <- filtered_data %>%
      group_by(section) %>%
      filter(n() > 0)  # Only keep sections with at least one reference
    
    print(sections)
    
    # If no sections are available after filtering, show a message
    if (nrow(sections) == 0) {
      return(h4("No sections found for the selected topics."))
    }
    
    # Create the accordion
    bs4Dash::accordion(
      id = "edition_accordion",
      lapply(unique(sections$section), function(section_name) {
        # Get references for this section
        references <- sections %>% filter(section == section_name)
        
        # Prepare the content for this section
        references_html <- lapply(1:nrow(references), function(i) {
          ref <- references[i, ]
          sprintf(
            "<b>Title:</b> %s<br><b>Page:</b> %d<br><b>Summary:</b> %s<br>",
            ref$title, ref$page, ref$summary
          )
        }) %>%
          unlist() %>%
          paste(collapse = "<hr>")  # Separate references with a line
        
        # Create the accordion item for this section
        bs4Dash::accordionItem(
          title = paste(section_name, "(", nrow(references), "references)"),
          status = "primary",  # Customize the color of the header
          #collapsed = TRUE,  # Start collapsed
          shiny::HTML(references_html)  # Insert the references as HTML
        )
      })
    )
  })
  
  # API Definitions Placeholder
  output$api_info <- renderText({
    "This section will provide definitions and details for the API."
  })
  
}

shinyApp(ui, server)
