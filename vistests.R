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
    tags$head(
      tags$script(src = "https://code.jquery.com/jquery-3.5.1.slim.min.js"),
      tags$script(src = "https://cdn.jsdelivr.net/npm/bootstrap@4.5.3/dist/js/bootstrap.bundle.min.js")
    ),
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
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate section names for better readability
        plot.margin = margin(0, 0, 0, 0)  # Remove any extra margin around the plot
      )
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
  
  output$accordion <- renderUI({
    titles <- selected_titles()
    
    if (is.null(titles) || length(titles) == 0) {
      return(h4("Please select topics and click Confirm to view their references."))
    }
    
    filtered_data <- indexfm %>%
      filter(title %in% titles) %>%
      arrange(page)
    
    sections <- filtered_data %>%
      group_by(section) %>%
      summarise(n_references = n(), .groups = "drop")
    
    if (nrow(sections) == 0) {
      return(h4("No sections found for the selected topics."))
    }
    
    # Create accordion wrapper
    do.call(tags$div, c(
      list(
        class = "accordion", 
        id = "edition_accordion"
      ),
      lapply(seq_len(nrow(sections)), function(i) {
        section_name <- as.character(sections$section[i])
        section_id <- paste0("collapse_edition_accordion_", i)
        
        references <- filtered_data %>%
          filter(section == section_name) %>%
          arrange(page)
        
        references_html <- references %>%
          mutate(
            ref_html = sprintf(
              "<b>Title:</b> %s<br><b>Page:</b> %d<br><b>Summary:</b> %s<br>",
              title, page, summary
            )
          ) %>%
          pull(ref_html) %>%
          paste(collapse = "<hr>")
        
        # Create card structure manually
        tags$div(
          class = "card",
          tags$div(
            class = "card-header",
            id = paste0("heading_", section_id),
            tags$h2(
              class = "mb-0",
              tags$button(
                class = "btn btn-link btn-block text-left",
                `data-toggle` = "collapse",
                `data-target` = paste0("#", section_id),
                `aria-expanded` = "false",
                `aria-controls` = section_id,
                paste(section_name, "(", nrow(references), "references)")
              )
            )
          ),
          tags$div(
            id = section_id,
            class = "collapse",
            `aria-labelledby` = paste0("heading_", section_id),
            `data-parent` = "#edition_accordion",
            tags$div(
              class = "card-body",
              HTML(references_html)
            )
          )
        )
      })
    ))
  })
  
  
  # API Definitions Placeholder
  output$api_info <- renderText({
    "This section will provide definitions and details for the API."
  })
  
}

shinyApp(ui, server)
