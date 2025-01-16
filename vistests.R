if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  "tidyverse", "dplyr", "shiny", "bs4Dash", "patchwork", 
  "ggplot2", "scales"
)

setwd("/Users/Thea/Desktop/proj25") # Replace with your directory
indexfm <- read.csv("index_items_prepared_comma.csv", header = TRUE)

# section and reference_type as factors
indexfm$section <- as.factor(indexfm$section)
indexfm$reference_type <- as.factor(indexfm$reference_type)

# sort data by title
indexfm <- indexfm[order(indexfm$title),] 

ui <- bs4DashPage(
  title = "Project 2025 Index",
  header = bs4DashNavbar(),
  sidebar = bs4DashSidebar(
    skin = "light",
    title = "Menu",
    collapsed = FALSE,
    bs4SidebarMenu(
      bs4SidebarMenuItem(
        "About", tabName = "about", icon = icon("info-circle")
      ),
      bs4SidebarMenuItem(
        "General Visualizations", tabName = "general", icon = icon("chart-bar")
      ),
      bs4SidebarMenuItem(
        "Result Edition", tabName = "edition", icon = icon("book")
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
      # About tab
      bs4TabItem(
        tabName = "about",
        h2("API Definitions"),
        textOutput("api_info"),
        tableOutput("title_stats_table")
      ),
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
        # Add this new filtering section
        fluidRow(
          column(
            width = 12,
            box(
              width = NULL,
              title = "Filter by Reference Type",
              checkboxGroupInput(
                inputId = "reference_type_filter",
                label = NULL,
                choices = c("Statement", "Critique", "Recommendation", "Other"),
                selected = c("Statement", "Critique", "Recommendation", "Other"),
                inline = TRUE
              )
            )
          )
        ),
        uiOutput("accordion")
      ),
      # API Definitions Tab
      bs4TabItem(
        tabName = "api",
        h2("API Definitions"),
        textOutput("about_info")
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
  
  # Generate statistics for the About page table
  output$title_stats_table <- renderTable({
    titles <- selected_titles()
    
    # If no titles are selected, return an empty table
    if (is.null(titles) || length(titles) == 0) {
      return(data.frame("Message" = "Please select titles and click Confirm"))
    }
    
    # Filter data for the selected titles
    filtered_data <- indexfm %>%
      filter(title %in% titles)
    
    # Create a summary table for each title
    stats_table <- filtered_data %>%
      group_by(title) %>%
      summarise(
        total_references = n(),
        total_sections = n_distinct(section),
        sections_percentage = round((n_distinct(section)/36) * 100, 2),
        
        # Calculate the total counts and percentages for each reference type
        total_statements = sum(reference_type == "Statement"),
        statement_percentage = round((total_statements / total_references) * 100, 2),
        
        total_critique = sum(reference_type == "Critique"),
        critique_percentage = round((total_critique / total_references) * 100, 2),
        
        total_recommendation = sum(reference_type == "Recommendation"),
        recommendation_percentage = round((total_recommendation / total_references) * 100, 2),
        
        total_other = sum(reference_type == "Other"),
        other_percentage = round((total_other / total_references) * 100, 2)
      ) %>%
      arrange(desc(total_references))  # Optional: Sort by total references
    
    # Format the percentages and total values together for each reference type
    stats_table <- stats_table %>%
      mutate(
        sections = paste(total_sections, " (", sections_percentage, "%)", sep = ""),
        statement = paste(total_statements, "(", statement_percentage, "%)", sep = ""),
        critique = paste(total_critique, "(", critique_percentage, "%)", sep = ""),
        recommendation = paste(total_recommendation, "(", recommendation_percentage, "%)", sep = ""),
        other = paste(total_other, "(", other_percentage, "%)", sep = "")
      ) %>%
      select(-total_sections, -sections_percentage, -total_statements, -statement_percentage, -total_critique, -critique_percentage,
             -total_recommendation, -recommendation_percentage, -total_other, -other_percentage)  # Remove intermediate columns
    
    # Return the formatted table
    stats_table
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
    
    ggplot(complete_data, aes(x = section, y = reorder(title, n), fill = n)) +
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
    
    ggplot(filtered_data, aes(x = reorder(title, n), y = n, fill = reference_type)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(
        title = "Stacked Bar Chart of Title Frequency by Reference Type",
        x = "Title", y = "Frequency", fill = "Reference Type"
      ) +
      scale_fill_manual(values = c(
        "Statement" = "#708d81", 
        "Critique" = "#bc4b51", 
        "Recommendation" = "#f4d58d", 
        "Other" = "#e0e1dd"
      )) +
      coord_flip() +
      theme_minimal()
  })
  
  # Edition accordion
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
      return(h4("No references found for the selected criteria."))
    }
    
    # Define a mapping of reference_type to colors
    reference_colors <- c(
      "Statement" = "#bc4b51",
      "Critique" = "#e0e1dd",
      "Recommendation" = "#f4d58d",
      "Other" = "#708d81"
    )
    
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
        
        references_html <- lapply(1:nrow(references), function(j) {
          ref <- references[j, ]
          ref_color <- reference_colors[ref$reference_type]
          
          sprintf(
            "<div style='background-color: %s; padding: 10px; margin-bottom: 5px; border-radius: 5px;'>
             <b>Title:</b> %s<br>
             <b>Page:</b> %d<br>
             <b>Summary:</b> %s
           </div>",
            ref_color, ref$title, ref$page, ref$summary
          )
        }) %>%
          unlist() %>%
          paste(collapse = "")
        
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
  
  # About Placeholder
  output$about_info <- renderText({
    "This section will provide definitions and details for the Section."
  })
}

shinyApp(ui, server)
