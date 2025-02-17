# Install pacman if not installed
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

# Load required packages
pacman::p_load(
  "here", "tidyverse", "dplyr", "shiny", "bs4Dash", "patchwork", 
  "ggplot2", "scales"
)

# Select file with data
library(here)
indexfm <- read.csv(here("index_cleaned.csv"), header = TRUE)

# Convert section and reference_type to factors
indexfm <- indexfm %>%
  mutate(across(c(section, reference_type), as.factor))

# Sort results by title
indexfm <- indexfm %>%
  arrange(title)

ui <- bs4DashPage(
  
  title = "Project 2025 Index Visualiser",
  help = NULL, # remove help checkbox
  header = bs4DashNavbar(
    title = "Project 2025 Index Visualiser",
    fixed = TRUE
  ),
  sidebar = bs4DashSidebar(
    skin = "light",
    title = "Menu",
    collapsed = FALSE,
    bs4SidebarMenu(
      
      # Main Page
      bs4SidebarMenuItem(
        "Visualizations", tabName = "general", icon = icon("chart-bar")
      ),
      
      # References Accordion Edition
      bs4SidebarMenuItem(
        "Reference Results", tabName = "edition", icon = icon("book")
      ),
      
      # Project About
      bs4SidebarMenuItem(
        "About Project", tabName = "about", icon = icon("info-circle")
      ),
      
      # Search function
      actionButton(inputId = "confirm_button", label = "Confirm Selection"),
      
      selectizeInput(
        inputId = "title_select",
        label = "Select Keywords",
        choices = NULL,
        multiple = TRUE,
        options = list(placeholder = "Type to search titles...")
      )
    )
  ),
  
  body = bs4DashBody(
    tags$head(
      tags$script(src = "https://cdn.jsdelivr.net/npm/bootstrap@4.5.3/dist/js/bootstrap.bundle.min.js"),
      tags$style(HTML("
        body {
          zoom: 1.1; /* Adjust the zoom level to make everything appear larger */
        }
        .content-wrapper {
          padding: 10px; /* Reduce padding if needed */
        }
      "))
    ),
    bs4TabItems(
      
      # Main Page
      bs4TabItem(
        tabName = "general",
        h2("Project 2025 Index Visualiser"),
        fluidRow(
          column(
            width = 12,
            box(
              width = NULL,
              title = "How to use this website",
              "Select the keywords you want to explore using the dropdown menu or checkboxes, and click the Confirm button to load related data.",br(), br(),
              "View keywords by chapter section on the heatmap, examine the statistics related to keyword use in the table, and compare the frequency of reference types in the bar chart.", br(), br(),
              "To view specific references within the document, go to the Reference Results tab in the sidebar."
            )
          )
        ),
        
        # Heatmap by chapter
        h3("Frequency of Keywords per Chapter"),
        plotOutput("heatmap"),
        
        # Statistics table
        h3("Statistics of Keyword Use"),
        textOutput("disclaimer"),
        tableOutput("title_stats_table"),
        
        # Type frequency bar chart
        h3("Frequency of Keyword Reference Types"),
        plotOutput("bar_chart")
      ),
      
      # Reference Result Edition
      bs4TabItem(
        tabName = "edition",
        h2("Reference Results"),
        
        # Filtering options
        fluidRow(
          column(
            width = 12,
            box(
              width = NULL,
              title = "How to use Reference Results",
              HTML("On this page, you can view the context in which selected keywords are used within the document, organized by chapter section. Opening a chapter section reveals the selected keywords used in that chapter, along with an AI-generated summary of their usage.
                   </br></br>
                   Clicking on a page number will take you directly to the corresponding page in the Project 2025 document. Additionally, you can sort and filter the results by reference type: 
                      <span style='background-color: #ffffb0; font-weight: bold;'>Statement</span>,
                      <span style='background-color: #ff7891; font-weight: bold;'>Critique</span>, 
                      <span style='background-color: #b0ffff; font-weight: bold;'>Recommendation</span>,  or 
                      <span style='background-color: #b0b0b0; font-weight: bold;'>Other</span>.
                    This helps you quickly locate and analyze the most relevant information.")
            ),
            checkboxGroupInput(
              inputId = "reference_type_filter",
              label = "Filter by Reference Type",
              choices = c("Statement", "Critique", "Recommendation", "Other"),
              selected = c("Statement", "Critique", "Recommendation", "Other"),
              inline = TRUE
            )
          )
        ),
        
        # Accordion output
        uiOutput("accordion")
      ),
      
      # About Page
      bs4TabItem(
        tabName = "about",
        h2("About this project"),
        HTML("This project was developed with the aim of improving the accessibility and analysis of the Project 2025 document by providing an intuitive way to explore its contents based on specific keywords. The primary goal of this tool is to allow users to quickly identify the context in which selected keywords appear throughout the document, organized by sections. By utilizing an interactive heatmap, reference filters, and detailed summaries, this app facilitates the identification and analysis of important references, enabling users to gain deeper insights into the document’s structure and key themes. This tool is particularly useful for those looking to focus on particular topics and understand their significance within the larger context of the document.
             </br></br>
              The application was designed to provide an interactive, user-friendly experience with features such as keyword filtering, page navigation, and customizable results views. This makes it easier for users to navigate the document, find relevant sections, and examine the relationships between references and specific keywords. Whether the user is conducting a thorough review or simply seeking a quick overview, this tool is a valuable resource for efficiently parsing the extensive content of the Project 2025 document.
            </br></br>
              I would like to express my heartfelt gratitude to the original producers of the code, the Autonomy Data Unit, and the various resources used in the development of this project. Their contributions laid the foundation for the functionality that drives this app. 
             </br></br>
             A special thanks goes to the individuals who provided assistance, feedback, and guidance during the development process. Their support has been crucial in refining the app and ensuring that it meets the needs of users. Without their collaboration, this project would not have come to fruition.")
      )
    )
  ),
  
  # Footer
  footer = bs4DashFooter("Thea Schaaf BA, University of Graz (2025)")
)

server <- function(input, output, session) {
  
  # Dynamically update the keyword choices
  observe({
    updateSelectizeInput(session, "title_select", choices = unique(indexfm$title), server = TRUE)
  })
  
  # Reactive value for selected keywords
  selected_titles <- reactiveVal(NULL)
  
  # Update selected keywords when the Confirm button is pressed
  observeEvent(input$confirm_button, {
    selected_titles(input$title_select)
  })
  
  
  # Heatmap for visualisations page
  output$heatmap <- renderPlot({
    titles <- selected_titles()
    if (is.null(titles) || length(titles) == 0) {
      plot(1, type = "n", xlab = "", ylab = "", main = "Please select keywords in sidebar.")
      return()
    }
    
    filtered_data <- indexfm %>%
      filter(title %in% titles) %>%
      count(title, section_simple)
    
    all_combinations <- expand.grid(
      title = titles,
      section_simple = unique(indexfm$section_simple)
    )
    
    complete_data <- all_combinations %>%
      left_join(filtered_data, by = c("title", "section_simple")) %>%
      mutate(n = ifelse(is.na(n), 0, n))
    
    
    # Generate heat map
    ggplot(complete_data, aes(x = section_simple, y = reorder(title, n), fill = n)) +
      geom_tile(color = "black") +
      scale_fill_gradient(low = "white", high = "red")+
      labs(title = "Keyword Occurrences by Chapter Section", x = NULL, y = NULL, fill = "Frequency") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate section names for better readability
        strip.text = element_blank() 
      ) +
      coord_fixed(ratio = 3)  # Height of cells
  })
  
  # Keyword statistics table for the Visualisations page
  output$title_stats_table <- renderTable({
    titles <- selected_titles()
    
    # If no keywords are selected, return an empty table
    if (is.null(titles) || length(titles) == 0) {
      return(data.frame("Keyword Statistics" = "Please select keywords and click confirm in the sidebar."))
    }
    
    # Filter data for the selected keywords
    filtered_data <- indexfm %>%
      filter(title %in% titles)
    
    # Create a summary table for each keyword
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
      arrange(desc(total_references))
    
    # Format the percentages and total values together for each reference type
    stats_table <- stats_table %>%
      mutate(
        Keyword = title,
        Total = total_references,
        Sections = paste(total_sections, " of 36", " (", sections_percentage, "%)", sep = ""),
        Statements = paste(total_statements, " (", statement_percentage, "%)", sep = ""),
        Crits = paste(total_critique, " (", critique_percentage, "%)", sep = ""),
        Recs = paste(total_recommendation, " (", recommendation_percentage, "%)", sep = ""),
        Other = paste(total_other, " (", other_percentage, "%)", sep = "")
      ) %>%
      select(-title, -total_references, -total_sections, -sections_percentage, -total_statements, -statement_percentage, -total_critique, -critique_percentage,
             -total_recommendation, -recommendation_percentage, -total_other, -other_percentage)  # Remove intermediate columns
    
    # Return the formatted table
    stats_table
  }, bordered = TRUE, colnames = TRUE, hover = TRUE, striped = TRUE)
  
  # Stacked Bar Chart for Visualisation page
  output$bar_chart <- renderPlot({
    titles <- selected_titles()
    
    # Error message
    if (is.null(titles) || length(titles) == 0) {
      plot(1, type = "n", xlab = "", ylab = "", main = "Please select titles and click Confirm")
      return()
    }
    
    filtered_data <- indexfm %>%
      filter(title %in% titles) %>%
      count(title, reference_type)
    
    # Generate plot
    ggplot(filtered_data, aes(x = reorder(title, n), y = n, fill = reference_type)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(
        title = "Keyword Frequency by Reference Type",
        x = "Title", y = "Frequency", fill = "Reference Type"
      ) +
      scale_fill_manual(values = c(
        "Statement" = "#fce61a", 
        "Critique" = "#ff7891", 
        "Recommendation" = "#b0ffff", 
        "Other" = "#b0b0b0"
      )) +
      coord_flip() +
      theme_minimal()
  })
  
  # Results accordion for Results Edition page
  output$accordion <- renderUI({
    titles <- selected_titles()
    
    if (is.null(titles) || length(titles) == 0) {
      return(h4("Please select topics and click Confirm to view their references."))
    }
    
    # Filter and sort the data
    filtered_data <- indexfm %>%
      filter(title %in% titles) %>%
      filter(reference_type %in% input$reference_type_filter) %>%
      arrange(page)
    
    # Group by section_index and retain section name for display
    sections <- filtered_data %>%
      group_by(section_index, section) %>%
      summarise(n_references = n(), .groups = "drop") %>%
      arrange(section_index)  # Sort by section_index
    
    if (nrow(sections) == 0) {
      return(h4("No references found for the selected criteria."))
    }
    
    # Define a mapping of reference_type to colors
    reference_colors <- c(
      "Critique" = "#ff7891",
      "Other" = "#b0b0b0",
      "Recommendation" = "#b0ffff",
      "Statement" = "#fce61a"
    )
    
    # Create accordion wrapper
    do.call(tags$div, c(
      list(
        class = "accordion",
        id = "edition_accordion"
      ),
      lapply(seq_len(nrow(sections)), function(i) {
        section_name <- as.character(sections$section[i])  # Use section for display
        current_section_index <- as.character(sections$section_index[i])  # Use section_index for filtering
        section_id <- paste0("collapse_edition_accordion_", i)
        
        references <- filtered_data %>%
          filter(current_section_index == section_index) %>%
          arrange(page)
        
        references_html <- lapply(1:nrow(references), function(j) {
          ref <- references[j, ]
          ref_color <- reference_colors[ref$reference_type]
          
          sprintf(
            "<div style='background-color: %s; padding: 10px; margin-bottom: 5px; border-radius: 5px; display: flex; justify-content: space-between; align-items: center;'>
      <span style='font-size: 1.25em;'><b>%s</b></span>
      <span style='font-size: 1.25em;'><b><a href='https://archive.org/details/2025_MandateForLeadership_FULL/page/%d/mode/2up' target = '_blank'>%d.</a></b></span> <!-- page number -->
    </div>
    <span>%s</span></br></br>", 
            ref_color, ref$title, ref$page, ref$page, ref$summary
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
                `aria-expanded` = "false",  # This will toggle dynamically
                `aria-controls` = section_id,
                paste(current_section_index, ": ", section_name, "(", nrow(references), " references)"),
                style = "font-size: .7em; color: black; border-radius: 5px;"  # Custom style
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
  
  # Disclaimer for table text
  output$disclaimer <- renderText({
    "The keyword results are categorized into four types (statement, critique, recommendations, and other) based on the word choice in each AI-generated summary.
    Approximately 53% of references are statements, 40% are recommendations, 4% are critiques, and 3% are other.
    This differentiation is used to identify trends and relevant information. However, be aware that results can be miscategorized."
  })
  
}

shinyApp(ui, server)
