if (!require("shiny")) install.packages("shiny")
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


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
