if (!require("jsonlite")) install.packages("jsonlite")

library(jsonlite)
library(ggplot2)
library(tidyr)

setwd("/Users/Thea2/Desktop/proj25") # add personal directory

#json_result <- fromJSON("index_items.json")

#str(json_result)

#indexfm <- as.data.frame(json_result)

data = read.csv("index_items_prepared_comma.csv", header=TRUE)

summary(indexfm)

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

# Preserve the order of sections as they appear in the dataset
section_order <- unique(indexfm$section)

# Example list of titles to filter
selected_titles <- c("Military", "Military service", "Military deployment", "Military personnel", "United States Army", "Military Logistics", "Military Technology", "Military Recruitment", "Military capability", "United States Armed Forces", "Military doctrine", "Military Health System", "Military campaign", "Military education and training", "Military budget", "Soldier", "Military academy", "Military strategy", "Military acquistion")
# Filter data for selected titles
filtered_data <- indexfm %>%
  filter(title %in% selected_titles)  # Only keep rows with selected titles

# Summarize counts by title and section
summary_data <- filtered_data %>%
  group_by(title, section) %>%
  summarize(count = n(), .groups = 'drop') %>%
  complete(title = selected_titles, section = section_order, fill = list(count = 0)) %>%
  mutate(section = factor(section, levels = section_order)) 

ggplot(summary_data, aes(x = section, y = title, fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Occurrences of Selected Titles by Section",
       x = "Section",
       y = "Title",
       fill = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
