if (!require("jsonlite")) install.packages("jsonlite")

library(jsonlite)
library(ggplot2)
library(tidyr)

setwd("/Users/Thea2/Desktop/proj25") # add personal directory

#json_result <- fromJSON("index_items.json")

#str(json_result)

#indexfm <- as.data.frame(json_result)



summary(indexfm)

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
