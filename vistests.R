if (!require("jsonlite")) install.packages("jsonlite")

library(jsonlite)
library(ggplot2)
library(tidyr)

setwd("/Users/Thea2/Desktop/proj25") # add personal directory

json_result <- fromJSON("index_items.json")

str(json_result)

indexfm <- as.data.frame(json_result)

summary(indexfm)

# Example list of titles to filter
selected_titles <- c("Presidency of Joe Biden", "Joe Biden", "Donald Trump", "Presidency of Donald Trump")

# Filter data for selected titles
filtered_data <- indexfm %>%
  filter(title %in% selected_titles)  # Only keep rows with selected titles

# Summarize counts by title and section
summary_data <- filtered_data %>%
  group_by(title) %>%
  summarize(count = n(), .groups = 'drop') %>%
  complete(title = selected_titles, section = unique(indexfm$section), fill = list(count = 0))

ggplot(summary_data, aes(x = section, y = title, fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Occurrences of Selected Titles by Section",
       x = "Section",
       y = "Title",
       fill = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
