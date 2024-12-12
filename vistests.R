
library(ggplot2)
library(tidyr)
library(forcats)

setwd("/Users/Thea2/Desktop/proj25") # add personal directory

#json_result <- fromJSON("index_items.json")

#str(json_result)

#indexfm <- as.data.frame(json_result)

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
#indexfm$section_simple = as.factor(indexfm$section_simple)
indexfm$reference_type = as.factor(indexfm$reference_type)

summary(indexfm)

# Example list of titles to filter
selected_titles_military <- c("Military", "Military service", "Military deployment", 
                     "Military personnel", "United States Army", "Military logistics", 
                     "Military technology", "Military recruitment", "Military capability", 
                     "United States Armed Forces", "Military doctrine", "Military Health System", 
                     "Military campaign", "Military education and training", "Military budget", 
                     "Soldier", "Military academy", "Military strategy", "Military acquisition")

selected_titles_gender <- c("Gender", "Transgender", "Gender identity", "Gender transition",
                                "Sex–gender distinction", "Non-binary gender", "Gender sensitivity",
                                "Gender-based activities", "Gender ideology", "Gender equality",
                                "Sexual characteristics", "Gender studies", "Gender dysphoria",
                                "Sexual dimorphism", "Sex", "Gender mainstreaming",
                                "Sexual Orientation and Gender Identity", "Gender-affirming surgery",
                              "Sexism")

selected_titles_racism <- c("Institutional racism", "Discrimination", "Race (human categorization)",
                            "Racial Equity", "Racial quota", "Race and education",
                            "Racial color blindness", "Affirmative discrimination",
                            "Race and gender activism", "Race-conscious policy",
                            "White privilege", "Intersectionality", "Religious discrimination",
                            "Critical race theory", "Anti-discrimination law", "Racial inequality in the United States",
                            "Ethnicity", "Genocide")

selected_titles_tax <- c("Tax", "Tax revenue", "Taxpayer",
                         "Sales tax", "Tax policy", "Tax rate",
                         "Tax deduction", "Taxation in the United States",
                         "Tax and spend", "Corporate tax", "Transfer tax", "Income tax in the United States")

selected_titles_single <- c("Joe Biden", "Donald Trump", "Presidency of Joe Biden", "Presidency of Donald Trump")

count_data <- indexfm %>%
  group_by(Title, Section) %>%
  summarise(Count = n(), .groups = 'drop')

# Step 2: Reshape the data for heatmap compatibility
heatmap_data <- count_data %>%
  tidyr::pivot_wider(names_from = Section, values_from = Count, values_fill = 0)

# Step 3: Melt the data for ggplot (if needed)
heatmap_melted <- count_data

# Step 4: Create the heatmap
ggplot(heatmap_melted, aes(x = Section, y = Title, fill = Count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(
    title = "Heatmap of Title Occurrences by Section",
    x = "Section",
    y = "Title",
    fill = "Occurrences"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
# Summarize counts by title and section
summary_data <- filtered_data %>%
  group_by(title, section_simple) %>%
  summarize(count = n(), .groups = 'drop')

# Create the heat map
ggplot(heatmap_data, aes(x = section_simple, y = title, fill = Frequency)) +
  geom_tile(color = "white") +  # Add white borders for better separation
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Frequency") +  # Color gradient
  scale_x_discrete(expand = c(0, 0)) +  # Remove padding on x-axis
  scale_y_discrete(expand = c(0, 0)) +  # Remove padding on y-axis
  labs(
    title = "Heat Map of Titles by Section",
    x = "Section",
    y = "Title"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
  )


section_counts <- indexfm %>%
  group_by(section) %>%
  summarise(Count = n())

ggplot(section_counts, aes(x = reorder(section, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  labs(title = "Number of Documents per Section",
       x = "Section",
       y = "Document Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# titles by references, subcategory reference type
filtered_data <- filtered_data %>%
  mutate(title = fct_reorder(title, reference_type, .fun = length, .desc = FALSE))

ggplot(filtered_data, aes(x = title, fill = reference_type)) +
  geom_bar() +
  coord_flip() +
  labs(
    title = "Distribution of Selected Titles by Section",
    x = "Categories",
    y = "Number of Titles",
    fill = "Section"
  ) +
  theme_minimal()

which.max(nchar(indexfm$title))
indexfm[3664,]

summary_data <- filtered_data %>%
  group_by(title) %>%
  summarize(count = n(), .groups = 'drop') %>%
  complete(title = selected_titles, section_simple = unique(indexfm$section_simple), fill = list(count = 0))

ggplot(summary_data, aes(x = section, y = title, fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Occurrences of Selected Titles by Section",
       x = "Section",
       y = "Title",
       fill = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
