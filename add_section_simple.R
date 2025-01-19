library(dplyr)

# Read the CSV file
setwd("/Users/Thea/Desktop/proj25") # Replace with your directory
my_data <- read.csv("index_items_prepared_comma.csv", header = TRUE)

section_mapping = c(
  
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
  "Federal Communications Commission" = "5.02 Communications Commission",
  "Financial Regulatory Agencies" = "5.01 Financial Agencies",
  "Federal Election Commission" = "5.03 Election Commission",
  "Federal Trade Commission" = "5.04 Trade Commission"
  
)

index_mapping = c(
  
  "Foreword" = "0", 
  "White House Office" = "1.01", 
  "Department of Energy and Related Commissions" = "3.03",
  "Department of Health and Human Services" = "3.05",
  "Department of Education" = "3.02",
  "Agency for International Development" = "2.06",
  "Department of Defense" = "2.01",
  "Department of Homeland Security" = "2.2",
  "Intelligence Community" = "2.04",
  "Department of Labor and Related Agencies" = "3.09",
  "Department of State" = "2.03",
  "Department of Justice" = "3.08",
  "Department of Commerce" = "4.01",
  "Environmental Protection Agency" = "3.04",
  "Department of the Treasury" = "4.02",
  "Executive Office of the President of the United States" = "1.02",
  "Department of the Interior" = "3.07",
  "Department of Transportation" = "3.10",
  "Department of Agriculture" = "3.01",
  "Central Personnel Agencies: Managing the Bureaucracy" = "1.03",
  "Media Agencies" = "2.05",
  "Department of Veterans Affairs" = "3.11",
  "Department of Housing and Urban Development" = "3.06",
  "Section 3: The General Welfare" = "3",
  "Section 4: The Economy" = "4",
  "Section 2: The Common Defense" = "2",
  "Section 1: Taking the Reins of Government" = "1",
  "Export-Import Bank" = "4.03",
  "Federal Reserve" = "4.04",
  "Small Business Administration" = "4.05",
  "Trade" = "4.06",
  "Section 5: Independent Regulatory Agencies" = "5",
  "Federal Communications Commission" = "5.02",
  "Financial Regulatory Agencies" = "5.01",
  "Federal Election Commission" = "5.03",
  "Federal Trade Commission" = "5.04"
  
)


# Create the new column based on the mapping
my_data <- my_data %>%
  mutate(section_simple = case_when(
    section %in% names(section_mapping) ~ section_mapping[section],
    TRUE ~ "unknown" # Default value if section is not in the mapping
  ))

# Create the new column based on the mapping
my_data <- my_data %>%
  mutate(section_index = case_when(
    section %in% names(index_mapping) ~ index_mapping[section],
    TRUE ~ "unknown" # Default value if section is not in the mapping
  ))

# View the updated data
head(my_data)

# Save the modified data back to a CSV
write.csv(my_data, "updated_file.csv", row.names = FALSE)
