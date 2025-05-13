# NewData.R
# This script loads and processes DMARC data, then creates summary plots.

# 1. Clear workspace: remove all objects from the environment
rm(list = ls())

# 2. Load required libraries for data manipulation and visualization
library(tidyverse)  # data wrangling, includes ggplot2 and dplyr
library(lubridate)  # date parsing and manipulation
library(haven)      # reading SPSS, SAS, and Stata files (if needed)
library(ggplot2)    # plotting
library(dplyr)      # data manipulation (included in tidyverse)

# 3. Read in the main DMARC data file
all <- read.csv("DMARC Data 2018-2024 copy.csv")

# 4. Convert the raw servedDate column to Date class and store in clean_date
all$clean_date <- as.Date(all$servedDate)

# 5. Filter records to include only those after January 31, 2020
all <- all[all$clean_date > as.Date("2020-01-31"), ]

# 6. (Optional) Read in existing CSV files for first_time and first_visit
first_time <- read.csv("first_time.csv")  # prior results, if available
first_visit <- read.csv("first_visit.csv")

# 7. Inspect unique education values to identify any anomalies
unique(all$education)
# Note: Negative or unexpected categories should be cleaned or removed

# 8. Create the first_visit dataframe by grouping and summarizing visits
first_visit <- all %>%
  group_by(houseHoldIdAfn, householdMembers, foodstamps, clean_date, annualIncome, education) %>%
  summarise(
    householdMembers = n()  # count the number of records per group
  ) %>%
  mutate(
    served_year        = year(clean_date),                 # extract year
    served_month       = month(clean_date),                # extract month
    served_day_of_month= mday(clean_date),                 # extract day of month
    first_visit        = if_else(clean_date == min(clean_date), 1, 0),  # flag first visit
    # Map education categories to numeric codes for analysis
    education_numeric  = case_when(
      education == "No Schooling" ~ 1,
      education == "Currently Preschool and younger" ~ 2,
      education == "Currently K-8" ~ 3,
      education == "Drop Out K-12" ~ 4,
      education == "Currently High School" ~ 5,
      education == "Drop Out K-12" ~ 6,
      education == "HS/HiSET Graduate" ~ 7,
      education == "Some College" ~ 8,
      education == "College Graduate (2 year, 4 year, or beyond)" ~ 9,
      education %in% c("Unknown", "Not Selected") ~ NA_real_,
      TRUE ~ NA_real_  # default to NA for any other values
    )
  )

# 9. Create the first_time dataframe by selecting the earliest visit per household
first_time <- first_visit %>%
  group_by(houseHoldIdAfn) %>%
  slice_min(order_by = clean_date, with_ties = FALSE) %>%
  ungroup()

# 10. Calculate monthly counts of first visits
monthly_first_visits <- first_time %>%
  mutate(
    month_start = floor_date(clean_date, unit = "month")  # round date down to first of month
  ) %>%
  count(month_start, name = "num_first_visits") %>%
  arrange(month_start)  # sort by month

# 11. Plot monthly first visits over time
ggplot(monthly_first_visits, aes(x = month_start, y = num_first_visits)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkred", size = 2) +
  labs(
    title = "Monthly First Visits Over Time",
    x     = "Month",
    y     = "Number of First Visits"
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%m/%y",
    limits      = c(as.Date("2023-01-01"), as.Date("2024-04-01"))
  ) +
  ylim(0, 250) +
  theme_minimal(base_size = 14)

# 12. Plot total monthly visits for comparison
#    a) Compute total visits per month
monthly_counts <- all %>%
  mutate(
    month_start = floor_date(clean_date, unit = "month")
  ) %>%
  count(month_start, name = "num_total_visits") %>%
  arrange(month_start)

#    b) Plot visits from Jan 2023 to Apr 2024
ggplot(
  monthly_counts %>% filter(
    month_start >= as.Date("2023-01-01") & month_start <= as.Date("2024-04-01")
  ),
  aes(x = month_start, y = num_total_visits)
) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkred", size = 2) +
  labs(
    title = "Monthly Total Visits (Jan 2023 – Apr 2024)",
    x     = "Month",
    y     = "Number of Visits"
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 13. Instructions for exporting dataframes for modeling:
# To write any dataframe to CSV, use:
# write.csv(df, "my_data.csv", row.names = FALSE)
# For example: write.csv(first_visit, "first_visit.csv", row.names = FALSE)
