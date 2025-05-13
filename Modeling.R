rm(list = ls())

library(tidyverse)
library(lubridate)
library(haven)
library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)

#Read in the pantry data 
all <- read.csv("DMARC Data 2018-2024 copy.csv")

#Make sure the date is usable
all$clean_date <- as.Date(all$servedDate)
all <- all[all$clean_date > as.Date("2020-01-31"), ]

#read in dataframes from NewData.R
first_time <- read.csv("first_time.csv")
first_visit <- read.csv("first_visit.csv")

#clean date just like the all data
first_time$clean_date <- as.Date(first_time$clean_date, format = "%m/%d/%Y")

first_visit$clean_date <- as.Date(first_visit$clean_date, format = "%m/%d/%Y")

# Create income brackets for pantry data to work with ACS
first_visit <- first_visit %>%
  mutate(
    income_bracket = case_when(
      annualIncome < 10000 ~ "income1",
      annualIncome >= 10000 & annualIncome < 15000 ~ "income2",
      annualIncome >= 15000 & annualIncome < 20000 ~ "income3",
      annualIncome >= 20000 & annualIncome < 25000 ~ "income4",
      annualIncome >= 25000 & annualIncome < 30000 ~ "income5",
      annualIncome >= 30000 & annualIncome < 35000 ~ "income6",
      annualIncome >= 35000 & annualIncome < 40000 ~ "income7",
      annualIncome >= 40000 & annualIncome < 45000 ~ "income8",
      annualIncome >= 45000 & annualIncome < 50000 ~ "income9",
      annualIncome >= 50000 & annualIncome < 60000 ~ "income10",
      annualIncome >= 60000 & annualIncome < 75000 ~ "income11",
      annualIncome >= 75000 & annualIncome < 100000 ~ "income12",
      annualIncome >= 100000 & annualIncome < 125000 ~ "income13",
      annualIncome >= 125000 & annualIncome < 150000 ~ "income14",
      annualIncome >= 150000 & annualIncome < 200000 ~ "income15",
      annualIncome >= 200000 ~ "income16",
      TRUE ~ NA_character_
    )
  )


#Make sure we avoid using the years that have all 12 months of data
valid_years <- first_visit %>%
  mutate(year = year(clean_date), month = month(clean_date)) %>%
  distinct(year, month) %>%
  count(year) %>%
  filter(n == 12) %>%
  pull(year)

all_filtered <- first_visit %>%
  mutate(year = year(clean_date)) %>%
  filter(year %in% valid_years)

#ADD FOOD STAMPS

all_filtered <- all_filtered %>%
  mutate(
    foodstamps_binary = ifelse(foodstamps == "Yes", 1, 0)
  )

#Train on the information provided by ACS in pantry
monthly_training_data <- all_filtered %>%
  filter(!is.na(clean_date)) %>%
  mutate(
    month_start = floor_date(as.Date(clean_date), "month"),
    foodstamps_binary = ifelse(foodstamps == "Yes", 1, 0)
  ) %>%
  group_by(month_start) %>%
  summarise(
    pantry_visits = n(),
    #avg_income = mean(annualIncome, na.rm = TRUE),
    avg_foodstamps = mean(foodstamps_binary, na.rm = TRUE),
    edu1 = sum(education_numeric == 1, na.rm = TRUE),
    edu2 = sum(education_numeric == 2, na.rm = TRUE),
    edu3 = sum(education_numeric == 3, na.rm = TRUE),
    edu4 = sum(education_numeric == 4, na.rm = TRUE),
    edu5 = sum(education_numeric == 5, na.rm = TRUE),
    edu6 = sum(education_numeric == 6, na.rm = TRUE),
    edu7 = sum(education_numeric == 7, na.rm = TRUE),
    edu8 = sum(education_numeric == 8, na.rm = TRUE),
    edu9 = sum(education_numeric == 9, na.rm = TRUE),
    hhsize1 = sum(householdMembers == 1, na.rm = TRUE),
    hhsize2 = sum(householdMembers == 2, na.rm = TRUE),
    hhsize3 = sum(householdMembers == 3, na.rm = TRUE),
    hhsize4 = sum(householdMembers == 4, na.rm = TRUE),
    hhsize5 = sum(householdMembers == 5, na.rm = TRUE),
    hhsize6 = sum(householdMembers >= 6, na.rm = TRUE),
    income1 = sum(income_bracket == "income1", na.rm = TRUE),
    income2 = sum(income_bracket == "income2", na.rm = TRUE),
    income3 = sum(income_bracket == "income3", na.rm = TRUE),
    income4 = sum(income_bracket == "income4", na.rm = TRUE),
    income5 = sum(income_bracket == "income5", na.rm = TRUE),
    income6 = sum(income_bracket == "income6", na.rm = TRUE),
    income7 = sum(income_bracket == "income7", na.rm = TRUE),
    income8 = sum(income_bracket == "income8", na.rm = TRUE),
    income9 = sum(income_bracket == "income9", na.rm = TRUE),
    income10 = sum(income_bracket == "income10", na.rm = TRUE),
    income11 = sum(income_bracket == "income11", na.rm = TRUE),
    income12 = sum(income_bracket == "income12", na.rm = TRUE),
    income13 = sum(income_bracket == "income13", na.rm = TRUE),
    income14 = sum(income_bracket == "income14", na.rm = TRUE),
    income15 = sum(income_bracket == "income15", na.rm = TRUE),
    income16 = sum(income_bracket == "income16", na.rm = TRUE)
  ) %>%
  ungroup()


model <- glm(pantry_visits ~ 
                edu1 + edu3 + edu4 + edu7 + edu8 + edu9 +
                hhsize1 + hhsize2 + hhsize3 + hhsize4 + hhsize5 + hhsize6 +
                income1 + income2 + income3 + income4 + income5 + income6 +
                income7 + income8 + income9 + income10 + income11 + income12 +
                income13 + income14 + income15,
              family = poisson(link = "log"),
              data = monthly_training_data)

summary(model)

#Pull in ACS data
source("ACS_Clean.R")

#Edu needed to be cleaned to match together 
dsm_totals <- dsm_totals %>%
  mutate(
    Edu1 = edu1,
    Edu3 = rowSums(select(., edu2, edu3, edu4), na.rm = TRUE),  # K–8
    Edu4 = rowSums(select(., edu5, edu6, edu7, edu8), na.rm = TRUE),  # Dropouts
    Edu7 = rowSums(select(., edu9, edu10), na.rm = TRUE),  # HS grad
    Edu8 = rowSums(select(., edu11, edu12), na.rm = TRUE),  # Some college
    Edu9 = rowSums(select(., edu13, edu14, edu15, edu16), na.rm = TRUE)   # College grads
  )

lowercase_edus <- paste0("edu", 1:16)

dsm_totals <- dsm_totals %>%
  select(-any_of(lowercase_edus))


selected_edu_nums <- c(1, 3, 4, 7, 8, 9)

dsm_totals <- dsm_totals %>%
  rename_with(
    .cols = paste0("Edu", selected_edu_nums),
    .fn = ~ paste0("edu", selected_edu_nums)
  )


edu_by_month <- all_filtered %>%
  filter(!is.na(clean_date), !is.na(education_numeric)) %>%
  mutate(month_start = floor_date(as.Date(clean_date), "month")) %>%
  group_by(month_start, education_numeric) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = education_numeric,
    values_from = n,
    names_prefix = "edu"
  ) %>%
  arrange(month_start)


#Gather dataframe for population of Des Moines
acs_total_pop <- dsm_totals %>%
  select(starts_with("income")) %>%
  rowSums(na.rm = TRUE)

#Estimate totals that fall in the most appearing categories from Pantry data
acs_estimate <- dsm_totals %>%
  transmute(
    matching_education = edu7 + edu3 + edu4,
    matching_income = income1 + income2 + income3 + income4 + income5 + income6 + income7 + income8
  ) %>%
  mutate(
    at_risk_estimate = pmin(matching_education, matching_income)
  )


#Group data by year
all %>%
  mutate(clean_date = as.Date(servedDate), year = year(clean_date)) %>%
  group_by(year) %>%
  summarise(unique_households = n_distinct(houseHoldIdAfn))

#total based on income shows us the number of the largest category
estimated_acs_need <- dsm_totals$income1 +
  dsm_totals$income2 +
  dsm_totals$income3 +
  dsm_totals$income4 +
  dsm_totals$income5 +
  dsm_totals$income6 + 
  dsm_totals$income7 

#Getting total for the expected need
estimated_acs_need
#43,213

#SOME PROJECTIONS
years <- 2018:2023
users <- c(17730, 19032, 18318, 15509, 16983, 17225)  # actual unique households per year

#What growth will be
logistic_growth <- function(year, K, r, x0) {
  K / (1 + exp(-r * (year - x0)))
}

# Try parameters:
K <- 42000  # carrying capacity
r <- 0.3    # growth rate
x0 <- 2025  # midpoint of growth

# Predict for next 5 years
future_years <- 2018:2028
predicted <- logistic_growth(future_years, K, r, x0)

# Combine into data frame
df <- data.frame(
  year = future_years,
  predicted_users = predicted,
  actual_users = c(users, rep(NA, length(future_years) - length(users)))
)

#create model for future years
model <- lm(users ~ poly(years, 2))
future_years <- data.frame(years = 2018:2028)
future_users <- predict(model, newdata = future_years)

predicted <- logistic_growth(future_years, 37000, 0.15, 2024)

predicted



# Parameters
K <- 37000       # carrying capacity
r <- 0.3         # growth rate (you can tweak this)
x0 <- 2030       # midpoint year where growth is fastest

# Generate years beyond 2028
future_years <- 2018:2070
predicted <- logistic_growth(future_years, K, r, x0)

# Stop once we reach (or exceed) 42,000
cutoff_index <- which(predicted >= 36900)[1]
final_years <- future_years[1:cutoff_index]
final_predicted <- predicted[1:cutoff_index]

# Combine into a data frame
df <- data.frame(
  year = final_years,
  projected_users = final_predicted
)


# Create data frame
df2 <- data.frame(
  year = 2021:2029,
  users = c(15509, 16983, 17225,
            21328, 23055, 25744, 26727, 27504, 28110),
  type = c(rep("Actual", 3), rep("Projected", 6))
)


actual_df <- df2[df2$year <= 2024, ]
projected_df <- df2[df2$year >= 2024, ]


#Graph out future year total households
ggplot() +
  geom_line(data = actual_df, aes(x = year, y = users), color = "black", size = 1.5) +
  geom_line(data = projected_df, aes(x = year, y = users), color = "black", linetype = "dotted", size = 1.5) +
  geom_point(data = df2, aes(x = year, y = users, color = type), size = 3) +
  scale_color_manual(values = c("Actual" = "black", "Projected" = "gray")) +
  labs(
    title = "Actual vs Projected Pantry Users (2021–2030)",
    x = "Year",
    y = "Number of Users",
    color = "Legend"
  ) +
  scale_x_continuous(
    breaks = 2021:2029,  # Show each year label
    limits = c(2021, 2029)
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),
    legend.position = "top"
  )


afn_data <- data.frame(
  year = 2018:2024,
  unique_afns = c(17730, 19032, 18318, 15509, 16983, 17225, 22167)
)

# Filter for 2019–2023
afn_filtered <- afn_data[afn_data$year >= 2019 & afn_data$year <= 2023, ]



visits_per_year <- all %>%
  mutate(served_year = year(clean_date)) %>%
  filter(!is.na(served_year), served_year >= 2019 & served_year <= 2023) %>%
  group_by(served_year) %>%
  summarise(total_visits = n())

#Total 
unique_afns <- data.frame(
  served_year = 2018:2024,
  unique_households = c(17730, 19032, 18318, 15509, 16983, 17225, 22167)
)

# Filter for 2020–2023
unique_filtered <- unique_afns %>% filter(served_year > 2019 & served_year <= 2023)


scale_y_continuous(labels = scales::label_number(scale = 1/1000, suffix = "K"))

#Combine the total visits and total households
combined_plot_data <- visits_per_year %>%
  rename(value = total_visits) %>%
  mutate(metric = "Total Visits") %>%
  bind_rows(
    unique_filtered %>%
      rename(value = unique_households) %>%
      mutate(metric = "Unique Households")
  )

#Graph with total visits compared to how many unique households yearly
ggplot(combined_plot_data, aes(x = factor(served_year), y = value, group = metric)) +
  geom_col(data = filter(combined_plot_data, metric == "Total Visits"),
           aes(fill = metric), width = 0.6) +
  geom_line(data = filter(combined_plot_data, metric == "Unique Households"),
            aes(color = metric), size = 1.2) +
  geom_point(data = filter(combined_plot_data, metric == "Unique Households"),
             aes(color = metric), size = 3) +
  scale_y_continuous(labels = label_number(scale = 1/1000, suffix = "K")) +
  scale_fill_manual(values = c("Total Visits" = "gray70")) +
  scale_color_manual(values = c("Unique Households" = "black")) +
  labs(
    title = "Total Visits vs. Unique Households (2019–2023)",
    x = "Year",
    y = "Count of Visits",
    fill = "Metric",
    color = "Metric"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16, margin = margin(t = 10)),
    axis.title.y = element_text(size = 16, margin = margin(r = 10)),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )

