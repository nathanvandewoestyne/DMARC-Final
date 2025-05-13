rm(list=ls())
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(forecast)
library(report)


fvisit <- read.csv("Data/first_time.csv")

#Every row is a person-visit combination
#AKA a family of 3 visits, 3 rows
# afn is household ID

summary(fvisit)
head(fvisit)
#Create 3 datasets
# 1 dataset where 1 visit is one row
#

#cleaning date column
fvisit$clean_date <- mdy(fvisit$clean_date)
str(fvisit$clean_date)


=
visits_ts_df <- fvisit %>%
  mutate(visit_date = as_date(clean_date)) %>%
  # aggregate to, e.g., monthly counts
  group_by(month = floor_date(clean_date, "month")) %>%
  summarise(n_visits = n()) %>%
  arrange(month)


summary(visits_ts_df)
#visits over time
ggplot(visits_ts_df, aes(month, n_visits)) +
  geom_line() +
  labs(x="Month", y="Number of Visits",
       title="Food-Pantry Visits Over Time")


ts_data <- ts(visits_ts_df$n_visits,
              start = c(year(min(visits_ts_df$month)),
                        month(min(visits_ts_df$month))),
              frequency = 12)
decomp <- stl(ts_data, s.window="periodic")
autoplot(decomp)
 

fit_arima <- auto.arima(ts_data)
summary(fit_arima)
checkresiduals(fit_arima)


demo_ts_df <- fvisit %>% 
  mutate(month = floor_date(clean_date, "month")) %>% 
  group_by(month) %>%
  summarise(
    pct_low_income = mean(annualIncome < 32150),
    avg_num_household     = mean(householdMembers),
    
  ) %>%
  arrange(month)

df_model <- left_join(visits_ts_df, demo_ts_df, by="month")
y <- ts(df_model$n_visits, start=2022, frequency=12)
xreg <- as.matrix(df_model %>% select(-month, -n_visits))


df_mean_imp <- df_model %>%
  mutate(
    pct_low_income  = if_else(is.na(pct_low_income),
                              mean(pct_low_income, na.rm = TRUE),
                              pct_low_income),
    avg_num_household      = if_else(is.na(avg_num_household),
                              mean(avg_num_household, na.rm = TRUE),
                              avg_num_household))


fit_arimax <- auto.arima(y, xreg = xreg)


summary(fit_arimax)

checkresiduals(fit_arimax)



