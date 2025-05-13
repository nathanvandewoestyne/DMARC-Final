rm(list = ls())


library(tidyverse)
library(lubridate)
library(haven)
library(ggplot2)
library(sf)
library(dplyr)
library(tigris)
library(ggrepel)
library(osmdata)
all <- read.csv("drake_export_v8_2024-02-13_100754_rev2_nolatlong.csv")


dim(all)

all <- all %>%
  mutate(
    served_date = ymd(served_date),
    dob = ymd(dob)
  )

head(all)
head(all$annual_income)
summary(all$annual_income)
sum(all$annual_income < 0, na.rm = TRUE)

# It looks like we have 5 observations with negative values, lets plan on dropping those

all <- all[all$annual_income >= 0, ]


visit <- all %>%
  group_by(afn, served_date, annual_income, education) %>%
  summarise(
    n_household = n(), 
    zip = first(zip)
  ) %>% 
  mutate(
    served_year = year(served_date),
    served_month = month(served_date),
    served_day_of_month = mday(served_date),
    round_month = round_date(served_date, "month"),
    first_visit = if_else(served_date == min(served_date), 1, 0),
    education_numeric = case_when(
      education == "Pre-K and younger (Currently)" ~ 1,
      education == "No Schooling" ~ 2,
      education == "K-8 (Currently)" ~ 3,
      education == "Low Grade Grad" ~ 4,
      education == "9-12 (Currently)" ~ 5,
      education == "K-12 Drop Out" ~ 6,
      education == "HSED / GED Grad (or current)" ~ 7,
      education == "HS Grad" ~ 8,
      education == "HS Grad / Some College" ~ 9,
      education == "College 2 or 4 yr  Degree" ~ 10,
      education == "College Advanced Degree" ~ 11,
      education == "Unknown" ~ 0,
      education == "Not Selected" ~ 0,
      TRUE ~ NA_real_
    )
  )

# Group by zip code and calculate the 95th percentile of annual income
zip_income_percentile <- visit %>%
  group_by(zip) %>%
  summarise(
    income_95th_percentile = quantile(annual_income, 0.95, na.rm = TRUE),
    .groups = 'drop'
  )

# View the result
head(zip_income_percentile)

hh_income <- all %>% 
  group_by(afn, annual_income, family_type, housing) %>%
  summarise(
    n_household = n() #counts the number of rows in each afn, served date
  )

first_visit_table <- visit %>%
  group_by(afn) %>%
  slice_max(order_by = education_numeric, n = 1, with_ties = FALSE) %>%
  ungroup()



ggplot(hh_income, aes(x = family_type)) +
  geom_bar(fill = "steelblue", color = "black", alpha = 0.5) +
  labs(title = "Income Distribution",
       x = "Family Type",
       y = "Annual Income") +
  theme_minimal()
    
race_table <- all %>%
  group_by(afn) %>%
  summarise(
    race = if_else(n_distinct(race) == 1, first(race), "Multi")
  )


race_table <- all %>%
  group_by(afn) %>%
  summarise(
    race = if_else(n_distinct(race) == 1, first(race), "Multi"),
    ethnicity = first(ethnicity)  # Assuming ethnicity is consistent within afn
  ) %>%
  mutate(
    race = if_else(ethnicity == "Hispanic or Latino" & race %in% c("Unknown", "Other"), 
                   "Hispanic/Latino", race)
  ) %>%
  select(-ethnicity)  # Remove ethnicity if not needed in the final table


first_visit_table <- first_visit_table %>%
  left_join(race_table, by = "afn")

#now we're in a place where we can count visits

monthly_counts <- visit %>% 
  group_by(round_month) %>% 
  summarise(num_VISITS = n(),
            num_PEOPLE_SERVED = sum(n_household)) #total number of people that month

ggplot(hh_income, aes(x = housing)) +
  geom_bar(fill = "steelblue", color = "black", alpha = 0.5) +
  labs(title = "Income Distribution",
       x = "Housing Type",
       y = "Annual Income") +
  theme_minimal()





visit %>%
  filter(first_visit == 1, served_date > as.Date("2019-01-01")) %>%
  group_by(round_month) %>%
  summarise(count_first_visits = n()) %>%
  ggplot(aes(x = round_month, y = count_first_visits)) +
  geom_col(fill = "skyblue") +
  geom_text(aes(label = count_first_visits), vjust = -0.5) +  # Adds count above each bar
  labs(
    title = "First Visits to Food Pantry by Month",
    x = "Month",
    y = "Number of First Visits"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(lubridate)

visit %>%
  filter(first_visit == 1, year(served_date) == 2023) %>%
  mutate(week_start = floor_date(served_date, "week")) %>%
  group_by(week_start) %>%
  summarise(count_first_visits = n()) %>%
  ggplot(aes(x = week_start, y = count_first_visits)) +
  geom_col(fill = "skyblue") +
  geom_text(aes(label = count_first_visits), vjust = -0.5) +  # Adds count above each bar
  labs(
    title = "First Visits to Food Pantry by Week in 2023",
    x = "Week Start",
    y = "Number of First Visits"
  ) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


all %>% 
  distinct(education)
# Make unique afn by zip table
first_visit_count <- first_visit_table %>%
  group_by(zip) %>%
  summarise(count = n())

first_visit_2023 <- first_visit_table %>%
  filter(served_year %in% 2019:2023) %>%
  group_by(zip) %>%
  summarise(count = n())

zip_shapes <- tigris::zctas(state = "IA", year = 2010)

first_visit_2023 %>%
  filter(zip %in% zip_shapes$ZCTA5CE10) 

map_data <- left_join(zip_shapes, first_visit_2023, by = c("ZCTA5CE10" = "zip"))

ggplot(map_data) +
  geom_sf(aes(fill = count), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(title = "First time visitors by zip code 2023",
       fill = "visitor count") +
  theme_minimal()

xmin <- -93.9
xmax <- -93.4
ymin <- 41.4
ymax <- 41.8

dsm_map <- st_crop(map_data, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)

bbox <- c(-93.9, 41.4, -93.4, 41.8)
highways <- opq(bbox=bbox) %>%
  add_osm_feature(key = "highway", value = c("motorway", "motorway_link")) %>%
  osmdata_sf()

dsm_highways <- highways$osm_lines

dsm_landmarks <- data.frame(
  name = c("Downtown", "DSM", "Jordan Creek Mall", "Drake"),
  lat = c(41.5868, 41.5341, 41.5703, 41.6031),
  long = c(-93.6250, -93.6638, -93.8077, -93.6546))
# Current household map
ggplot(dsm_map) +
  geom_sf(aes(fill = count), color = "white", size = 0.1) + #visitor data
  geom_sf(data = dsm_highways, color = "red", size = 1)+ #highway lines
  geom_text_repel(data = dsm_landmarks, aes(x=long, y=lat, label=name),
                  color="black", size = 2, fontface="bold",
                  box.padding =2) +
  scale_fill_gradientn(colors = c("#f7fbff","#deebf7", "#9ecae1", "#3182bd"),
                       labels = scales::comma) +
  labs(title = "Household Visitors by Zip Code",
       fill = "Visitor Count") +
  theme_minimal()

# Predicted Map
predictedtotal <- read.csv("predictedtotals.csv")

predictedtotal <- predictedtotal %>%
  mutate(zip = as.character(zip))

zip_shapes <- tigris::zctas(state = "IA", year = 2010)

predictedtotal %>%
  filter(zip %in% zip_shapes$ZCTA5CE10) 

map_data_predicted <- left_join(zip_shapes, predictedtotal, by = c("ZCTA5CE10" = "zip"))

xmin <- -93.9
xmax <- -93.4
ymin <- 41.4
ymax <- 41.8

dsm_map_predicted <- st_crop(map_data_predicted, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)

bbox <- c(-93.9, 41.4, -93.4, 41.8)
highways <- opq(bbox=bbox) %>%
  add_osm_feature(key = "highway", value = c("motorway", "motorway_link")) %>%
  osmdata_sf()

dsm_highways <- highways$osm_lines
# Predicted total visitors map
dsm_landmarks <- data.frame(
  name = c("Downtown", "DSM", "Jordan Creek Mall", "Drake"),
  lat = c(41.5868, 41.5341, 41.5703, 41.6031),
  long = c(-93.6250, -93.6638, -93.8077, -93.6546))

ggplot(dsm_map_predicted) +
  geom_sf(aes(fill = estimated_acs_need), color = "white", size = 0.1) + #visitor data
  geom_sf(data = dsm_highways, color = "red", size = 1)+ #highway lines
  geom_text_repel(data = dsm_landmarks, aes(x=long, y=lat, label=name),
                  color="black", size = 2, fontface="bold",
                  box.padding =2) +
  scale_fill_gradientn(colors = c("#f7fbff","#deebf7", "#9ecae1", "#3182bd"),
                       labels = scales::comma) +
  labs(title = "Household Visitors by Zip Code",
       fill = "Visitor Count") +
  theme_minimal()

#predicting additional need
additional_need <- full_join(first_visit_2023, predictedtotal, by="zip")

filter_zips <- c("50021", "50023", "50131", "50313", "50322", "50310", "50317",
              "50009", "50325", "50311", "50314", "50316", "50266", "50265",
              "50312", "50309", "50327", "50321", "50315", "50320")
additional_need <- additional_need %>%
  filter(zip %in% filter_zips)


additional_need <- additional_need %>%
  mutate(additional = coalesce(estimated_acs_need, 0) - coalesce(count, 0))


zip_shapes <- tigris::zctas(state = "IA", year = 2010)

additional_need %>%
  filter(zip %in% zip_shapes$ZCTA5CE10) 

map_data_additional <- left_join(zip_shapes, additional_need, by = c("ZCTA5CE10" = "zip"))

xmin <- -93.9
xmax <- -93.4
ymin <- 41.4
ymax <- 41.8

dsm_map_additional <- st_crop(map_data_additional, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)

bbox <- c(-93.9, 41.4, -93.4, 41.8)
highways <- opq(bbox=bbox) %>%
  add_osm_feature(key = "highway", value = c("motorway", "motorway_link")) %>%
  osmdata_sf()

dsm_highways <- highways$osm_lines

dsm_landmarks <- data.frame(
  name = c("Downtown", "DSM", "Jordan Creek Mall", "Drake"),
  lat = c(41.5868, 41.5341, 41.5703, 41.6031),
  long = c(-93.6250, -93.6638, -93.8077, -93.6546))
# Predicted Additional Data
ggplot(dsm_map_additional) +
  geom_sf(aes(fill = additional), color = "white", size = 0.1) + #visitor data
  geom_sf(data = dsm_highways, color = "red", size = 1)+ #highway lines
  geom_text_repel(data = dsm_landmarks, aes(x=long, y=lat, label=name),
                  color="black", size = 2, fontface="bold",
                  box.padding =2) +
  scale_fill_gradientn(colors = c("#f7fbff","#deebf7", "#9ecae1", "#3182bd"),
                       labels = scales::comma) +
  labs(title = "Additional Predicted Visitors by Zip Code",
       fill = "Visitor Count") +
  theme_minimal()
