library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)

# Set base URL and variables
BASE_URL <- "https://api.census.gov/data"


income_vars <- paste0("B19001_", sprintf("%03d", 2:17), "E")

# Educational attainment (25+): B15003_002E to B15003_017E
education_vars <- paste0("B15003_", sprintf("%03d", 2:17), "E")

# Household size counts: B11016_002E to B11016_008E (1-person to 7+ person households)
hhsize_vars <- paste0("B11016_", sprintf("%03d", 2:8), "E")

#Foodstamps yes or no
foodstamp_vars <- c("B22003_001E", "B22003_002E")

# Combine all into a single list
VARIABLES <- c(income_vars, education_vars, hhsize_vars, foodstamp_vars)

# Initialize results
results <- data.frame()

# Loop over years
for (year in 2023:2023) {
  
  url <- paste0(BASE_URL, "/", year, "/acs/acs5")
  
  # Build the comma-separated variable list
  var_string <- paste(c("NAME", VARIABLES), collapse = ",")
  
  # Query parameters
  params <- list(
    get = var_string,
    `for` = "zip code tabulation area:*"
  )
  
  # Only for pre-2020 data, include "in=state:19" (Iowa)
  if (year < 2020) {
    params[["in"]] <- "state:19"
  }
  
  # Make API request
  response <- GET(url, query = params)
  
  # Process response
  if (status_code(response) == 200) {
    raw <- content(response, as = "text", encoding = "UTF-8")
    parsed <- fromJSON(raw)
    df <- as.data.frame(parsed)
    colnames(df) <- df[1, ]
    df <- df[-1, ]
    df$year <- year
    df <- df %>%
      mutate(across(all_of(VARIABLES), as.numeric))
    results <- bind_rows(results, df)
  } else {
    print(paste("Failed for year", year, ":", status_code(response)))
  }
}

# Print a preview

print(head(results))


dsm_zips <- c("50021", "50023", "50131", "50313", "50322", "50310", "50317", "50009", "50325", "50311", "50314", "50316", "50266", "50265", "50312", "50309", "50327", "50321", "50315", "50320")

dsm_results <- results %>%
  filter(`zip code tabulation area` %in% dsm_zips)

dsm_results <- dsm_results %>%
  mutate(
    snap_rate = as.numeric(B22003_002E) / as.numeric(B22003_001E)
  )

dsm_by_zip <- dsm_results %>%
  group_by(year, zip = `zip code tabulation area`) %>%
  summarise(across(all_of(VARIABLES), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(
    snap_rate = B22003_002E / B22003_001E
  ) %>%
  rename_with(~ paste0("income", seq_along(income_vars)), all_of(income_vars)) %>%
  rename_with(~ paste0("edu", seq_along(education_vars)), all_of(education_vars)) %>%
  rename_with(~ paste0("hhsize", seq_along(hhsize_vars)), all_of(hhsize_vars)) %>%
  rename(avg_foodstamps = snap_rate)


print(head(dsm_results))



dsm_totals <- dsm_results %>%
  group_by(year) %>%
  summarise(across(all_of(VARIABLES), ~ sum(.x, na.rm = TRUE)))

dsm_totals <- dsm_totals %>%
  mutate(snap_rate = B22003_002E / B22003_001E)

dsm_totals <- dsm_totals %>%
  rename_with(~ paste0("income", seq_along(income_vars)), all_of(income_vars)) %>%
  rename_with(~ paste0("edu", seq_along(education_vars)), all_of(education_vars)) %>%
  rename_with(~ paste0("hhsize", seq_along(hhsize_vars)), all_of(hhsize_vars)) %>% 
  rename(avg_foodstamps = snap_rate)
