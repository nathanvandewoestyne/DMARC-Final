# =========================== 0. Setup & Libraries ===========================
# install.packages(c("tidyverse","cluster","factorextra"))
library(tidyverse)
library(cluster)
library(factoextra)

# ======================= 1. Load & Cluster DMARC ==========================
# 1a. Load & tag
dmarc <- read_csv("Raw_Data/first_time.csv") %>%
  mutate(id = row_number())

# 1b. Build clustering frame
cluster_df <- dmarc %>%
  mutate(food_stamps = if_else(str_to_lower(foodstamps) == "yes", 1L, 0L)) %>%
  select(id, householdMembers, annualIncome, education_numeric, food_stamps) %>%
  drop_na()

# 1c. Scale & save parameters
scaled_mat <- scale(cluster_df %>% select(-id))
centers    <- attr(scaled_mat, "scaled:center")
scales     <- attr(scaled_mat, "scaled:scale")

# 1d. Run k-means (k = 4)
set.seed(123)
k_opt <- 4
km4   <- kmeans(scaled_mat, centers = k_opt, nstart = 25)

# 1e. Merge cluster labels
clusters_lookup <- tibble(id = cluster_df$id,
                          cluster = factor(km4$cluster))
dmarc_labeled <- dmarc %>% left_join(clusters_lookup, by = "id")

# 1f. Summarize DMARC by cluster
cluster_profiles <- dmarc_labeled %>%
  filter(!is.na(cluster)) %>%
  group_by(cluster) %>%
  summarise(
    n_families    = n(),
    avg_household = mean(householdMembers),
    avg_income    = mean(annualIncome),
    avg_education = mean(education_numeric),
  ) %>%
  arrange(cluster)

print(cluster_profiles)

# ======================= 2. Load & Prepare ACS Data =========================
# (Assumes ACS_Clean.R produces `dsm_by_zip` or `dsm_by_tract` with:
#   - a geographic ID column (zip or tract)
#   - hhsize1...hhsize7, income1...income16, edu1...edu16, avg_foodstamps
# )
source("ACS_Clean.R")

acs <- dsm_by_zip %>%      # or dsm_by_tract
  ungroup() %>%            # drop any grouping
  mutate(
    total_hh    = rowSums(across(starts_with("hhsize"))),
    avg_hh_size = rowSums(across(starts_with("hhsize")) * (1:7)) / total_hh,
    avg_income  = {
      mids <- c(5000,12500,17500,22500,27500,32500,
                37500,42500,47500,55000,67500,87500,
                112500,137500,175000,225000)
      rowSums(across(starts_with("income")) * mids) / total_hh
    },
    avg_edu_num = {
      ord <- 1:16
      rowSums(across(starts_with("edu")) * ord) / total_hh
    },
    snap_rate   = avg_foodstamps
  ) %>%
  select(everything(), avg_hh_size, avg_income, avg_edu_num, snap_rate, total_hh)

# ============= 3. Rescale & Assign ACS to Clusters ========================
acs_scaled <- acs %>%
  mutate(
    sc_hh   = (avg_hh_size - centers["householdMembers"])   / scales["householdMembers"],
    sc_inc  = (avg_income  - centers["annualIncome"])       / scales["annualIncome"],
    sc_edu  = (avg_edu_num - centers["education_numeric"])  / scales["education_numeric"],
  )

# helper to find nearest centroid
predict_cluster <- function(mat, cent) {
  apply(mat, 1, function(x) {
    which.min(colSums((t(cent) - x)^2))
  })
}

acs_mat <- acs_scaled %>% select(sc_hh, sc_inc, sc_edu) %>% as.matrix()
acs_scaled$assigned_cluster <- factor(predict_cluster(acs_mat, km4$centers))

# ======== 4. Summarize ACS vs. DMARC & Compute Growth ====================
# 4a. ACS counts by cluster
acs_counts <- acs_scaled %>%
  group_by(assigned_cluster) %>%
  summarise(acs_households = sum(total_hh)) %>%
  arrange(assigned_cluster)

# 4b. Combine with DMARC known families
cluster_projection <- acs_counts %>%
  left_join(cluster_profiles,
            by = c("assigned_cluster" = "cluster")) %>%
  rename(
    known_families = n_families
  ) %>%
  mutate(
    projected_growth = acs_households - known_families
  )

cluster_projection$Percentage_Known = cluster_projection$known_families/cluster_projection$acs_households

print(cluster_projection)
cluster_projection

# compute the silhouette widths
sil <- silhouette(km4$cluster, dist(scaled_mat))

# plot them
plot(sil,
     border = NA,
     main   = "Silhouette Plot for k = 4")


# ===================== 5. Export Results ================================
write_csv(dmarc_labeled,     "first_time_with_clusters.csv")
write_csv(cluster_profiles,   "cluster_profiles.csv")
write_csv(cluster_projection, "cluster_projection.csv")
