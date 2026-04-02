# Nowacek Project 1
# A1_ 250000540.R

# library important packages, install first if needed

library(tidyverse)
library(Distance)
library(knitr)
library(gt)

# import, view data dictionary, and wrangle data for analysis

d <- read_csv("A1_data.csv")

data_dict <- tribble(
  ~Variable,          ~Description,
  "Sample.Label",     "ID number of the 1km square that was surveyed",
  "object_id",        "ID number of the observation for each individual bird detected",
  "distance_cat",     "Distance in m of the bird from the transect line (midpoint of 10m wide bands)",
  "Urban",            "Percentage of the 1km square that is Urban habitat (min = 0, max = 100)",
  "Coniferous",       "Percentage of the 1km square that is Coniferous forest (min = 0, max = 100)",
  "Broadleaf",        "Percentage of the 1km square that is Broadleaf forest (min = 0, max = 100)",
  "MountainHeathBog", "Percentage of the 1km square that is Mountain, Heath, or Bog (min = 0, max = 100)",
  "Freshwater",       "Percentage of the 1km square that is Freshwater habitat (min = 0, max = 100)"
)

kable(data_dict)

d <- d |>
  mutate(distance = distance_cat,
         area = 78000,
         region = "Scot")

d_a <- d |>
  filter(species == "A")

d_b <- d |>
  filter(species == "B")

# set up region, sample, and observation table for explicit definition of 
# study parameters

region_table <- tibble(Region.Label = "Scot",
                       Area = 78000)


sample_table_a <- unique(tibble(Sample.Label = d_a$Sample.Label, 
                                Region.Label = d_a$region,
                                Effort = 2))

obs_a <- d_a |>
  filter(!is.na(object_id))

obs_table_a <- tibble(object = obs_a$object_id,
                      Region.Label = obs_a$region,
                      Sample.Label = obs_a$Sample.Label)


sample_table_b <- unique(tibble(Sample.Label = d_b$Sample.Label, 
                                Region.Label = d_b$region,
                                Effort = 2))

obs_b <- d_b |>
  filter(!is.na(object_id))

obs_table_b <- tibble(object = obs_b$object_id,
                      Region.Label = obs_b$region,
                      Sample.Label = obs_b$Sample.Label)


# exploratory data analysis

ggplot(d_a, aes(x = distance)) +
  geom_histogram(bins = 8) +
  theme_bw() +
  labs(x = "Distance", y = "Frequency", title = "Species A")

ggplot(d_b, aes(x = distance)) +
  geom_histogram(bins = 20) +
  theme_bw() +
  labs(x = "Distance", y = "Frequency", title = "Species B")


# finding a truncation distance that includes about 95% of the data, 
# as per the recommendations in lecture and distance documentation

d_a_q <- d_a |>
  filter(!is.na(distance))

quantile(probs = 0.95, d_a_q$distance)

d_b_q <- d_b |>
  filter(!is.na(distance))

quantile(probs = 0.95, d_b_q$distance)


# setting up a unit conversion for the distances and areas

units <- convert_units("Meter", "Kilometer", "Square kilometer")


# Species A Models

a_covs <- ds(
  data = d_a,
  truncation = 50,
  transect = "line",
  formula = ~ Urban + Coniferous + Broadleaf + MountainHeathBog + Freshwater,
  cutpoints = c(0, 20, 30, 40, 50),
  key = "hr",
  region_table = region_table,
  sample_table = sample_table_a,
  obs_table = obs_table_a,
  convert_units = units)

plot(a_covs)

# a_adj <- ds(
#   data = d_a,
#   truncation = 80,
#   transect = "line",
#   adjustment =  "cos",
#   cutpoints = c(0, 20, 30, 40, 50, 60, 70, 80),
#   key = "hr",
#   region_table = region_table,
#   sample_table = sample_table_a,
#   obs_table = obs_table_a,
#   convert_units = 0.001)

# plot(a_adj)


## Species A Goodness of Fit

gof_ds(a_covs)

# gof_ds(a_adj)


## Species A Abundance Estimate

A_res <- summary(a_covs)

res <- data.frame(A_est = A_res[["dht"]][["individuals"]][["N"]][["Estimate"]],
                  A_lower = A_res[["dht"]][["individuals"]][["N"]][["lcl"]],
                  A_upper = A_res[["dht"]][["individuals"]][["N"]][["ucl"]])


## Species B Model

b_covs <- ds(
  data = d_b,
  truncation = 130,
  transect = "line",
  formula = ~ Urban + Coniferous + Broadleaf + 
    MountainHeathBog + Freshwater,
  cutpoints = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130),
  key = "hn",
  region_table = region_table,
  sample_table = sample_table_b,
  obs_table = obs_table_b,
  convert_units = 0.001)

# b_adj <- ds(
#   data = d_b,
#   truncation = 130,
#   transect = "line",
#   # formula = ~ Urban + Coniferous + Broadleaf + 
#     MountainHeathBog + Freshwater,
#   adjustment =  "cos",
#   cutpoints = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130),
#   key = "hn",
#   region_table = region_table,
#   sample_table = sample_table_b,
#   obs_table = obs_table_b,
#   convert_units = 0.001)

## Species B Plot

plot(b_covs)

## Species B Goodness of Fit

gof_ds(b_covs)

# gof_ds(b_adj)

## Species B Abundance Estimate

B_res <- summary(b_covs)

res <- res |>
  mutate(B_est = B_res[["dht"]][["individuals"]][["N"]][["Estimate"]],
         B_lower = B_res[["dht"]][["individuals"]][["N"]][["lcl"]],
         B_upper = B_res[["dht"]][["individuals"]][["N"]][["ucl"]])

# Detection Function Plot

par(mfrow = c(1, 2)) 

plot(a_covs, 
     main = "Species A Detection Function",
     xlab = "Distance (m)")
plot(b_covs, 
     main = "Species B Detection Function",
     xlab = "Distance (m)") 

par(mfrow = c(1, 1))

# Results plot

res|>
  gt()|>
  fmt_number(
    columns = everything(),
    decimals = 0)|>
  
  cols_merge(
    columns = c(A_lower, A_upper),
    pattern = "({1} to {2})")|>
  
  cols_merge(
    columns = c(B_lower, B_upper),
    pattern = "({1} to {2})")|>
  
  tab_spanner(
    label = "Species A",
    columns = starts_with("A_"))|>
  
  tab_spanner(
    label = "Species B",
    columns = starts_with("B_"))|>
  
  cols_label(
    ends_with("_est") ~ "Estimate",
    ends_with("_lower") ~ "Confidence Interval")|>
  
  tab_source_note(
    source_note = "Abundance estimates for both species, with confidence intervals.")
