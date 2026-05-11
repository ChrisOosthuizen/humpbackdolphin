
library(tidyverse)

dat = read.csv("./data/individual_encounters.csv")

head(dat)

# how many IDs?
dat %>%
  summarise(n_ids = n_distinct(id))

# (1) Overall number of unique IDs per site
ids_per_site <- dat %>%
  group_by(site) %>%
  summarise(n_ids = n_distinct(id))

ids_per_site



# (3) Breakdown by number of sites each ID was seen at
multi_site_breakdown <- dat %>%
  group_by(id) %>%
  summarise(
    n_sites = n_distinct(site),
    sites_visited = paste(sort(unique(site)), collapse = ", ")
  ) %>%
  count(n_sites, name = "n_ids") %>%
  mutate(category = case_when(
    n_sites == 1 ~ "Single site only",
    n_sites == 2 ~ "Two sites",
    n_sites == 3 ~ "All three sites"
  ))

multi_site_breakdown

# Combined summary table
summary_table <- dat %>%
  group_by(id) %>%
  summarise(
    n_sites = n_distinct(site),
    sites = paste(sort(unique(site)), collapse = ", ")
  ) %>%
  group_by(n_sites) %>%
  summarise(
    n_ids = n(),
    percentage = round(n() / n_distinct(dat$id) * 100, 1)
  )

summary_table

# Detailed: which specific sites for multi-site IDs
multi_site_details <- dat %>%
  group_by(id) %>%
  summarise(
    n_sites = n_distinct(site),
    sites = paste(sort(unique(site)), collapse = " + ")
  ) %>%
  filter(n_sites > 1) %>%
  count(sites, name = "n_ids")

multi_site_details

# Complete summary
complete_summary <- dat %>%
  group_by(id) %>%
  summarise(
    n_sites = n_distinct(site),
    site_combination = paste(sort(unique(site)), collapse = " + ")
  ) %>%
  count(site_combination, n_sites, name = "n_ids") %>%
  arrange(n_sites, site_combination)

complete_summary
