
#----------------------------------------------------------
# Chris Oosthuizen
# Feb 2026

# Plot encounter history matrix and transient histogram
#----------------------------------------------------------

#----------
# Setup 
#----------

library(tidyverse)

# Plotting theme
theme_gg <- function () { 
  theme_bw() %+replace% 
    theme(
      axis.text = element_text(colour = "black"),
      # axis.title = element_blank(),
      axis.ticks = element_line(colour = "black"),
      panel.grid = element_blank(),
      strip.background = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA),
      axis.line = element_line(colour = "black")
    )
}


#----------
# load data
#----------
dat = read.csv("./data/2026_2014_15_individual_encounters.csv")
head(dat)

dat %>% 
   summarise(No_ids = n_distinct(id))

dat %>% 
  group_by(site) %>%
  summarise(No_ids = n_distinct(id))

# First count observations per ID
id_counts <- dat %>%
  count(id, name = "n_observations")

id_counts

# Then count how many IDs have each observation count
frequency_table <- id_counts %>%
  count(n_observations, name = "n_ids")

# View the result
frequency_table

#--------------------
# View the counts
#--------------------
head(id_counts)

id_counts <- id_counts %>%
  mutate(color_group = ifelse(n_observations == 1, "Transient", "Resident"))

ehm_hist = ggplot(id_counts, aes(x = n_observations, fill = color_group)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Transient" = "red2", "Resident" = "steelblue")) +
  scale_x_continuous(breaks = 1:9) +
  labs(x = "Number of observations per individual",
       y = "Number of individuals",
       fill = "") +
  theme_gg() +
  theme(legend.position = c(0.85, 0.85))

ehm_hist 

## Save Plot 
# pdf("./figures/Figure4B.pdf",
#     useDingbats = FALSE, width = 5, height = 5)
# print(ehm_hist )
# dev.off()
# 
# png(filename = "./figures/Figure4B.png", width = 1100, height = 1100, 
#     pointsize = 6,  res = 300)
# plot(ehm_hist )
# dev.off()


summary_stats <- dat %>%
  group_by(id) %>%
  summarise(n_sightings = n()) %>%
  ungroup() %>%
  summarise(
    total_individuals = n(),
    transients = sum(n_sightings == 1),
    prop_transient = mean(n_sightings == 1),
    mean_sightings = mean(n_sightings),
    median_sightings = median(n_sightings),
    max_sightings = max(n_sightings)
  )

print(summary_stats)

# 4. Capture history matrix (visual)

library(tidyr)

# Create complete sequence of months
all_months <- seq(from = as.Date("2014-03-01"), 
                  to = as.Date("2015-06-01"), 
                  by = "month")
all_months_char <- format(all_months, "%Y-%m")

# Get first sighting date for each ID
first_sighting_order <- dat %>%
  group_by(id) %>%
  summarise(first_date = min(as.Date(date))) %>%
  arrange(first_date) %>%
  pull(id)

# Identify transients in EHM (seen in only 1 month)
transients_ehm <- dat %>%
  mutate(year_month = format(as.Date(date), "%Y-%m")) %>%
  group_by(id) %>%
  summarise(n_months = n_distinct(year_month),
            total_sightings = n()) %>%
  filter(n_months == 1) %>%
  mutate(transient_type = ifelse(total_sightings == 1, "single_sighting", "multiple_in_month")) %>%
  dplyr::select(id, transient_type)

# Create matrix WITH sighting counts per month
capture_matrix_counts <- dat %>%
  mutate(year_month = format(as.Date(date), "%Y-%m")) %>%
  group_by(id, year_month) %>%
  summarise(n_sightings = n(), .groups = "drop") %>%
  complete(id, year_month = all_months_char, fill = list(n_sightings = 0))

# Convert to long format and add transient status
capture_long <- capture_matrix_counts %>%
  left_join(transients_ehm, by = "id") %>%
  mutate(id = factor(id, levels = rev(first_sighting_order)),
         status = case_when(
           n_sightings == 0 ~ "not_seen",
           !is.na(transient_type) & transient_type == "single_sighting" ~ "transient_single",
           !is.na(transient_type) & transient_type == "multiple_in_month" ~ "transient_multiple",
           n_sightings == 1 ~ "resident_single",
           n_sightings > 1 ~ "resident_multiple",
           TRUE ~ "resident_single"
         ))

# Plot with five categories
ehm = ggplot(capture_long, aes(x = year_month, y = id, fill = status)) +
  geom_tile(color = "grey80", linewidth = 0.3) +
  scale_fill_manual(
    values = c("not_seen" = "white", 
               "resident_single" = "steelblue",
               "resident_multiple" = "steelblue4",
               "transient_single" = "red2",
               "transient_multiple" = "brown"),
    labels = c("Not seen", 
               "Resident (1 sighting/month)",
               "Resident (2+ sightings/month)",
               "Transient (1 sighting total)",
               "Transient (multiple sightings, 1 month)"),
    breaks = c("not_seen", "resident_single", "resident_multiple", 
               "transient_single", "transient_multiple")
  ) +
  scale_y_discrete(labels = 65:1) +
  labs(x = "Month", y = "Individual ID", fill = "") +
  coord_fixed(ratio = 0.8) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 12),
    axis.title.x  = element_text(size = 12),
    legend.position = c(2.85, 1), 
    legend.text  = element_text(size = 12),
    legend.title = element_text(size = 12),# x > 1 pushes into right margin, y = 1 is top
    legend.justification = c(1, 1),      # anchors legend by its top-right corner
    legend.box.clip = "off",             # allows legend to render outside plot area
    plot.margin = margin(5, 120, 5, 5)   # adds right margin space (in pts) for the legend
  )


ehm

## Save Plot 
# pdf("./figures/Figure4A.pdf",
#      useDingbats = FALSE, width = 11, height = 8)
# print(ehm)
# dev.off()
#  
# png(filename = "./figures/Figure4A.png", width = 4500, height = 2000, 
#      pointsize = 6,  res = 300)
# plot(ehm)
# dev.off()


library(cowplot)

# combine
fig_4_png = ggdraw() +
  draw_plot(ehm) +  # takes up full canvas by default
  draw_plot(ehm_hist, 
            x = 0.55,      # left edge position (0-1)
            y = 0,      # bottom edge position (0-1)
            width = 0.29, # width of inset
            height = 0.62) +# height of inset
  draw_plot_label(label = c("A", "B"), 
                x = c(0.36, 0.55),      # match x positions of each plot
                y = c(1, 0.63),        # top of each plot
                size = 14,
                fontface = "plain")  # or "bold", "italic", "bold.italic")

fig_4_png

png(filename = "./figures/Figure4_EHM.png", width = 4500, height = 2000, 
    pointsize = 6,  res = 300)
plot(fig_4_png)
dev.off()

# combine
fig_4_pdf = ggdraw() +
  draw_plot(ehm) +  # takes up full canvas by default
  draw_plot(ehm_hist, 
            x = 0.59,      # left edge position (0-1)
            y = 0,      # bottom edge position (0-1)
            width = 0.37, # width of inset
            height = 0.54) +# height of inset
  draw_plot_label(label = c("A", "B"), 
                  x = c(0.29, 0.57),      # match x positions of each plot
                  y = c(1, 0.53),        # top of each plot
                  size = 14,
                  fontface = "plain")  # or "bold", "italic", "bold.italic")

fig_4_pdf

## Save Plot 
pdf("./figures/Figure4_EHM.pdf",
    useDingbats = FALSE, width = 11, height = 8)
print(fig_4_pdf)
dev.off()


