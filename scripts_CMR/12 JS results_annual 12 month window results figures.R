# Chris Oosthuizen

# Sousa analysis with JS model 
# Results summary
# 20 July 2026

#---------------
# Setup
#---------------
library(tidyverse)
library(ggpubr)
library(ggridges)
library(patchwork)
library(HDInterval)


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

#--------------------------------------------------------------------
# Load data 
#--------------------------------------------------------------------
models <- list(
  JS_2014_15   = readRDS("./scripts_CMR/BUGSmodel/JS_sousa_2014_15_65_individuals.rds"),
  plet2014     = readRDS("./scripts_CMR/BUGSmodel/JS_sousa_2014_15_Plettenberg.rds"),
  plet2012     = readRDS("./scripts_CMR/BUGSmodel/JS_sousa_2012_Plettenberg.rds"),
  plet2002     = readRDS("./scripts_CMR/BUGSmodel/JS_sousa_2002_Plettenberg.rds")
)

# Marking rates for each model - assume Jobson (2006) had same distinctive proportion as later data.
mark_rates <- c(
  JS_2014_15 = 0.957,
  plet2014   = 0.957,
  plet2012   = 0.957,
  plet2002   = 0.957
)

# Marking rates for each model - assume 77 % as in Jobson 2006
mark_rates <- c(
  JS_2014_15 = 0.957,
  plet2014   = 0.957,
  plet2012   = 0.957,
  plet2002   = 0.77
)

ci_interval = 0.90
#--------------------------------------------------------------------
# Function to summarise abundance for one model
#--------------------------------------------------------------------
summarise_abundance <- function(mod, mark_rate) {
  
  chains <- mod$chains_mat[, "Nsuper"]
  
  # Marked individuals
  hdi_marked <- HDInterval::hdi(chains, credMass = ci_interval)
  summary_marked <- data.frame(
    mean  = mean(chains),
    lower = hdi_marked[1],
    upper = hdi_marked[2]
  )
  
  # All individuals
  N_all <- chains / mark_rate
  hdi_all <- HDInterval::hdi(N_all, credMass = ci_interval)
  summary_all <- data.frame(
    mean  = mean(N_all),
    lower = hdi_all[1],
    upper = hdi_all[2]
  )
  
  # Data frame for plotting
  plot_df <- data.frame(N_marked = as.numeric(chains))
  plot_df$N_all <- plot_df$N_marked / mark_rate
  
  list(
    summary_marked = summary_marked,
    summary_all    = summary_all,
    plot_df        = plot_df
  )
}

#--------------------------------------------------------------------
# Apply to all models
#--------------------------------------------------------------------
results <- mapply(summarise_abundance,
                  mod       = models,
                  mark_rate = mark_rates,
                  SIMPLIFY  = FALSE)

# Access results by model name, e.g.:
#results$JS_2014_15$summary_all
#results$plet2014$summary_marked

#--------------------------------------------------------------------
# Combined summary table across all models
#--------------------------------------------------------------------
summary_table <- do.call(rbind, lapply(names(results), function(nm) {
  cbind(model = nm, results[[nm]]$summary_all)
}))

summary_table

#--------------------------------------------------------------------
# Plot all models 
#--------------------------------------------------------------------
#--------------------------------------------------------------------
# Panel A: JS_2014_15 on its own
#--------------------------------------------------------------------
panel_A <- ggplot(results$JS_2014_15$plot_df,
                  aes(x = N_all, y = 0, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
                      quantile_lines = F, quantiles = 2,
                      rel_min_height = 0.0001,
                      scale = 1) +
  scale_fill_continuous(name = "Tail probability", trans = 'reverse') +
  labs(
    x = expression("Superpopulation size (" * hat(N) * ")"),
    y = "Probability density") +
  scale_x_continuous(expand = c(0, 0), limits = c(50, 160)) +
  theme_gg() +
  font("xylab", size = 12) +
  font("xy", size = 12) +
  font("xy.text", size = 12) +
  font("legend.text", size = 12) +
  theme(legend.position = c(0.8, 0.8))

panel_A

#--------------------------------------------------------------------
# Load annual (sliding 12-month window) abundance estimates 
# for the 3 Plettenberg studies
#--------------------------------------------------------------------
plet2014_windows <- readRDS("./scripts_CMR/BUGSmodel/JS_sousa_2014_15_Plettenberg_allwindows.rds")
plet2012_windows <- readRDS("./scripts_CMR/BUGSmodel/JS_sousa_2012_Plettenberg_allwindows.rds")
plet2002_windows <- readRDS("./scripts_CMR/BUGSmodel/JS_sousa_2002_Plettenberg_allwindows.rds")

#--------------------------------------------------------------------
# Function to extract Nsuper (-> N_all) from every window within one study
# We need chains_mat[, "Nsuper"]
#--------------------------------------------------------------------
extract_windows <- function(window_list, study_label, mark_rate) {
  do.call(rbind, lapply(seq_along(window_list), function(i) {
    z <- window_list[[i]]$chains_mat[, "Nsuper"]
    data.frame(
      n      = as.numeric(z) / mark_rate,
      study  = study_label,
      window = i
    )
  }))
}

Plet <- rbind(
  extract_windows(plet2014_windows, "2014/15", mark_rates["plet2014"]),
  extract_windows(plet2012_windows, "2012/13", mark_rates["plet2012"]),
  extract_windows(plet2002_windows, "2002/03", mark_rates["plet2002"])
)

Plet$year <- factor(Plet$study, levels = c("2014/15", "2012/13", "2002/03"))

# Group identifies each individual window's ridge so overlapping windows 
# within the same study are drawn as separate (overlaid) ridges, 
# rather than being merged into one blended density.
Plet$group_id <- interaction(Plet$year, Plet$window, drop = TRUE)

head(Plet)
table(Plet$year, Plet$window)   # check number of windows (and posterior samples) per study

#--------------------------------------------------------------------
# Panel B: plet2014, plet2012, plet2002 - windows overlaid within each year
#--------------------------------------------------------------------
panel_B <- ggplot(Plet, aes(x = n, y = year, group = group_id, fill = year)) +
  stat_density_ridges(
    geom = "density_ridges",
    quantile_lines = F,
    quantiles = 2,
    rel_min_height = 0.0001,
    scale = 0.9,
    alpha = 0.2,
    colour = NA,
    linewidth = 0.3
  ) +
  scale_fill_manual(values = c("2014/15" = "#0072B2",
                               "2012/13" = "#D55E00",
                               "2002/03" = "#009E73"),
                    guide = "none") +
  scale_y_discrete(limits = rev) +
  labs(
    x = expression("Plettenberg Bay superpopulation size (" * hat(N) * ")"),
    y = "Year"
  ) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 260)) +   # extend for 648 upper CI
  theme_gg() +
  font("xylab", size = 12) +
  font("xy", size = 12) +
  font("xy.text", size = 12) +
  font("legend.text", size = 12)

panel_B

#--------------------------------------------------------------------
# Combine panels A and B with patchwork
#--------------------------------------------------------------------
NN = panel_A + panel_B +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'A')

NN

ggsave("./figures/Figure5_abundance_annual_12month windows.png",  NN, width = 12, height = 5.5, dpi = 300)

# Save Plot 
pdf("./figures/Figure5_abundance_annual_12 month windows.pdf",
    useDingbats = FALSE, width = 11, height = 6)
print(NN)
dev.off()


#------------------------------------------------------
# Study-level comparison (pooled across windows) 
#------------------------------------------------------

set.seed(123)
n_boot <- 1e5  # large number of Monte Carlo draws

# Pooled posterior samples per study (all windows combined)
pool_2002 <- Plet$n[Plet$year == "2002/03"]
pool_2012 <- Plet$n[Plet$year == "2012/13"]
pool_2014 <- Plet$n[Plet$year == "2014/15"]

length(pool_2002) / 9
length(pool_2012) / 6
length(pool_2014) / 5 

#--------------------------------------------------------------------
# Function: posterior distribution of percent change, early -> late
#--------------------------------------------------------------------
pct_change_posterior <- function(early, late, n_boot = 1e5) {
  x <- sample(early, n_boot, replace = TRUE)
  y <- sample(late,  n_boot, replace = TRUE)
  100 * (y - x) / x
}

# Compute for each comparison of interest
change_2002_2012 <- pct_change_posterior(pool_2002, pool_2012)
change_2002_2014 <- pct_change_posterior(pool_2002, pool_2014)
change_2012_2014 <- pct_change_posterior(pool_2012, pool_2014)

# 90% credible interval (5th-95th percentile), matching your original approach
#quantile(change_2002_2012, c(0.05, 0.95))
#quantile(change_2002_2014, c(0.05, 0.95))
#quantile(change_2012_2014, c(0.05, 0.95))

#Since pct_change = 100 * (late - early) / early:
# Negative → later period is smaller → a decrease
# Positive → later period is larger → an increase

mean(change_2002_2014)
median(change_2002_2014)
HDInterval::hdi(change_2002_2014, credMass = ci_interval)

HDInterval::hdi(change_2002_2012, credMass = ci_interval)
HDInterval::hdi(change_2012_2014, credMass = ci_interval)

#--------------------------------------------------------------------
# Function: compute density curve + flag HDI region for one comparison
#--------------------------------------------------------------------

hdi_table <- data.frame(
  comparison = c("2002/03 to 2012/13",
                 "2002/03 to 2014/15",
                 "2012/13 to 2014/15"),
  lower = c(
    HDInterval::hdi(change_2002_2012, credMass = ci_interval)[1],
    HDInterval::hdi(change_2002_2014, credMass = ci_interval)[1],
    HDInterval::hdi(change_2012_2014, credMass = ci_interval)[1]
  ),
  upper = c(
    HDInterval::hdi(change_2002_2012, credMass = ci_interval)[2],
    HDInterval::hdi(change_2002_2014, credMass = ci_interval)[2],
    HDInterval::hdi(change_2012_2014, credMass = ci_interval)[2]
  )
)

hdi_table

build_density_with_hdi <- function(values, comparison_label, ci = ci_interval) {
  d <- density(values, n = 512)
  hdi_vals <- HDInterval::hdi(values, credMass = ci_interval)
  
  data.frame(
    x = d$x,
    y = d$y,
    comparison = comparison_label,
    in_hdi = d$x >= hdi_vals[1] & d$x <= hdi_vals[2]
  )
}

# Build for each comparison (using the same pct_change vectors from before)
density_all <- rbind(
  build_density_with_hdi(change_2002_2012, "2002/03 -> 2012/13"),
  build_density_with_hdi(change_2012_2014, "2012/13 -> 2014/15"),
  build_density_with_hdi(change_2002_2014, "2002/03 -> 2014/15")
)

density_all$comparison <- factor(density_all$comparison,
                                 levels = c("2002/03 -> 2012/13",
                                            "2012/13 -> 2014/15",
                                            "2002/03 -> 2014/15"))

density_all$comparison <- factor(density_all$comparison,
                                 levels = c("2002/03 -> 2012/13",
                                            "2002/03 -> 2014/15",
                                            "2012/13 -> 2014/15"),
                                 labels = c("2002/03 to 2012/13",
                                            "2002/03 to 2014/15",
                                            "2012/13 to 2014/15"))

#--------------------------------------------------------------------
# Define 3 colours, one per comparison
#--------------------------------------------------------------------
comparison_colours <- c(
  "2002/03 to 2012/13" = "#492050",
  "2002/03 to 2014/15" = "#C490CF",
  "2012/13 to 2014/15" = "#72B173")

#--------------------------------------------------------------------
# Plot: full density (light) + HDI region shaded (darker), per comparison colour
#--------------------------------------------------------------------
panel_C <- ggplot(density_all, aes(x = x, y = y)) +
  # Full density outline, light fill
  geom_area(aes(fill = comparison), alpha = 0.15, colour = "black", linewidth = 0.3) +
  # HDI region, same colour but more opaque
  geom_area(data = subset(density_all, in_hdi),
            aes(fill = comparison), alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey30") +
  facet_wrap(~ comparison, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = comparison_colours, guide = "none") +
  labs(
    x = expression("Change (%) in Plettenberg Bay superpopulation size (" * hat(N) * ")"),
    y = "Probability density"
  ) +
  theme_gg() +
  font("xylab", size = 12) +
  font("xy", size = 12) +
  font("xy.text", size = 12) + 
  scale_x_continuous(lim = c(-100, 150)) + 
  theme(strip.text = element_text(size = 12))

panel_C


#--------------------------------------------------------------------
# Function: summarise pooled posterior per study (mirrors single-site code)
#--------------------------------------------------------------------
summarise_pooled <- function(pooled_N) {
  hdi_vals <- HDInterval::hdi(pooled_N, credMass = ci_interval)
  data.frame(
    mean  = mean(pooled_N),
    median = median(pooled_N),
    lower = hdi_vals[1],
    upper = hdi_vals[2]
  )
}

summary_2002 <- summarise_pooled(pool_2002)
summary_2012 <- summarise_pooled(pool_2012)
summary_2014 <- summarise_pooled(pool_2014)

summary_table <- rbind(
  cbind(study = "2002/03", summary_2002),
  cbind(study = "2012/13", summary_2012),
  cbind(study = "2014/15", summary_2014)
)

summary_table

# Compare precision: full-period fit vs each 12-month window
# (HDI width relative to the mean, as a simple measure of estimation precision)
window_summary <- Plet %>%
  group_by(year, window) %>%
  summarise(
    mean  = mean(n),
    median = median(n),
    lower = HDInterval::hdi(n, credMass = ci_interval)[1],
    upper = HDInterval::hdi(n, credMass = ci_interval)[2],
    hdi_width = upper - lower,
    hdi_width_rel = hdi_width / mean,   # relative precision
    .groups = "drop"
  ) %>%
  arrange(year, window)

window_summary


#--------------------------------------------------------------------
# Combine panels A and B and C with patchwork
#--------------------------------------------------------------------
NN = panel_A + panel_B + panel_C + 
  plot_layout(ncol = 3) +
  plot_annotation(tag_levels = 'A')

NN

ggsave("./figures/Figure5_A_B_C.png",  NN, width = 15, height = 6.5, dpi = 300)

# Save Plot 
pdf("./figures/Figure5_A_B_C.pdf",
    useDingbats = FALSE, width = 11, height = 6)
print(NN)
dev.off()
