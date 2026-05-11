
# Chris Oosthuizen

# Sousa analysis with JS model 
# Results summary
# 20 Feb 2026

#---------------
# Setup
#---------------
library(tidyverse)
library(ggpubr)
library(ggridges)
library(patchwork)


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

# Marking rates for each model (adjust as needed)
mark_rates_const <- c(
  JS_2014_15 = 0.957,
  plet2014   = 0.957,
  plet2012   = 0.957,
  plet2002   = 0.957
)

mark_rates <- c(
  JS_2014_15 = 0.957,
  plet2014   = 0.957,
  plet2012   = 0.957,
  plet2002   = 0.77
)


#--------------------------------------------------------------------
# Function to summarise abundance for one model
#--------------------------------------------------------------------
summarise_abundance <- function(mod, mark_rate) {
  
  chains <- mod$chains_mat[, "Nsuper"]
  
  # Marked individuals
  hdi_marked <- HDInterval::hdi(chains)
  summary_marked <- data.frame(
    mean  = mean(chains),
    lower = hdi_marked[1],
    upper = hdi_marked[2]
  )
  
  # All individuals
  N_all <- chains / mark_rate
  hdi_all <- HDInterval::hdi(N_all)
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

results_const <- mapply(summarise_abundance,
                  mod       = models,
                  mark_rate = mark_rates_const,
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

# Constant high marking rates
summary_table_const <- do.call(rbind, lapply(names(results_const), function(nm) {
  cbind(model = nm, results_const[[nm]]$summary_all)
}))

summary_table_const


#--------------------------------------------------------------------
# Plot all models 
#--------------------------------------------------------------------
#--------------------------------------------------------------------
# Panel A: JS_2014_15 on its own
#--------------------------------------------------------------------
panel_A <- ggplot(results$JS_2014_15$plot_df,
                  aes(x = N_all, y = 0, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
                      quantile_lines = TRUE, quantiles = 2,
                      rel_min_height = 0.0001,
                      scale = 1) +
  scale_fill_continuous(name = "Tail probability", trans = 'reverse') +
  labs(
    x = expression("Superpopulation size (" * hat(N) * ")"),
    y = "Probability density") +
  scale_x_continuous(expand = c(0, 0), limits = c(50, 160)) +
  theme_gg() +
  font("xylab", size = 14) +
  font("xy", size = 14) +
  font("xy.text", size = 14) +
  font("legend.text", size = 14) +
  theme(legend.position = c(0.8, 0.8))

#--------------------------------------------------------------------
# Panel B: plet2014, plet2012, plet2002 stacked by year
#--------------------------------------------------------------------

# Build combined Plet dataframe with a year label
Plet <- do.call(rbind, lapply(
  list(plet2014 = "2014-15", plet2012 = "2012", plet2002 = "2002"),
  function(yr) NULL  # placeholder — see below
))

# Better approach: directly from results list
Plet <- rbind(
  data.frame(n = results$plet2014$plot_df$N_all, year = "2014/15"),
  data.frame(n = results$plet2012$plot_df$N_all, year = "2012/13"),
  data.frame(n = results$plet2002$plot_df$N_all, year = "2002/03(0.77)"),
  data.frame(n = results_const$plet2002$plot_df$N_all, year = "2002/03(0.957)")
)

Plet$year <- factor(Plet$year, levels = c("2014/15", "2012/13", "2002/03(0.77)", "2002/03(0.957)"))

names(Plet)

panel_B <- ggplot(Plet, aes(x = n, y = year,
                            fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantile_lines = TRUE,
    quantiles = 2,
    rel_min_height = 0.0001,
    scale = 0.9,
    show.legend = FALSE   # <--- suppress ridge legend
  ) +
  scale_shape_manual(values = c(16, 17)) +   # circle, triangle
  scale_colour_manual(values = c("blue3", "red3")) +
  
  scale_fill_continuous(name = "Tail probability", trans = "reverse") +
  scale_y_discrete(limits = rev) +
  labs(
    x = expression("Superpopulation size (" * hat(N) * ")"),
    y = "Year",
    colour = "External estimate",
    shape  = "External estimate"
  ) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 260)) +   # extend for 648 upper CI
  theme_gg() +
  font("xylab", size = 14) +
  font("xy", size = 14) +
  font("xy.text", size = 14) +
  font("legend.text", size = 14) + 
  theme(
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = "white", colour = "white"),
    legend.title = element_blank()
  )

panel_B

ggsave("./supplement/Sup7_abundance_2002 with the same distinctive rate.png",  panel_B, width = 8, height = 5.5, 
       dpi = 300)


