
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
  data.frame(n = results$plet2002$plot_df$N_all, year = "2002/03")
)

Plet$year <- factor(Plet$year, levels = c("2014/15", "2012/13", "2002/03"))



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

#--------------------------------------------------------------------
# Combine panels A and B with patchwork
#--------------------------------------------------------------------
NN = panel_A + panel_B +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'A')

NN

ggsave("./figures/Figure5_abundance.png",  NN, width = 12, height = 5.5, dpi = 300)

# Save Plot 
pdf("./figures/Figure5_abundance.pdf",
    useDingbats = FALSE, width = 11, height = 6)
print(NN)
dev.off()

#--------------------------------
# Add Kwok 2017 data to figure B
#--------------------------------
#The super-population size (N) of marked humpback dolphins was weight-averaged and
#estimated at 267 individuals (CV=16.12%; 95% CI=177-357). By adopting the mark-ID ratio from
#Algoa Bay, the total super-population size including unmarked individuals was estimated at 461
#dolphins (CV=17.51%; 95% CI=328-648).

267/0.58  # Kwok 2017: adults and calves 
267/0.957 # What we would have reported based on our proportion identifiable
177/0.957 # lower CI
357/0.957 # upper CI

# put the above values in a df:
external_est <- data.frame(
  estimate = c(267, 461, 279),
  lower    = c(177, 328, 185),
  upper    = c(357, 648, 373),
  label    = c("Adults (marked only)", "Calves and adults (incl. unmarked)", "Adults (incl. unmarked)"),
  year     = "2002/03"
)

external_est$year  <- factor(external_est$year,
                             levels = levels(Plet$year))

external_est$label <- factor(external_est$label,
                             levels =  c("Adults (marked only)",
                                         "Calves and adults (incl. unmarked)", 
                                         "Adults (incl. unmarked)"))

external_est$y_pos <- c(1.1, 1.2, 1.3)

panel_B2 <- ggplot(Plet, aes(x = n, y = year,
                         fill = 0.5 - abs(0.5 - stat(ecdf)))) +
    stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantile_lines = TRUE,
    quantiles = 2,
    rel_min_height = 0.0001,
    scale = 1,
    show.legend = FALSE   # <--- suppress ridge legend
      ) +
  # ---- external study estimates ----
geom_errorbar(
  data = external_est,
  aes(y = y_pos, 
      xmin = lower, xmax = upper, colour = label),
  orientation = "y",
  height = 0,
  linewidth = 0.8,
  inherit.aes = FALSE) +
  
  geom_point(
    data = external_est,
    aes(x = estimate, y = y_pos,
        colour = label,
        shape = label),
    size = 3,
    inherit.aes = FALSE) + 
 
  scale_shape_manual(values = c(16, 17, 18)) +   # circle, triangle
  scale_colour_manual(values = c("blue3", "red3", "orange")) +
  
  scale_fill_continuous(name = "Tail probability", trans = "reverse") +
  scale_y_discrete(limits = rev) +
  labs(
    x = expression("Superpopulation size (" * hat(N) * ")"),
    y = "Year",
    colour = "External estimate",
    shape  = "External estimate"
  ) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 700)) +   # extend for 648 upper CI
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

panel_B2 


ggsave("./supplement/Sup7_abundance.png", panel_B2 , width = 8, height = 5.5, dpi = 300)

#--------------------------------------------------------------------
# Function to produce all diagnostic/summary plots for one model
#--------------------------------------------------------------------

#The error bars represent the 95% Highest Density Interval (HDI) 
# from the posterior distribution of each parameter — so for each occasion, 
# the point is the posterior mean and the bars span the narrowest interval 
# containing 95% of the posterior probability.

plot_model_diagnostics <- function(mod, model_name, n_chains = 3) {
  
  out <- as.data.frame(mod$chains_mat)
  
  niter <- nrow(mod$chains_mat)
  n_per_chain <- niter / n_chains
  
  #--- Recapture probabilities ---
  plot_p_full <- out %>%
    dplyr::select(matches("^p\\[")) %>%
    pivot_longer(everything(), names_to = "parameter", values_to = "value") %>%
    group_by(parameter) %>%
    summarise(
      mean  = mean(value),
      lower = HDInterval::hdi(value)[1],
      upper = HDInterval::hdi(value)[2]
    ) %>%
    mutate(occasion = as.numeric(str_extract(parameter, "\\d+")))
  
  p1 <- ggplot(plot_p_full, aes(x = occasion, y = mean)) +
    geom_pointrange(aes(ymin = lower, ymax = upper)) +
    xlab("Occasion") + ylab("Recapture probability") +
    ylim(0, 1) +
    ggtitle(paste(model_name, "")) +
    theme_gg() +
    theme(text = element_text(size = 14))+
    scale_x_continuous(breaks = scales::pretty_breaks(n = nrow(plot_p_full))) 
  
  p1
  
    #--- Entry probabilities ---
  plot_b_full <- out %>%
    dplyr::select(matches("^rho\\[")) %>%
    pivot_longer(everything(), names_to = "parameter", values_to = "value") %>%
    group_by(parameter) %>%
    summarise(
      mean  = mean(value),
      lower = HDInterval::hdi(value)[1],
      upper = HDInterval::hdi(value)[2]
    ) %>%
    mutate(occasion = as.numeric(str_extract(parameter, "\\d+")))
  
  p2 <- ggplot(plot_b_full, aes(x = occasion, y = mean)) +
    geom_pointrange(aes(ymin = lower, ymax = upper)) +
    xlab("Occasion") + ylab("Entry probability") +
    ylim(0, 0.4) +
    ggtitle(paste(model_name, "")) +
    theme_gg() +
    theme(text = element_text(size = 14))+
    scale_x_continuous(breaks = scales::pretty_breaks(n = nrow(plot_p_full))) 
  
p2
  
  #--- Traceplot: Nsuper ---
  p3 <- mod$chains_mat[, grepl("Nsuper", colnames(mod$chains_mat))] %>%
    as.data.frame() %>%
    mutate(
      Iter  = rep(1:n_per_chain, times = n_chains),
      chain = rep(as.character(1:n_chains), each = n_per_chain) %>% factor()
    ) %>%
    ggplot(aes(x = Iter, y = ., color = chain)) +
    geom_line(alpha = 0.5) +
    scale_color_manual(values = c("red", "orange", "blue"), name = "Chain") + 
    labs(x = "Iteration", 
         y =  expression("Superpopulation size (" * hat(N) * ")"),
         title = paste(model_name, "")) +
    theme_gg() +
    theme(text = element_text(size = 14), legend.position = "right")
  
  #--- Density: Nsuper ---
  p4 <- mod$chains_mat[, grepl("Nsuper", colnames(mod$chains_mat))] %>%
    as.data.frame() %>%
    mutate(
      Iter  = rep(1:n_per_chain, times = n_chains),
      chain = rep(as.character(1:n_chains), each = n_per_chain) %>% factor()
    ) %>%
    ggplot(aes(x = ., y = after_stat(density), color = chain)) +
    geom_density(position = "identity", linewidth = 2, aes(linetype = chain),key_glyph = "path") +
    scale_color_manual(values = c("red", "orange", "blue"), name = "Chain") +
    scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = "Chain") +
    guides(color = guide_legend(override.aes = list(fill = NA, linewidth = 1))) + 
    labs(y = "Density",
         x = expression("Superpopulation size (" * hat(N) * ") (distinct individuals only)"),
                  title = paste(model_name, "")) +
    theme_gg() +
    theme(text = element_text(size = 14), legend.position = "right")
  
  list(recapture = p1, entry = p2, trace = p3, density = p4,
       summary_p = plot_p_full,
       summary_b = plot_b_full)
  
}

#--------------------------------------------------------------------
# Re-run with nice labels
#--------------------------------------------------------------------
model_labels <- c(
  JS_2014_15 = "Entire study area 2014/15",
  plet2014   = "Plettenberg Bay 2014/15",
  plet2012   = "Plettenberg Bay 2012/13",
  plet2002   = "Plettenberg Bay 2002/03"
)

all_plots <- mapply(plot_model_diagnostics,
                    mod        = models,
                    model_name = model_labels[names(models)],
                    SIMPLIFY   = FALSE)

# Access individual plots, e.g.:
#all_plots$JS_2014_15$recapture
#all_plots$plet2014$trace

all_plots$JS_2014_15$summary_p

all_plots$plet2014$summary_p
all_plots$plet2014$summary_b


#--------------------------------------------------------------------
# One page per plot type, all 4 models together
#--------------------------------------------------------------------

# Recapture probabilities: all 4 models
p1 = wrap_plots(lapply(names(all_plots), function(nm) all_plots[[nm]]$recapture),
           ncol = 2) +
  plot_annotation(title = "", tag_levels = "A")

# Entry probabilities: all 4 models
p2 = wrap_plots(lapply(names(all_plots), function(nm) all_plots[[nm]]$entry),
           ncol = 2) +
  plot_annotation(title = "", tag_levels = "A")

# Traceplots: all 4 models
p3 = wrap_plots(lapply(names(all_plots), function(nm) all_plots[[nm]]$trace),
           ncol = 2) +
  plot_annotation(title = "", tag_levels = "A")

# Density plots: all 4 models
p4 = wrap_plots(lapply(names(all_plots), function(nm) all_plots[[nm]]$density),
           ncol = 2) +
  plot_annotation(title = "", tag_levels = "A")



ggsave("./supplement/Sup6_recapture_probabilities.png",  p1, width = 12, height = 10, dpi = 300)
ggsave("./supplement/Sup6_entry_probabilities.png",  p2, width = 12, height = 10, dpi = 300)
ggsave("./supplement/Sup6_MCMC_chains.png",  p3, width = 12, height = 10, dpi = 300)
ggsave("./supplement/Sup6_density.png",  p4, width = 12, height = 10, dpi = 300)




#----------------------------------------------------------------------
# What is the approximate "constant" or "average" detection and entry:
#----------------------------------------------------------------------

#Calculate a grand mean across all occasions for each dataset — 
# collapsing the time-varying p and rho into a single summary value per model, 
# analogous to what a constant model would give you.(BUT NOT THE SAME!)

# go back to the raw MCMC samples, pool all occasions together, and summarise:

summarise_mean_p_b <- function(mod, model_name) {
  out <- as.data.frame(mod$chains_mat)
  
  # All posterior samples for all p[t] occasions, pooled
  p_samples <- out %>%
    dplyr::select(matches("^p\\[")) %>%
    unlist()  # collapses all occasions into one vector
  
  # All posterior samples for all rho[t] occasions, pooled
  b_samples <- out %>%
    dplyr::select(matches("^rho\\[")) %>%
    unlist()
  
  data.frame(
    model = model_name,
    mean_p     = mean(p_samples),
    lower_p    = HDInterval::hdi(p_samples)[1],
    upper_p    = HDInterval::hdi(p_samples)[2],
    mean_rho   = mean(b_samples),
    lower_rho  = HDInterval::hdi(b_samples)[1],
    upper_rho  = HDInterval::hdi(b_samples)[2]
  )
}

mean_pb_table <- do.call(rbind, mapply(summarise_mean_p_b,
                                       mod        = models,
                                       model_name = model_labels[names(models)],
                                       SIMPLIFY   = FALSE))

mean_pb_table


