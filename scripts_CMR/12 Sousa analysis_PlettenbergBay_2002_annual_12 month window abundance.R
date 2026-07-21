
# Chris Oosthuizen

# SOusa analysis with JS model model for 2002/03 EHM data

# July 2026
# Plettenberg Bay only

#--------------------------------------------------------------------
# Load data
#--------------------------------------------------------------------
# Load encounter histories from Conry Msc (humpback dolphins) and inspect
# There is no column of '1's at the end
CH = read.table("./data/2002_input.txt", header = T)
CH = data.matrix(CH)
head(CH)
colnames(CH) <- NULL
head(CH)

rowSums(CH)
table(rowSums(CH))  # 25 transients
dim(CH)    # number if individuals

#------------------------
# Augment ehm data
#------------------------
nz <- 200
CH <- rbind(CH, matrix(0, ncol=dim(CH)[2], nrow=nz))
dim(CH)

#---------------------------------------------------------------
# Caruso et al 2024 
#---------------------------------------------------------------
## Fitting JS-type models on real data ##
#---------------------------------------------------------------
# Loading functions for model fitting 
source("./scripts_CMR/JSsimfit_fun.R") 

# JS model -------------------------------------------------
# Specify Jolly Seber model

competingModels <- data.frame(shortname=paste0("mod",1),
                              BUGSname=c(rep("phi,p_t.txt",1)),
                              numComp=1)

competingModels

# Model fitting ------------------------------------------------------------

modNumber <- 1  #change this value to change the model to fit

### THE FOLLOWING PART OF THE CODE MAKES USE OF INTENSIVE COMPUTATIONS,
### FOR WHICH IT IS SUGGESTED TO USE HIGH PERFORMANCE COMPUTING.

#---------------------------------------------------------------
# Set up sliding 12-month windows
#---------------------------------------------------------------
n_months   <- ncol(CH)          # 20
window_len <- 12
n_windows  <- n_months - window_len + 1   # 9

#------------------------------------
# List to store all model outputs
#------------------------------------
JS_mod_list <- vector("list", n_windows)
names(JS_mod_list) <- paste0("months_", 1:n_windows, "_", window_len:n_months)

set.seed(123)
#------------
# RUN MODEL
#------------
for (i in 1:n_windows) {
  
  cols_i <- i:(i + window_len - 1)
  CH_sub <- CH[, cols_i]

  rowSums(CH_sub)  # there are now '0' recaptures (never seen) IN-BETWEEN the 'real' animals (not augmented, because followed by other 'real' animals)
  # These needs to be removed. Cannot have real animals (not augmented) which wasn't seen. 
  # (they got removed because we're looking only at 12 month EHM from larger original)
  
  # Keep only individuals actually captured within this 12-month window
  CH_sub <- CH_sub[rowSums(CH_sub) > 0, , drop = FALSE]   # 2 commas correct
  
  # Re-augment with all-zero rows for this window
  CH_sub <- rbind(CH_sub, matrix(0, ncol = window_len, nrow = nz))
  
  cat("Window", i, ": cols", min(cols_i), "-", max(cols_i),
      "| real individuals =", sum(rowSums(CH_sub) > 0),
      "| total M =", nrow(CH_sub), "\n")
  
  # Specify time lag between each capture occasion. The model framework allows for variable input.
  # My EHM is summarized per month so each time difference is 1 time step.
  time_lag1 = rep(1,ncol(CH_sub)-1) # 15 time transitions for 16 occasions.
  time_lag1
  
  year_start1 = c(1, ncol(CH_sub)+1)  # I don't want to split my EHM into years. 
  year_start1

  cat("Fitting window", i, ": columns", min(cols_i), "-", max(cols_i), "\n")
  
  JS_mod_list[[i]] <- JStype.fit.jags(
    CR.data.matrix = CH_sub,
    t_lag = time_lag1,
    year_start = year_start1,
    G = competingModels$numComp[modNumber],
    bugs_model = competingModels$BUGSname[modNumber],
    nc = 3,
    sample = 2e4,
    burnin = 5e3,
    thin = 2
  )
  
  # Optional: save each fit individually as you go, in case of crashes
  saveRDS(JS_mod_list[[i]],
          file = paste0("./scripts_CMR/BUGSmodel/JS_sousa_2002_Plettenberg_win", i, ".rds"))
}

# Save the full list of results
saveRDS(JS_mod_list, file = "./scripts_CMR/BUGSmodel/JS_sousa_2002_Plettenberg_allwindows.rds")



# reload with:
#JS_mod_plet <- readRDS("./scripts_CMR/BUGSmodel/JS_sousa_2014_15_Plettenberg.rds")

#-------------------------
# Model output
#-------------------------

out = as.data.frame(JS_mod_plet$chains_mat)
head(out)

mean(out$Nsuper)   

z <- JS_mod_plet$chains_mat[, "Nsuper"]

JS_hdi_vals <- HDInterval::hdi(z)

JS_Nsuper_mean <- data.frame(
  mean  = mean(z),
  lower = JS_hdi_vals[1],
  upper = JS_hdi_vals[2]
)

JS_Nsuper_mean

total_N = as.data.frame(z)
names(total_N) = "N_marked"
head(total_N)

total_N$N_all = total_N$N_marked / 0.957

JS_hdi_vals_all <- HDInterval::hdi(total_N$N_all)

JS_Nsuper_all <- data.frame(
  mean  = mean(total_N$N_all),
  lower = JS_hdi_vals_all[1],
  upper = JS_hdi_vals_all[2]
)

JS_Nsuper_all

library(ggpubr)
library(ggridges)

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


abundance = ggplot(total_N,
                   aes(x = N_all, y = 0, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
                      quantile_lines = TRUE, quantiles = 2,
                      #   rel_min_height = 0.01, 
                      scale = 1) +
  scale_fill_continuous(name = "Tail probability", trans = 'reverse')+
  xlab("Population size") +
  ylab("Probability density") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 160)) + 
  theme_gg() +
  font("xylab",size=14)+
  font("xy",size=14)+
  font("xy.text", size = 14) +
  font("legend.text",size = 14)+
  theme(legend.position=c(0.8,0.8)) 
# theme(legend.position="none")

abundance 


library(tidyverse)

mean_all_par = out %>% summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))
mean_all_par = mean_all_par %>% pivot_longer(everything(), names_to = "parameter", values_to = "value")
mean_all_par

# plot recapture
plot_p = mean_all_par %>% 
  dplyr::filter(str_starts(parameter, "p"))
plot_p

plot(1:16, plot_p$value[1:16], type = "b", col = "black")  # model output

# plot entry
plot_b = mean_all_par %>% 
  dplyr::filter(str_starts(parameter, "rho"))
plot_b
plot(1:16, plot_b$value, type = "b", col = "black", ylim = c(0,0.4))   # model output


niter <- nrow(JS_mod_plet$chains_mat)

##################### TRACEPLOTS AND DENSITIES IN THE APPENDIX ######################################
#jpeg("Traceplot_Nsup.jpg", width = 800, height = 600, res = 100)
JS_mod_plet$chains_mat[, grepl("Nsuper", colnames(JS_mod_plet$chains_mat))] %>% 
  as.data.frame %>% 
  mutate(Iter = rep(1:(niter/3), times = 3), chain = rep(c(1,2,3), each = (niter/3)) %>% 
           factor()) %>%
  ggplot(aes(x = Iter, y = ., color = chain)) + 
  geom_line(alpha = 0.5) +
  scale_color_manual(values = c("red", "orange", "blue")) +
  labs(x = "iteration", y = expression(paste(hat(N)[super]))) +
  theme_bw() +
  theme(text = element_text(size = 22), legend.position = "top")
#dev.off()

#jpeg("Density_Nsup.jpg", width = 800, height = 600, res = 100)
JS_mod_plet$chains_mat[, grepl("Nsuper", colnames(JS_mod_plet$chains_mat))] %>% 
  as.data.frame %>% 
  mutate(Iter = rep(1:(niter/3), times = 3), chain = rep(c(1,2,3), each = (niter/3)) %>% 
                             factor()) %>%
  ggplot(aes(x = ., y = after_stat(density), color = chain)) +
  geom_density(position = "identity", linewidth = 2, aes(linetype = chain)) +
  scale_color_manual(values = c("red", "orange", "blue")) +
  labs(x = expression(paste(hat(N)[super]))) +
  theme_bw() +
  theme(text = element_text(size = 22), legend.position = "top")
#dev.off()


