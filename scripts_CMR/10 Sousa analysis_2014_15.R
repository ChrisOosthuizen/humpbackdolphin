
# Chris Oosthuizen

# SOusa analysis with JS model and RPT models for 2014-15 EHM data

# 20 Feb 2026

#--------------------------------------------------------------------
# Load data
#--------------------------------------------------------------------
# Load encounter histories from Conry Msc (humpback dolphins) and inspect
# There is no column of '1's at the end
CH = read.table("./data/multimark_input.txt", header = T)
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
nz <- 300
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

set.seed(123)

# Specify time lag between each capture occasion. The model framework allows for variable input.
# My EHM is summarized per month so each time difference is 1 time step.

time_lag1 = rep(1,ncol(CH)-1) # 15 time transitions for 16 occasions.

year_start1 = c(1, ncol(CH)+1)  # I don't want to split my EHM into years. 
year_start1
# year_start
# Original file has 3 years. The dimensions of that CH is 
#> dim(datJS.3years.aug)
#[1] 695  87   and 
#> year_start_3years  is
# [1]  1 16 51 88
# EHM columns 1:15 was 2018, so 1 16 above,
# EHM columns 16:50 was 2019, so 51 above.
# 88 is one more than the ncol of the EHM.

#------------
# RUN MODEL
#------------
JS_mod =  JStype.fit.jags(CR.data.matrix = CH,
                          t_lag = time_lag1,
                          year_start = year_start1,
                          G=competingModels$numComp[modNumber],
                          bugs_model=competingModels$BUGSname[modNumber],
                          nc = 3,
                          sample = 2e4,
                          burnin = 5e3, 
                        # sample = 1e4,
                        #  burnin = 2e3, 
                          thin = 2)

saveRDS(JS_mod, file = "./scripts_CMR/BUGSmodel/JS_sousa_2014_15_65_individuals.rds")
# reload with:
JS_mod <- readRDS("./scripts_CMR/BUGSmodel/JS_sousa_2014_15_65_individuals.rds")

#-------------------------
# Model output
#-------------------------

str(JS_mod )

out = as.data.frame(JS_mod$chains_mat)

head(out)

mean(out$Nsuper)   

z <- JS_mod$chains_mat[, "Nsuper"]

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
  scale_x_continuous(expand = c(0, 0), limits = c(50, 160)) + 
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


niter <- nrow(JS_mod$chains_mat)

##################### TRACEPLOTS AND DENSITIES IN THE APPENDIX ######################################
#jpeg("Traceplot_Nsup.jpg", width = 800, height = 600, res = 100)
JS_mod$chains_mat[, grepl("Nsuper", colnames(JS_mod$chains_mat))] %>% 
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
JS_mod$chains_mat[, grepl("Nsuper", colnames(JS_mod$chains_mat))] %>% 
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


#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# RPT Model
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------

# Model fitting ------------------------------------------------------------

### THE FOLLOWING PART OF THE CODE MAKES USE OF INTENSIVE COMPUTATIONS,
### FOR WHICH IT IS SUGGESTED TO USE HIGH PERFORMANCE COMPUTING.

set.seed(123)
#-------------------------
# Run model
#-------------------------

RPT_mod = JS.RPT.fit.jags(CR.data.matrix = CH,
                          t_lag = time_lag1,
                          year_start = year_start1,
                          nc = 3,
                             sample = 2e4,
                             burnin = 5e3, 
                        #  sample = 1e4,
                        #  burnin = 2e3, 
                          thin = 2,
                          pars.to.save = c("w","p","phi","rho","Nsuper","N.y","clust","loglik_i","z","mu","delta"))


saveRDS(RPT_mod, file = "./scripts_CMR/BUGSmodel/RPT_sousa_2014_15_65_individuals.rds")
# reload with:
#RPT_mod <- readRDS("./scripts_CMR/BUGSmodel/RPT_sousa_2014_15_65_individuals.rds")

#-------------------------
# RPT Model output
#-------------------------

out_rpt = as.data.frame(RPT_mod$chains_mat)
head(out_rpt)

mean(out_rpt$Nsuper)   # 1050 was the simulated value


# Takes very long to run? 
# #library(tidyverse)
# rpt_mean_all_par = out_rpt %>% summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))
# rpt_mean_all_par = rpt_mean_all_par %>% pivot_longer(everything(), names_to = "parameter", values_to = "value")
# rpt_mean_all_par

#------------------------------
# Summarize RPT Model results
#------------------------------
est_mod = RPT_mod 

###### Estimates of N_super, N_t and N_gt
# median and mean
(Nsuper_median <- c(est_mod$chains_mat[, "Nsuper"] %>% HDInterval::hdi(), median(est_mod$chains_mat[, "Nsuper"])))
#(Nsuper_mean <- c(est_mod$chains_mat[, "Nsuper"] %>% HDInterval::hdi(), mean(est_mod$chains_mat[, "Nsuper"])))

x <- est_mod$chains_mat[, "Nsuper"]

hdi_vals <- HDInterval::hdi(x)

Nsuper_mean <- data.frame(
  mean  = mean(x),
  lower = hdi_vals[1],
  upper = hdi_vals[2]
)

Nsuper_mean

# Apparent survival of the two groups (transients (low) and residents (high))
###### Estimates of N_super, N_t and N_gt
(Phi2 <- c(est_mod$chains_mat[, "phi[2]"] %>% HDInterval::hdi(), median(est_mod$chains_mat[, "phi[2]"])))
###### Estimates of N_super, N_t and N_gt
(Phi1 <- c(est_mod$chains_mat[, "phi[1]"] %>% HDInterval::hdi(), mean(est_mod$chains_mat[, "phi[1]"])))

########### Figure 3a (Chris's own version) ########### 

ggplot(Nsuper_mean, aes(x = 2014, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) +
  scale_x_continuous(breaks = 2014) +
  theme_bw()+
  labs(x = "Year", y = "Nsuper")


########## Figure 3b ########### 

M = nrow(CH)
occ = ncol(CH)
niter <- nrow(est_mod$chains_mat)
ind_alive <- matrix(rep(NA,niter*M), ncol=M)

for(i in 1:M){
  ind_alive[,i] <- rowSums(est_mod$chains_mat[,paste0("z[",i,",",1:occ,"]")])>0  # 16 occasions
}

clust_chains <- est_mod$chains_mat[,paste0("clust[",1:M,"]")]

# Divided by group and year
Nsup_R_chain <- rowSums(ind_alive *(clust_chains==1))

Nsup_P_chain <- rowSums(ind_alive *(clust_chains==2))

Nsup_T_chain <- rowSums(ind_alive *(clust_chains==3))

lapply(list(Nsup_R_chain,Nsup_P_chain,Nsup_T_chain), 
       summary)

# Put Ngt iterations into a dataframe
rtp_groups <- as.data.frame(cbind(Nsup_R_chain, Nsup_P_chain, Nsup_T_chain))
rtp_groups

rtp_long <- rtp_groups %>%
  pivot_longer(
    cols = everything(),
    names_to = "group",
    values_to = "value"
  )

rtp_long$group <- factor(
  rtp_long$group,
  levels = c("Nsup_R_chain", "Nsup_P_chain", "Nsup_T_chain"),
  labels = c("Resident", "Part-time", "Transient")
)

########### Figure 3b Chris ########### 

ggplot(rtp_long, aes(x = group, y = value, fill = group)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = NULL, y = "Nsuper") + 
  scale_fill_manual(values = c("skyblue2", "pink2", "gold")) +
  labs(x = "Group", y = expression(paste(hat(N)[group])), fill = "") +
  theme_bw() +
  theme(legend.position = "top", text = element_text(size = 18))

library(HDInterval)
summary_df <- rtp_groups %>%
  pivot_longer(everything(),
               names_to = "group",
               values_to = "value") %>%
  group_by(group) %>%
  summarise(
    mean  = mean(value),
    lower = hdi(value)[1],
    upper = hdi(value)[2],
    .groups = "drop"
  )

summary_df 

sum(summary_df$mean)  # overall predicted N

# If you prefer quantile intervals instead of HDI
summary_quantile  <- data.frame(
  group = names(rtp_groups),
  mean  = sapply(rtp_groups, mean),
  lower = sapply(rtp_groups, quantile, probs = 0.025),
  upper = sapply(rtp_groups, quantile, probs = 0.975)
)

summary_quantile 

sum(summary_quantile$mean)  # overall predicted N



##### Posterior estimates of the mixture's weights ############
G <- max(clust_chains) #number of mixture components
#relative frequency distribution of the group labels for each individual
rel_freq_estimated_labels <- matrix(unlist(apply(clust_chains,2,function(x) table(factor(x,levels=1:G))/length(x))), ncol=G,byrow=T)
colMeans(rel_freq_estimated_labels) # Allocation with the cluster labels
colMeans(est_mod$chains_mat[,paste0("w[",1:G,"]")]) # Mixture's weights


# end


