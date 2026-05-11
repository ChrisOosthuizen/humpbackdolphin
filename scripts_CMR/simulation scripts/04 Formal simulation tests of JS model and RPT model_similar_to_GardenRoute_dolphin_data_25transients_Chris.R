
# Chris Oosthuizen

# Simulate data and test JS model and RPT models

# 20 Feb 2026

# 1. Simulate 62 JS EHM - this is 85 individuals in the total population

# This gives approximately 38 % transients in the EHM - the same as observed for Indian Ocean humpback dolphins
# Use low capture probabilities - same as observed for Indian Ocean humpback dolphins

# 2. Augment EHM for MCMC

# Run with long MCMC chains
#nc = 3,
#sample = 2e4,
#burnin = 5e3, 


#------------------------------------------------------------------------------------
# Simulate capture histories
# Code from BPA book (Kery and Schaub)
# Model: constant phi, time-dependent p, time-dependent entry
#------------------------------------------------------------------------------------

# Define parameter values

n.occasions <- 16           # Number of capture occasions
N <- 85                     # Superpopulation size
phi <- rep(0.95, n.occasions-1)           # Survival probabilities (constant)
b <- c(0.2, 0.1, 0.2, 0.05, 0.05, 0.05, 0.1, 0.05, 0, 0.05, 0, 0.05,0.1, 0, 0, 0)   # Entry probabilities
length(b)
sum(b) # must sum to 1.

#p <- c(0.3, 0.5, 0.9, 0.2, 0.5, 0.9, 0.1, 0.8, 0.5, 0.2, 0.9, 0.9, 0.2, 0.1, 0.9, 0.8)  # Time-varying capture probabilities
p <- c(0.2, 0.15, 0.2, 0.2, 0.1, 0.15, 0.1, 0.2, 0.2, 0.15, 0.3, 0.2, 0.2, 0.2, 0.3, 0.2)  # Time-varying capture probabilities
length(p)

PHI <- matrix(rep(phi, N), ncol=n.occasions-1, nrow=N, byrow=TRUE)
P <- matrix(rep(p, N), ncol=n.occasions, nrow=N, byrow=TRUE)

set.seed(231)

simul.js <- function(PHI, P, b, N){
  B <- rmultinom(1, N, b)                # Generate no. of entering ind. per occasion
  n.occasions <- dim(PHI)[2] + 1
  CH.sur <- CH.p <- matrix(0, ncol=n.occasions, nrow=N)
  # Define a vector with the occasion of entering the population
  ent.occ <- numeric()
  for (t in 1:n.occasions){
    ent.occ <- c(ent.occ, rep(t, B[t]))
  }
  # Simulating survival
  for (i in 1:N){
    CH.sur[i, ent.occ[i]] <- 1           # Write 1 when ind. enters the pop.
    if (ent.occ[i] == n.occasions) next
    for (t in (ent.occ[i]+1):n.occasions){
      # Bernoulli trial: has individual survived occasion?
      sur <- rbinom(1, 1, PHI[i,t-1])
      ifelse (sur==1, CH.sur[i,t] <- 1, break)
    } #t
  } #i
  # Simulating capture
  for (i in 1:N){
    CH.p[i,] <- rbinom(n.occasions, 1, P[i,])  # P[i,] now varies by column (time)
  }
  # Full capture-recapture matrix
  CH <- CH.sur * CH.p
  
  # Remove individuals never captured
  cap.sum <- rowSums(CH)
  never <- which(cap.sum == 0)
  CH <- CH[-never,]
  Nt <- colSums(CH.sur)                  # Actual population size
  return(list(CH=CH, B=B, N=Nt))
}

# Execute simulation function
sim <- simul.js(PHI, P, b, N)
CH <- sim$CH

CH
dim(CH)

row_sums <- rowSums(CH)
row_sums
table(row_sums)
dim(CH)

# Actual Sousa dolphin EHM
# 1  2  3  4  5  6  7 
# 25 17  8  6  5  3  1 

# There are 24 'transient' animals to this EHM - the dolphins have 25/65
# CH is 62 observed (we have 65 observed) - population size is not 65, but 85.

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

source("JSsimfit_fun.R") 

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

time_lag1 = rep(1,15) # 15 time transitions for 16 occasions.

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
              #     sample = 1e4,
              #      burnin = 2e3, 
                       thin = 2)

saveRDS(JS_mod, file = "./BUGSmodel/results_JS_mod_simulation_65dolphins.rds")
# reload with:
#JS_mod <- readRDS("./BUGSmodel/results_JS_mod_simulation_65dolphins.rds")

#-------------------------
# Model output
#-------------------------

JS_mod 

out = as.data.frame(JS_mod$chains_mat)
head(out)

mean(out$Nsuper)   # 85 was the simulated value

hist(out$Nsuper)   # 85 was the simulated value

library(tidyverse)

mean_all_par = out %>% summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))
mean_all_par = mean_all_par %>% pivot_longer(everything(), names_to = "parameter", values_to = "value")
mean_all_par

# plot recapture
plot_p = mean_all_par %>% 
           dplyr::filter(str_starts(parameter, "p"))
plot_p

plot(1:16, plot_p$value[1:16], type = "b", col = "black")  # model output
points(1:16, p, col = "red")   # simulated data
lines(1:16, p, col = "red")

# plot entry
plot_b = mean_all_par %>% 
         dplyr::filter(str_starts(parameter, "rho"))
plot_b
plot(1:16, plot_b$value, type = "b", col = "black", ylim = c(0,0.4))   # model output
points(1:16, b, col = "red")
lines(1:16, b, col = "red")  # simulated data




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
              #   burnin = 2e3, 
                  thin = 2,
                  pars.to.save = c("w","p","phi","rho","Nsuper","N.y","clust","loglik_i","z","mu","delta"))


saveRDS(RPT_mod, file = "./BUGSmodel/results_RPT_mod_simulation_65dolphins.rds")
# reload with:
#JS_mod <- readRDS("./BUGSmodel/results_RPT_mod_simulation_65dolphins.rds")

#-------------------------
# RPT Model output
#-------------------------

out_rpt = as.data.frame(RPT_mod$chains_mat)
head(out_rpt)

mean(out_rpt$Nsuper)   # 85 was the simulated value


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