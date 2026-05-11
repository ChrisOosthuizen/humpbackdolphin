

#--------------------------------------------------------------------
# 10.4. Models with constant survival and time-dependent entry
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Fitting a Multi-state formulation of a Jolly-Seber model with:
# time dependent p, 
# constant phi    
# time dependent entry.
#--------------------------------------------------------------------

# Code Resources:
# Main source:
# Kery and Schaub 2012 Chapter 10
# 10.3.2 The JS Model as a Multistate Model
# 10.4.2 Analysis of the JS Model as a Multistate Model

# And code adapted for JAGS
# https://www.vogelwarte.ch/assets/files/publications/BPA/BPA%20with%20JAGS.txt

# However, when trying to run the multistate Jolly-Seber model in JAGS using Kéry & Schaub BPA. 
# I get the following error after running JAGS:
# "Error in setParameters(init.values[[i]], i) : Error in node po[1,1,1,2]
# Cannot set value of non-variable node"
# This was the same error as here:
# https://groups.google.com/g/hmecology/c/S4HO-tnzep8

# I used the answer (attachment) from this question as the backbone of this code. 

# To change from constant p to time dependent p I relied on code from
# A Multistate Extension of the Jolly-Seber Model: Combining adult mark-recapture data with juvenile data
# by
# Halverson-Duncan 2014
# MSc, University of Victoria

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

#--------------------------------------------------------------------
# 10.4.2 Analysis of the JS model as a multistate model
#--------------------------------------------------------------------

# Add dummy occasion
CH.du <- cbind(rep(0, dim(CH)[1]), CH)

my.z.init <- CH.du

first.one <- apply(my.z.init[,1:ncol(CH.du)], 1, function(x) min(which(x == 1)))
last.one  <- apply(my.z.init[,1:ncol(CH.du)], 1, function(x) max(which(x == 1)))

for(i in 1:nrow(my.z.init)) {
  my.z.init[i,     first.one[i]  : last.one[i]        ] = 2
  if(first.one[i] > 1)               my.z.init[i,                1  : (first.one[i] - 1) ] = 1
  if(last.one[i]  < ncol(my.z.init)) my.z.init[i, (last.one[i] + 1) : ncol(my.z.init)    ] = 3
}

nz <- 500

CH.ms <- rbind(CH.du, matrix(0, ncol = dim(CH.du)[2], nrow = nz))

CH.ms[CH.ms==0] <- 2

my.z.init.ms <- rbind(my.z.init, matrix(0, ncol = dim(my.z.init)[2], nrow = nz))
my.z.init.ms[my.z.init.ms==0] <- 1

head(CH.ms)

#--------------------------------------------------------------------
# Specify model in BUGS language
#--------------------------------------------------------------------

sink("js-ms.jags")
cat("
model {
  
  #--------------------------------------
  # Parameters:
  # phi: survival probability
  # gamma: removal entry probability
  # p: capture probability
  #--------------------------------------
  # States (S):
  # 1 not yet entered
  # 2 alive
  # 3 dead
  # Observations (O):
  # 1 seen
  # 2 not seen
  #--------------------------------------
  
  # Priors and constraints
  for (t in 1:(n.occasions-1)){
    phi[t] <- mean.phi
    gamma[t] ~ dunif(0, 1) # Prior for entry probabilities
#   p[t] <- mean.p
  }
# You need to define p in its own loop, for n.occasions (not n.occasions-1)
  for(t in 1:n.occasions){                         
   p[t] ~ dunif(0,1)
   }
  
  mean.phi ~ dunif(0, 1)    # Prior for mean survival
#  mean.p ~ dunif(0, 1)      # Prior for mean capture
  
  # Define state-transition and observation matrices 	
  for (i in 1:M){  
     # Define probabilities of state S(t+1) given S(t)
     for (t in 1:(n.occasions-1)){
        ps[1,i,t,1] <- 1-gamma[t]
        ps[1,i,t,2] <- gamma[t]
        ps[1,i,t,3] <- 0
        ps[2,i,t,1] <- 0
        ps[2,i,t,2] <- phi[t]
        ps[2,i,t,3] <- 1-phi[t]
        ps[3,i,t,1] <- 0
        ps[3,i,t,2] <- 0
        ps[3,i,t,3] <- 1
     }
        # Define probabilities of O(t) given S(t)
        
# You need to define p in its own loop, for n.occasions (not n.occasions-1) 
      for(t in 1:n.occasions){                                  
        po[1,i,t,1] <- 0
        po[1,i,t,2] <- 1
        po[2,i,t,1] <- p[t]
        po[2,i,t,2] <- 1-p[t]
        po[3,i,t,1] <- 0
        po[3,i,t,2] <- 1
        } #t
     } #i
  
  # Likelihood 
  for (i in 1:M){
     # Define latent state at first occasion
     z[i,1] <- 1   # Make sure that all M individuals are in state 1 at t=1
     for (t in 2:n.occasions){
        # State process: draw S(t) given S(t-1)
        z[i,t] ~ dcat(ps[z[i,t-1], i, t-1,])
        # Observation process: draw O(t) given S(t)
#        y[i,t] ~ dcat(po[z[i,t], i, t-1,])
       y[i,t] ~ dcat(po[z[i,t], i, t,])
        } #t
     } #i
  
  # Calculate derived population parameters
  for (t in 1:(n.occasions-1)){
     qgamma[t] <- 1-gamma[t]
     }
  cprob[1] <- gamma[1]
  for (t in 2:(n.occasions-1)){
     cprob[t] <- gamma[t] * prod(qgamma[1:(t-1)])
     } #t
  psi <- sum(cprob[])            # Inclusion probability
  for (t in 1:(n.occasions-1)){
     b[t] <- cprob[t] / psi      # Entry probability
     } #t
  
  for (i in 1:M){
     for (t in 2:n.occasions){
        al[i,t-1] <- equals(z[i,t], 2)
        } #t
     for (t in 1:(n.occasions-1)){
        d[i,t] <- equals(z[i,t]-al[i,t],0)
        } #t   
     alive[i] <- sum(al[i,])
     } #i
  
  for (t in 1:(n.occasions-1)){
     N[t] <- sum(al[,t])        # Actual population size
     B[t] <- sum(d[,t])         # Number of entries
     } #t
  for (i in 1:M){
     w[i] <- 1-equals(alive[i],0)
     } #i
  Nsuper <- sum(w[])            # Superpopulation size
}
",fill = TRUE)
sink()

#--------------------------------------------------------------------
# Set parameters
#--------------------------------------------------------------------

# Define parameter values
n.occasions <-  ncol(CH)               # Number of capture occasions
n.occasions_augmented <-  ncol(CH.ms)               # Number of capture occasions

# Bundle data
jags.data <- list(y = CH.ms, n.occasions = dim(CH.ms)[2], M = dim(CH.ms)[1])       
jags.data  # 17 occasions

# Initial values
inits <- function(){list(mean.phi = runif(1, 0, 1), 
                         mean.p = runif(1, 0, 1),
                         p = runif(n.occasions_augmented,0,1),                                  # plus 1 occasion here over original CH, because augmentation added a occasion to the CH 
                         z = cbind(rep(NA, dim(my.z.init.ms)[1]), my.z.init.ms[,-1]))}


# Parameters monitored
parameters <- c('p', 'mean.phi', 'gamma', 'b', 'Nsuper', 'N', 'B')

# MCMC settings
ni <- 200000
nt <- 3
nb <- 50000
nc <- 3

# MCMC settings (code test - short runs)
# ni <- 1000
# nt <- 3
# nb <- 100
# nc <- 3

library(jagsUI)
library(doParallel)

#--------------------------------------------------------------------
# Run model
#--------------------------------------------------------------------

js.ms <- jags(jags.data, inits, parameters, "js-ms.jags", n.chains = nc,
              n.thin = nt, n.iter = ni, n.burnin = nb,
              parallel = T, n.cores = detectCores())

print(js.ms, digits = 3)

#saveRDS(js.ms, "./dolphin_jsms_200000iterations_2May2025.rds")


out = js.ms
#-----------------------------------------------------------
# Import saved model run
#-----------------------------------------------------------

out = readRDS("./dolphin_jsms_200000iterations.rds")
str(out)

#---------------------------------------------------------
# Figures from Kery and Schaub 2012
#---------------------------------------------------------
# Code to produce Fig. 10.4

# Plot Super population:
#par(mfrow = c(1,2), mar = c(5, 6, 2, 1), mgp=c(3.4, 1, 0), las = 1)
plot(density(out$sims.list$Nsuper), main = "", xlab = "",
     ylab = "Density", 
     frame = T, 
     lwd = 2, 
     ylim=c(0, 0.023),
     col = "blue")
mtext("Size of superpopulation", 1, line = 3)

# How many occasions in the data (get from CH above)
n.occasions = 16

# make empty objects to take data from loop:
b1.lower <- b1.upper <- numeric()

for (t in 1:n.occasions){
  b1.lower[t] <- quantile(out$sims.list$b[,t], 0.025)
  b1.upper[t] <- quantile(out$sims.list$b[,t], 0.975)
 }

time <- 1:n.occasions

plot(x = time, y = out$mean$b, 
     xlab = "", ylab = "Entry probability", 
     frame = T,
     las = 1, 
     xlim = c(0, n.occasions+1), 
     ylim = c(0, max(c(b1.upper, b1.upper))),
     pch = 16)

segments(time, b1.lower, time, b1.upper)

mtext("Occasion", 1, line = 3)

#------------------------------------------------------
# Figures from MCMCvis
#------------------------------------------------------

#https://cran.r-project.org/web/packages/MCMCvis/vignettes/MCMCvis.html

library(MCMCvis)

MCMCsummary(out, round = 2)

MCMCtrace(out, 
          params = c('Nsuper'), 
          ISB = FALSE, 
          exact = TRUE,
          pdf = FALSE)


MCMCtrace(out, 
          params = c('p[1]', 'p[2]', 'p[3]', 'p[4]', 'p[5]', 'p[6]'), 
          type = 'density', 
          ind = T,
          ISB = FALSE, 
          exact = TRUE,
          pdf = FALSE)


# note that the same prior used for all parameters
# the following prior is equivalent to dnorm(0, 0.001) in JAGS
PR <- rnorm(15000, 0, 32)

MCMCtrace(out, 
          params = c('p[1]', 'p[2]', 'p[3]', 'p[4]', 'p[5]', 'p[6]'), 
          type = 'density', 
          ind = T,
          priors = PR,
          ISB = FALSE, 
          exact = TRUE,
          pdf = FALSE)


MCMCplot(out, 
         params = "p\\[[1-9]\\]", 
         exact = F,
         ISB = F,
         ci = c(50, 90))

# This doesn't work - only plots 1 and 6 - ignores the 1 from 10 onward?
MCMCplot(out, 
         params = "p\\[[1-16]\\]", 
         exact = F,
         ISB = F,
         ci = c(50, 90))

#---------------------------------------------------------
# Figures with bayesplot
#---------------------------------------------------------
#
# http://mc-stan.org/bayesplot/reference/MCMC-intervals.html
# Plot central (quantile-based) posterior interval estimates from MCMC draws. 
# library(bayesplot)

library(ggplot2)
library(bayesplot)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 50% intervals")

# make a matrix of posterior values
posterior = as.matrix(out$sims.list$Nsuper)
colnames(posterior) <- "Nsuper"
head(posterior )
mcmc_areas(posterior,
           pars = c("Nsuper"),  # character vector of parameter names
           prob = 0.5,  # The probability mass to include in the inner interval (for mcmc_intervals()) or in the shaded region (for mcmc_areas()). The default is 0.5 (50% interval) and 1 for mcmc_areas_ridges()
           prob_outer = 1,  # The probability mass to include in the outer interval. The default is 0.9 for mcmc_intervals() (90% interval) and 1 for mcmc_areas() and for mcmc_areas_ridges().
           point_est = "median", # "median" or "mean"
           border_size = 1.2)+ # make the ridgelines fatter
  plot_title


posterior = as.matrix(out$sims.list$p)
#colnames(posterior) <- c("M", "A", "M", "J", "J", "A", "S", "O", "N", "D",
#                         "J", "F", "M" , "A", "M", "J", "J")
colnames(posterior) <- c("Occ1", "Occ2", "Occ3", "Occ4", "Occ5",
                         "Occ6", "Occ7", "Occ8", "Occ9", "Occ10",
                         "Occ11", "Occ12", "Occ13" , "Occ14", "Occ15", "Occ16", "Occ17")

head(posterior )


mcmc_intervals(posterior,
               pars =c("Occ1", "Occ2", "Occ3", "Occ4", "Occ5",
                       "Occ6", "Occ7", "Occ8", "Occ9", "Occ10",
                       "Occ11", "Occ12", "Occ13" , "Occ14", "Occ15", "Occ16", "Occ17"),
               prob = 0.8) + plot_title

mcmc_areas(posterior,
           pars =c("Occ1", "Occ2", "Occ3", "Occ4", "Occ5",
                   "Occ6", "Occ7", "Occ8", "Occ9", "Occ10",
                   "Occ11", "Occ12", "Occ13" , "Occ14", "Occ15", "Occ16", "Occ17"),
           prob = 0.8,
           area_method = "equal area") + plot_title


mcmc_areas(posterior,
           pars =c("Occ1", "Occ2", "Occ3", "Occ4", "Occ5",
                   "Occ6", "Occ7", "Occ8", "Occ9", "Occ10",
                   "Occ11", "Occ12", "Occ13" , "Occ14", "Occ15", "Occ16", "Occ17"),
           prob = 0.8,
           area_method = "scaled height") + plot_title


mcmc_areas_ridges(posterior, 
                  pars =c("Occ1", "Occ2", "Occ3", "Occ4", "Occ5",
                          "Occ6", "Occ7", "Occ8", "Occ9", "Occ10",
                          "Occ11", "Occ12", "Occ13" , "Occ14", "Occ15", "Occ16", "Occ17"),
                  border_size = 0.75) 


#--------------------------------------------------------------------
# Definitions from Kery and Schaub Chapter 10
#--------------------------------------------------------------------

# b 
# The probability that a member of Ns enters the population at occasion t is bt (t = 1, …, T) 
# and is called the entry probability (Schwarz and Arnason, 1996).

# gamma
# We denote γt (t = 1, …, T), the probability that an available individual in M enters
# the population at occasion t. 
# This corresponds to the transition probability# from state “not yet entered” to the state “alive”. 
# Importantly, γ refers to available individuals, that is, to those in M that have not yet entered.

# Chris: It seems to me that b is the entry probability you want to interpret, whereas gamma may depend on 
# M, the 'augmented data base":

# M and Ns
# After augmentation, the capture–recapture data set contains M individuals, of which Ns are genuine and M-Ns are pseudo-individuals




library(bayestestR)
library(insight)
library(see)
library(rstanarm)
library(ggplot2)

theme_set(theme_modern())

Nsuperpost = out$sims.list$Nsuper

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")

library(bayesplot)

mcmc_areas(Nsuperpost,
           #           pars = c("cyl", "drat", "am", "wt"),
           prob = 0.8) + plot_title





