

# Fitting a Multi-state formulation of a Jolly-Seber model with:

# constant p, 
# constant phi    
# time dependent entry.

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

# This is the code from the answer (attachment) from this question:


#--------------------------------------------------------------------
# 10.4. Models with constant survival and time-dependent entry
#--------------------------------------------------------------------

# Define parameter values
n.occasions <- 7                         # Number of capture occasions
N <- 400                                 # Superpopulation size
phi <- rep(0.7, n.occasions-1)           # Survival probabilities
b <- c(0.34, rep(0.11, n.occasions-1))   # Entry probabilities
p <- rep(0.5, n.occasions)               # Capture probabilities

PHI <- matrix(rep(phi, (n.occasions-1)*N), ncol = n.occasions-1, nrow = N, byrow = T)
P <- matrix(rep(p, n.occasions*N), ncol = n.occasions, nrow = N, byrow = T)

# Function to simulate capture-recapture data under the JS model
simul.js <- function(PHI, P, b, N){
   B <- rmultinom(1, N, b) # Generate no. of entering ind. per occasion
   n.occasions <- dim(PHI)[2] + 1
   CH.sur <- CH.p <- matrix(0, ncol = n.occasions, nrow = N)
   # Define a vector with the occasion of entering the population
   ent.occ <- numeric()
   for (t in 1:n.occasions){
      ent.occ <- c(ent.occ, rep(t, B[t]))
      }
   # Simulating survival
   for (i in 1:N){
      CH.sur[i, ent.occ[i]] <- 1   # Write 1 when ind. enters the pop.
      if (ent.occ[i] == n.occasions) next
      for (t in (ent.occ[i]+1):n.occasions){
         # Bernoulli trial: has individual survived occasion?
         sur <- rbinom(1, 1, PHI[i,t-1])
         ifelse (sur==1, CH.sur[i,t] <- 1, break)
         } #t
      } #i
   # Simulating capture
   for (i in 1:N){
      CH.p[i,] <- rbinom(n.occasions, 1, P[i,])
      } #i
   # Full capture-recapture matrix
   CH <- CH.sur * CH.p

   # Remove individuals never captured
   cap.sum <- rowSums(CH)
   never <- which(cap.sum == 0)
   CH <- CH[-never,]
   Nt <- colSums(CH.sur)    # Actual population size
   return(list(CH=CH, B=B, N=Nt))
   }

# Execute simulation function
sim <- simul.js(PHI, P, b, N)
CH <- sim$CH


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
    p[t] <- mean.p
  }
  
  mean.phi ~ dunif(0, 1)    # Prior for mean survival
  mean.p ~ dunif(0, 1)      # Prior for mean capture
  
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
        
        # Define probabilities of O(t) given S(t)
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
        y[i,t] ~ dcat(po[z[i,t], i, t-1,])
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

# Bundle data
jags.data <- list(y = CH.ms, n.occasions = dim(CH.ms)[2], M = dim(CH.ms)[1])

# Initial values
inits <- function(){list(mean.phi = runif(1, 0, 1), 
  mean.p = runif(1, 0, 1),
  z = cbind(rep(NA, dim(my.z.init.ms)[1]), my.z.init.ms[,-1]))}

# Parameters monitored
parameters <- c('mean.p', 'mean.phi', 'gamma', 'b', 'Nsuper', 'N', 'B')

# MCMC settings
ni <- 20000
nt <- 3
nb <- 5000
nc <- 3

# MCMC settings - test short runs
# ni <- 20
# nt <- 3
# nb <- 5
# nc <- 3

library(jagsUI)

#--------------------------------------------------------------------
# Run model
#--------------------------------------------------------------------

js.ms <- jags(jags.data, inits, parameters, "js-ms.jags", n.chains = nc,
  n.thin = nt, n.iter = ni, n.burnin = nb)

print(js.ms, digits = 3)

