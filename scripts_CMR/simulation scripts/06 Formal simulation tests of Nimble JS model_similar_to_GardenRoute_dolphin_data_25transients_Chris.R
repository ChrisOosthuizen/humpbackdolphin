
# Get convergence failures

# Chris Oosthuizen

# Simulate data and test JS model and RPT models

# 20 Feb 2026

# 1. Simulate 32 JS EHM - this is 55 individuals in the total population

# This gives approximately 53 % transients in the EHM - almost the same as observed for Indian Ocean humpback dolphins
# at Plett
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
N <- 50                     # Superpopulation size
phi <- rep(0.95, n.occasions-1)           # Survival probabilities (constant)
b <- c(0.2, 0.1, 0.2, 0.05, 0.05, 0.05, 0.1, 0.05, 0, 0.05, 0, 0.05,0.1, 0, 0, 0)   # Entry probabilities
length(b)
sum(b) # must sum to 1.

#p <- c(0.3, 0.5, 0.9, 0.2, 0.5, 0.9, 0.1, 0.8, 0.5, 0.2, 0.9, 0.9, 0.2, 0.1, 0.9, 0.8)  # Time-varying capture probabilities
p <- c(0.1, 0.15, 0.1, 0.05, 0.1, 0.1, 0.1, 0.05, 0.2, 0.05, 0.3, 0.05, 0.2, 0.05, 0.05, 0.1)  # Time-varying capture probabilities
length(p)

PHI <- matrix(rep(phi, N), ncol=n.occasions-1, nrow=N, byrow=TRUE)
P <- matrix(rep(p, N), ncol=n.occasions, nrow=N, byrow=TRUE)

set.seed(123456)

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




#----------------------------------------------------------------------
# 10.3.2. The JS model as a multistate model
#----------------------------------------------------------------------

# Write NIMBLE model file
js2Code <- nimbleCode({
  
  #--------------------------------------
  # Parameters:
  # phi: survival probability
  # gamma: removal entry probability
  # # p[t]: time-dependent capture probability   ### CHANGED
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
    gamma[t] ~ dunif(0, 1)                 # Prior for entry probabilities
    p[t] ~ dunif(0, 1)                     ### CHANGED: was p[t] <- mean.p
  }
  
  mean.phi ~ dunif(0, 1)                   # Prior for mean survival
  #  mean.p ~ dunif(0, 1)                ## CHANGED: deleted mean.p prior     
  
  #-------------------------------------------------
  # State-transition and observation matrices
  #-------------------------------------------------
  
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
      
      #-----------------------------------
      # Observation process
      #-----------------------------------
      # Define probabilities of O(t) given S(t)
      po[1,i,t,1] <- 0
      po[1,i,t,2] <- 1
      po[2,i,t,1] <- p[t]               ###NOT CHANGED: SAME FORMULA, but now stochastic
      po[2,i,t,2] <- 1-p[t] 
      po[3,i,t,1] <- 0
      po[3,i,t,2] <- 1
    } #t
  } #i
  
  #------------------
  # Likelihood 
  #------------------
  for (i in 1:M){
    # Define latent state at first occasion
    z[i,1] <- 1   # Make sure that all M individuals are in state 1 at t=1
    for (t in 2:n.occasions){
      # State process: draw S(t) given S(t-1)
      z[i,t] ~ dcat(ps[z[i,t-1], i, t-1, 1:3])
      # Observation process: draw O(t) given S(t)
      y[i,t] ~ dcat(po[z[i,t], i, t-1, 1:2])
    } #t
  } #i
  
  #-------------------------------------------------
  # Derived parameters (unchanged)
  #-------------------------------------------------
  # Calculate derived population parameters
  qgamma[1:(n.occasions-1)] <- 1-gamma[1:(n.occasions-1)]
  cprob[1] <- gamma[1]
  for (t in 2:(n.occasions-1)){
    cprob[t] <- gamma[t] * prod(qgamma[1:(t-1)])
  }
  psi <- sum(cprob[1:(n.occasions-1)])     # Inclusion probability
  b[1:(n.occasions-1)] <- cprob[1:(n.occasions-1)] / psi  # Entry probability
  
  for (i in 1:M){
    for (t in 2:n.occasions){
      al[i,t-1] <- equals(z[i,t], 2)
    } #t
    for (t in 1:(n.occasions-1)){
      d[i,t] <- equals(z[i,t]-al[i,t], 0)
    } #t   
    alive[i] <- sum(al[i,1:(n.occasions-1)])
  } #i
  
  for (t in 1:(n.occasions-1)){
    N[t] <- sum(al[1:M,t])                 # Actual population size
    B[t] <- sum(d[1:M,t])                  # Number of entries
  }
  for (i in 1:M){
    w[i] <- 1-equals(alive[i], 0)
  }
  Nsuper <- sum(w[1:M])                    # Superpopulation size
})



#----------------------------------------------------------------------
# 10.4.2 Analysis of the JS model as a multistate model
#----------------------------------------------------------------------


# Add dummy occasion
CH.du <- cbind(rep(0, dim(CH)[1]), CH)

# Augment data
nz <- 200
CH.ms <- rbind(CH.du, matrix(0, ncol=dim(CH.du)[2], nrow=nz))

# Recode CH matrix: a 0 is not allowed!
CH.ms[CH.ms==0] <- 2                     # Not seen=2, seen=1

# Bundle data and constants
dataList <- list(y=CH.ms); str(dataList)
constList <- list(n.occasions=dim(CH.ms)[2], M=dim(CH.ms)[1]); str(constList)

# Initial values
# Good initial values need to be specified for the latent state z. They need to correspond to the true state, 
# which is not the same as the observed state. Thus, we have given initial values of "1" for the latend state at
# all places before an individual was observed, an initial value of "2" at all places when the individual was observed 
# alive or known to be alive and an initial value of "3" at all places after the last observation. The following function 
# creates the initial values.

js.multistate.init <- function(ch, nz){
  ch[ch==2] <- NA
  state <- ch
  for (i in 1:nrow(ch)){
    n1 <- min(which(ch[i,]==1))
    n2 <- max(which(ch[i,]==1))
    state[i,n1:n2] <- 2
  }
  state[state==0] <- NA
  get.first <- function(x) min(which(!is.na(x)))
  get.last <- function(x) max(which(!is.na(x)))   
  f <- apply(state, 1, get.first)
  l <- apply(state, 1, get.last)
  for (i in 1:nrow(ch)){
    state[i,1:f[i]] <- 1
    if(l[i]!=ncol(ch)) state[i, (l[i]+1):ncol(ch)] <- 3
    state[i, f[i]] <- 2
  }   
  state <- rbind(state, matrix(1, ncol=ncol(ch), nrow=nz))
  state[,1] <- NA
  return(state)
}


nOcc <- dim(CH.ms)[2]### CHANGED
Mtot <- dim(CH.ms)[1]### CHANGED
constList <- list(n.occasions = nOcc, M = Mtot)### CHANGED
constList

### CHANGED: #inits <- function(){list(mean.phi=runif(1, 0, 1), mean.p=runif(1, 0, 1), z=js.multistate.init(CH.du, nz))}    

inits <- function(){
  list(
    mean.phi = runif(1, 0, 1),
    #p = runif(n.occasions-1, 0, 1),   ### CHANGED SUGGESTION - DOES NOT WORK
    
    p = runif(constList$n.occasions - 1, 0, 1), ### CHANGED 
    
    
    z = js.multistate.init(CH.du, nz)
  )
}

length(inits()$p) == constList$n.occasions - 1

# Parameters monitored
# parameters <- c("mean.p", "mean.phi", "b", "Nsuper", "N", "B")  ### CHANGED

parameters <- c("p", "mean.phi", "b", "Nsuper", "N", "B")

# MCMC settings
#ni <- 5000  ;  nt <- 4  ;  nb <- 1000  ;  nc <- 4    # test
ni <- 20000  ;  nt <- 15  ;  nb <- 5000  ;  nc <- 2   # serious

# Call NIMBLE from R (ART 45 min)
system.time(
  out2 <- nimbleMCMC(code=js2Code, data=dataList, constants=constList, inits=inits(),
                     monitors=parameters, niter=ni, nburnin=nb, nchains=nc, thin=nt,
                     samplesAsCodaMCMC=TRUE)  )

#----------------------------------------------------------------------
# Assess convergence and print marginal posterior summary
#----------------------------------------------------------------------
library(BPAbook)
library(jagsUI)

out2S <- nimbleSummary(out2, parameters) # Convert to jagsUI output format
traceplot(out2S)                         # Traceplots      
print(out2S , 3)                          # Summary
