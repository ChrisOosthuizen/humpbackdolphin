
# Chris Oosthuizen

# Simulate data and test JS model and RPT models

# 20 Feb 2026

# 1. Simulate 1000 JS EHM
# 2. Add 50 transients to EHM
# There should be 1000 + 50 = 1050 individuals in superpopulation.

#------------------------------------------------------------------------------------
# Simulate capture histories
# Code from BPA book (Kery and Schaub)
# Model: constant phi, time-dependent p, time-dependent entry
#------------------------------------------------------------------------------------

# Define parameter values

n.occasions <- 16             # Number of capture occasions
N <- 1000                     # Superpopulation size
phi <- rep(0.95, n.occasions-1)           # Survival probabilities (constant)
b <- c(0.2, 0.05, 0, 0, 0.05, 0, 0.3, 0.05, 0, 0.1, 0, 0.05,0.1, 0.1, 0, 0)   # Entry probabilities
length(b)
sum(b) # must sum to 1.

p <- c(0.3, 0.5, 0.9, 0.2, 0.5, 0.9, 0.1, 0.8, 0.5, 0.2, 0.9, 0.9, 0.2, 0.1, 0.9, 0.8)  # Time-varying capture probabilities
#p <- c(0.2, 0.15, 0.1, 0.2, 0.1, 0.15, 0.28, 0.15,0.2, 0.15, 0.3, 0.1,0.2, 0.2, 0.3, 0.1)  # Time-varying capture probabilities
length(p)

PHI <- matrix(rep(phi, N), ncol=n.occasions-1, nrow=N, byrow=TRUE)
P <- matrix(rep(p, N), ncol=n.occasions, nrow=N, byrow=TRUE)

set.seed(123)

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

#----------------------------------------------------
# Add some 'transient' animals to this EHM
#----------------------------------------------------
transient_N  <- 50
n_cols <- ncol(CH)

# create empty matrix of zeros
new_rows <- matrix(0, nrow = transient_N, ncol = n_cols)

# randomly choose one of the first 10 columns for each row
cols_with_1 <- sample(1:10, transient_N, replace = TRUE)

# assign the 1s
new_rows[cbind(1:transient_N, cols_with_1)] <- 1

dim(new_rows)
sum(new_rows)

# add to original matrix
CH <- rbind(CH, new_rows)

dim(CH)  

#------------------------------------
# RMARK
#------------------------------------

library(RMark)

# Convert each row to a character string
ch <- apply(CH, 1, paste0, collapse = "")

ch.tab <- table(ch)

mark.data <- data.frame(
  ch   = names(ch.tab),
  freq = as.numeric(ch.tab)
)


## process data 

proc <- process.data(mark.data, model = "POPAN")
ddl  <- make.design.data(proc)

model <- mark(
  proc,
  ddl,
  model.parameters = list(
    Phi   = list(formula = ~1),
    p     = list(formula = ~time),
    pent  = list(formula = ~time),
    N     = list(formula = ~1)
  ),
  delete = T
)

# There should be 1000 + 50 = 1050 individuals in superpopulation.
