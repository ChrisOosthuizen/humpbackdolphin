## Function to simulate from the RPT model.  ##
## Functions to fit the RPT model and        ##
## the JS-type considered alternative models ##

# Function for simulating data -----------------------------------------

#The following function allows to simulate capture histories from the RPT model

sim.RPT <-  function(M.simul,T.simul,time.lag,year.start=NULL,rho,phi,mu,tau,delta,w,nzeros=500){
  
  if(is.unsorted(phi)){  #we want survival probabilities sorted by increasing order
    phi <- sort(phi)
  }
  
  ##latent variables
  
  clust <- sample(1:3,size = M.simul,replace = T,prob = w)
  
  r <- matrix(NA,nrow = M.simul,ncol = T.simul)   
  z <- d <- r                                   
  
  r[,1] <- rep(1,M.simul)  #individual recruitability (binary variable)
  z[,1] <- rbinom(M.simul,1, rho[clust,1])  #individual belonging to the population (binary variable)
  d[,1] <- z[,1] #if an individual is alive at t=1, it is also detectable
  
  for(i in 1:M.simul){
    phi.temp <-  ifelse(clust[i]!=3,phi[2],phi[1])
    for(t in 2:T.simul){
      r[i,t] <- min(r[i,t-1],1-z[i,t-1])
      z[i,t] <- rbinom(1,1,phi.temp^time.lag[t-1]*z[i,t-1]+rho[clust[i],t]*r[i,t])
      d[i,t] <- ifelse(clust[i]!=2,z[i,t],rbinom(1,1,delta*z[i,t]))
    }
  }
  
  ##derived parameters
  
  ind <- ifelse(rowSums(z)>0,T,F)  #individual belonging to the superpopulation (binary variable)
  
  Nsuper <- sum(ind)  #superpopulation size
  
  if(is.null(year.start)){
    N.y <- NULL
  }else{
    
    N.y <- rep(NA,length(year.start)-1) #yearly estimated abundance (if yearly information is available)
    for(y in 1:length(N.y)) N.y[y] <-  sum(rowSums(z[,year.start[y]:(year.start[y+1]-1)])>0)
    
  }
  
  ##augmented data
  
  p <-  exp(mu+tau)/(1+exp(mu+tau))
  
  y.sim <- matrix(NA,nrow = M.simul,ncol = T.simul)  #matrix of the observed data (frequency of capture of individual i in period t)
  
  for (i in 1:M.simul) {
    for (t in 1:T.simul) {
      y.sim[i,t] <- rbinom(1,1,p[t]*d[i,t])
    }
  }
  
  ind.observed <- rowSums(y.sim)>0  #individual observed (binary variable)
  
  y.obs <- y.sim[ind.observed,] #matrix of the observed data 
  D <- nrow(y.obs)        #number of distinct individuals observed
  
  y <- rbind(y.obs,matrix(rep(0,nzeros*T.simul),ncol=T.simul)) #augmented data matrix
  M <- nrow(y)
  
  return(list(M.simul=M.simul,M=M,T.simul=T.simul,time.lag=time.lag,year.start=year.start,
              rho=rho,phi=phi,mu=mu,tau=tau,p=p,delta=delta,w=w,
              d=d,z=z,r=r,y.sim=y.sim,y.obs=y.obs,y=y,clust=clust,
              Nsuper=Nsuper,N.y=N.y,
              D=D,ind=ind,ind.observed=ind.observed))
  
}



# Functions for model fitting -----------------------------------

#RPT model

JS.RPT.fit.jags <- function(CR.data.matrix, #matrix with elements y_it (capture frequency of individual i at period t)
                         t_lag, #time lags btw consecutive primary occasions
                         year_start,
                         nc=2,  #number of chains
                         sample=2e4, #samples per chain
                         burnin=5e3, #burn in per chain
                         thin=1,
                         pars.to.save = c("w","p","phi","rho","mu","delta","Nsuper","N.y","clust","z","loglik_i"),
                         start.values=NULL,
                         seed=123 #the seed will be set once, before generating the starting values
){
  
  M <- nrow(CR.data.matrix)
  T.occ <- ncol(CR.data.matrix)
  D <- sum(rowSums(CR.data.matrix)>0)
  
  require(rjags)
  require(R2jags)
  require(coda)
  
  #Defining starting values
  if(is.null(start.values)){
    
    
    z_init <- matrix(rep(0,M*T.occ), ncol=T.occ)
    for(i in 1:D){
      pos <- which(CR.data.matrix[i,]==1)
      z_init[i,pos[1]:pos[length(pos)]] <- 1
    }
    
    set.seed(seed)
    clust_init <- sample(1:3,M,replace = T)
    
    start.values <-  replicate(nc,
                               simplify=FALSE,
                               list(z=z_init,
                                    rho=matrix(c(runif(3,0,1),runif((T.occ-1)*3,0.000001,1/M)),nrow=3),
                                    phi=sort(runif(2,0,1)),
                                    mu=runif(1,-5,5),
                                    delta=runif(1,0,1),
                                    clust=clust_init,
                                    w=as.vector(MCMCpack::rdirichlet(1,rep(1,3)))))
    
  }
  
  input.data<- list("y"=CR.data.matrix,
                    "T"=T.occ,
                    "M"=M,
                    "t_lag"=t_lag,
                    "year_start"=year_start) 
  
  start.time <- Sys.time()
  
  out<- jags(data = input.data,
             inits = start.values,
             parameters.to.save = pars.to.save,
             model.file = "scripts_CMR/BUGSmodel/RPT.txt",
             n.chains = nc,
             n.iter = sample,
             n.burnin = burnin,
             n.thin = thin)
  
  end.time <- Sys.time()
  print(end.time-start.time)
  
  #Let's add the information about the WAIC:
  
  loglik <- out$BUGSoutput$sims.list$loglik_i[,1:D]
  #columns are observed data points, rows are samples
  
  waic <- loo::waic(loglik)
  
  #let's simplify the jags output in order to reduce the size of the RData file
  
  chains_mat <- out$BUGSoutput$sims.matrix[,!grepl("^loglik", colnames(out$BUGSoutput$sims.matrix))]
  
  return(list(chains_mat=chains_mat,
              start.values=start.values,
              WAIC=waic,
              elapsed.time=end.time-start.time,
              model=out$model,
              nchains=out$BUGSoutput$n.chains,
              niter=out$BUGSoutput$n.iter,
              nburnin=out$BUGSoutput$n.burnin,
              thin=out$BUGSoutput$n.thin))
  
}

#JS-type models

JStype.fit.jags <- function(CR.data.matrix,
                            t_lag,
                            year_start,
                            G,
                            bugs_model,
                            nc=2,
                            sample=2e4,
                            burnin=5e3,
                            thin=1,
                            start.values=NULL,
                            pars.to.save=NULL,
                            seed=123){
  
  M <- nrow(CR.data.matrix)
  T.occ <- ncol(CR.data.matrix)
  D <- sum(rowSums(CR.data.matrix)>0)
  
  require(R2jags)
  require(rjags)
  require(coda)
  
  # STARTING VALUES
  
  if(is.null(start.values)){
    
    z_init <- matrix(rep(0,M*T.occ), ncol=T.occ)
    for(i in 1:D){
      pos <- which(CR.data.matrix[i,]==1)
      z_init[i,pos[1]:pos[length(pos)]] <- 1
    }
    
    set.seed(seed)
    
    #no mixture 
    if(bugs_model=="phi,p_t.txt"){
      
      if(G>1){
        stop("This model does not allow for mixture components.")
      }
      
      rho_init  <-  c(runif(1,0,1),runif(T.occ-1,0.000001,1/M))
      
      start.values  <-  replicate(nc,
                                  simplify=FALSE,
                                  list(z=z_init,
                                       rho=rho_init,
                                       phi=runif(1,0,1),
                                       mu=runif(1,-5,5)))
      
    }else{
      
      #mixture models
      
      if(G<2){
        stop("A mixture model should have at least two components.")
      }
      
      clust_init <- sample(1:G,M,replace = T)
      w_init <- as.vector(MCMCpack::rdirichlet(1,rep(1,G)))
      rho_init <- matrix(c(runif(G,0,1),runif((T.occ-1)*G,0.000001,1/M)),nrow=G)
      
      #is phi class-dependent?
      if(any(bugs_model==c("phi_g,p_t.txt",
                           "phi_g,p_tg.txt"))){
        
        phi_init  <-  sort(runif(G,0,1))
        
      }else{
        
        phi_init  <-  runif(1,0,1)
        
      }
      
      #is p class-dependent?
      if(any(bugs_model==c("phi,p_tg.txt",
                           "phi_g,p_tg.txt"))){
        
        mu_init  <-  sort(runif(G,-5,5))
        
        
      }else{
        
        mu_init  <-  runif(1,-5,5)
        
      }
      
      start.values  <-  replicate(nc,
                                  simplify=FALSE,
                                  list(z=z_init,
                                       rho=rho_init,
                                       phi=phi_init,
                                       mu=mu_init,
                                       clust=clust_init,
                                       w=w_init))
      
    }
    
    
  }
  
  # INPUT DATA
  
  #if the considered model has two class-dependent survival probabilities,
  #then the conditional beta distribution is considered for phi2;
  #otherwise, we consider a conditional uniform distribution on phi_g, g=2,...G (if G>1) or a uniform on the unit (if G=1)
  
  b <- ifelse(G==2 & any(bugs_model==c("phi_g,p_t.txt",
                                      "phi_g,p_tg.txt")),
             2,
             1)
  
  
  input.data <-  list("y"=CR.data.matrix,
                    "T"=T.occ,
                    "M"=M,
                    "t_lag"=t_lag,
                    "year_start"=year_start,
                    "b"=b) 
  
  if(G>1){
    
    input.data$G <-  G
    
  }
  
  # PARAMETER TO MONITOR
  
  if(is.null(pars.to.save)){
    
    pars.to.save <-  c("rho","phi","p","Nsuper","N.y","loglik_i")
    
    if(G>1){
      
      pars.to.save <- c(pars.to.save, "w")
      
    }
    
  }
  
  # FITTING
  
  start.time <- Sys.time()
  
  out <-  jags(data = input.data,
             inits = start.values,
             parameters.to.save = pars.to.save,
             model.file = paste0("scripts_CMR/BUGSmodel/",bugs_model),
             n.chains = nc,
             n.iter = sample,
             n.burnin = burnin,
             n.thin = thin)
  
  end.time <- Sys.time()
  print(end.time-start.time)
  
  # COMPUTE WAIC
  
  loglik <- out$BUGSoutput$sims.list$loglik_i[,1:D]
  #columns are observed data points, rows are samples
  
  waic <- loo::waic(loglik)
  
  #let's simplify the jags output in order to reduce the size of the RData file
  
  chains_mat <-  out$BUGSoutput$sims.matrix[,!grepl("^loglik", colnames(out$BUGSoutput$sims.matrix))]
  
  return(list(chains_mat=chains_mat,
              start.values=start.values,
              model=out$model,
              WAIC=waic,
              nchains=out$BUGSoutput$n.chains,
              niter=out$BUGSoutput$n.iter,
              nburnin=out$BUGSoutput$n.burnin,
              thin=out$BUGSoutput$n.thin,
              elapsed.time=end.time-start.time))
  
  
}

#####END-------------------------

