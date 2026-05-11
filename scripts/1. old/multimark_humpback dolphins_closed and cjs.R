
# Paper by McClintock et al 2015
# Also see Curtis et al 2020 paper and R code (fully reproducable and nicely annotated).

# multimark can analyse left and right identifications separately. We use the
# Single mark implementation of this method.

# Both closed models and open models can be fitted.
# Heterogeneity can be added to both closed and open models
# Abundance is ONLY computed for closed models.

library(multimark)

# Load encounter histories from Conry Msc (humpback dolphins) and inspect
# There is no column of '1's at the end
dat = read.table("multimark_input.txt", header = T)
dat = data.matrix(dat)
head(dat)

# Load covariates to include in models. This is z-standardized survey effort (hours surveyed per month)
surveyeffort = c(-0.71, 3.05, -0.18, -0.35, -0.88, -0.35, -0.72, 0.03, -0.95, -0.89, 0.24, 0.51, 0.95, -0.30, -0.24, 0.77)

#----------aside-----------------------------------------------------------------------------------------
# For multi-mark models (more than 1 tag type), you need to set up models in this way. Not needed for normal CJS type models
# setup = processdata(Enc.Mat = dat, data.type = 'never', covs = data.frame(effort = surveyeffort))
# m.dot = multimarkClosed(mms = setup,
#                               mod.p = ~1)
# summary(m.dot$mcmc)
#---------aside ends-------------------------------------------------------------------------------------


#---------------------------------------------------------------------
# Model fitting
#---------------------------------------------------------------------

#---------------------------------------------------------------------
# markCJS = Open population models. No abundance
#---------------------------------------------------------------------

CJS.dot <- markCJS(Enc.Mat = dat,
                           mod.p = ~1, mod.phi = ~1,
                           parms = "all",
                           printlog = TRUE)
                             
#Posterior summary for monitored parameters 
summary(CJS.dot$mcmc)
plot(CJS.dot$mcmc)
summary(getprobsCJS(CJS.dot))


#---------------------------------------------------------------------
# markClosed = closed population models
#---------------------------------------------------------------------

closed_pc = markClosed(Enc.Mat = dat,
                   mod.p=~ c,
                   parms = "all",
                   printlog = TRUE)

#Posterior summary for monitored parameters 
closed_pc_prob = getprobsClosed(closed_pc)
summary(closed_pc_prob[,c("p[1]","c[2]")])     


N = data.frame(as.matrix(closed_pc$mcmc[,"N"]))
names(N) = "n"

library(ggplot2)
g2 <- ggplot(data=N, aes(n)) + 
  geom_density(fill=I("gray70"), col=I("black"), size = 0.2) +
  theme_bw() + theme(panel.grid.major= element_line(colour="white"), panel.grid.minor = element_line(colour="white"),
                     text = element_text(size = 10)) + 
  xlab("Population size") + ylab("Density") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 450)) + 
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

g2

#---------------------------------------------------------------------
# Model averaging of closed models
#---------------------------------------------------------------------

m.c <- markClosed(Enc.Mat = dat,
                   mod.p= ~ 1,
                   parms="all")

m.th <- markClosed(Enc.Mat = dat,
                  mod.p= ~time + h,
                  parms="all")

m.time <- markClosed(Enc.Mat = dat,
                     mod.p=~time,
                     parms="all")

m.h <- markClosed(Enc.Mat = dat,
                  mod.p=~h,
                  parms="all")

modlist <- list(mod1=m.c,
                mod2=m.time,
                mod3=m.h,
                mod4=m.th)

M <- multimodelClosed(modlist=modlist)
M

(M$rjmcmc) 

plot(M$rjmcmc[[1]])


N = data.frame(as.matrix(M$rjmcmc[,"N"]))
names(N) = "n"
mean(N$n)

g2 <- ggplot(data=N, aes(n)) + geom_density(fill=I("gray70"), col=I("black"), size = 0.2) +
  theme_bw() + theme(panel.grid.major= element_line(colour="white"), panel.grid.minor = element_line(colour="white"),
                     text = element_text(size = 10)) + 
  xlab("Population size") + ylab("Density") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 150)) + 
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
g2

# posterior model probabilities
(M$pos.prob)


#---------------------------------------------------------------------
# Model averaging of CJS models
#---------------------------------------------------------------------

CJS.th_i <- markCJS(Enc.Mat = dat,
                     mod.p = ~ time + h, mod.phi = ~ 1,
                     parms = "all",
                     printlog = TRUE)

CJS.t_i <- markCJS(Enc.Mat = dat,
                     mod.p = ~ time, mod.phi = ~ 1,
                     parms = "all",
                     printlog = TRUE)

CJS.h_i <- markCJS(Enc.Mat = dat,
                    mod.p = ~ h , mod.phi = ~ 1,
                    parms = "all",
                    printlog = TRUE)

modlist <- list(mod1=CJS.th_i ,
                mod2=CJS.t_i ,
                mod3=CJS.h_i )

M <- multimodelCJS(modlist=modlist)
# posterior model probabilities
M$pos.prob

(M$rjmcmc) 
summary(CJS.t_i$mcmc)

plot(M$rjmcmc[[1]])

phi = data.frame(x=as.matrix(M[,11])[,1],
                 b=as.matrix(M$mcmc[,2])[,1])

coda::effectiveSize(CJS.t_i$mcmc)

# This does not work as there is no abundance for open models;

N = data.frame(as.matrix(M$rjmcmc[,"N"]))
names(N) = "n"
mean(N$n)


# Some code from Curtis et al:

# models
multi.effiRE.sub = markClosed(Enc.Mat = dat,
                              covs = data.frame(effort = surveyeffort),
                              mod.p = ~surveyeffort+h,
                           #   mod.delta = ~1,
#                              nchains=3,
 #                             burnin=250000,
  #                            iter=1000000,
   #                           thin=100,
                              parms="all")


multi.effiRE.sub.p = getprobsClosed(multi.effiRE.sub)
multi.effiRE.sub.p
summary(multi.effiRE.sub.p)


gelman.diag(singleCJS.dot.h$mcmc)

heidel.diag(singleCJS.dot.h$mcmc)

rmeanplot(singleCJS.dot.h$mcmc)



# summary stats for multimark CJS model
out = process.mcmc.list.stats(singleCJS.dot.h$mcmc)
out.p = process.mcmc.list.stats(multi.effbssniRE.pc.long.p)
out <- rbind(out, out.p)
neff = round(c( effectiveSize(multi.effbssniRE.pc.long$mcmc),
                effectiveSize(multi.effbssniRE.pc.long.p)))
out <- cbind(out, neff)
write.csv(out, file="summarystats.multimark.cjs.20190315.csv",row.names=F)
detach(2)
