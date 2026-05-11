
# Humpback dolphins

# multimark can analyse left and right identifications separately. We use the
# Single mark implementation of this method.

# Both closed models and open models can be fitted.
# We estimate abundance using closed models.

library(multimark)   # McClintock et al 2015
  
# Load encounter histories from Conry Msc (humpback dolphins) and inspect
# There is no column of '1's at the end
dat = read.table("./data/multimark_input.txt", header = T)
dat = data.matrix(dat)
head(dat)

# Load covariates to include in models. This is z-standardized survey effort (hours surveyed per month)
surveyeffort = c(-0.71, 3.05, -0.18, -0.35, -0.88, -0.35, -0.72, 0.03, -0.95, -0.89, 0.24, 0.51, 0.95, -0.30, -0.24, 0.77)

#---------------------------------------------------------------------
# Model averaging of closed models
#---------------------------------------------------------------------

m.i <- markClosed(Enc.Mat = dat,
                  mod.p= ~ 1,
       #           nchains=3,
      #            burnin=50000,
       #           iter=500000,
      #            thin=10,
                  parms="all",
                  printlog=TRUE)

saveRDS(m.i, './output/Model_i.rds')

m.th <- markClosed(Enc.Mat = dat,
                   mod.p= ~time + h,
#                   nchains=3,
 #                  burnin=50000,
  #                 iter=500000,
   #                thin=10,
                   parms="all",
                   printlog=TRUE)

saveRDS(m.th, './output/Model_th.rds')

m.t <- markClosed(Enc.Mat = dat,
                     mod.p=~time,
          #         nchains=3,
          #        burnin=50000,
          #        iter=500000,
          #        thin=10,
                     parms="all",
                  printlog=TRUE)

saveRDS(m.t, './output/Model_t.rds')

m.h <- markClosed(Enc.Mat = dat,
          #        nchains=3,
          #        burnin=50000,
          #        iter=500000,
          #        thin=10,
                  mod.p=~h,
                  parms="all",
                  printlog=TRUE)

saveRDS(m.h, './output/Model_h.rds')

m.eh <- markClosed(Enc.Mat = dat,
#                   nchains=3,
#                   burnin=50000,
 #                  iter=500000,
  #                 thin=10,
                   mod.p= ~surveyeffort + h,
                   parms="all",
                   printlog=TRUE)

saveRDS(m.eh, './output/Model_eh.rds')

m.e <- markClosed(Enc.Mat = dat,
#                  nchains=3,
 #                 burnin=50000,
  #                iter=500000,
   #               thin=10,
                   mod.p= ~surveyeffort,
                   parms="all",
                  printlog=TRUE)

saveRDS(m.e, './output/Model_e.rds')

modlist <- list(mod1=m.i,
                mod2=m.th,
                mod3=m.t,
                mod4=m.h,
                mod5=m.eh,
                mod6=m.e)

M <- multimodelClosed(modlist=modlist)
saveRDS(M, './output/Model_list.rds')

M = readRDS('./output/Model_list.rds')
m.th = readRDS('./output/Model_th.rds')

M$pos.prob

plot(M$rjmcmc[[1]])

N = data.frame(as.matrix(M$rjmcmc[,"N"]))
names(N) = "n"
N$n = N$n * 1.045   # scale for unmarked individuals
mean(N$n)
median(N$n)
length(N$n)
quantile(N$n, probs = c(2.5,97.5)/100)

#------------------------------------------------------
library(ggplot2)
library(ggpubr)

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

abundance = ggplot(data=N, aes(n)) + geom_density(fill=I("gray70"), col=I("black"), size = 0.2) +
  theme_bw() + theme(panel.grid.major= element_line(colour="white"), panel.grid.minor = element_line(colour="white"),
                     text = element_text(size = 10)) + 
  xlab("Population size") +
  ylab("Probability density") +
  scale_x_continuous(expand = c(0, 0), limits = c(50, 200)) + 
  theme_gg() +
  font("xylab",size=14)+
  font("xy",size=14)+
  font("xy.text", size = 14) +
  font("legend.text",size = 14)

abundance


# best model
library(coda)
gelman.diag(m.th$mcmc)
heidel.diag(m.th$mcmc)
#library(mcmcplots)
#rmeanplot(m.th$mcmc)

m.th.out = getprobsClosed(m.th)
out = summary(m.th.out)
par = as.data.frame(out$statistics)
par
par$time = seq(1:31)


#-------------------------------------
# Functions from Curtis et al.

# define  function to extract stats
summstats <- function(x) {
  # calculate mode and hpdi
  den <- density(x, kernel=c("gaussian"),from=min(x),to=max(x))
  mode = den$x[den$y==max(den$y)]
  denranks = order(den$y,decreasing=T)
  cum.perc.ranked = cumsum(den$y[denranks])/sum(den$y)
  hpdi.50 = range(den$x[denranks][!(cum.perc.ranked>0.50)])
  hpdi.90 = range(den$x[denranks][!(cum.perc.ranked>0.9)])
  # calculate median and percentiles
  percs = as.numeric(quantile(x,probs=c(0.05,.25,0.5,.75,0.95)))
  return(round(data.frame(hpdi.90.l=hpdi.90[1],hpdi.50.l=hpdi.50[1],mode=mode,hpdi.50.u=hpdi.50[2],hpdi.90.u=hpdi.90[2],
                          perc.05=percs[1],perc.25=percs[2],perc.50=percs[3],perc.75=percs[4],perc.95=percs[5],
                          mean=mean(x), sd=sd(x)),3))
}

# process mcmc.list object 
process.mcmc.list.stats <- function(x) {
  all = as.matrix(x)
  nv = dim(all)[2]
  summarystats = data.frame(NULL)
  for (n in 1:nv) {
    summarystats = plyr::rbind.fill(summarystats, summstats(all[,n]))
    summarystats$par[n] = dimnames(all)[[2]][n]
  }
  summarystats = summarystats[c("par",names(summarystats)[1:(dim(summarystats)[2]-1)])]
  return(summarystats)
}

#--------------------------------------------------------------
# summary stats for multimark closed-population model
out = process.mcmc.list.stats(m.th$mcmc)
out.p = process.mcmc.list.stats(m.th.out)
out <- rbind(out, out.p)

cf =  1.045

out[3,2:ncol(out)] <- cf*out[3,2:ncol(out)]   

library(coda)
neff = round(c( effectiveSize(m.th$mcmc),
                effectiveSize(m.th.p)))

out <- cbind(out, neff)

out
#########################################################################
out = process.mcmc.list.stats(multi.effiRE.sub$mcmc)
out.p = process.mcmc.list.stats(multi.effiRE.sub.p)
out <- rbind(out, out.p)
out[3,2:ncol(out)] <- cf.nc*out[3,2:ncol(out)]   
neff = round(c( effectiveSize(multi.effiRE.sub$mcmc),
                effectiveSize(multi.effiRE.sub.p)))
out <- cbind(out, neff)
#########################################################################


#plot
ggplot(data = par[1:16,],
             aes(x = as.factor(time), y = Mean,  color = "black")) + 
  geom_point(color="black", show.legend = F) +
  theme_gg()+
  font("xylab",size=16)+
  font("xy",size=16)+
  font("xy.text", size = 16) +
  font("legend.text",size = 16)+
  ylab("Capture probability")  +
  xlab("Month")  +
#  scale_x_discrete(labels = c("Knysna", "Plettenberg Bay", "Tsitsikamma"))+
  #  rremove("legend.title")+
  guides(fill = guide_legend(override.aes = list(alpha = 0.75,color="black")))

p.posterior = as.data.frame(m.th.out[[1]][,1:16])

p.posterior = p.posterior %>% 
              tidyr::pivot_longer(names_to = "Month", cols = everything())


# https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html

library(ggridges)
library(viridis)

ggplot(p.posterior, aes(x = value, y = Month, fill = stat(x))) +
  geom_density_ridges_gradient(quantile_lines = TRUE, quantiles = 2,
                      rel_min_height = 0.01, scale = 5) +
  scale_fill_viridis_c(name = "Detection p", option = "C") +
  xlab("Detection probability") +
  ylab("Month") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 0.65)) + 
  theme_gg() +
  font("xylab",size=14)+
  font("xy",size=14)+
  font("xy.text", size = 14) +
  font("legend.text",size = 14)

 
p.posterior

p.posterior$Month = factor(p.posterior$Month,
                           levels = c(
                             "p[1]", "p[2]",
                             "p[3]", "p[4]",
                             "p[5]", "p[6]",
                             "p[7]", "p[8]",
                             "p[9]", "p[10]",
                             "p[11]","p[12]",
                             "p[13]","p[14]",
                             "p[15]","p[16]"))


detection = ggplot(p.posterior, aes(x = value, y = Month, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
                      quantile_lines = TRUE, quantiles = 2,
                      rel_min_height = 0.01, 
                      scale = 4) +  # scale = overlap
  scale_fill_continuous(name = "Tail probability", trans = 'reverse')+
  xlab("Detection probability") +
  ylab("Month") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 0.65)) + 
  theme_gg() +
  font("xylab",size=14)+
  font("xy",size=14)+
  font("xy.text", size = 14) +
  font("legend.text",size = 14)+
  scale_y_discrete(limits=rev,
                   labels = c(
                     "2015_6",
                     "2015_5",  
                     "2015_4",
                     "2015_3",
                     "2015_2",
                     "2015_1",
                     "2014_12",
                     "2014_11",
                     "2014_10",
                     "2014_9",
                     "2014_8",
                     "2014_7",
                     "2014_6",
                     "2014_5",
                     "2014_4",
                     "2014_3"))+
  theme(
    legend.position=c(0.8,0.8))
detection

## Save Plot 
pdf("./figures/detection.pdf",
    useDingbats = FALSE, width = 6, height = 6)
print(detection)
dev.off()

png(filename = "./figures/detection.png", width = 2000, height = 1300, 
    pointsize = 8,  res = 300)
plot(detection)
dev.off()



abundance = ggplot(N, aes(x = n, y = site, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
                      quantile_lines = TRUE, quantiles = 2,
                    #   rel_min_height = 0.01, 
                      scale = 1) +
  scale_fill_continuous(name = "Tail probability", trans = 'reverse')+
    xlab("Population size") +
  ylab("Probability density") +
  scale_x_continuous(expand = c(0, 0), limits = c(50, 200)) + 
  theme_gg() +
  font("xylab",size=14)+
  font("xy",size=14)+
  font("xy.text", size = 14) +
  font("legend.text",size = 14)+
#  theme(legend.position=c(0.8,0.8)) 
  theme(legend.position="none")

abundance 

library(patchwork)

figure3 = abundance + detection  + 
  plot_layout(ncol = 2)

figure3

## Save Plot 
pdf("./figures/figure3.pdf",
    useDingbats = FALSE, width = 12, height = 5)
print(figure3)
dev.off()

png(filename = "./figures/figure3.png", width = 2000, height = 1300, 
    pointsize = 8,  res = 300)
plot(figure3)
dev.off()









