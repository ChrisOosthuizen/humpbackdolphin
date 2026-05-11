

# Fit closed models to Plettenberg Bay data collected in 
# 2002/03, 2012/13 and 2014/15

library(multimark)  # McClintock et al 2015

#-----------------------------------------
# 2002 data
#-----------------------------------------
# Load encounter histories and inspect
dat = read.table("./data/2002_input.txt", header = T)
dat = data.matrix(dat)
head(dat)
dim(dat)

# dat = dat[, 1:16]   # keep only 16 occasions to 'standardize' with 2014/15 data
# head(dat)
# dim(dat)
# dat = dat[(which(rowSums(dat != 0) > 0)), ]  # remove rows that are all 0
# dim(dat)
# 
# dat = dat[, 3:18]   # keep only 16 occasions to 'standardize' with 2014/15 data
# head(dat)
# dim(dat)
# dat = dat[(which(rowSums(dat != 0) > 0)), ]  # remove rows that are all 0
# dim(dat)

#---------------------------------------------------------------------
# Model averaging of closed models
#---------------------------------------------------------------------

# m.i <- markClosed(Enc.Mat = dat,
#                   mod.p= ~ 1,
#                   nchains=3,
#                   burnin=50000,
#                   iter=500000,
#                   thin=10,
#                   parms="all",
#                   printlog=TRUE)

#saveRDS(m.i, './output/2002_Model_i.rds')
m.i = readRDS('./output/2002_Model_i.rds')

# m.th <- markClosed(Enc.Mat = dat,
#                    mod.p= ~time + h,
#                    nchains=3,
#                    burnin=50000,
#                    iter=500000,
#                    thin=10,
#                    parms="all",
#                    printlog=TRUE)
# 
# saveRDS(m.th, './output/2002_Model_th.rds')
m.th = readRDS('./output/2002_Model_th.rds')

# m.t <- markClosed(Enc.Mat = dat,
#                   mod.p=~time,
#                   nchains=3,
#                   burnin=50000,
#                   iter=500000,
#                   thin=10,
#                   parms="all",
#                   printlog=TRUE)
# 
# saveRDS(m.t, './output/2002_Model_t.rds')
m.t = readRDS('./output/2002_Model_t.rds')


# m.h <- markClosed(Enc.Mat = dat,
#                   nchains=3,
#                   burnin=50000,
#                   iter=500000,
#                   thin=10,
#                   mod.p=~h,
#                   parms="all",
#                   printlog=TRUE)
# 
# saveRDS(m.h, './output/2002_Model_h.rds')
m.h = readRDS('./output/2002_Model_h.rds')


modlist <- list(mod1=m.i,
                mod2=m.th,
                mod3=m.t,
                mod4=m.h)

# M <- multimodelClosed(modlist=modlist)
# 
# saveRDS(M, './output/2002_Model_list.rds')
M = readRDS('./output/2002_Model_list.rds')

M$pos.prob
head(M$rjmcmc) 
plot(M$rjmcmc[[1]])

N = data.frame(as.matrix(M$rjmcmc[,"N"]))
names(N) = "n"
N$n = N$n * 1.045   # scale for unmarked individuals
mean(N$n)
median(N$n)
length(N$n)
quantile(N$n, probs = c(2.5,97.5)/100)

Plet2002 = data.frame(N$n)
names(Plet2002) = "n"
Plet2002$year = 2002
Plet2002$y = 0



#------------------------------------------------------
# Plot
#------------------------------------------------------

library(ggplot2)
library(ggpubr)
# https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
library(ggridges)
library(viridis)

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



abundance2002 = ggplot(Plet2002, aes(x = n, y = y, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
                      quantile_lines = TRUE, quantiles = 2,
                      #   rel_min_height = 0.01, 
                      scale = 1) +
  scale_fill_continuous(name = "Tail probability", trans = 'reverse')+
  xlab("Population size") +
  ylab("Probability density") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 350)) + 
  theme_gg() +
  font("xylab",size=14)+
  font("xy",size=14)+
  font("xy.text", size = 14) +
  font("legend.text",size = 14)+
  #  theme(legend.position=c(0.8,0.8)) 
  theme(legend.position="none")

abundance2002 


#-----------------------------------------
# 2012 data
#-----------------------------------------

# Load encounter histories and inspect
dat = read.table("./data/2012_input.txt", header = T)
dat = data.matrix(dat)
head(dat)
dim(dat)

# dat = dat[, 1:16]   # keep only 16 occasions to 'standardize' with 2014/15 data
# head(dat)
# dim(dat)
# dat = dat[(which(rowSums(dat != 0) > 0)), ]  # remove rows that are all 0
# dim(dat)

#---------------------------------------------------------------------
# Model averaging of closed models
#---------------------------------------------------------------------

# m.i <- markClosed(Enc.Mat = dat,
#                   mod.p= ~ 1,
#                     nchains=3,
#                     burnin=50000,
#                     iter=500000,
#                     thin=10,
#                   parms="all",
#                   printlog=TRUE)
# 
# saveRDS(m.i, './output/2012_Model_i.rds')
m.i = readRDS('./output/2012_Model_i.rds')

# m.th <- markClosed(Enc.Mat = dat,
#                    mod.p= ~time + h,
#                          nchains=3,
#                          burnin=50000,
#                          iter=500000,
#                          thin=10,
#                    parms="all",
#                    printlog=TRUE)
# 
# saveRDS(m.th, './output/2012_Model_th.rds')
m.th = readRDS('./output/2012_Model_th.rds')


# m.t <- markClosed(Enc.Mat = dat,
#                   mod.p=~time,
#                      nchains=3,
#                      burnin=50000,
#                      iter=500000,
#                      thin=10,
#                   parms="all",
#                   printlog=TRUE)
# 
# saveRDS(m.t, './output/2012_Model_t.rds')
m.t = readRDS('./output/2012_Model_t.rds')

# m.h <- markClosed(Enc.Mat = dat,
#                        nchains=3,
#                        burnin=50000,
#                        iter=500000,
#                        thin=10,
#                   mod.p=~h,
#                   parms="all",
#                   printlog=TRUE)
# 
# saveRDS(m.h, './output/2012_Model_h.rds')
m.h = readRDS('./output/2012_Model_h.rds')


modlist <- list(mod1=m.i,
                mod2=m.th,
                mod3=m.t,
                mod4=m.h)

# M <- multimodelClosed(modlist=modlist)
#  
# saveRDS(M, './output/2012_Model_list.rds')
M = readRDS('./output/2012_Model_list.rds')

M$pos.prob
head(M$rjmcmc) 
plot(M$rjmcmc[[1]])

N = data.frame(as.matrix(M$rjmcmc[,"N"]))
names(N) = "n"
N$n = N$n * 1.045   # scale for unmarked individuals
mean(N$n)
median(N$n)
length(N$n)
quantile(N$n, probs = c(2.5,97.5)/100)

Plet2012 = data.frame(N$n)
names(Plet2012) = "n"
Plet2012$year = 2012
Plet2012$y = 0

#------------------------------------------------------
# Plot
#------------------------------------------------------

abundance2012 = ggplot(Plet2012, aes(x = n, y = y, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
                      quantile_lines = TRUE, quantiles = 2,
                      #   rel_min_height = 0.01, 
                      scale = 1) +
  scale_fill_continuous(name = "Tail probability", trans = 'reverse')+
  xlab("Population size") +
  ylab("Probability density") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 350)) + 
  theme_gg() +
  font("xylab",size=14)+
  font("xy",size=14)+
  font("xy.text", size = 14) +
  font("legend.text",size = 14)+
  #  theme(legend.position=c(0.8,0.8)) 
  theme(legend.position="none")

abundance2012 


#-----------------------------------------
# 2014 data - D Conry - Plettenberg Bay Only
#-----------------------------------------

# Load encounter histories and inspect
dat = read.table("./data/multimark_input_PlettenbergBay.txt", header = T)
dat = data.matrix(dat)
head(dat)
dim(dat)
#---------------------------------------------------------------------
# Model averaging of closed models
#---------------------------------------------------------------------

# m.i <- markClosed(Enc.Mat = dat,
#                   mod.p= ~ 1,
#                     nchains=3,
#                     burnin=50000,
#                     iter=500000,
#                     thin=10,
#                   parms="all",
#                   printlog=TRUE)
# 
# saveRDS(m.i, './output/2014_Model_i.rds')
m.i = readRDS('./output/2014_Model_i.rds')

# m.th <- markClosed(Enc.Mat = dat,
#                    mod.p= ~time + h,
#                            nchains=3,
#                            burnin=50000,
#                            iter=500000,
#                            thin=10,
#                    parms="all",
#                    printlog=TRUE)
# 
# saveRDS(m.th, './output/2014_Model_th.rds')
m.th = readRDS('./output/2014_Model_th.rds')


# m.t <- markClosed(Enc.Mat = dat,
#                   mod.p=~time,
#                       nchains=3,
#                       burnin=50000,
#                       iter=500000,
#                       thin=10,
#                   parms="all",
#                   printlog=TRUE)
# 
# saveRDS(m.t, './output/2014_Model_t.rds')
m.t = readRDS('./output/2014_Model_t.rds')


# m.h <- markClosed(Enc.Mat = dat,
#                          nchains=3,
#                          burnin=50000,
#                          iter=500000,
#                          thin=10,
#                   mod.p=~h,
#                   parms="all",
#                   printlog=TRUE)
# 
# saveRDS(m.h, './output/2014_Model_h.rds')
m.h = readRDS('./output/2014_Model_h.rds')


modlist <- list(mod1=m.i,
                mod2=m.th,
                mod3=m.t,
                mod4=m.h)

# M <- multimodelClosed(modlist=modlist)
# 
# saveRDS(M, './output/2014_Model_list.rds')
M = readRDS('./output/2014_Model_list.rds')

M$pos.prob
head(M$rjmcmc) 
plot(M$rjmcmc[[1]])

N = data.frame(as.matrix(M$rjmcmc[,"N"]))
names(N) = "n"
N$n = N$n * 1.045   # scale for unmarked individuals
mean(N$n)
median(N$n)
length(N$n)
quantile(N$n, probs = c(2.5,97.5)/100)

Plet2014 = data.frame(N$n)
names(Plet2014) = "n"
Plet2014$year = 2014
Plet2014$y = 0

#------------------------------------------------------
# Plot
#------------------------------------------------------

abundance2014 = ggplot(Plet2014, aes(x = n, y = y, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
                      quantile_lines = TRUE, quantiles = 2,
                      #   rel_min_height = 0.01, 
                      scale = 1) +
  scale_fill_continuous(name = "Tail probability", trans = 'reverse')+
  xlab("Population size") +
  ylab("Probability density") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 350)) + 
  theme_gg() +
  font("xylab",size=14)+
  font("xy",size=14)+
  font("xy.text", size = 14) +
  font("legend.text",size = 14)+
  #  theme(legend.position=c(0.8,0.8)) 
  theme(legend.position="none")

abundance2014 


#------------------------------
# plot all together
#------------------------------

Plet = rbind(Plet2002, Plet2012, Plet2014)
head(Plet)
Plet$year = factor(Plet$year,
                          levels = c('2002', '2012', '2014'))
                   
saveRDS(Plet, './output/PlettenbergBay_N.rds')

abundance_Plet = ggplot(Plet, aes(x = n, y = year,  fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
                      quantile_lines = TRUE, quantiles = 2,
                      #   rel_min_height = 0.01, 
                      scale = 0.95) +
  scale_fill_continuous(name = "Tail probability", trans = 'reverse')+
  scale_y_discrete(limits=rev)+
  xlab("Population size") +
  ylab("Year") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 255)) + 
  theme_gg() +
  font("xylab",size=14)+
  font("xy",size=14)+
  font("xy.text", size = 14) +
  font("legend.text",size = 14)+
  theme(legend.position='right') 
#  theme(legend.position="none")

abundance_Plet


## Save Plot 
pdf("./figures/abundance_Plet.pdf",
    useDingbats = FALSE, width = 6, height = 6)
print(abundance_Plet)
dev.off()

png(filename = "./figures/abundance_Plet.png", width = 1300, height = 1300, 
    pointsize = 8,  res = 300)
plot(abundance_Plet)
dev.off()




