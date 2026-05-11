

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



#------------------------------------------------------
# Plot Plettenberg Bay abundance
#------------------------------------------------------

Plet = readRDS('./output/PlettenbergBay_N.rds')
head(Plet)

abundance_Plet = ggplot(Plet, aes(x = n, y = year,  fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
                      quantile_lines = TRUE, quantiles = 2,
                      rel_min_height = 0.0001, 
                      scale = 1) +
  scale_fill_continuous(name = "Tail probability", trans = 'reverse')+
  scale_y_discrete(limits=rev)+
  xlab("Population size") +
  ylab("Year") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 220)) + 
  theme_gg() +
  font("xylab",size=14)+
  font("xy",size=14)+
  font("xy.text", size = 14) +
  font("legend.text",size = 14)+
# theme(legend.position='right') 
  theme(legend.position="none")

abundance_Plet 

#-----------------------------------------------------------------------------------------
# add Kwok 2017 estimates

c = 1 / 0.9565  # Assume Chris's correction for unmarked dolphins
est = 267 *c  
lci = 177 * c   
uci = 357 * c

Kwok = data.frame(n = est, year = as.factor(2000), lci = lci, uci = uci)
Kwok


# new x axis scale

abundance_Plet2 = ggplot(Plet, aes(x = n, y = year,  fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
                      quantile_lines = TRUE, quantiles = 2,
                      rel_min_height = 0.0001, 
                      scale = 1) +
  scale_fill_continuous(name = "Tail probability", trans = 'reverse')+
  scale_y_discrete(limits=rev)+
  xlab("Population size") +
  ylab("Year") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 380)) + 
  theme_gg() +
  font("xylab",size=14)+
  font("xy",size=14)+
  font("xy.text", size = 14) +
  font("legend.text",size = 14)+
  theme(legend.position='right') 
#  theme(legend.position="none")

abundance_Plet2

abundance_Plet2 = abundance_Plet2 + 
  geom_point(data = Kwok,
             aes(x = n, y = year),
             color = "red",
             size = 5,
             inherit.aes = FALSE) +
  geom_segment(data = Kwok,
               aes(x= lci, xend= uci, y= year, yend=year),
               inherit.aes = FALSE)

abundance_Plet2

# But this is from a POPAN model, so perhaps I should not report it. 

#------------------------------------------------------
# Plot 2014/15 abundance
#------------------------------------------------------

abundance = ggplot(N, aes(x = n, y = 0, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
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

library(patchwork)

figure4 = abundance + abundance_Plet  + 
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'A')

figure4

## Save Plot 
pdf("./figures/figure4.pdf",
    useDingbats = FALSE, width = 12, height = 5)
print(figure4)
dev.off()

png(filename = "./figures/figure4.png", width = 2000, height = 1300, 
    pointsize = 8,  res = 300)
plot(figure4)
dev.off()

