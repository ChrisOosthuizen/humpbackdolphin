
# plot groups sizes

# load libraries
library(ggplot2)
library(ggbeeswarm)
library(tidyverse)
library(ggpubr)
library(viridisLite)

# load data
dat = read.csv("./data/2026_group_size.csv")
head(dat)

names(dat)

median(dat$Knysna_2020, na.rm = T)
median(dat$Knysna_2021, na.rm = T)
median(dat$Knysna_2022, na.rm = T)

median(dat$Best_2014.15, na.rm = T)
median(dat$Knysna_2014.1, na.rm = T)
median(dat$Plettenberg_2014.15, na.rm = T)
median(dat$Tsitsikamma_2014.15, na.rm = T)


table(dat$Knysna_2020)
table(dat$Knysna_2021)
table(dat$Knysna_2022)

table(dat$Best_2014.15)
table(dat$Knysna_2014.1)
table(dat$Plettenberg_2014.15)
table(dat$Tsitsikamma_2014.15)



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


# make long format data frame for ggplot
dat = dat %>% 
  pivot_longer(names_to = "Survey_period",  cols = -c(Max_2014.15, Min_2014.15, Best_2014.15))  %>% 
  arrange(Survey_period)

head(dat)
unique(dat$Survey_period)

# select only 2014/15 data
dat1 = subset(dat, dat$Survey_period == "Plettenberg_2014.15" |
             dat$Survey_period ==  "Tsitsikamma_2014.15"| 
             dat$Survey_period == "Knysna_2014.15")

# set factor levels
dat1$Survey_period = factor(dat1$Survey_period,
                           levels = c(
                             "Knysna_2014.15", "Plettenberg_2014.15",  "Tsitsikamma_2014.15" 
                           ))


#--------------------------------------------------
# plot group size by section and year (2020s)
#--------------------------------------------------

#plot
g1 = ggplot(data = dat1,
            aes(x = Survey_period, y = value,  fill = Survey_period))+
  scale_fill_viridis_d( option = "D", direction = 1)+
  geom_violin(position = position_dodge(width = .75), size = 1, alpha=0.3, color = NA, show.legend = F) +
  geom_boxplot(notch = F,  outlier.size = -1, color="black",lwd=0.75, alpha = 0.1, show.legend = F)+
  ggbeeswarm::geom_quasirandom(shape = 21,
                               size=2, dodge.width = .75, color="black",alpha = .95,show.legend = F)+
  
  theme_gg()+
  font("xylab",size=12)+
  font("xy",size=12)+
  font("xy.text", size = 12) +
  font("legend.text",size = 12)+
  ylab(  c("Group size")  )  +
  xlab(  c("Section")  )  +
  scale_x_discrete(labels = c("Knysna\n2014/15", "Plettenberg\n2014/15", "Tsitsikamma\n2014/15"))+
  #  rremove("legend.title")+
  guides(fill = guide_legend(override.aes = list(alpha = 0.75,color="black")))+
  theme(plot.margin=grid::unit(c(5,0,5,5), "mm")) # (top, right, bottom, and left) 

g1 


unique(dat$Survey_period)

# now select only 2020s data
dat2 = subset(dat, dat$Survey_period == "Knysna_2020"  |
               dat$Survey_period ==  "Knysna_2021"| 
               dat$Survey_period == "Knysna_2022")


#plot
g2 = ggplot(data = dat2,
            aes(x = Survey_period, y = value,  fill = "#440154"))+
  scale_fill_viridis_d( option = "D", direction = 1)+
  geom_violin(position = position_dodge(width = .75), size = 1, alpha=0.3, color = NA, show.legend = F) +
  geom_boxplot(notch = F,  outlier.size = -1, color="black",lwd=0.75, alpha = 0.1, show.legend = F)+
  ggbeeswarm::geom_quasirandom(shape = 21,
                               size=2, dodge.width = .75, color="black",alpha = .95,show.legend = F)+
  
  theme_gg()+
  font("xylab",size=12)+
  font("xy",size=12)+
  font("xy.text", size = 12) +
  font("legend.text",size = 12)+
  ylab(  c("Group size")  )  +
  xlab(  c("Year")  )  +
  scale_x_discrete(labels = c("Knysna\n2020","Knysna\n2021", "Knysna\n2022"))+
  #  rremove("legend.title")+
  guides(fill = guide_legend(override.aes = list(alpha = 0.75,color="black")))+
  theme(plot.margin=grid::unit(c(5,5,5,0), "mm"))  #(top, right, bottom, and left) 

g2

# library(cowplot)
# p = plot_grid(g1,
#               g2 + theme(axis.text.y = element_blank(),
#                                    axis.ticks.y = element_blank(),
#                                    axis.title.y = element_blank()),
#                    nrow = 1)
# p

# combine plots
library(patchwork)
patchwork = g1 + g2 
patchwork[[2]] = patchwork[[2]] + theme(axis.text.y = element_blank(),
                                        axis.ticks.y = element_blank(),
                                        axis.title.y = element_blank() )

# patchwork = patchwork + plot_annotation(tag_levels = "a")
patchwork

## Save Plot 
pdf("./figures/group size.pdf",
    useDingbats = FALSE, width = 10, height = 7)
print(patchwork)
dev.off()

png(filename = "./figures/group size.png", width = 2000, height = 1300, 
    pointsize = 8,  res = 300)
plot(patchwork)
dev.off()


#------------start--------------------------------------------------------------------------------------------

#----------------
# combine plots
#----------------

library(patchwork)

patchwork3 = p1 + p2 + patchwork  + 
  plot_layout(ncol = 2)

patchwork3


patchwork[[2]] = patchwork[[2]] + theme(axis.text.y = element_blank(),
                                        axis.ticks.y = element_blank(),
                                        axis.title.y = element_blank() )

# https://patchwork.data-imaginist.com/articles/guides/layout.html
p1 + p2 / patchwork

top = p1 + p2
combo = top / patchwork

combo

## Save Plot 
pdf("./figures/figure 1.pdf",
    useDingbats = FALSE, width = 10, height = 7)
print(combo)
dev.off()


# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/
library("cowplot")
ggdraw() +
  p1 + p2 + patchwork  +
  draw_plot_label(label = c("A", "B", "C"), size = 15,
                  x = c(0, 0.5, 0), y = c(1, 1, 0.5))

library("gridExtra")
grid.arrange(patchwork,                                    # bar plot spaning two columns
             p1, p2,                               # box plot and scatter plot
             ncol = 2, nrow = 2, 
             layout_matrix = rbind(c(1,1), c(2,3)))                      # Number of rows


#----------end-------------------------------------------------------------------------------------------------

#--------------------------------------------------
# plot min, best and max estimates of group size
#--------------------------------------------------
head(dat)

mean(dat$Max_2014.15)
mean(dat$Min_2014.15)
mean(dat$Best_2014.15)



# make long format
dat2 = dat %>% 
  dplyr::select(-Survey_period, -value) %>%
  rename(Maximum= Max_2014.15, Minimum = Min_2014.15, Best = Best_2014.15) %>%
  pivot_longer(names_to = "Variability",  cols = everything())

head(dat2)
unique(dat2$Variability)

dat2$Variability = factor(dat2$Variability,
                          levels = c(
                            "Minimum",
                            "Best",
                            "Maximum"     
                          ))



#plot
px = ggplot(data = dat2,
            aes(x = Variability, y = value,  fill = Variability))+
 # scale_fill_viridis_d( option = "F")+
  geom_violin(position = position_dodge(width = .75), size = 1, alpha=0.3, color = NA, show.legend = F) +
  geom_boxplot(notch = F,  outlier.size = -1, color="black",lwd=0.75, alpha = 0.1, show.legend = F)+
  ggbeeswarm::geom_quasirandom(shape = 21,
                               size=2, dodge.width = .75, color="black",alpha = .95,show.legend = F)+
  
  theme_gg()+
  font("xylab",size=12)+
  font("xy",size=12)+
  font("xy.text", size = 12) +
  #  font("legend.text",size = 12)+
  ylab(  c("Group size")  )  +
  xlab(  c("Estimate")  )  +
  scale_x_discrete(labels = c("Minimum estimate","Best estimate", "Maximum estimate"))+
  #  rremove("legend.title")+
  guides(fill = guide_legend(override.aes = list(alpha = 0.75,color="black")))

px 

## Save Plot 
pdf("./figures/group size min max.pdf",
    useDingbats = FALSE, width = 10, height = 7)
print(p2)
dev.off()

png(filename = "./figures/group size min max.png", width = 2000, height = 1300, 
    pointsize = 8,  res = 300)
plot(p2)
dev.off()



#-----------------------------------------------------------------------
# ggstatsplots 

# https://r-graph-gallery.com/web-violinplot-with-ggstatsplot.html
#-----------------------------------------------------------------------

library(ggstatsplot)

plt <- ggbetweenstats(
  data = dat,
  x = Survey_period,
  y = value
)

plt

plt <- plt + 
  # Add labels and title
  labs(
    x = "Survey",
    y = "Group size",
    #    title = "Distribution of bill length across penguins species"
  ) + 
  # Customizations
  theme(
    # This is the new default font in the plot
    text = element_text(family = "Roboto", size = 8, color = "black"),
    plot.title = element_text(
      family = "Lobster Two", 
      size = 20,
      face = "bold",
      color = "#2a475e"
    ),
    # Statistical annotations below the main title
    plot.subtitle = element_text(
      family = "Roboto", 
      size = 15, 
      face = "bold",
      color="#1b2838"
    ),
    plot.title.position = "plot", # slightly different from default
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12)
  )

plt

plt <- plt  +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_line(colour = "grey50"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")
  )

plt
