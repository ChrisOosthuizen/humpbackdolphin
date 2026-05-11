
# plot groups sizes

library(ggplot2)
library(ggbeeswarm)
library(tidyverse)
library(ggpubr)
library(viridisLite)

dat = read.csv("./data/group size.csv")
head(dat)

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


# make long format
dat = dat %>% 
  pivot_longer(names_to = "Survey_period",  cols = -c(Max_2014.15, Min_2014.15, Best_2014.15))  %>% 
  arrange(Survey_period)

head(dat)
unique(dat$Survey_period)

dat$Survey_period = factor(dat$Survey_period,
                           levels = c(
                                      "Plettenberg_2014.15",  "Tsitsikamma_2014.15", "Knysna_2014.15", 
                                      "Knysna_2020",
                                      "Knysna_2021",   
                                      "Knysna_2022"   
                           ))


#plot
p1 = ggplot(data = dat,
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
  xlab(  c("Survey area and time")  )  +
  scale_x_discrete(labels = c("Plettenberg\n2014/15", "Tsitsikamma\n2014/15", "Knysna\n2014/15",
                              "Knysna\n2020","Knysna\n2021", "Knysna\n2022"))+
  #  rremove("legend.title")+
  guides(fill = guide_legend(override.aes = list(alpha = 0.75,color="black")))

p1 

## Save Plot 
pdf("./group size.pdf",
    useDingbats = FALSE, width = 10, height = 7)
print(p1)
dev.off()

png(filename = "./group size.png", width = 2000, height = 1300, 
    pointsize = 8,  res = 300)
plot(p1)
dev.off()



#----------------------------------
head(dat)

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
p2 = ggplot(data = dat2,
            aes(x = Variability, y = value,  fill = Variability))+
  scale_fill_viridis_d( option = "D")+
  geom_violin(position = position_dodge(width = .75), size = 1, alpha=0.3, color = NA, show.legend = F) +
  geom_boxplot(notch = F,  outlier.size = -1, color="black",lwd=0.75, alpha = 0.1, show.legend = F)+
  ggbeeswarm::geom_quasirandom(shape = 21,
                               size=2, dodge.width = .75, color="black",alpha = .95,show.legend = F)+
  
  theme_rr()+
  font("xylab",size=12)+
  font("xy",size=12)+
  font("xy.text", size = 12) +
#  font("legend.text",size = 12)+
  ylab(  c("Group size")  )  +
  xlab(  c("Estimate")  )  +
  scale_x_discrete(labels = c("Minimum estimate","Best estimate", "Maximum estimate"))+
  #  rremove("legend.title")+
  guides(fill = guide_legend(override.aes = list(alpha = 0.75,color="black")))

p2 

## Save Plot 
pdf("./group size min max.pdf",
    useDingbats = FALSE, width = 10, height = 7)
print(p2)
dev.off()

png(filename = "./group size min max.png", width = 2000, height = 1300, 
    pointsize = 8,  res = 300)
plot(p2)
dev.off()



#-------------------------------------------
# https://r-graph-gallery.com/web-violinplot-with-ggstatsplot.html

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
