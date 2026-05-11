# Chris Oosthuizen
# Feb 2026

# plot discovery curves 2002

#--------------------------------------------
library(ggplot2)
library(ggpubr)
library(scales)

dat = read.csv("./data/discoverycurve2002.csv", header = T)
head(dat)
str(dat)

dat$date = as.POSIXct(dat$date)
str(dat)

dat$date2 <- as.Date(dat$date, format = "%d/%m/%Y")
dat$date2 <- format(dat$date2, "%Y/%m")
dat


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


# plot discovery curve against time

library(scales)

dc1 = ggplot(data = dat,
             aes(x = date, y = CumNew)) + 
  geom_line()+
  geom_point(aes(size = NewIDs)) +
  scale_size_area(max_size = 5, breaks = c(1, 2,5,10), name   = "New IDs") + 
  xlab("Time") +
  ylab("Cumulative individuals") +
  #  scale_x_continuous(expand = c(0, 0), limits = c(50, 200)) + 
  theme_gg() +
  font("xylab",size=14)+
  font("xy",size=14)+
  font("xy.text", size = 14) +
  font("legend.text",size = 14)+
  scale_x_datetime(labels = date_format("%Y/%m"), date_breaks = "2 months") +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5)) +
  theme(
    legend.position=c(0.8,0.3))+
  guides(fill=guide_legend(title="New IDs")) 

dc1

# discovery curve: new vs old individuals
dc2 = ggplot(data = dat,
             aes(x = CumAll, y = CumNew)) + 
  geom_line() + 
  geom_point(aes(size = NewIDs)) +
  scale_size_area(max_size = 5, breaks = c(1,2,5,10), name   = "New IDs") + 
  geom_abline(intercept = 0, slope = 1, size = 0.5, linetype = "dashed") +  # 1:1 line
  xlab("Cumulative identifications") +
  ylab("Cumulative individuals") +
  #  scale_x_continuous(expand = c(0, 0), limits = c(50, 200)) + 
  theme_gg() +
  font("xylab",size=14)+
  font("xy",size=14)+
  font("xy.text", size = 14) +
  font("legend.text",size = 14)+
  theme(
    legend.position=c(0.8,0.3))

dc2

library(patchwork)

dc = dc1 + dc2 + 
  plot_layout(ncol = 2)+
  plot_annotation(tag_levels = 'A')

dc

## Save Plot 
pdf("./supplement/Sup5_discovery_curve_2002.pdf",
    useDingbats = FALSE, width = 8, height = 5)
print(dc)
dev.off()

png(filename = "./supplement/Sup5_discovery_curve_2002.png", width = 2000, height = 1300, 
    pointsize = 8,  res = 300)
plot(dc)
dev.off()



