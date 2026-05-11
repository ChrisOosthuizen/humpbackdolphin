# 
# # Plot encounter rate and Survey effort
# 
# library(tidyverse)
# library(ggplot2)
# library(ggbeeswarm)
# library(ggpubr)
# library(viridisLite)
# 
# dat = read.csv("./data/2026_2014_15_encounter_rate.csv")
# head(dat)
# 
# # Plotting theme
# theme_gg <- function () { 
#   theme_bw() %+replace% 
#     theme(
#       axis.text = element_text(colour = "black"),
#       # axis.title = element_blank(),
#       axis.ticks = element_line(colour = "black"),
#       panel.grid = element_blank(),
#       strip.background = element_blank(),
#       panel.border = element_rect(colour = "black", fill = NA),
#       axis.line = element_line(colour = "black")
#     )
# }
# 
# #----------------------------------
# #plot encounter rate per site
# #----------------------------------
# 
# p1 = ggplot(data = dat,
#             aes(x = as.factor(Site), y = ER,  fill = Site)) + 
#   scale_fill_viridis_d( option = "D")+
#   geom_violin(position = position_dodge(width = .75), linewidth = 1, alpha=0.3, color = NA, show.legend = F) +
#   geom_boxplot(notch = F,  outlier.size = -1, color="black",lwd=0.75, alpha = 0.1, show.legend = F)+
#   ggbeeswarm::geom_quasirandom(shape = 21,
#                                size=2, dodge.width = .75, color="black",alpha = .95,show.legend = F)+
#   
#   theme_gg()+
#   font("xylab",size=16)+
#   font("xy",size=16)+
#   font("xy.text", size = 16) +
#   font("legend.text",size = 16)+
#   ylab("Encounter rate (groups/hr)")  +
#   xlab("")  +
#   scale_x_discrete(labels = c("Knysna", "Plettenberg Bay", "Tsitsikamma"))+
#   #  rremove("legend.title")+
#   guides(fill = guide_legend(override.aes = list(alpha = 0.75,color="black")))
# 
# p1 
# 
# # Add prior Plett data to this graph as points
# # Greenwood thesis :Figure 3: SPUE of boat surveys between 2002-2003 and 2012-2013 (excluding commercial trips)
# er_past <- data.frame(
#   Site = "Plettenberg",
#   er_hour_2002 = 0.107,
#   er_hour_2012 = 0.156
# )
# 
# er_past
# 
# p1 = p1 + 
#   geom_point(data = er_past, aes(x = Site, y = er_hour_2002), 
#              col = "red", shape = 15, size = 4, show.legend = FALSE) +
#   geom_point(data = er_past, aes(x = Site, y = er_hour_2012),
#              col = "red", shape = 17, size = 4, show.legend = FALSE) + 
# 
#   # Manual legend points
#   annotate("point", x = 2.3, y = 0.5, shape = 15, color = "red", size = 4) +
#   annotate("point", x = 2.3, y = 0.45, shape = 17, color = "red", size = 4) +
#   
#   # Manual legend text
#   annotate("text", x = 2.35, y = 0.5, label = "2002/03", hjust = 0, size = 5) +
#   annotate("text", x = 2.35, y = 0.45, label = "2012/13", hjust = 0, size = 5)
# 
# p1
# 
# ## Save Plot 
# pdf("./figures/encounter rate by site.pdf",
#     useDingbats = FALSE, width = 10, height = 7)
# print(p1)
# dev.off()
# 
# png(filename = "./figures/encounter rate by site.png", width = 2000, height = 1300, 
#     pointsize = 8,  res = 300)
# plot(p1)
# dev.off()

# 
# #---------------------------------------------------------
# # Create a month (time) factor
# #---------------------------------------------------------
# dat$Survey = paste(dat$Year, dat$Month, sep = "_")
# unique(dat$Survey)
# dat$Survey = factor(dat$Survey,
#                            levels = c(
#                              "2014_3",
#                              "2014_4",  
#                              "2014_5",
#                              "2014_6",
#                              "2014_7",
#                              "2014_8",
#                              "2014_9",
#                              "2014_10",
#                              "2014_11",
#                              "2014_12",
#                              "2015_1",
#                              "2015_2",
#                              "2015_3",
#                              "2015_4",
#                              "2015_5",
#                              "2015_6"))
# 
# 
# #------------------------------------------
# # plot encounter rate per site per month
# #------------------------------------------
# 
# p2 = ggplot(data = dat,
#             aes(x = as.factor(Survey), y = ER,  fill = Site)) +  facet_wrap(~Site) + 
#   scale_fill_viridis_d( option = "D")+
#   geom_bar(stat = "identity", width = .8, color = "gray", show.legend = F) +
#   coord_flip() +
#   theme_gg()+
#   font("xylab",size=16)+
#   font("xy",size=16)+
#   font("xy.text", size = 14) +
# #  font("legend.text",size = 16)+
#   ylab("Encounter rate")  +
#   xlab("Month")  +
#   #  rremove("legend.title")+
#   theme(legend.position = c(0.85, 0.85))+
# #  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   theme(strip.text.x = element_text(size = 16))+
#   guides(fill = guide_legend(override.aes = list(alpha = 0.75,color="black")))+
#   scale_x_discrete(limits=rev)
# 
# p2
# 
# ## Save Plot 
# pdf("./figures/encounter rate by month.pdf",
#     useDingbats = FALSE, width = 10, height = 7)
# print(p2)
# dev.off()
# 
# png(filename = "./figures/encounter rate by month.png", width = 2000, height = 1300, 
#     pointsize = 8,  res = 300)
# plot(p2)
# dev.off()

# 
# #------------------------------------------
# # plot survey effort per site per month
# #------------------------------------------
# 
# p3 =  ggplot(data = dat, 
#        aes(x = as.factor(Survey), y = Sum.of.NUMERIC.Hours.per.survey.per.month,  fill = Site)) + facet_wrap(~Site) + 
#        scale_fill_viridis_d( option = "D")+
#   geom_bar(stat = "identity", width = .8, color = "gray", show.legend = F) +
#   coord_flip() +
#   theme_gg()+
#   font("xylab",size=16)+
#   font("xy",size=16)+
#   font("xy.text", size = 16) +
#   font("legend.text",size = 16)+
#   ylab("Survey hours")  +
#   xlab("Month")  +
#   #  rremove("legend.title")+
#   theme(legend.position = c(0.85, 0.85))+
# #  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   theme(strip.text.x = element_text(size = 16))+
#   guides(fill = guide_legend(override.aes = list(alpha = 0.75,color="black")))+
#   scale_x_discrete(limits=rev)
# 
# p3
# 
# ## Save Plot 
# pdf("./supplement/survey effort by site.pdf",
#     useDingbats = FALSE, width = 10, height = 7)
# print(p3)
# dev.off()
# 
# png(filename = "./supplement/survey effort by site.png", width = 2000, height = 1300, 
#     pointsize = 8,  res = 300)
# plot(p3)
# dev.off()
# 

# 
# #------------------------------------------
# # plot survey effort overall
# #------------------------------------------
# 
# p4 =  ggplot(data = dat, 
#              aes(x = as.factor(Survey), y = Sum.of.NUMERIC.Hours.per.survey.per.month,  fill = Site)) +
#   #facet_wrap(~Site) + 
#   scale_fill_viridis_d( option = "D")+
#   geom_bar(stat = "identity", width = .8, color = "gray", show.legend = T) +
#   coord_flip() +
#   theme_gg()+
#   font("xylab",size=16)+
#   font("xy",size=16)+
#   font("xy.text", size = 16) +
#   font("legend.text",size = 16)+
#   ylab("Survey hours")  +
#   xlab("Month")  +
#   theme(legend.position = c(0.75, 0.55))+
#   #  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   theme(strip.text.x = element_text(size = 16))+
#   guides(fill = guide_legend(override.aes = list(alpha = 0.75,color="black")))+
#   scale_x_discrete(limits=rev)
# 
# p4
# 
# ## Save Plot 
# pdf("./supplement/survey effort overall.pdf",
#     useDingbats = FALSE, width = 6, height = 7)
# print(p4)
# dev.off()
# 
# png(filename = "./supplement/survey effort overall.png", width = 1500, height = 2000, 
#     pointsize = 8,  res = 300)
# plot(p4)
# dev.off()
# 



#---------------------------------
# 2020s - number of boats trips (survey effort)
#---------------------------------
# 
# dat = read.csv("./data/surveyeffort_2020s.csv")
# head(dat)
# 
# dat$survey2020 = paste(dat$year2020, dat$trip2020, sep = "_")
# dat$survey2021 = paste(dat$year2021, dat$trip2021, sep = "_")
# dat$survey2022 = paste(dat$year2022, dat$trip2022, sep = "_")
# head(dat)
# 
# length(unique(dat$year2020))
# length(unique(dat$survey2020))
# 
# length(unique(dat$year2021))
# length(unique(dat$survey2021))
# 
# length(unique(dat$year2021))
# length(unique(dat$survey2022))
# 
# dat2020 <- dat[!duplicated(dat$survey2020),]
# dat2020 = as.data.frame(table(dat2020$month2020))
# dat2020
# names(dat2020) = c('month', 'n')
# dat2020$year = 2020
# 
# dat2021 <- dat[!duplicated(dat$survey2021),]
# dat2021 = as.data.frame(table(dat2021$month2021))
# dat2021
# names(dat2021) = c('month', 'n')
# dat2021$year = 2021
# 
# dat2022 <- dat[!duplicated(dat$survey2022),]
# dat2022 = as.data.frame(table(dat2022$month2022))
# dat2022
# names(dat2022) = c('month', 'n')
# dat2022$year = 2022
# 
# dat = rbind(dat2020, dat2021, dat2022)
# dat$month = factor(dat$month, 
#                    levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))
# 
# #------------------------------------------
# # plot survey effort per month for 2020s
# #------------------------------------------
# 
# p4 =  ggplot(data = dat, 
#              aes(x = as.factor(month), 
#                  y = n, fill = "#440154" )) + facet_wrap(~year) + 
#   scale_fill_viridis_d( option = "D")+
#   geom_bar(stat = "identity", width = .8, color = "gray", show.legend = F) +
#   coord_flip() +
#   theme_gg()+
#   font("xylab",size=16)+
#   font("xy",size=16)+
#   font("xy.text", size = 16) +
#   font("legend.text",size = 16)+
#   ylab("No. of trips to sea")  +
#   xlab("Month")  +
#   #  rremove("legend.title")+
#   theme(legend.position = c(0.85, 0.85))+
#   #  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   theme(strip.text.x = element_text(size = 16))+
#   #  guides(fill = guide_legend(override.aes = list(alpha = 0.75,color="black")))+
#   scale_x_discrete(limits=rev)
# 
# p4
# 
# ## Save Plot 
# pdf("./figures/survey effort in 2020s.pdf",
#     useDingbats = FALSE, width = 10, height = 7)
# print(p4)
# dev.off()
# 
# png(filename = "./figures/survey effort in 2020s.png", width = 2000, height = 1300, 
#     pointsize = 8,  res = 300)
# plot(p4)
# dev.off()
