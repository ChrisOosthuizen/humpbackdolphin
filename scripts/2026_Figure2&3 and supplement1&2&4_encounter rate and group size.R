
#------------------------------------
# Chris Oosthuizen
# Feb 2026

# Plot encounter rate and Survey effort
#------------------------------------

# load 
library(tidyverse)
library(ggbeeswarm)
library(ggpubr)
library(viridisLite)
library(patchwork)


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

#------------------------------------
# Encounter rate
#------------------------------------

dat = read.csv("./data/2026_2014_15_encounter_rate.csv")
head(dat)

dat$enc_rate = dat$Encounters / dat$Sum.of.NUMERIC.Hours.per.survey.per.month
head(dat)

#-----------------------------------------
# FIGURE 1 A plot encounter rate per site
#-----------------------------------------

p1 = ggplot(data = dat,
            aes(x = as.factor(Site), y = ER,  fill = Site)) + 
  scale_fill_viridis_d( option = "D")+
  geom_violin(position = position_dodge(width = .75), linewidth = 1, alpha=0.3, color = NA, show.legend = F) +
  geom_boxplot(notch = F,  outlier.size = -1, color="black",lwd=0.75, alpha = 0.1, show.legend = F)+
  ggbeeswarm::geom_quasirandom(shape = 21,
                               size=2, dodge.width = .75, color="black",alpha = .95,show.legend = F)+
  
  theme_gg()+
  font("xylab",size=12)+
  font("xy",size=12)+
  font("xy.text", size = 12) +
  font("legend.text",size = 12)+
  ylab("Encounter rate (groups/hr)")  +
  xlab("")  +
  scale_x_discrete(labels = c("Knysna\n2014/15", "Plettenberg Bay\n2014/15", "Tsitsikamma\n2014/15"))+
  #  rremove("legend.title")+
  guides(fill = guide_legend(override.aes = list(alpha = 0.75,color="black")))

p1 


# Add prior Plett data to this graph as points
# Greenwood thesis :Figure 3: SPUE of boat surveys between 2002-2003 and 2012-2013 (excluding commercial trips)
er_past <- data.frame(
  Site = "Plettenberg",
  er_hour_2002 = 0.107,
  er_hour_2012 = 0.156)

er_past

p1 = p1 + 
  geom_point(data = er_past, aes(x = Site, y = er_hour_2002), 
             col = "red", shape = 15, size = 4, show.legend = FALSE) +
  geom_point(data = er_past, aes(x = Site, y = er_hour_2012),
             col = "red", shape = 17, size = 4, show.legend = FALSE) + 
  
  # Manual legend points
  annotate("point", x = 2.3, y = 0.5, shape = 15, color = "red", size = 3) +
  annotate("point", x = 2.3, y = 0.45, shape = 17, color = "red", size = 3) +
  
  # Manual legend text
  annotate("text", x = 2.35, y = 0.5, label = "2002/03", hjust = 0, size = 4) +
  annotate("text", x = 2.35, y = 0.45, label = "2012/13", hjust = 0, size = 4)

p1

## Save Plot 
# pdf("./figures/encounter rate by site.pdf",
#     useDingbats = FALSE, width = 10, height = 7)
# print(p1)
# dev.off()
# 
# png(filename = "./figures/encounter rate by site.png", width = 2000, height = 1300, 
#     pointsize = 8,  res = 300)
# plot(p1)
# dev.off()

# Median ER for plot
vline_data = dat %>%
  group_by(Site) %>%
  summarize(ER = median(ER, na.rm = TRUE))

vline_data 

#---------------------------------------------------------
# Create a month (time) factor
#---------------------------------------------------------
dat$Survey = paste(dat$Year, dat$Month, sep = "_")
unique(dat$Survey)

dat$Survey = factor(dat$Survey,
                    levels = c(
                      "2014_3",
                      "2014_4",  
                      "2014_5",
                      "2014_6",
                      "2014_7",
                      "2014_8",
                      "2014_9",
                      "2014_10",
                      "2014_11",
                      "2014_12",
                      "2015_1",
                      "2015_2",
                      "2015_3",
                      "2015_4",
                      "2015_5",
                      "2015_6"))


#------------------------------------------
# FIGURE 1 B plot encounter rate per site per month
#------------------------------------------

p2 = ggplot(data = dat,
            aes(x = as.factor(Survey), y = enc_rate,  fill = Site)) +  
  facet_wrap(~Site, labeller = labeller(Site = c("Plettenberg" = "Plettenberg Bay"))) + 
  scale_fill_viridis_d( option = "D")+
  geom_bar(stat = "identity", width = .8, color = "gray", show.legend = F) +
  coord_flip() +
  theme_gg()+
  font("xylab",size=12)+
  font("xy",size=12)+
  font("xy.text", size = 12) +
  #  font("legend.text",size = 12)+
  ylab("Encounter rate (groups/hr)")  +
  xlab("Month")  +
  #  rremove("legend.title")+
  theme(legend.position = c(0.85, 0.85))+
  #  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 12))+
  guides(fill = guide_legend(override.aes = list(alpha = 0.75,color="black")))+
  scale_x_discrete(limits=rev, labels = 
    c(
      "6",
      "5",
      "4",
      "3",
      "2",
      "2015_1",
      "12",
      "11",
      "10",
      "9",
      "8",
      "7",
      "6",
      "5",
      "4",
      "2014_3"
      )) #+ 
#    geom_hline(data = vline_data, aes(yintercept = ER),  # Add vertical line with median ER for each site
#      linetype = "dashed", color = "black", linewidth = 0.5) 

p2

## Save Plot 
# pdf("./figures/encounter rate by month.pdf",
#     useDingbats = FALSE, width = 11, height = 7)
# print(p2)
# dev.off()
# 
# png(filename = "./figures/encounter rate by month.png", width = 2500, height = 1300, 
#     pointsize = 8,  res = 300)
# plot(p2)
# dev.off()

#----------------
# combine plots
#----------------

p1_tagged <- p1 + theme(plot.tag.position = c(0.02, 0.97))
p2_tagged <- p2 + theme(plot.tag.position = c(0.02, 0.97))

ERcombo <- p1_tagged + p2_tagged + 
  plot_annotation(tag_levels = list(c('A', 'B')))

ERcombo

# Save Plot 
pdf("./figures/Figure2_encounterrate.pdf",
    useDingbats = FALSE, width = 12, height = 6)
print(ERcombo)
dev.off()

png(filename = "./figures/Figure2_encounterrate.png", width = 3500, height = 1500,
    pointsize = 8,  res = 300)
plot(ERcombo)
dev.off()


#--------------------------------------------------
# Stats: do encounter rates differ between sites? 
#--------------------------------------------------
head(dat)

range(dat$enc_rate, na.rm = TRUE)  # 2 months with no surveys have NaN values

hist(dat$enc_rate, na.rm = TRUE)  # not normal - this is a RATE! 

# exclude 2 months with no survey effort at Tsitsikamma
dat2 = dat %>% 
     dplyr::filter(Sum.of.NUMERIC.Hours.per.survey.per.month > 0) 

# Encounter_rate = count/effort, so we model counts directly
# This is the best option when you have the raw data

model <- glm(Encounters ~ Site + offset(log(Sum.of.NUMERIC.Hours.per.survey.per.month)), 
             data = dat2, 
             family = poisson(link = "log"))

summary(model)

# Check overdispersion
residual_deviance <- 51.206
residual_df <- 43

dispersion <- residual_deviance / residual_df
dispersion  # 1.19


library(emmeans)

# All pairwise comparisons
emm <- emmeans(model, ~ Site, type = "response")
pairs(emm)

# Or see it more clearly
pairs(emm, adjust = "bonferroni")  # With Bonferroni correction
pairs(emm, adjust = "tukey")  # With Bonferroni correction


#------------------------------------------
# SUPPLEMENT: plot survey effort per site per month
#------------------------------------------

p3 =  ggplot(data = dat, 
             aes(x = as.factor(Survey), y = Sum.of.NUMERIC.Hours.per.survey.per.month,  fill = Site)) + 
  facet_wrap(~Site, labeller = labeller(Site = c("Plettenberg" = "Plettenberg Bay"))) + 
  scale_fill_viridis_d( option = "D")+
  geom_bar(stat = "identity", width = .8, color = "gray", show.legend = F) +
  coord_flip() +
  theme_gg()+
  font("xylab",size=12)+
  font("xy",size=12)+
  font("xy.text", size = 12) +
  font("legend.text",size = 12)+
  ylab("Survey hours")  +
  xlab("Month")  +
  #  rremove("legend.title")+
  theme(legend.position = c(0.85, 0.85))+
  #  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 12))+
  guides(fill = guide_legend(override.aes = list(alpha = 0.75,color="black")))+
  scale_x_discrete(limits=rev)

p3

## Save Plot 
pdf("./supplement/Sup1_survey effort by site.pdf",
    useDingbats = FALSE, width = 10, height = 7)
print(p3)
dev.off()

png(filename = "./supplement/Sup1_survey effort by site.png", width = 2000, height = 1300, 
    pointsize = 8,  res = 300)
plot(p3)
dev.off()


#------------------------------------------
# plot survey effort overall
#------------------------------------------

p4 =  ggplot(data = dat, 
             aes(x = as.factor(Survey), y = Sum.of.NUMERIC.Hours.per.survey.per.month,  fill = Site)) +
  #facet_wrap(~Site) + 
  scale_fill_viridis_d(option = "D", labels = c("Plettenberg" = "Plettenberg Bay")) +
  geom_bar(stat = "identity", width = .8, color = "gray", show.legend = T) +
  coord_flip() +
  theme_gg()+
  font("xylab",size=16)+
  font("xy",size=16)+
  font("xy.text", size = 16) +
  font("legend.text",size = 16)+
  ylab("Survey hours")  +
  xlab("Month")  +
  theme(legend.position = c(0.75, 0.55))+
  #  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 16))+
  guides(fill = guide_legend(override.aes = list(alpha = 0.75,color="black")))+
  scale_x_discrete(limits=rev)

p4

## Save Plot 
pdf("./supplement/Sup1_survey effort overall.pdf",
    useDingbats = FALSE, width = 6, height = 7)
print(p4)
dev.off()

png(filename = "./supplement/Sup1_survey effort overall.png", width = 1700, height = 2000, 
    pointsize = 8,  res = 300)
plot(p4)
dev.off()



#--------------------------------
# FIGURE 1 C: Group sizes
#--------------------------------

# load data
df = read.csv("./data/2026_group_size.csv")
head(df)

df <- df %>% 
 dplyr::select(Knysna_2014.15, Plettenberg_2014.15, Tsitsikamma_2014.15) %>%
 pivot_longer(
    cols = ends_with("_2014.15"),
    names_to = "Survey_period",
    values_to = "value",
    values_drop_na = TRUE  # Drop rows where value is NA
  ) %>% 
  arrange(Survey_period)

df
unique(df$Survey_period)
dim(df)

# # make long format data frame for ggplot
# df = df %>% 
#   dplyr::select(Max_2014.15, Min_2014.15, Best_2014.15)
#   pivot_longer(names_to = "Survey_period",  cols = -c(Max_2014.15, Min_2014.15, Best_2014.15))  %>% 
#   arrange(Survey_period)

# # select only 2014/15 data
# df1 = subset(df, df$Survey_period == "Plettenberg_2014.15" |
#                 df$Survey_period ==  "Tsitsikamma_2014.15"| 
#                 df$Survey_period == "Knysna_2014.15")

# set factor levels
df$Survey_period = factor(df$Survey_period,
                            levels = c(
                              "Knysna_2014.15", "Plettenberg_2014.15",  "Tsitsikamma_2014.15" 
                            ))

dim(df)

# mean group size with all sigthings
mean(df$value)
sd(df$value)

#------------
# Statistics
#------------
df %>% 
  group_by(Survey_period) %>%
  summarise(Mean_group = mean(value),
            SD_group = sd(value))

# # PLet 
# group_plet = subset(df, df$Survey_period == 'Plettenberg_2014.15')
# mean(group_plet$value)
# sd(group_plet$value)

# Subset singletons
df_1 <- subset(df, value == 1)
nrow(df_1)  # number of singletons
df_1 %>% 
  group_by(Survey_period) %>%
  tally()
  
#--------------------------------------
# Limit group size to 2 and more individuals. 
#--------------------------------------

df <- subset(df, value > 1)
mean(df$value)  # mean group size with 2 and more
sd(df$value)  # mean group size with 2 and more

#------------
# Statistics
#------------
df %>% 
  group_by(Survey_period) %>%
  summarise(Mean_group = mean(value),
            SD_group = sd(value))



# PLet 
group_plet_2plus = subset(df, df$Survey_period == 'Plettenberg_2014.15')
mean(group_plet_2plus$value)
sd(group_plet_2plus$value)


#--------------------------------------------------
# plot group size by section and year (2020s)
#--------------------------------------------------

#plot
g1 = ggplot(data = df,
            aes(x = Survey_period, y = value,  fill = Survey_period))+
  scale_fill_viridis_d( option = "D", direction = 1)+
  geom_violin(position = position_dodge(width = .75), linewidth = 1, alpha=0.3, color = NA, show.legend = F) +
  geom_boxplot(notch = F,  outlier.size = -1, color="black",lwd=0.75, alpha = 0.1, show.legend = F)+
  ggbeeswarm::geom_quasirandom(shape = 21,
                               size=2, dodge.width = .75, color="black",alpha = .95,show.legend = F)+
  
  theme_gg()+
  font("xylab",size=12)+
  font("xy",size=12)+
  font("xy.text", size = 12) +
  font("legend.text",size = 12)+
  ylab(  c("Group size")  )  +
  xlab(  c("")  )  +
  scale_x_discrete(labels = c("Knysna\n2014/15", "Plettenberg Bay\n2014/15", "Tsitsikamma\n2014/15"))+
  #  rremove("legend.title")+
  guides(fill = guide_legend(override.aes = list(alpha = 0.75,color="black")))+
  theme(plot.margin=grid::unit(c(5,0,5,5), "mm")) + # (top, right, bottom, and left)  + 
  scale_y_continuous(limits = c(0, 12), breaks = scales::pretty_breaks(n = 5))

g1 

# Add singletons

g1 = g1 +
  ggbeeswarm::geom_quasirandom(data = df_1, 
                               aes(x = Survey_period, y = value,  fill = Survey_period),
                               shape = 22, 
                               size=2.1, dodge.width = .75,  color = "black", fill = "blue", 
                               alpha = 1, show.legend = F)
   
g1

# Add prior Plett data to this graph as points
# Greenwood thesis : group sizes
group_past <- data.frame(
  Survey_period = "Plettenberg_2014.15",
  group_1999 = 5,
  group_2002 = 9,
  group_2012 = 4.7)

group_past

# greenwood group size data
greenwood_groupsize = data.frame(
          value = c(2,2,6,11,8,5,8,2,4,5,1,6,3,6,4,1,8,6,8,8,1,1,5,6,1,4))
mean(greenwood_groupsize$value)  # group size with singletons

# Subset singletons
greenwood_groupsize_1 = subset(greenwood_groupsize, value == 1)
nrow(greenwood_groupsize_1)  # how many singletons

# Limit group size to 2 and more individuals. 
greenwood_groupsize_2 = subset(greenwood_groupsize, value > 1)
mean(greenwood_groupsize_2$value)
sd(greenwood_groupsize_2$value)


#-------------------------------------------------------------
# Is 2014 Plett groups less than Greenwoods?
#-------------------------------------------------------------

wilcox.test(greenwood_groupsize_2$value, group_plet_2plus$value)

#-------------------------------------------------------------

g1 = g1 + 
  geom_point(data = group_past, aes(x = Survey_period, y = group_1999), 
             col = "red", shape = 16, size = 4, show.legend = FALSE) +
  geom_point(data = group_past, aes(x = Survey_period, y = group_2002), 
             col = "red", shape = 15, size = 4, show.legend = FALSE) +
  geom_point(data = group_past, aes(x = Survey_period, y = group_2012),
             col = "red", shape = 17, size = 4, show.legend = FALSE) + 
  
  # Manual legend points
  annotate("point", x = 2.3, y = 11, shape = 16, color = "red", size = 3) +
  annotate("point", x = 2.3, y = 10, shape = 15, color = "red", size = 3) +
  annotate("point", x = 2.3, y = 9, shape = 17, color = "red", size = 3) +
  
  # Manual legend text
  annotate("text", x = 2.35, y = 11, label = "1999-2004", hjust = 0, size = 4) +
  annotate("text", x = 2.35, y = 10, label = "2002/03", hjust = 0, size = 4) +
  annotate("text", x = 2.35, y = 9, label = "2012/13", hjust = 0, size = 4)

g1


# Save Plot 
# pdf("./figures/2026_figure3_group size.pdf",
#     useDingbats = FALSE, width = 10, height = 7)
# print(g1)
# dev.off()
# 
# png(filename = "./figures/2026_figure3_group size.png", width = 2000, height = 1300,
#     pointsize = 8,  res = 300)
# plot(g1)
# dev.off()

#----------------------------------
# Plot histogram of group size
#----------------------------------

group_hist <- ggplot(df, aes(x = value)) +
  geom_bar(color = "black", fill = "steelblue") +
  scale_x_continuous(breaks = 1:12) +
  labs(x = "Group size",
       y = "Frequency (number of groups)") +
  theme_gg() +
  theme(legend.position = c(0.85, 0.85))

group_hist


# # Save Plot 
# pdf("./figures/2026_figure3_group size_hist.pdf",
#     useDingbats = FALSE, width = 5, height = 5)
# print(group_hist)
# dev.off()
# 
# png(filename = "./figures/2026_figure3_group size_hist.png", width = 1200, height = 1200,
#     pointsize = 8,  res = 300)
# plot(group_hist)
# dev.off()


# Combine group sizes with library(patchwork)

g1_themed <- g1 + theme(plot.tag.position = c(0.01, 1))
group_hist_themed <- group_hist + theme(plot.tag.position = c(0.05, 1))

group_combo <- g1_themed + group_hist_themed + 
  plot_annotation(tag_levels = list(c('A', 'B'))) +
 # plot_layout(widths = c(2, 1))  # 2/3 for g1, 1/3 for group_hist
  plot_layout(widths = c(0.6, 0.4))

group_combo

# Save Plot 
pdf("./figures/Figure3_group_size.pdf",
    useDingbats = FALSE, width = 10, height = 5)
print(group_combo)
dev.off()

png(filename = "./figures/Figure3_group_size.png", width = 3200, height = 1500,
    pointsize = 8,  res = 300)
plot(group_combo)
dev.off()

#------------
# Statistics
#------------
df %>% 
  group_by(Survey_period) %>%
  summarise(Mean_group = mean(value),
            SD_group = sd(value))


#------------
# Statistics
#------------
df %>% 
  group_by(Survey_period) %>%
  summarise(Mean_group = mean(value),
            SD_group = sd(value))


hist(df$value)

# Poisson GLM (count data)
model <- glm(value ~ Survey_period, 
             family = poisson(link = "log"), 
             data = df)
summary(model)

# Check overdispersion
model$deviance / model$df.residual  # Should be ~1

# Post-hoc comparisons
library(emmeans)
emmeans(model, pairwise ~ Survey_period, type = "response")

#Negative Binomial (a bit overdispersed)
library(MASS)

# Better for overdispersed count data
model_nb <- glm.nb(value ~ Survey_period, data = df)
summary(model_nb)

# Post-hoc
library(emmeans)
emmeans(model_nb, pairwise ~ Survey_period, type = "response")


#--------------------------------------------------
# plot min, best and max estimates of group size
#--------------------------------------------------

# load data
gdat = read.csv("./data/2026_group_size.csv")
head(gdat)

gdat = gdat %>%
   dplyr::select(Max_2014.15, Min_2014.15, Best_2014.15)

gdat   

98*3

# make long format
gdat2 = gdat %>% 
  rename(Maximum= Max_2014.15, Minimum = Min_2014.15, Best = Best_2014.15) %>%
  pivot_longer(names_to = "Variability",  cols = everything())

gdat2

unique(gdat2$Variability)
range(gdat2$value)
mean(gdat2$value)

#----------------------------------
# Only select groups larger than 1
#----------------------------------

gdat3 = subset(gdat2, gdat2$value > 1) 

# select the singletons
gdat4 = subset(gdat2, gdat2$value == 1) 

gdat3$Variability = factor(gdat3$Variability,
                          levels = c(
                            "Minimum",
                            "Best",
                            "Maximum"     
                          ))

gdat4$Variability = factor(gdat4$Variability,
                           levels = c(
                             "Minimum",
                             "Best",
                             "Maximum"     
                           ))

# Mean group size per group (no singletons)
gdat3 %>%
  group_by(Variability) %>%
  summarize(mean = mean(value))

# singleton sample sizes
gdat4 %>%
  group_by(Variability) %>%
  summarize(N = n())



#plot
px = ggplot(data = gdat3,
            aes(x = Variability, y = value,  fill = Variability))+
  # scale_fill_viridis_d( option = "F")+
  geom_violin(position = position_dodge(width = .75), linewidth = 1, alpha=0.3, color = NA, show.legend = F) +
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
  scale_y_continuous(limits = c(0, 16), breaks = scales::pretty_breaks(n = 5)) + 

  guides(fill = guide_legend(override.aes = list(alpha = 0.75, color="black")))

px 

# Add singletons

px = px +
  ggbeeswarm::geom_quasirandom(data = gdat4, 
                               aes(x = Variability, y = value,  fill = Variability),
                               shape = 22, 
                               size=2.1, dodge.width = .75,  color = "black", fill = "blue", 
                               alpha = 1, show.legend = F)

px

## Save Plot 
pdf("./supplement/Sup2_group size min max.pdf",
    useDingbats = FALSE, width = 10, height = 7)
print(px )
dev.off()

png(filename = "./supplement/Sup2_group size min max.png", width = 2000, height = 1300, 
    pointsize = 8,  res = 300)
plot(px )
dev.off()

