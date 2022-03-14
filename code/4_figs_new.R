# Figures & Tables for temp/salin effects on juv whitefishes
# Justin Priest
# justin.priest@alaska.gov
# 15 March 2022



library(tidyverse)
library(here)
library(gridExtra)
library(grid)
library(scales)
library(extrafont)
library(cowplot)
library(patchwork)
# font_import()  #this only needs to be run once
loadfonts(device="win") #change for mac users



#source(here::here("code/2_growth_summary.R"))
source(here::here("code/3_abund_summary.R"))


#### SET COLORS ####
arcs0_color = "#2A6A79"
arcs1_color = "#2A6A79"
bdwf0_color = "#F0803C"
bdwf1_color = "#F0803C"




###################################
####  FIGURE 2 - ENV BOXPLOTS  ####
###################################

# Env Var
tempbyyear <- addbiwknum(env_allyears) %>% group_by(Year, Station, biweekly) %>% 
  summarise(meantemp = mean(Temp_Top, na.rm = TRUE)) %>% ungroup() %>% 
  ggplot(aes(x=Year, y=meantemp, group = Year)) + 
  geom_boxplot() + 
  scale_x_continuous(breaks = seq(from=2001, to=2018, by=2)) +
  scale_y_continuous(limits = c(2.5, 12.5), breaks = seq(from=2.5, to = 12.5, by=2.5)) +
  labs(x = "", y = "Mean temperature (°C)") + 
  #theme_sleek()+
  theme_crisp() +
  theme(text=element_text(family="Arial", size=12))


salinbyyear <- addbiwknum(env_allyears) %>% group_by(Year, Station, biweekly) %>% 
  summarise(meansalin = mean(Salin_Top, na.rm = TRUE)) %>% ungroup() %>% 
  ggplot(aes(x=Year, y=meansalin, group = Year)) + 
  geom_boxplot() + 
  scale_x_continuous(breaks = seq(from=2001, to=2018, by=2)) +
  scale_y_continuous(limits = c(0, 29), breaks = seq(from=0, to = 25, by=5)) +
  labs(x = "", y = "Mean salinity (psu)") + 
  #theme_sleek() +
  theme_crisp() +
  theme(text=element_text(family="Arial", size=12))


tempsalinyr <- cowplot::plot_grid(tempbyyear, salinbyyear, labels = c("a", "b"), 
                                  label_fontfamily = "Arial",
                                  label_x = 0.11, label_y = 0.96, ncol =1, align = 'v')
tempsalinyr
# ggsave(here::here("plotexports/fig2_tempsalinyr_final.png"), plot = tempsalinyr, 
#        dpi = 600, width = 7.5, height = 5, units = "in")






###################################################
####  IN-TEXT NUMBERS - ENVIRONMENTAL SUMMARY  ####
###################################################

addbiwknum(env_allyears) %>% group_by(Year, biweekly) %>%
  summarise(meanbiwktemp = mean(Temp_Top, na.rm = TRUE)) %>% ungroup() %>%
  summarise(meantemp = mean(meanbiwktemp, na.rm = TRUE), 
            sdtemp = sd(meanbiwktemp, na.rm = TRUE),
            mintemp = min(meanbiwktemp, na.rm = TRUE), 
            maxtemp = max(meanbiwktemp, na.rm = TRUE))
addbiwknum(env_allyears) %>% group_by(Year, biweekly) %>%
  summarise(meanbiwktemp = mean(Temp_Top, na.rm = TRUE)) %>% group_by(Year) %>%
  summarise(meantemp = mean(meanbiwktemp, na.rm = TRUE)) %>% top_n(1, meantemp) #2012 highest temp
addbiwknum(env_allyears) %>% group_by(Year, biweekly) %>%
  summarise(meanbiwktemp = mean(Temp_Top, na.rm = TRUE)) %>% group_by(Year) %>%
  summarise(meantemp = mean(meanbiwktemp, na.rm = TRUE)) %>% top_n(-1, meantemp) # 2001 lowest temp
summary(lm(meanbiwktemp ~ Year, data = addbiwknum(env_allyears) %>% 
             group_by(Year, biweekly) %>% 
             summarise(meanbiwktemp = mean(Temp_Top, na.rm = TRUE)))) # signif increase (0.088/yr)


addbiwknum(env_allyears) %>% group_by(Year, biweekly) %>%
  summarise(meanbiwksal = mean(Salin_Top, na.rm = TRUE)) %>% ungroup() %>%
  summarise(meansal = mean(meanbiwksal, na.rm = TRUE), 
            sdsal = sd(meanbiwksal, na.rm = TRUE),
            minsal = min(meanbiwksal, na.rm = TRUE), 
            maxsal = max(meanbiwksal, na.rm = TRUE))
addbiwknum(env_allyears) %>% group_by(Year, biweekly) %>%
  summarise(meanbiwksal = mean(Salin_Top, na.rm = TRUE)) %>% group_by(Year) %>%
  summarise(meansal = mean(meanbiwksal, na.rm = TRUE)) %>% top_n(1, meansal) #2011 highest salin
addbiwknum(env_allyears) %>% group_by(Year, biweekly) %>%
  summarise(meanbiwksal = mean(Salin_Top, na.rm = TRUE)) %>% group_by(Year) %>%
  summarise(meansal = mean(meanbiwksal, na.rm = TRUE)) %>% top_n(-1, meansal) # 2001 lowest salin
summary(lm(meanbiwksalin ~ Year, data = addbiwknum(env_allyears) %>% 
             group_by(Year, biweekly) %>% 
             summarise(meanbiwksalin = mean(Salin_Top, na.rm = TRUE)))) # no signif change









########################################
####  FIGURE 3 - TEMP/SALIN BIWEEK  ####
########################################


tempbiwk <- addbiwknum(env_allyears) %>%
  group_by(Year, biweekly) %>% summarise(meantemp = mean(Temp_Top, na.rm = TRUE))
summary(gam(meantemp ~ s(biweekly, k=4), data = tempbiwk)) # significant curvature. Use LOESS smoothing

tempbiwkplot <- ggplot(data = tempbiwk, mapping = aes(x=biweekly, y = meantemp)) + 
  geom_point(color = "#5e5e5e") + 
  geom_smooth(method = "loess", se=FALSE, size=2, span=1.25, color = "black") +
  scale_x_continuous(breaks = 1:4) +
  scale_y_continuous(limits = c(2.5, 12), breaks = seq(from=3, to=12, by=3), oob=scales::rescale_none) +
  labs(x = "", y = "Mean Water Temperature (°C)") +
  theme_crisp() +
  theme(text=element_text(family = "Arial", size = 12), 
        axis.title.x = element_blank())
tempbiwkplot


salinitybiwk <- addbiwknum(env_allyears) %>%
  group_by(Year, biweekly) %>% summarise(meansal = mean(Salin_Top, na.rm = TRUE))
summary(gam(meansal ~ s(biweekly, k=4), data = salinitybiwk)) # significant curvature. Use LOESS smoothing

salinbiwkplot <- ggplot(data = salinitybiwk, mapping = aes(x=biweekly, y = meansal)) + 
  geom_point(color = "#5e5e5e") + 
  geom_smooth(method = "loess", se=FALSE, size=2, span=1.25, color = "black") +
  scale_x_continuous(breaks = 1:4) +
  scale_y_continuous(limits = c(0, 21.5), breaks = seq(from=0, to=20, by = 5), oob=scales::rescale_none) +
  labs(x= "", y = "Mean Salinity (psu)") +
  theme_crisp() +
  theme(text=element_text(family="Arial", size=12), axis.title.x=element_blank())
salinbiwkplot



environbiwk <- cowplot::plot_grid(tempbiwkplot, salinbiwkplot, labels = c("a", "b"), 
                                  label_fontfamily = "Arial",
                                  label_x = 0.12, label_y = 0.96, ncol = 2, align = 'v')
environbiwk

environbiwk <- grid.arrange(arrangeGrob(environbiwk, 
                                        bottom = textGrob("Biweekly Period", 
                                                          gp=gpar(fontfamily = "Arial", fontsize=12))))
# ggsave(here::here("plotexports/fig3_environbiwk_final.png"), plot = environbiwk,
#        dpi = 600, width = 7.5, height = 5, units = "in")








#################################################
####  IN-TEXT NUMBERS - CATCH/GROWTH BY AGE  ####
#################################################

agesummaryabund&growth
allcatch.arcsages %>% filter(age.put == "age0" | age.put == "age1") %>% 
  group_by(Year, age.put) %>% summarise(total = sum(totalages)) %>% 
  spread(age.put, total) %>% rename("ARCS_age0" = "age0", "ARCS_age1" = "age1") %>%
  left_join(allcatch.bdwfages %>% filter(age.put == "age0" | age.put == "age1") %>% 
              group_by(Year, age.put) %>% summarise(total = sum(totalages)) %>% 
              spread(age.put, total) %>% rename("BDWF_age0" = "age0", "BDWF_age1" = "age1"),
            by = c("Year" = "Year")) #%>% View() # view this, then copy and paste it into Excel for formatting, Table 1

summary(lm(ARCS_age0 ~ Year, data = temp3)) # change above to "temp3" then run these. None are significant
summary(lm(ARCS_age1 ~ Year, data = temp3)) # Catch did not change significantly
summary(lm(BDWF_age0 ~ Year, data = temp3))
summary(lm(BDWF_age1 ~ Year, data = temp3))


# Overall mean lengths and SD for Table 2
addbiwknum(indivfish.arcsages) %>% filter(age.put == "age0") %>% 
  summarise(meanlen = mean(Length), n = sum(totcount), SD = sd(Length)) 
addbiwknum(indivfish.arcsages) %>% filter(age.put == "age1") %>% 
  summarise(meanlen = mean(Length), n = sum(totcount), SD = sd(Length)) 
addbiwknum(indivfish.bdwfages) %>% filter(age.put == "age0") %>% 
  summarise(meanlen = mean(Length), n = sum(totcount), SD = sd(Length)) 
addbiwknum(indivfish.bdwfages) %>% filter(age.put == "age1") %>% 
  summarise(meanlen = mean(Length), n = sum(totcount), SD = sd(Length)) 


# Age by year summary, extracted for Table 2
addbiwknum(indivfish.arcsages) %>% filter(age.put == "age0") %>% 
  group_by(Year) %>% summarise(meanlen = mean(Length), n = sum(totcount), SD = sd(Length)) 
addbiwknum(indivfish.arcsages) %>% filter(age.put == "age1") %>% 
  group_by(Year) %>% summarise(meanlen = mean(Length), n = sum(totcount), SD = sd(Length)) 
addbiwknum(indivfish.bdwfages) %>% filter(age.put == "age0") %>% 
  group_by(Year) %>% summarise(meanlen = mean(Length), n = sum(totcount), SD = sd(Length)) 
addbiwknum(indivfish.bdwfages) %>% filter(age.put == "age1") %>% 
  group_by(Year) %>% summarise(meanlen = mean(Length), n = sum(totcount), SD = sd(Length)) 


# Did mean length change over time?
summary(lm(meanlen ~ Year, data = addbiwknum(indivfish.arcsages) %>% filter(age.put == "age0") %>% 
             group_by(Year) %>% summarise(meanlen = mean(Length))))
summary(lm(meanlen ~ Year, data = addbiwknum(indivfish.arcsages) %>% filter(age.put == "age1") %>% 
             group_by(Year) %>% summarise(meanlen = mean(Length))))

summary(lm(meanlen ~ Year, data = addbiwknum(indivfish.bdwfages) %>% filter(age.put == "age0") %>% 
             group_by(Year) %>% summarise(meanlen = mean(Length))))
summary(lm(meanlen ~ Year, data = addbiwknum(indivfish.bdwfages) %>% filter(age.put == "age1") %>% 
             group_by(Year) %>% summarise(meanlen = mean(Length))))
# No change on yearly basis. If you add biweekly, age-1 ARCS mean length has significantly increased

# However, length has DEFINITELY increased over time
summary(lm(Length ~ Year, data = addbiwknum(indivfish.arcsages) %>% filter(age.put == "age0")))
summary(lm(Length ~ Year, data = addbiwknum(indivfish.arcsages) %>% filter(age.put == "age1")))

summary(lm(Length ~ Year, data = addbiwknum(indivfish.bdwfages) %>% filter(age.put == "age0")))
summary(lm(Length ~ Year, data = addbiwknum(indivfish.bdwfages) %>% filter(age.put == "age1")))


# ARCS age-0 increased from 74.788 to 83.445
(2001*0.50927)-944.261; (2018*0.50927)-944.261

# ARCS age-1 increased from 105.856 to 120.408
(2001*.8560)-1607; (2018*0.8560)-1607

# BDWF age-0 increased from 67.643 to 68.314
(2001*0.03948)-11.356; (2018*0.03948)-11.356

# BDWF age-1 increased from 106.177 to 117.426
(2001*0.6617)-1217.8844; (2018*0.6617)-1217.8844









##########################################
####  FIGURE 4 - MEAN LENGTH BY YEAR  ####
##########################################

growthovertimedat <- addbiwknum(indivfish.arcsages) %>% 
  filter(age.put == "age0" | age.put == "age1", biweekly == 4) %>% 
  group_by(Year,Species, age.put) %>%
  summarise(meanlen = mean(Length),
            n = sum(totcount)) %>% ungroup() %>% #end left_join 
  full_join(
    addbiwknum(indivfish.bdwfages) %>% 
      filter(age.put == "age0" | age.put == "age1", biweekly == 4) %>% 
      group_by(Year,Species, age.put) %>%
      summarise(meanlen = mean(Length),
                n = sum(totcount)) %>% ungroup()) %>% 
  
  filter(n > 30) %>% # remove years with very low counts so as not to skew it
  right_join(expand.grid(Year = 2001:2018, Species = c("ARCS", "BDWF"), 
                         age.put=c("age0", "age1")) ) # this will make sure no gaps 

growthovertime <- ggplot(growthovertimedat,
                         aes(x=Year, y = meanlen, group = interaction(Species, age.put), #this allows grouping by two levels
                             color = Species, fill = Species)) + 
  geom_point(shape = 21, size = 4) +
  geom_line(size=2) + 
  scale_x_continuous(breaks = seq(from=2001, to=2018, by=2)) +
  scale_color_manual(values=c(arcs0_color, bdwf0_color), guide = "none") +
  scale_fill_manual(labels = c("Arctic Cisco", "Broad Whitefish"), values=c(arcs0_color, bdwf0_color)) +
  labs(x="", y = "Mean Length (mm)", fill = "") +
  annotate("text", x = 2002, y = 85, label="Age 0", family="Arial", fontface = "bold") +
  annotate("text", x = 2002, y = 135, label="Age 1", family="Arial", fontface = "bold") +
  theme_crisp() +
  theme(text=element_text(family="Arial", size=12))
growthovertime
# ggsave(here::here("plotexports/fig4_growthovertime_color.png"), plot = growthovertime, 
#        dpi = 600, width = 7.5, height = 5, units = "in")












##############################################
####  FIGURE 5 - BIWEEKLY GROWTH BOXPLOT  ####
##############################################

growth.biwk %>% dplyr::select(Year, biweekly, arcs0.growth, arcs1.growth, 
                              bdwf0.growth, bdwf1.growth) %>% 
  gather(Sp.age, growth, -Year, -biweekly) %>%
  group_by(Sp.age) %>%
  summarise(meangrowth = mean(growth, na.rm = TRUE),
            mingrowth = min(growth, na.rm = TRUE),
            maxgrowth = max(growth, na.rm = TRUE), 
            sdgrowth = sd(growth, na.rm = TRUE))


allgrowthplot <- growth.biwk %>% dplyr::select(Year, biweekly, arcs0.growth, arcs1.growth, 
                                               bdwf0.growth, bdwf1.growth) %>% 
  gather(Sp.age, growth, -Year, -biweekly) %>% #make df of growth & merge with intercepts
  mutate(Sp.age = substr(Sp.age, 1,5),
         Sp.age = replace(Sp.age, Sp.age=="arcs0", "Arctic Cisco Age-0"),
         Sp.age = replace(Sp.age, Sp.age=="arcs1", "Arctic Cisco Age-1"),
         Sp.age = replace(Sp.age, Sp.age=="bdwf0", "Broad Whitefish Age-0"),
         Sp.age = replace(Sp.age, Sp.age=="bdwf1", "Broad Whitefish Age-1")) %>%
  ggplot(aes(x = as.factor(biweekly), y = growth, fill = Sp.age)) + 
  geom_boxplot() + 
  scale_fill_manual(values=c(arcs0_color, arcs1_color, bdwf0_color, bdwf1_color)) +
  labs(x="Biweekly Period", y = "Growth (mm/day)") +
  theme_crisp() +
  theme(legend.position = "none",
        text=element_text(family="Arial", size=12),
        axis.text.x = element_text(family="Arial", size=12)) + 
  facet_wrap(. ~ Sp.age, nrow =1) +
  geom_text(data = data.frame(label = c("a", "b", "c", "d"), 
                              Sp.age = c("Arctic Cisco Age-0", "Arctic Cisco Age-1", 
                                         "Broad Whitefish Age-0", "Broad Whitefish Age-1")),
            mapping = aes(x = -Inf, y = -Inf, label = label),
            hjust = -1, vjust = -23, # manually adjust
            family="Arial", fontface = "bold", color = "black")
allgrowthplot
# ggsave(here::here("plotexports/fig5_allgrowthplot_color.png"), plot = allgrowthplot, 
#        dpi = 600, width = 7.2, height = 3.6, units = "in") # had to adjust width


#Test if there are growth differences between biweekly periods

summary(aov(arcs0.growth ~ biweekly, data = growth.biwk %>% mutate(biweekly = as.factor(biweekly))))
summary(aov(arcs1.growth ~ biweekly, data = growth.biwk %>% mutate(biweekly = as.factor(biweekly))))
summary(aov(bdwf0.growth ~ biweekly, data = growth.biwk %>% mutate(biweekly = as.factor(biweekly))))
summary(aov(bdwf1.growth ~ biweekly, data = growth.biwk %>% mutate(biweekly = as.factor(biweekly))))
# Only BDWF age-1 have signif diffs btwn biweekly periods
TukeyHSD(aov(bdwf1.growth ~ biweekly, 
             data = growth.biwk %>% mutate(biweekly = as.factor(biweekly)))) #diffs are 4 to 1&2









###########################################
####  FIGURE 6 - TEMP VS PRED GROWTH   ####
###########################################

growthtemp <- ggplot(pred_growth %>% 
                       filter(Year == 2013, meansalin == 16, biweekly == 2) %>% 
                       # arbitrarily chose the above values as constants to best show fit
                       group_by(Sp.Age, meantemp) %>% 
                       summarise(pred.gam = mean(preds),
                                 pred.gam.se = mean(pred.se)) %>% ungroup() %>%
                       mutate(Sp.Age = replace(Sp.Age, Sp.Age=="ARCS_Age0", "Arctic Cisco Age-0"),
                              Sp.Age = replace(Sp.Age, Sp.Age=="ARCS_Age1", "Arctic Cisco Age-1"),
                              Sp.Age = replace(Sp.Age, Sp.Age=="BDWF_Age0", "Broad Whitefish Age-0"),
                              Sp.Age = replace(Sp.Age, Sp.Age=="BDWF_Age1", "Broad Whitefish Age-1")), 
                     aes(x=meantemp, y=pred.gam, color = Sp.Age)) + 
  geom_ribbon(aes(x=meantemp, ymin=pred.gam-(1.96*pred.gam.se), ymax=pred.gam+(1.96*pred.gam.se)), 
              alpha = 0.3, inherit.aes = FALSE) + 
  geom_line(size = 1.5) +
  scale_color_manual(values=c(arcs0_color, arcs1_color, bdwf0_color, bdwf1_color)) +
  scale_fill_manual(values=c(arcs0_color, arcs1_color, bdwf0_color, bdwf1_color)) +
  scale_y_continuous("Predicted Growth (mm/day)", limits = c(0, 1.6), oob=scales::rescale_none) +
  scale_x_continuous("Water Temperature (°C)", limits = c(2,12.6), breaks = seq(from=2.5, to = 12.5, by = 2.5)) +
  geom_point(data = pointvals %>% 
               mutate(Sp.Age = replace(Sp.Age, Sp.Age=="ARCS_Age0", "Arctic Cisco Age-0"),
                      Sp.Age = replace(Sp.Age, Sp.Age=="ARCS_Age1", "Arctic Cisco Age-1"),
                      Sp.Age = replace(Sp.Age, Sp.Age=="BDWF_Age0", "Broad Whitefish Age-0"),
                      Sp.Age = replace(Sp.Age, Sp.Age=="BDWF_Age1", "Broad Whitefish Age-1")), 
             aes(fill = Sp.Age), shape = 21, color = "black") +
  theme_crisp() +
  theme(legend.position = "none",
        text=element_text(family="Arial", size=12),
        axis.text.x = element_text(family="Arial", size=12),
        panel.spacing = unit(0.75, "lines")) + # this increases gap between facet panels
  facet_wrap(.~Sp.Age, nrow = 2) +
  geom_text(data = data.frame(label = c("a", "b", "c", "d"), 
                              Sp.Age = c("Arctic Cisco Age-0", "Arctic Cisco Age-1", "Broad Whitefish Age-0",
                                         "Broad Whitefish Age-1")),
            mapping = aes(x = -Inf, y = -Inf, label = label),
            hjust = -1, vjust = -15, 
            family="Arial", fontface = "bold", color = "black")

growthtemp
# ggsave(here::here("plotexports/fig6_growthtemp_color.png"), plot = growthtemp, 
#        dpi = 600, width = 7.5, height = 5, units = "in")





###############################################
####  FIGURE 7 - SALINITY VS PRED GROWTH   ####
###############################################

growthsalin <- ggplot(pred_growth %>% filter(Sp.Age == "ARCS_Age0") %>% 
                        filter(Year == 2013, meantemp == 8, biweekly == 2) %>%
                        # chose these as they are representative midpoints
                        mutate(Sp.Age = replace(Sp.Age, Sp.Age=="ARCS_Age0", "Arctic Cisco Age-0")) %>%
                        group_by(Sp.Age, meansalin) %>% 
                        summarise(pred.gam = mean(preds),
                                  pred.gam.se = mean(pred.se)) %>%
                        filter(meansalin >=7), # remove predictions outside observed
                      aes(x=meansalin, y=pred.gam, color = Sp.Age)) + 
  geom_ribbon(aes(x=meansalin, ymin=pred.gam-(1.96*pred.gam.se), ymax=pred.gam+(1.96*pred.gam.se)), 
              alpha = 0.3, inherit.aes = FALSE) + 
  geom_line(size = 1.5) +
  scale_color_manual(values=c(arcs0_color)) + 
  scale_fill_manual(values=c(arcs0_color)) + 
  scale_y_continuous("Predicted Growth (mm/day)", limits = c(0, 1.68), oob=scales::rescale_none) +
  scale_x_continuous("Salinity (psu)", limits = c(5, 23), breaks = seq(from=0, to = 25, by = 5)) +
  geom_point(data = pointvals %>% filter(Sp.Age == "ARCS_Age0") %>%
               mutate(Sp.Age = replace(Sp.Age, Sp.Age=="ARCS_Age0", "Arctic Cisco Age-0")),
             shape = 21, aes(fill=Sp.Age), color = "black") +
  theme_crisp() +
  theme(legend.position = "none",
        text=element_text(family="Arial", size=12),
        axis.text.x = element_text(family="Arial", size=12)) +
  facet_grid(~Sp.Age)
growthsalin

# ggsave(here::here("plotexports/fig7_growthsalin_color.png"), plot = growthsalin, 
#        dpi = 600, width = 4, height = 3, units = "in")







################################################
####  FIGURE 8 - ANNUAL CATCH BY SPP/AGE    ####
################################################

# ARCS 0
new.df <- data.frame(meansalin=mean(datA0$meansalin), meantemp = mean(datA0$meantemp),
                     biweekly = 3, Station="218", Year.fac=as.factor(seq(from=2001, to=2018))) 
predA0_byyear <- predict(abundmodNB.A0.FJM3, newdata=new.df, se=T, type="link") # Std. error on log-scale
preddf_A0_yr <- data.frame(Year.fac = seq(from=2001, to=2018), 
                           lwr = exp(predA0_byyear$fit - 1.96*predA0_byyear$se.fit),  # Back-trans lower CI, 
                           predval = exp(predA0_byyear$fit), 
                           upr = exp(predA0_byyear$fit + 1.96*predA0_byyear$se.fit))
# ARCS 1
predA1_byyear <- predict(abundmodNB.A1.FJM5, newdata=new.df, se=T, type="link") 
preddf_A1_yr <- data.frame(Year.fac = seq(from=2001, to=2018), 
                           lwr = exp(predA1_byyear$fit - 1.96*predA1_byyear$se.fit),  
                           predval = exp(predA1_byyear$fit), 
                           upr = exp(predA1_byyear$fit + 1.96*predA1_byyear$se.fit))
# BDWF 0
predB0_byyear <- predict(abundmodNB.B0.FJM6, newdata=new.df, se=T, type="link") 
preddf_B0_yr <- data.frame(Year.fac = seq(from=2001, to=2018), 
                           lwr = exp(predB0_byyear$fit - 1.96*predB0_byyear$se.fit),
                           predval = exp(predB0_byyear$fit), 
                           upr = exp(predB0_byyear$fit + 1.96*predB0_byyear$se.fit))
# BDWF 1
predB1_byyear <- predict(abundmodNB.B1.FJM4, newdata=new.df, se=T, type="link") 
preddf_B1_yr <- data.frame(Year.fac = seq(from=2001, to=2018), 
                           lwr = exp(predB1_byyear$fit - 1.96*predB1_byyear$se.fit),  
                           predval = exp(predB1_byyear$fit), 
                           upr = exp(predB1_byyear$fit + 1.96*predB1_byyear$se.fit))
rm(predA0_byyear, predA1_byyear, predB0_byyear, predB1_byyear, new.df)



catchovertimeplot <- preddf_A0_yr %>% mutate(Sp.age = "Arctic Cisco Age-0") %>%
  full_join(preddf_A1_yr %>% mutate(Sp.age = "Arctic Cisco Age-1")) %>%
  full_join(preddf_B0_yr %>% mutate(Sp.age = "Broad Whitefish Age-0")) %>%
  full_join(preddf_B1_yr %>% mutate(Sp.age = "Broad Whitefish Age-1")) %>%
  ggplot(aes(x=Year.fac, y=predval, color = Sp.age)) +
  geom_ribbon(aes(x=Year.fac, ymin=lwr, ymax=upr), alpha = 0.3, inherit.aes = FALSE) +
  geom_line(size = 1.5) +
  scale_y_continuous("CPUE", labels=comma) +
  scale_x_continuous("", breaks = seq(from=2001, to=2017, by=2)) +
  scale_color_manual(values=c(arcs0_color, arcs1_color, bdwf0_color, bdwf1_color)) +
  facet_wrap(.~Sp.age, scales="free_y") +
  theme_crisp() +
  theme(legend.position = "none", 
        text=element_text(family="Arial", size=12),
        axis.text.x = element_text(family="Arial", size=9, angle = 0, hjust=0.5)) +
  geom_text(data = data.frame(label = c("a", "b", "c", "d"), 
                              Sp.age = c("Arctic Cisco Age-0", "Arctic Cisco Age-1", 
                                         "Broad Whitefish Age-0", "Broad Whitefish Age-1")),
            mapping = aes(x = -Inf, y = -Inf, label = label),
            hjust = -1, vjust = -15, 
            family="Arial", fontface = "bold", color = "black")
catchovertimeplot
# ggsave(here::here("plotexports/fig8_catchovertimeplot_color.png"), 
#        plot = catchovertimeplot, dpi = 600, width = 7.5, height = 5, units = "in")







###############################################
####  FIGURE 9 - PRED CPUE BY TEMPERATURE  ####
###############################################

CPUEplottempA1 <- ggplot(preddf_A1_temp %>%
                           mutate(Sp.age = "Arctic Cisco Age-1"), 
                         aes(x=temp, y = predval, color = Sp.age)) + 
  geom_ribbon(aes(x=temp, ymin=lwr, ymax=upr), alpha = 0.3, inherit.aes = FALSE) +
  geom_line(size = 1.5) + 
  labs(x = "", y = "Predicted CPUE", title = "Arctic Cisco Age-1") +
  coord_cartesian(xlim = c(2.5, 12.5), ylim=c(0, 600)) +
  scale_y_continuous(limits = c(0,600), breaks = c(0, 150, 300, 450, 600)) +
  scale_color_manual(values = c(arcs1_color)) +
  theme_crisp() + 
  theme(legend.position = "none",
        text=element_text(family="Arial", size=12),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(family="Arial", size=12),
        axis.title.x=element_blank())  
CPUEplottempA1

CPUEplottempB1 <- ggplot(preddf_B1_temp %>%
                           mutate(Sp.age = "Arctic Cisco Age-1"), 
                         aes(x = temp, y = predval, color = Sp.age)) + 
  geom_ribbon(aes(x=temp, ymin=lwr, ymax=upr), alpha = 0.3, inherit.aes = FALSE) +
  geom_line(size = 1.5) + 
  labs(x = "", y="", title = "Broad Whitefish Age-1") +
  coord_cartesian(xlim = c(2.5, 12.5), ylim=c(0, 600)) +
  scale_y_continuous(breaks = c(0, 150, 300, 450, 600)) +
  scale_color_manual(values = c(bdwf1_color)) +
  theme_crisp() + 
  theme(legend.position = "none",
        text=element_text(family="Arial", size=12),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(family="Arial", size=12),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank())  
CPUEplottempB1

predCPUE_temp <- CPUEplottempA1 + CPUEplottempB1 + 
  plot_annotation(tag_levels = 'a', caption = 'Water Temperature (°C)') & 
  theme(plot.tag.position = c(0.05, 1), 
        plot.tag = element_text(hjust = -5, vjust = 5, size = 12, face = "bold", family ="Arial"),
        plot.caption = element_text(hjust = 0.5, size = 12, family ="Arial")) 
predCPUE_temp
# ggsave(here::here("plotexports/fig9_CPUEtemp_color.png"), plot = predCPUE_temp, 
#        dpi = 600, width = 7.2, height = 3.6, units = "in")







#############################################
####  FIGURE 10 - PRED CPUE BY SALINITY  ####
#############################################


CPUEplotsalB0 <- ggplot(preddf_B0_sal %>%
                          mutate(Sp.age = "Broad Whitefish Age-0"), 
                        aes(x=salin, y = predval, color = Sp.age)) + 
  geom_ribbon(aes(x=salin, ymin=lwr, ymax=upr), alpha = 0.3, inherit.aes = FALSE) +
  geom_line(size = 1.5) +
  labs(y = "Predicted CPUE", title = "Broad Whitefish Age-0") +
  scale_y_continuous(limits = c(0,60), breaks = c(0, 10, 20, 30, 40, 50), oob=scales::rescale_none) +
  scale_color_manual(values = c(bdwf0_color)) +
  theme_crisp() + 
  theme(legend.position = "none",
        text=element_text(family="Arial", size=12),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(family="Arial", size=12),
        axis.title.x=element_blank())
CPUEplotsalB0

CPUEplotsalB1 <- ggplot(preddf_B1_sal%>%
                          mutate(Sp.age = "Broad Whitefish Age-1"), 
                        aes(x=salin, y = predval, color = Sp.age)) + 
  geom_ribbon(aes(x=salin, ymin=lwr, ymax=upr), alpha = 0.3, inherit.aes = FALSE) +
  geom_line(size = 1.5) +
  labs(y="", title = "Broad Whitefish Age-1") +
  scale_y_continuous(limits = c(0, 600), breaks = c(0, 100, 200, 300, 400, 500), 
                     oob=scales::rescale_none) +
  scale_color_manual(values = c(bdwf1_color)) +
  theme_crisp() + 
  theme(legend.position = "none",
        text=element_text(family="Arial", size=12),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(family="Arial", size=12),
        axis.title.x=element_blank())  
CPUEplotsalB1




predCPUE_sal <- CPUEplotsalB0 + CPUEplotsalB1 + plot_annotation(tag_levels = "a", caption = "Salinity (psu)") & 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(hjust = -7, vjust = 4, size = 12, face = "bold", family ="Arial"),
        plot.caption = element_text(hjust = 0.5, size = 12, family ="Arial"))
predCPUE_sal
# ggsave(here::here("plotexports/fig10_CPUEsal_color.png"), plot = predCPUE_sal, 
#        dpi = 600, width = 7.2, height = 3.6, units = "in")



# TOP MODELS
# No temp/salin for ARCS0
summary(abundmodNB.A1.FJM5) 
summary(abundmodNB.B0.FJM6)
summary(abundmodNB.B1.FJM4)
#summary(mgcv::gam(ages_CPUE ~ s(Year.fac, bs="re") + meantemp + meansalin + 
#                    s(Station, bs="re"), family = "nb", link = "log", data = datB1))
#This is to get the slope estimates for BDWF1. 


