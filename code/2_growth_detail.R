# Growth Modeling - Detail
# Justin Priest
# justin.priest@alaska.gov
# Originally coded Sept 2019, updated March 2022


# This file covers detailed analysis and documents modeling decisions for growth models
# Script "2_growth_summary.R" is a summarized version of this file



library(broom)
#library(lme4)
library(nlme)
library(mgcv)


source(here::here("code/1_dataimport.R"))

### DATA MANIPULATION ###

# Add biweekly numbers (biweek 1 = July 15 and earlier, 2 = July 16-31, 3 = Aug 1-15, 4 = Aug 16 and after)
# Add column in dataframes to start the season at approximately July 1 (about when sampling starts)
# On an annual basis, this would mean that the intercept is approx equal to length at start of season

# Note that dataframes indivfish.bdwfages & indivfish.arcsages only include LG 1&2 fish (<250 mm)
indivfish.bdwfages <- indivfish.bdwfages %>% addbiwknum() %>% 
  mutate(doy.adj = day.of.year - 180,
         yearbiwk = as.character(paste0(Year, biweekly))) 

indivfish.arcsages <- indivfish.arcsages %>% addbiwknum() %>% 
  mutate(doy.adj = day.of.year - 180,
         yearbiwk = as.character(paste0(Year, biweekly))) 

env_allyears <- env_allyears %>% 
  rename(EndDate = Date) %>%
  addbiwknum()


############################
### PRELIMINARY MODELING ###

indivfish.bdwfages %>% filter(age.put == "age0") %>% group_by(Year) %>% do(model = lm(Length ~ day.of.year, data = .)) %>% 
  tidy(model)

summary(lm(Length ~ day.of.year * as.factor(Year), data=indivfish.bdwfages %>% filter(age.put == "age0")))

allcatch.arcsages %>% group_by(Year, age.put) %>% summarise(summ = sum(totalages)) # %>% View()
allcatch.arcsages %>% group_by(Year, LG) %>% summarise(summ = sum(totalages)) # %>% View()



##############################
### ANNUAL GROWTH MODELING ###
##############################

### DETERMINE BDWF AGE 1 GROWTH / DAY ###

# Use a mixed effects model to get growth for each
bdwfage1growth <- lme(Length ~ doy.adj,random=~doy.adj|Year,data = indivfish.bdwfages %>% filter(age.put == "age1"))
bdwfage1growth
summary(bdwfage1growth)
coef(bdwfage1growth) # intercept is starting length, slope is growth/day
ranef(bdwfage1growth) #not as useful


arcsage1growth <- lme(Length ~ doy.adj,random=~doy.adj|Year,data = indivfish.arcsages %>% filter(age.put == "age1"))
arcsage1growth
summary(arcsage1growth)
coef(arcsage1growth) # intercept is starting length, slope is growth/day


### DETERMINE BDWF AGE 1 GROWTH / DAY ###
age1bdwf <- data.frame(coef(bdwfage1growth)) %>% 
  mutate(Year = 2001:2018) %>%
  rename(int = X.Intercept.,
         growthperday = doy.adj) %>%
  dplyr::select(Year, everything()) %>%
  left_join(env_allyears %>% group_by(Year) %>% 
            summarise(meantemp = mean(Temp_Top, na.rm=TRUE),
            meansalin = mean(Salin_Top, na.rm=TRUE)),
            by = c("Year" = "Year"))

plot(growthperday ~ Year, data = age1bdwf)


summary(lm(growthperday ~ meantemp + meansalin + Year, data = age1bdwf))



### DETERMINE ARCS AGE 1 GROWTH / DAY ###
age1arcs <- data.frame(coef(arcsage1growth)) %>% 
  mutate(Year = 2001:2018) %>%
  rename(int = X.Intercept.,
         growthperday = doy.adj) %>%
  dplyr::select(Year, everything()) %>%
  left_join(env_allyears %>% group_by(Year) %>% 
              summarise(meantemp = mean(Temp_Top, na.rm=TRUE),
                        meansalin = mean(Salin_Top, na.rm=TRUE)),
            by = c("Year" = "Year"))

plot(growthperday ~ meansalin, data = age1arcs)
plot(growthperday ~ Year, data = age1arcs)
summary(lm(growthperday ~ meansalin + meantemp + Year, data = age1arcs))


# View dataframe totals by year, biweek. Consider throwing out low samples
indivfish.arcsages %>% filter(age.put == "age1") %>% 
  group_by(Year, biweekly) %>% summarise(summ = sum(totcount)) # %>% View()





################################
### BIWEEKLY GROWTH MODELING ###
################################


### DETERMINE BROAD WHITEFISH AGE 1 GROWTH / DAY ###
age1bdwf.biwk.mod <- lme(Length ~ doy.adj, random=~doy.adj|yearbiwk, data = indivfish.bdwfages %>% filter(age.put == "age1"))
age1bdwf.biwk.mod
summary(age1bdwf.biwk.mod)
coef(age1bdwf.biwk.mod)


### DETERMINE ARCTIC CISCO  AGE 1 GROWTH / DAY ###
age1arcs.biwk.mod <- lme(Length ~ doy.adj, random=~doy.adj|yearbiwk, data = indivfish.arcsages %>% filter(age.put == "age1"))
age1arcs.biwk.mod
summary(age1arcs.biwk.mod)
coef(age1arcs.biwk.mod)



### DETERMINE BROAD WHITEFISH AGE 0 GROWTH / DAY ###
age0bdwf.biwk.mod <- lme(Length ~ doy.adj, random=~doy.adj|yearbiwk, data = indivfish.bdwfages %>% filter(age.put == "age0"))
age0bdwf.biwk.mod
summary(age0bdwf.biwk.mod)
coef(age0bdwf.biwk.mod)


### DETERMINE ARCTIC CISCO  AGE 0 GROWTH / DAY ###
age0arcs.biwk.mod <- lme(Length ~ doy.adj, random=~doy.adj|yearbiwk, data = indivfish.arcsages %>% filter(age.put == "age0"))
age0arcs.biwk.mod
summary(age0arcs.biwk.mod)
coef(age0arcs.biwk.mod)





### CREATE GROWTH DATAFRAME ###
mincatch = 60 # filter for low catches

growth.biwk <- data.frame(coef(age1bdwf.biwk.mod)) %>% #Start with age1 BDWF
  mutate(Year = as.numeric(substr(rownames(.), 1,4)),
         biweekly = as.numeric(substr(rownames(.), 5, 5))) %>%
  rename(bdwf1.int = X.Intercept.,
         bdwf1.growth = doy.adj) %>% # This is growth per day
  dplyr::select(Year, biweekly, everything()) %>%
  left_join(indivfish.bdwfages %>% filter(age.put == "age1") %>% # join with catch totals by yr/biwk
              group_by(Year, biweekly) %>% summarise(bdwf1total = sum(totcount)),
            by = c("Year" = "Year", "biweekly" = "biweekly")) %>%
  filter(bdwf1total > mincatch) %>% # Remove biwks with low catches from analysis
  # Next add in age 1 ARCS. Need to full join because we removed some observations
  full_join(data.frame(coef(age1arcs.biwk.mod)) %>% # join with age1 ARCS
            mutate(Year = as.numeric(substr(rownames(.), 1,4)),
                   biweekly = as.numeric(substr(rownames(.), 5, 5))) %>%
            rename(arcs1.int = X.Intercept.,
                   arcs1.growth = doy.adj) %>%
            dplyr::select(Year, biweekly, everything()) %>%
            left_join(indivfish.arcsages %>% filter(age.put == "age1") %>% # join with catch totals by yr/biwk
                            group_by(Year, biweekly) %>% summarise(arcs1total = sum(totcount)),
                          by = c("Year" = "Year", "biweekly" = "biweekly")) %>%
                filter(arcs1total > mincatch),
            by = c("Year", "biweekly")) %>%
  # Next add age0s
  full_join(data.frame(coef(age0bdwf.biwk.mod)) %>% # join with age0 BDWF
              mutate(Year = as.numeric(substr(rownames(.), 1,4)),
                     biweekly = as.numeric(substr(rownames(.), 5, 5))) %>%
              rename(bdwf0.int = X.Intercept.,
                     bdwf0.growth = doy.adj) %>% # This is growth per day
              dplyr::select(Year, biweekly, everything()) %>%
              left_join(indivfish.bdwfages %>% filter(age.put == "age0") %>% # join with catch totals by yr/biwk
                          group_by(Year, biweekly) %>% summarise(bdwf0total = sum(totcount)),
                        by = c("Year" = "Year", "biweekly" = "biweekly")) %>%
              filter(bdwf0total > mincatch)) %>%
  full_join(data.frame(coef(age0arcs.biwk.mod)) %>% #age0 ARCS
              mutate(Year = as.numeric(substr(rownames(.), 1,4)),
                     biweekly = as.numeric(substr(rownames(.), 5, 5))) %>%
              rename(arcs0.int = X.Intercept.,
                     arcs0.growth = doy.adj) %>% # This is growth per day
              dplyr::select(Year, biweekly, everything()) %>%
              left_join(indivfish.arcsages %>% filter(age.put == "age0") %>% # join with catch totals by yr/biwk
                          group_by(Year, biweekly) %>% summarise(arcs0total = sum(totcount)),
                        by = c("Year" = "Year", "biweekly" = "biweekly")) %>%
              filter(arcs0total > mincatch)) %>%
  # Finally, join with the environmental data
  left_join(env_allyears %>% group_by(Year, biweekly) %>% 
            summarise(meantemp = mean(Temp_Top, na.rm=TRUE),
                      meansalin = mean(Salin_Top, na.rm=TRUE)),
            by = c("Year" = "Year", "biweekly" = "biweekly")) %>%
  arrange(Year, biweekly) #order by time

rm(age0arcs.biwk.mod, age1arcs.biwk.mod, age0bdwf.biwk.mod, age1bdwf.biwk.mod)
# Note that there's not biweek 1 in 2004 because of low catches in all four species/ages.



###############################################
### MODEL GROWTH EFFECTS BY TEMP & SALINITY ###


#### LINEAR MODEL ####
### AGE 0 ###
summary(lm(bdwf0.growth ~ Year + biweekly + meantemp + meansalin, data = growth.biwk))
plot(bdwf0.growth ~ meantemp, data = growth.biwk)
plot(bdwf0.growth ~ meansalin, data = growth.biwk)
summary(lme(bdwf0.growth ~ Year + meantemp, random=~1|Year, data = growth.biwk, na.action = na.exclude)) # test lme 


summary(lm(arcs0.growth ~ Year + biweekly + meansalin + meantemp, data = growth.biwk))
plot(arcs0.growth ~ meantemp, data = growth.biwk)
plot(arcs0.growth ~ meansalin, data = growth.biwk)


### AGE 1 ###
summary(lm(bdwf1.growth ~ meantemp, data = growth.biwk))
summary(lm(bdwf1.growth ~ Year + biweekly + meansalin + meantemp, data = growth.biwk))
summary(lme(bdwf1.growth ~ Year + meantemp, random=~1|Year, data = growth.biwk, na.action = na.exclude)) # test lme 
plot(bdwf1.growth ~ meantemp, data = growth.biwk)
plot(bdwf1.growth ~ Year, data = growth.biwk)


summary(lm(arcs1.growth ~ Year + biweekly + meansalin + meantemp, data = growth.biwk))
summary(lm(arcs1.growth ~ meantemp, data = growth.biwk))
summary(lme(arcs1.growth ~ Year + biweekly + meantemp, random=~1|Year, data = growth.biwk, na.action = na.exclude))

plot(arcs1.growth ~ meantemp, data = growth.biwk)





#### GAM MODEL ####

# Exploratory
summary(gam(bdwf1.growth ~ Year + meantemp, data = growth.biwk)) 
summary(gam(bdwf1.growth ~ Year + s(meantemp), data = growth.biwk)) 

temp1 <- (gam(bdwf1.growth ~ Year + biweekly + s(meantemp, k=7) , data = growth.biwk)) #arbitrarily chose k=7
summary(temp1)
vis.gam(temp1, view = c("meantemp", "Year"), plot.type="contour", color="terrain")
#Best gam BDWF1 model outperforms linear model. Top BDWF1 model is Year+biweekly+s(meantemp)

temp2 <- gam(arcs1.growth ~ biweekly + s(meantemp), data = growth.biwk)
summary(temp2)
vis.gam(temp2, view = c("meantemp", "biweekly"), plot.type="contour", color="terrain")

rm(temp1, temp2)



# MODEL SELECTION

# Chose k=4 to reduce params and prevent overfitting
growthmodA0_1 <- gam(arcs0.growth ~ Year + biweekly + s(meantemp, k=4) + s(meansalin, k=4), data = growth.biwk)
growthmodA0_2 <- gam(arcs0.growth ~ Year + biweekly + s(meantemp, k=4), data = growth.biwk)
growthmodA0_3 <- gam(arcs0.growth ~ Year + biweekly + s(meansalin, k=4), data = growth.biwk)
growthmodA0_4 <- gam(arcs0.growth ~ Year + biweekly, data = growth.biwk)
growthmodA0_5 <- gam(arcs0.growth ~ Year + s(meantemp, k=4) + s(meansalin, k=4), data = growth.biwk)
growthmodA0_6 <- gam(arcs0.growth ~ Year, data = growth.biwk)
growthmodA0_7 <- gam(arcs0.growth ~ s(meantemp, k=4) + s(meansalin, k=4), data = growth.biwk)
growthmodA0_8 <- gam(arcs0.growth ~ biweekly, data = growth.biwk)
growthmodA0_9 <- gam(arcs0.growth ~ 1, data = growth.biwk)

AIC(growthmodA0_1, growthmodA0_2, growthmodA0_3, growthmodA0_4, growthmodA0_5, growthmodA0_6,
    growthmodA0_7, growthmodA0_8, growthmodA0_9) 
#TOP ARCS0 model is s(temp) + s(salin)



growthmodA1_1 <- gam(arcs1.growth ~ Year + biweekly + s(meantemp, k=4) + s(meansalin, k=4), data = growth.biwk)
growthmodA1_2 <- gam(arcs1.growth ~ Year + biweekly + s(meantemp, k=4), data = growth.biwk)
growthmodA1_3 <- gam(arcs1.growth ~ Year + biweekly + s(meansalin, k=4), data = growth.biwk)
growthmodA1_4 <- gam(arcs1.growth ~ Year + biweekly, data = growth.biwk)
growthmodA1_5 <- gam(arcs1.growth ~ Year + s(meantemp, k=4) + s(meansalin, k=4), data = growth.biwk)
growthmodA1_6 <- gam(arcs1.growth ~ Year, data = growth.biwk)
growthmodA1_7 <- gam(arcs1.growth ~ s(meantemp, k=4) + s(meansalin, k=4), data = growth.biwk)
growthmodA1_8 <- gam(arcs1.growth ~ biweekly, data = growth.biwk)
growthmodA1_9 <- gam(arcs1.growth ~ Year + s(meantemp, k=4), data = growth.biwk)
growthmodA1_10 <- gam(arcs1.growth ~ 1, data = growth.biwk)

AIC(growthmodA1_1, growthmodA1_2, growthmodA1_3, growthmodA1_4, growthmodA1_5, growthmodA1_6,
    growthmodA1_7, growthmodA1_8, growthmodA1_9, growthmodA1_10) 
#TOP ARCS1 model is Year + biweekly + s(temp)


growthmodB0_1 <- gam(bdwf0.growth ~ Year + biweekly + s(meantemp, k=4) + s(meansalin, k=4), data = growth.biwk)
growthmodB0_2 <- gam(bdwf0.growth ~ Year + biweekly + s(meantemp, k=4), data = growth.biwk)
growthmodB0_3 <- gam(bdwf0.growth ~ Year + biweekly + s(meansalin, k=4), data = growth.biwk)
growthmodB0_4 <- gam(bdwf0.growth ~ Year + biweekly, data = growth.biwk)
growthmodB0_5 <- gam(bdwf0.growth ~ Year + s(meantemp, k=4) + s(meansalin, k=4), data = growth.biwk)
growthmodB0_6 <- gam(bdwf0.growth ~ Year, data = growth.biwk)
growthmodB0_7 <- gam(bdwf0.growth ~ s(meantemp, k=4) + s(meansalin, k=4), data = growth.biwk)
growthmodB0_8 <- gam(bdwf0.growth ~ s(meantemp, k=4), data = growth.biwk)
growthmodB0_9 <- gam(bdwf0.growth ~ s(meansalin, k=4), data = growth.biwk)
growthmodB0_10 <- gam(bdwf0.growth ~ biweekly, data = growth.biwk)
growthmodB0_11 <- gam(bdwf0.growth ~ 1, data = growth.biwk)

AIC(growthmodB0_1, growthmodB0_2, growthmodB0_3, growthmodB0_4, growthmodB0_5, growthmodB0_6,
    growthmodB0_7, growthmodB0_8, growthmodB0_9, growthmodB0_10, growthmodB0_11) 
#TOP BDWF0 model is s(temp) 



growthmodB1_1 <- gam(bdwf1.growth ~ Year + biweekly + s(meantemp, k=4) + s(meansalin, k=4), data = growth.biwk)
growthmodB1_2 <- gam(bdwf1.growth ~ Year + biweekly + s(meantemp, k=4), data = growth.biwk)
growthmodB1_3 <- gam(bdwf1.growth ~ Year + biweekly + s(meansalin, k=4), data = growth.biwk)
growthmodB1_4 <- gam(bdwf1.growth ~ Year + biweekly, data = growth.biwk)
growthmodB1_5 <- gam(bdwf1.growth ~ Year + s(meantemp, k=4) + s(meansalin, k=4), data = growth.biwk)
growthmodB1_6 <- gam(bdwf1.growth ~ Year, data = growth.biwk)
growthmodB1_7 <- gam(bdwf1.growth ~ s(meantemp, k=4) + s(meansalin, k=4), data = growth.biwk)
growthmodB1_8 <- gam(bdwf1.growth ~ biweekly, data = growth.biwk)
growthmodB1_9 <- gam(bdwf1.growth ~ 1, data = growth.biwk)

AIC(growthmodB1_1, growthmodB1_2, growthmodB1_3, growthmodB1_4, growthmodB1_5, growthmodB1_6,
    growthmodB1_7, growthmodB1_8, growthmodB1_9) 
#TOP BDWF1 model is #2: Year + biweekly + s(temp)


# I experimented using library(AICcmodavg). Same results for AICc as AIC for all age/spp
# ARCS0 model is temp + salin
# ARCS1 model is Year + biweekly + temp
# BDWF0 model is s(temp) 
# BDWF1 model is Year + biweekly + s(temp)


# FINAL MODELS
summary(gam(arcs0.growth ~ s(meantemp, k=4) + s(meansalin, k=4), data = growth.biwk))
growthmodA0 <- (gam(arcs0.growth ~ meantemp + meansalin, data = growth.biwk)) 
# EDF=1, so drop smoothing to see slope est
summary(growthmodA0)
sd(residuals(growthmodA0))
# use sqrt of the scale est. for a better estimate. sqrt(growthmodA1$sig2)
sqrt(growthmodA0$sig2)

summary(gam(arcs1.growth ~ Year + biweekly + s(meantemp, k=4), data = growth.biwk))
growthmodA1 <- gam(arcs1.growth ~ Year + biweekly + meantemp, data = growth.biwk)
summary(growthmodA1)
anova(growthmodA1)
sqrt(growthmodA1$sig2)

summary(gam(bdwf0.growth ~ s(meantemp, k=4), data = growth.biwk))
growthmodB0 <- gam(bdwf0.growth ~ meantemp, data = growth.biwk)
summary(growthmodB0) 
anova(growthmodB0)
sqrt(growthmodB0$sig2)

summary(gam(bdwf1.growth ~ Year + biweekly + s(meantemp, k=4), data = growth.biwk))
growthmodB1 <- gam(bdwf1.growth ~ Year + biweekly + meantemp, data = growth.biwk)
summary(growthmodB1) 
anova(growthmodB1)
sqrt(growthmodB1$sig2)


# preliminary visualization
vis.gam(gam(arcs0.growth ~ Year + biweekly + 
                      s(meantemp, k=5) + s(meansalin, k=5), data = growth.biwk), 
        view = c("meantemp", "meansalin"), plot.type="contour", main = "ARCS0", color="terrain")

#BEWARE INTERPRETING THE FOLLOWING 3 BECAUSE SALINITY WASN'T SIGNIFICANT
vis.gam(gam(arcs1.growth ~ Year + biweekly + 
              s(meantemp) + s(meansalin), data = growth.biwk), 
        view = c("meantemp", "meansalin"), plot.type="contour", main = "ARCS1",color="terrain")

vis.gam(gam(bdwf0.growth ~ Year + biweekly + 
              s(meantemp) + s(meansalin), data = growth.biwk), 
        view = c("meantemp", "meansalin"), plot.type="contour", main = "BDWF0",color="terrain")

vis.gam(gam(bdwf1.growth ~ Year + biweekly + 
              s(meantemp) + s(meansalin), data = growth.biwk), 
        view = c("meantemp", "meansalin"), plot.type="contour", main = "BDWF1",color="terrain")




########################
### PREDICTED VALUES ###
########################

pred_growth <- expand.grid(Year = 2001:2018, biweekly = seq(from=1, to = 4, by=0.5),
                           meansalin = seq(floor(min(growth.biwk$meansalin, na.rm = TRUE)),
                                           ceiling(max(growth.biwk$meansalin, na.rm = TRUE)), by=0.5),
                           meantemp = seq(floor(min(growth.biwk$meantemp, na.rm = TRUE)),
                                           ceiling(max(growth.biwk$meantemp, na.rm = TRUE)), by=0.5))
pred_growth$ARCS_Age0 <- predict.gam(growthmodA0, newdata = pred_growth)
#pred_growth$A0pred.se <- predict.gam(growthmodA0, newdata = pred_growth, se = TRUE)$se.fit
pred_growth$ARCS_Age1 <- predict.gam(growthmodA1, newdata = pred_growth)
#pred_growth$A1pred.se <- predict.gam(growthmodA1, newdata = pred_growth, se = TRUE)$se.fit
pred_growth$BDWF_Age0 <- predict.gam(growthmodB0, newdata = pred_growth)
#pred_growth$B0pred.se <- predict.gam(growthmodB0, newdata = pred_growth, se = TRUE)$se.fit
pred_growth$BDWF_Age1 <- predict.gam(growthmodB1, newdata = pred_growth)
#pred_growth$B1pred.se <- predict.gam(growthmodB1, newdata = pred_growth, se = TRUE)$se.fit



pred_growth <- gather(pred_growth, Sp.Age, preds, -c(Year, biweekly, meansalin, meantemp) )
pred_growth <- pred_growth %>% 
  mutate(pred.se = if_else(Sp.Age=="ARCS_Age0", predict.gam(growthmodA0, newdata = pred_growth, se = TRUE)$se.fit,
                           if_else(Sp.Age=="ARCS_Age1", predict.gam(growthmodA1, newdata = pred_growth, se = TRUE)$se.fit,
                                   if_else(Sp.Age=="BDWF_Age0", predict.gam(growthmodB0, newdata = pred_growth, se = TRUE)$se.fit,
                                           if_else(Sp.Age=="BDWF_Age1", predict.gam(growthmodB1, newdata = pred_growth, se = TRUE)$se.fit,
                                                   0)))))



######################
### VISUALIZATIONS ###
######################

# Final visualizations are in the final script
# In order to create a plot with the actual values plotted over the top, we create the below
pointvals <- growth.biwk %>% dplyr::select(c(Year, biweekly, arcs0.growth, arcs1.growth, 
                                             bdwf0.growth, bdwf1.growth, meantemp, meansalin)) %>%
  gather(Sp.Age, pred.gam, -c(Year, biweekly, meansalin, meantemp) ) %>%
  mutate(Sp.Age = recode(Sp.Age, "arcs0.growth" = "ARCS_Age0", "arcs1.growth" = "ARCS_Age1",
                         "bdwf0.growth" = "BDWF_Age0", "bdwf1.growth" = "BDWF_Age1"))




library(ggplot2)
library(ggsidekick)
ggplot(pred_growth %>% group_by(Sp.Age, meantemp) %>% 
         summarise(pred.gam = mean(preds),
                   pred.gam.se = mean(pred.se)), 
       aes(x=meantemp, y=pred.gam)) + 
  geom_line() +
  geom_ribbon(aes(ymin=pred.gam-(2*pred.gam.se), ymax=pred.gam+(2*pred.gam.se)), alpha = 0.3) + 
  scale_y_continuous("Predicted Growth (mm/day)", limits = c(0, 2), oob=scales::rescale_none) +
  scale_x_continuous("Water Temperature (C)") +
  geom_point(data = pointvals, alpha = 0.5) +
  facet_wrap(. ~ Sp.Age) +
  theme_sleek() # BDWF_Age0 ISN'T SIGNIFICANT
# cut ARCS0 to not extend past min/max
# because one of these is insignificant, we'll need to manuallly exclude it
  


ggplot(pred_growth %>% group_by(Sp.Age, meansalin) %>% 
         summarise(pred.gam = mean(preds),
                   pred.gam.se = mean(pred.se)), 
       aes(x=meansalin, y=pred.gam)) + 
  geom_line() +
  geom_ribbon(aes(ymin=pred.gam-(2*pred.gam.se), ymax=pred.gam+(2*pred.gam.se)), alpha = 0.3) + 
  scale_y_continuous("Predicted Growth (mm/day)", limits = c(0, 2), oob=scales::rescale_none) +
  scale_x_continuous("Salinity (ppt)") +
  facet_wrap(. ~ Sp.Age) + 
  geom_point(data = pointvals, alpha = 0.5) +
  theme_sleek() # Only ARCS Age0 should be used





ggplot(pred_growth %>% filter(Sp.Age == "ARCS_Age0") %>% group_by(Sp.Age, meansalin) %>% 
         summarise(pred.gam = mean(preds),
                   pred.gam.se = mean(pred.se)), 
       aes(x=meansalin, y=pred.gam)) + 
  geom_line() +
  geom_ribbon(aes(ymin=pred.gam - (2*pred.gam.se), ymax=pred.gam + (2*pred.gam.se)), alpha = 0.3) + 
  scale_y_continuous("Predicted Growth (mm/day)", limits = c(0, 2), oob=scales::rescale_none) +
  scale_x_continuous("Salinity (ppt)", limits = c(7, 20), expand = c(0.5, 3)) +
  geom_point(data = pointvals %>% filter(Sp.Age == "ARCS_Age0"), alpha = 0.5) +
  theme_sleek()

min((pointvals %>% filter(Sp.Age == "ARCS_Age0" & !is.na(pred.gam)))$meansalin, na.rm = TRUE)
pointvals %>% filter(Sp.Age == "ARCS_Age0" & !is.na(pred.gam))



## Final visualizations in RMD ##


summary(aov(Length ~ Station, data = indivfish.arcsages %>% filter(Group == 1 | Group == 2)))
TukeyHSD(aov(Length ~ Station, data = 
               indivfish.arcsages %>% filter(age.put == "age0", Species == "ARCS", Station != 231) %>%
               mutate(Station = as.factor(Station))))
TukeyHSD(aov(Length ~ Station, data = 
               indivfish.arcsages %>% filter(age.put == "age1", Species == "ARCS", Station != 231) %>%
               mutate(Station = as.factor(Station))))
# Summary: there is indeed different growth for ARCS between Stations
# Age-0 ARCS lengths largest at 230, lowest at 218. 214/220 are ~same. Age-1: 230>214>220>218

summary(aov(Length ~ Station, data = indivfish.bdwfages %>% filter(Group == 1 | Group == 2)))
TukeyHSD(aov(Length ~ Station, data = 
               indivfish.bdwfages %>% filter(age.put == "age0", Species == "BDWF", Station != 231) %>%
               mutate(Station = as.factor(Station))))
TukeyHSD(aov(Length ~ Station, data = 
               indivfish.bdwfages %>% filter(age.put == "age1", Species == "BDWF", Station != 231) %>%
               mutate(Station = as.factor(Station))))
# Summary: there is indeed different growth for BDWF between Stations
# Age-0 BDWF lengths: largest at 218, then 214, 230/220 are same. Age-1: 220 lowest, 230 ~> 214/218




###### NEW SECTION #####
# Modeling based on ideas from Franz
# This daily difference approach was not used in final drafts of the document


# first aggregate the individual fish to each day and pool stations
lengthdiff_temp <- indivfish.arcsages %>% full_join(indivfish.bdwfages) %>%
  filter(age.put == "age0" | age.put == "age1", Station != 231) %>% 
  group_by(Species, Year, EndDate, age.put) %>%
  summarise(meanlength = mean(Length),
            totcount = sum(totcount)) %>%
  ungroup() %>%
  arrange(Species, age.put, Year, EndDate) %>%
  #next take the difference in length between days
  mutate(timeelapsed = EndDate - dplyr::lag(EndDate, 1),
         dummygrouping = paste0(age.put, Year), # create grouping variable
         lengdiff = meanlength - dplyr::lag(meanlength, 1, order_by = dummygrouping)) %>% 
  mutate(lengdiff = if_else(between(timeelapsed, 1, 10), lengdiff, NA_real_),
         #above line is to exclude when lagged group exceeds 10 day difference
         Nweight = (totcount * dplyr::lag(totcount, 1, order_by = dummygrouping)) / 
                   (totcount + dplyr::lag(totcount, 1, order_by = dummygrouping))) %>%
  # now measure N. N = (N_t * N_t-1)/(N_t + N_t-1). FJM email 9/4/2019
  mutate(Nweight = if_else(between(timeelapsed, 1, 10), Nweight, NA_real_))


lengthdiff <- lengthdiff_temp %>% 
  left_join(env_allyears %>% filter(Station != 231) %>% group_by(EndDate) %>%
  summarise(meantemp = mean(Temp_Top, na.rm = TRUE), 
            meansalin = mean(Salin_Top, na.rm = TRUE)) %>%
    mutate(tempanom = (meantemp - mean(meantemp, na.rm=TRUE)) / sd(meantemp, na.rm =TRUE),
           salinanom = (meansalin - mean(meansalin, na.rm=TRUE)) / sd(meansalin, na.rm =TRUE)), 
  by = c("EndDate" = "EndDate")) %>%
  mutate(tempanomlag = dplyr::lag(tempanom, 1, order_by = dummygrouping),
         salinanomlag = dplyr::lag(salinanom, 1, order_by = dummygrouping)) %>%
  mutate(tempanomlag = if_else(between(timeelapsed, 1, 10), tempanomlag, NA_real_),
         salinanomlag = if_else(between(timeelapsed, 1, 10), salinanomlag, NA_real_))
rm(lengthdiff_temp) # will redo code soon to make it all one section

#Modeling
summary(lm(lengdiff ~ Year + tempanomlag + salinanomlag, weights = Nweight,
           data = lengthdiff %>% filter(Species == "ARCS", age.put == "age0")))
summary(lm(lengdiff ~ Year + tempanomlag + salinanomlag, weights = Nweight,
           data = lengthdiff %>% filter(Species == "ARCS", age.put == "age1")))
# Temp is important to both ARCS 0 & 1. Salinity not significant

summary(lm(lengdiff ~ Year + tempanomlag + salinanomlag, weights = Nweight,
           data = lengthdiff %>% filter(Species == "BDWF", age.put == "age0")))
summary(lm(lengdiff ~ Year + tempanomlag + salinanomlag, weights = Nweight,
           data = lengthdiff %>% filter(Species == "BDWF", age.put == "age1")))
# Temp important to BDWF age 1. Salinity not significant

ggplot(lengthdiff %>% filter(Species == "BDWF", age.put == "age1"), 
       aes(x=tempanomlag, y=lengdiff, color = Nweight)) +
  geom_point() + geom_smooth(method = "lm") +
  #scale_y_continuous(limits = c(-2,2)) +
  geom_abline(intercept = 0, slope = 0) +
  theme_sleek()


summary(lme(lengdiff ~ tempanomlag + salinanomlag, random = ~ 1 | Year, 
            weights = varFixed(~1/Nweight), na.action = "na.omit", 
            data = lengthdiff %>% filter(Species == "ARCS", age.put == "age0")))
summary(lme(lengdiff ~ tempanomlag + salinanomlag, random = ~ 1 | Year, 
            weights = varFixed(~1/Nweight), na.action = "na.omit", 
            data = lengthdiff %>% filter(Species == "ARCS", age.put == "age1")))

summary(lme(lengdiff ~ tempanomlag + salinanomlag, random = ~ 1 | Year, 
            weights = varFixed(~1/Nweight), na.action = "na.omit", 
            data = lengthdiff %>% filter(Species == "BDWF", age.put == "age0")))
summary(lme(lengdiff ~ tempanomlag + salinanomlag, random = ~ 1 | Year, 
            weights = varFixed(~1/Nweight), na.action = "na.omit", 
            data = lengthdiff %>% filter(Species == "BDWF", age.put == "age1")))


summary(gam(lengdiff ~ s(tempanomlag) + s(salinanomlag), 
    weights = Nweight, 
    data = lengthdiff %>% filter(Species == "ARCS", age.put == "age0")))

#Temp signif effect on growth for ARCS 0, ARCS 1, and BDWF 1. Salin not signif for any
# Better AIC for lm than lme. Better AIC for dropping year, but leaving in for now


# This section is not included in the manuscript, but drafted up just in case
# I had already wrote up the methods in MS Word. Saved here:
# To quantify the effects of water temperature and salinity upon changes in daily growth 
# of age-0 and age-1 whitefish, we measured the change in observed mean length from day t-1 to day t. 
# These growth increments were modeled as a function of temperature and salinity using a linear model: 
# L ̅_(i,j,t)-L ̅_(i,j,t-1)=a+β_1 〖TemperatureAnomaly〗_(t-1)+β_2 〖SalinityAnomaly〗_(t-1)+ε_(i,j)
# where L ̅_(i,j,t) is the mean daily length of species i and age-class j (age-0 or age-1), 
# TemperatureAnomaly is the standardized water temperature anomaly 
# ((daily temperature – mean temperature) / standard deviation of temperature), SalinityAnomaly 
# is the standardized salinity anomaly ((daily salinity – mean salinity) / standard deviation of salinity), 
# and ε_(i,j) is the error for each species and age-class.  Predictor terms in the linear model 
# were weighted using the sample size at day t (Nt) and the sample size at day t-1 (Nt-1) 
# in the formula N* = (Nt * Nt-1) / (Nt / Nt-1). 

