# Growth Modeling - Summary
# Justin Priest
# justin.priest@alaska.gov
# Updated March 2022


# This is a summary of the "2_growth_detail.R" script which covers all models,
#    model selection, statistical summaries, and diagnostics. 
# This file is just a summary intended to be used for visualizations


source(here::here("code/1_dataimport.R"))
library(mgcv)



###########################
#### DATA MANIPULATION ####
###########################

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








##################################
#### BIWEEKLY GROWTH MODELING ####
##################################

### DETERMINE BROAD WHITEFISH AGE 1 GROWTH / DAY ###
age1bdwf.biwk.mod <- lme(Length ~ doy.adj, random=~doy.adj|yearbiwk, 
                         data = indivfish.bdwfages %>% filter(age.put == "age1"))

### DETERMINE ARCTIC CISCO  AGE 1 GROWTH / DAY ###
age1arcs.biwk.mod <- lme(Length ~ doy.adj, random=~doy.adj|yearbiwk, 
                         data = indivfish.arcsages %>% filter(age.put == "age1"))

### DETERMINE BROAD WHITEFISH AGE 0 GROWTH / DAY ###
age0bdwf.biwk.mod <- lme(Length ~ doy.adj, random=~doy.adj|yearbiwk, 
                         data = indivfish.bdwfages %>% filter(age.put == "age0"))

### DETERMINE ARCTIC CISCO  AGE 0 GROWTH / DAY ###
age0arcs.biwk.mod <- lme(Length ~ doy.adj, random=~doy.adj|yearbiwk, 
                         data = indivfish.arcsages %>% filter(age.put == "age0"))




###############################
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
# Note that there's no biweek 1 in 2004 because of low catches in all four species/ages.








###########################
#### TOP GROWTH MODELS ####
###########################

growthmodA0 <- (gam(arcs0.growth ~ meantemp + meansalin, data = growth.biwk)) 
growthmodA1 <- gam(arcs1.growth ~ Year + biweekly + meantemp, data = growth.biwk)
growthmodB0 <- gam(bdwf0.growth ~ meantemp, data = growth.biwk)
growthmodB1 <- gam(bdwf1.growth ~ Year + biweekly + meantemp, data = growth.biwk)





##########################
#### PREDICTED VALUES ####
##########################

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
  mutate(pred.se = if_else(Sp.Age=="ARCS_Age0", predict.gam(growthmodA0, 
                                                            newdata = pred_growth, se = TRUE)$se.fit,
                           if_else(Sp.Age=="ARCS_Age1", predict.gam(growthmodA1, 
                                                                    newdata = pred_growth, se = TRUE)$se.fit,
                                   if_else(Sp.Age=="BDWF_Age0", predict.gam(growthmodB0, 
                                                                            newdata = pred_growth, se = TRUE)$se.fit,
                                           if_else(Sp.Age=="BDWF_Age1", predict.gam(growthmodB1, 
                                                                                    newdata = pred_growth, se = TRUE)$se.fit,
                                                   0)))))









# In order to create a plot with the actual values plotted over the top, we create the below
pointvals <- growth.biwk %>% dplyr::select(c(Year, biweekly, arcs0.growth, arcs1.growth, 
                                             bdwf0.growth, bdwf1.growth, meantemp, meansalin)) %>%
  gather(Sp.Age, pred.gam, -c(Year, biweekly, meansalin, meantemp) ) %>%
  mutate(Sp.Age = recode(Sp.Age, "arcs0.growth" = "ARCS_Age0", "arcs1.growth" = "ARCS_Age1",
                         "bdwf0.growth" = "BDWF_Age0", "bdwf1.growth" = "BDWF_Age1"))

