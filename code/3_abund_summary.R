# CPUE modeling - summary
# Justin Priest
# justin.priest@alaska.gov
# Updated March 2022


# This is a summary of the "3_abundance.R" script which covers all models,
#    model selection, statistical summaries, and diagnostics. 
# This file is just a summary intended to be used for visualizations



library(mgcv)
source(here::here("code/2_growth_summary.R"))



#### Create effort summary by biweek and station ####
effort.biwk.stn <- addbiwknum(effort) %>% group_by(Year, biweekly, Station) %>% 
  summarise(Effort_NetHrs = sum(Effort_NetHrs, na.rm = TRUE)) %>%
  mutate(Station = as.factor(Station))



##############################################

# Add together the ARCS and the BDWF catch and age dataframes, 
#  then get totals for each station, year, biweek of each species & age
# Finally, account for varying effort among biweeks

# abundance.biwk <- allcatch.arcsages %>% 
#   full_join(allcatch.bdwfages, c("Species", "Year", "EndDate", "Net", "Station", "LG", 
#                                  "totcount", "Lengthcount", "unmeasured", "age.put", 
#                                  "agecount", "unleng_ages", "totalages")) %>% 
#   addbiwknum() %>% 
#   left_join(env_allyears, 
#             by = c("Year", "EndDate", "Station", "day.of.year", "biweekly")) %>%
#   group_by(Year, biweekly, Station, Species, age.put) %>% 
#   summarise(totalages = sum(totalages), 
#             meantemp = mean(Temp_Top, na.rm=TRUE), 
#             meansalin = mean(Salin_Top, na.rm=TRUE)) %>% 
#   left_join(effort.biwk.stn %>%
#               mutate(Station = as.factor(Station)), 
#             by = c("Year" = "Year", "biweekly" = "biweekly", 
#                    "Station" = "Station")) %>%
#   mutate(ages_CPUE = totalages/(Effort_NetHrs/(24*2*14))) %>%
#   filter( Station != "231") # rm station 231 (only sampled in 2001 then it moved across island to 214)
# # "ages_CPUE" is biweekly catch by age, adjusted for biweekly effort
# # The CPUE accounts for 2 nets fishing 24 hrs/day, for ~14 days. A "full" biweek is 672 hrs
# # CPUE can be higher than 672 hrs in a biweek if:
# #    - there was a 2-3 day set checked at beginning of biweek
# #    - started sampling much earlier than ~July 1
# #    - there's actually 15 days from July 15-31, etc.







abundance.biwk <- allcatch.arcsages %>% 
  full_join(allcatch.bdwfages, c("Species", "Year", "EndDate", "Net", "Station", "LG", 
                                 "totcount", "Lengthcount", "unmeasured", "age.put", 
                                 "agecount", "unleng_ages", "totalages")) %>% 
  addbiwknum() %>% 
  #left_join(env_allyears, by = c("Year", "EndDate", "Station", "day.of.year", "biweekly")) %>%
  group_by(Year, biweekly, Station, Species, age.put) %>% 
  summarise(totalages = sum(totalages)) %>%
  #meantemp = mean(Temp_Top, na.rm=TRUE), 
  #meansalin = mean(Salin_Top, na.rm=TRUE)) %>% 
  left_join(effort.biwk.stn, by = c("Year" = "Year", "biweekly" = "biweekly", 
                                    "Station" = "Station")) %>%
  mutate(ages_CPUE = totalages/(Effort_NetHrs/(24*2*14))) %>%
  full_join(crossing(effort.biwk.stn %>% dplyr::select(-Effort_NetHrs), 
                     Species = c("ARCS", "BDWF"), age.put = c("age0", "age1"))) %>% 
  #the two previous lines create blank dataframe (zero catch) for all biweeks where there was
  # fishing effort, but no catch of age0/age1 ARCS/BDWF
  mutate(ages_CPUE = replace_na(ages_CPUE, 0)) %>% 
  filter(age.put == "age0" | age.put == "age1", Station != 231) %>%
  left_join(effort.biwk.stn %>% rename(Effort_NetHrs_new = Effort_NetHrs), 
            by = c("Year" = "Year", "biweekly" = "biweekly", "Station" = "Station")) %>% 
  #filter(is.na(Effort_NetHrs))
  mutate(Effort_NetHrs = Effort_NetHrs_new) %>% dplyr::select(-Effort_NetHrs_new) %>%
  left_join(env_allyears %>% group_by(Year, biweekly, Station) %>% 
              summarise(meantemp = mean(Temp_Top, na.rm=TRUE), 
                        meansalin = mean(Salin_Top, na.rm=TRUE)), # group/average by biwk
            by = c("Year", "biweekly", "Station"))
# "ages_CPUE" is biweekly catch by age, adjusted for biweekly effort
# The CPUE accounts for 2 nets fishing 24 hrs/day, for ~14 days. A "full" biweek is 672 hrs
# CPUE can be higher than 672 hrs in a biweek if:
#    - there was a 2-3 day set checked at beginning of biweek
#    - started sampling much earlier than ~July 1
#    - there's actually 15 days from July 15-31, etc.







################################################
#### CREATE DATAFRAMES FOR EACH AGE/SPECIES ####
################################################


datA0 <- abundance.biwk %>% filter(Species == "ARCS", age.put == "age0") %>% 
  ungroup() %>% mutate(Station = as.factor(Station)) %>%
  filter(!is.na(meansalin) & !is.na(meantemp)) # rm rows w/ missing values to be able to compare
# For compare the different models (with and without temp/salinity), 
#  I eliminated the rows with missing values for these variables.

datA1 <- abundance.biwk %>% filter(Species == "ARCS", age.put == "age1") %>% 
  ungroup() %>% mutate(Station = as.factor(Station)) %>%
  filter(!is.na(meansalin) & !is.na(meantemp)) # rm rows w/ missing values to be able to compare

datB0 <- abundance.biwk %>% filter(Species == "BDWF", age.put == "age0") %>% 
  ungroup() %>% mutate(Station = as.factor(Station)) %>%
  filter(!is.na(meansalin) & !is.na(meantemp)) # rm rows w/ missing values to be able to compare

datB1 <- abundance.biwk %>% filter(Species == "BDWF", age.put == "age1") %>% 
  ungroup() %>% mutate(Station = as.factor(Station)) %>%
  filter(!is.na(meansalin) & !is.na(meantemp)) # rm rows w/ missing values to be able to compare


datA0$Year.fac <- factor(datA0$Year)
datA1$Year.fac <- factor(datA1$Year)
datB0$Year.fac <- factor(datB0$Year)
datB1$Year.fac <- factor(datB1$Year)



#### TOP MODELS ####
# See script "3_abundance_detail.R" for more info on model selection
abundmodNB.A0.FJM3 <- mgcv::gam(ages_CPUE ~ s(Year.fac, bs="re") + biweekly + s(Station, bs="re"), 
                                family = "nb", link = "log", data = datA0) 

abundmodNB.A1.FJM5 <- mgcv::gam(ages_CPUE ~ s(Year.fac, bs="re") + biweekly + s(meantemp, k=4) + 
                                  s(Station, bs="re"), family = "nb", link = "log", 
                                data = datA1, na.action=na.exclude)

abundmodNB.B0.FJM6 <- mgcv::gam(ages_CPUE ~ s(Year.fac, bs="re") + biweekly + 
                                  s(meansalin, k=4) + s(Station, bs="re"), family = "nb", link = "log", 
                                data = datB0, na.action=na.exclude) 

abundmodNB.B1.FJM4 <- mgcv::gam(ages_CPUE ~ s(Year.fac, bs="re") + meantemp + 
                                  meansalin + s(Station, bs="re"), 
                                family = "nb", link = "log", data = datB1) 







#### PREDICTED DATAFRAMES ####

new.df <- data.frame(meantemp=seq(min(datB0$meantemp), max(datB0$meantemp), length=100), 
                     biweekly = 3, meansalin = mean(datB0$meansalin),Year.fac="2017", Station="218") 
predB0 <- predict(abundmodNB.B0.FJM6, newdata=new.df, se=T, type="link") # Std. error on log-scale
preddf_B0_temp <- data.frame(temp = seq(min(datB0$meantemp), max(datB0$meantemp), length=100), 
                             lwr = exp(predB0$fit - 1.96*predB0$se.fit),  # Back-transformed lower confidence limit, 
                             predval = exp(predB0$fit), 
                             upr = exp(predB0$fit + 1.96*predB0$se.fit))  # Back-transformed upper confidence limit)
rm(new.df)

new.df <- data.frame(meansalin=seq(min(datB0$meansalin), max(datB0$meansalin), length=100), 
                     biweekly = 3, meantemp = mean(datB0$meantemp),Year.fac="2017", Station="218") 
predB0 <- predict(abundmodNB.B0.FJM6, newdata=new.df, se=T, type="link") # Std. error on log-scale
preddf_B0_sal <- data.frame(salin = seq(min(datB0$meansalin), max(datB0$meansalin), length=100), 
                            lwr = exp(predB0$fit - 1.96*predB0$se.fit),  # Back-transformed lower confidence limit, 
                            predval = exp(predB0$fit), 
                            upr = exp(predB0$fit + 1.96*predB0$se.fit))  # Back-transformed upper confidence limit)
rm(new.df)


new.df <- data.frame(meantemp=seq(min(datB1$meantemp), max(datB1$meantemp), length=100), 
                     biweekly = 3, meansalin = mean(datB1$meansalin),Year.fac="2017", Station="218") 
predB1 <- predict(abundmodNB.B1.FJM4, newdata=new.df, se=T, type="link") # Std. error on log-scale
preddf_B1_temp <- data.frame(temp = seq(min(datB1$meantemp), max(datB1$meantemp), length=100), 
                             lwr = exp(predB1$fit - 1.96*predB1$se.fit),  # Back-transformed lower confidence limit, 
                             predval = exp(predB1$fit), 
                             upr = exp(predB1$fit + 1.96*predB1$se.fit))  # Back-transformed upper confidence limit
rm(new.df)

new.df <- data.frame(meansalin=seq(min(datB1$meansalin), max(datB1$meansalin), length=100), 
                     biweekly = 3, meantemp = mean(datB1$meantemp), Year.fac="2017", Station="218") 
predB1 <- predict(abundmodNB.B1.FJM4, newdata=new.df, se=T, type="link") # Std. error on log-scale
preddf_B1_sal <- data.frame(salin = seq(min(datB1$meansalin), max(datB1$meansalin), length=100), 
                            lwr = exp(predB1$fit - 1.96*predB1$se.fit),  # Back-transformed lower confidence limit, 
                            predval = exp(predB1$fit), 
                            upr = exp(predB1$fit + 1.96*predB1$se.fit))  # Back-transformed upper confidence limit


new.df <- data.frame(meantemp=seq(min(datA1$meantemp), max(datA1$meantemp), length=100), 
                     biweekly = 3, meansalin = mean(datA1$meansalin),Year.fac="2017", Station="218") 
predA1 <- predict(abundmodNB.A1.FJM5, newdata=new.df, se=T, type="link") # Std. error on log-scale
preddf_A1_temp <- data.frame(temp = seq(min(datA1$meantemp), max(datA1$meantemp), length=100), 
                             lwr = exp(predA1$fit - 1.96*predA1$se.fit),  # Back-transformed lower confidence limit, 
                             predval = exp(predA1$fit), 
                             upr = exp(predA1$fit + 1.96*predA1$se.fit))  # Back-transformed upper confidence limit)
rm(new.df)


