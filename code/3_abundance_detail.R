# CPUE modeling - Detail
# Justin Priest
# justin.priest@alaska.gov
# Originally coded Sept 2019, updated March 2022

# This file covers detailed analysis and documents modeling decisions for CPUE
# Script "3_abund_summary.R" is a summarized version of this file


#Note: throughout this file I used the word 'abundance' frequently when I should have specified 'catch' or 'CPUE'

library(MASS)
library(nlme)
library(mgcv)
library(lme4)
library(ggsidekick)
library(visreg)




source(here::here("code/2_growth_summary.R"))


#########################
### DATA MANIPULATION ###
#########################


# Add biweekly numbers (biweek 1 = July 15 and earlier, 2 = July 16-31, 3 = Aug 1-15, 4 = Aug 16 and after)
# Add column in dataframes to start the season at approximately July 1 (about when sampling starts)

# env_allyears <- env_allyears %>%
#   rename(EndDate = Date) %>%
#   addbiwknum() # If this fails, it's because you've already changed "Date" to be "EndDate"
# Uncomment this out if you only want to run this script w/o importing Ch2_2_growth


# Effort summary by biweek and station
effort.biwk.stn <- addbiwknum(effort) %>% group_by(Year, biweekly, Station) %>% 
  summarise(Effort_NetHrs = sum(Effort_NetHrs, na.rm = TRUE)) %>%
  mutate(Station = as.factor(Station))



##############################################

# Add together the ARCS and the BDWF catch and age dataframes, 
#  then get totals for each station, year, biweek of each species & age
#  finally, account for varying effort among biweeks

abundance.biwk <- allcatch.arcsages %>% 
  full_join(allcatch.bdwfages, c("Species", "Year", "EndDate", "Net", "Station", "LG", 
                                 "totcount", "Lengthcount", "unmeasured", "age.put", 
                                 "agecount", "unleng_ages", "totalages")) %>% 
  addbiwknum() %>% 
  left_join(env_allyears, 
            by = c("Year", "EndDate", "Station", "day.of.year", "biweekly")) %>%
  group_by(Year, biweekly, Station, Species, age.put) %>% 
  summarise(totalages = sum(totalages), 
            meantemp = mean(Temp_Top, na.rm=TRUE), 
            meansalin = mean(Salin_Top, na.rm=TRUE)) %>% 
  left_join(effort.biwk.stn %>%
              mutate(Station = as.factor(Station)), 
            by = c("Year" = "Year", "biweekly" = "biweekly", 
                                                     "Station" = "Station")) %>%
  mutate(ages_CPUE = totalages/(Effort_NetHrs/(24*2*14))) %>%
  filter( Station != "231") # rm station 231 (only sampled in 2001 then it moved across island to 214)
  # "ages_CPUE" is biweekly catch by age, adjusted for biweekly effort
  # The CPUE accounts for 2 nets fishing 24 hrs/day, for ~14 days. A "full" biweek is 672 hrs
  # CPUE can be higher than 672 hrs in a biweek if:
  #    - there was a 2-3 day set checked at beginning of biweek
  #    - started sampling much earlier than ~July 1
  #    - there's actually 15 days from July 15-31, etc.







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
  

  



# use this in model. Then add effort as covariate
rm(effort, effort.biwk.stn)  

########################
### EXPLORE THE DATA ###
########################

# First check the correlation between the variables
cor(abundance.biwk[complete.cases(abundance.biwk),]$meansalin, 
    abundance.biwk[complete.cases(abundance.biwk),]$meantemp) #-0.11
cor(abundance.biwk[complete.cases(abundance.biwk),]$Year, 
    abundance.biwk[complete.cases(abundance.biwk),]$meantemp) #0.17
cor(abundance.biwk[complete.cases(abundance.biwk),]$biweekly, 
    abundance.biwk[complete.cases(abundance.biwk),]$meantemp) #-0.44
cor(abundance.biwk[complete.cases(abundance.biwk),]$Year, 
    abundance.biwk[complete.cases(abundance.biwk),]$meansalin) #0.00
cor(abundance.biwk[complete.cases(abundance.biwk),]$biweekly, 
    abundance.biwk[complete.cases(abundance.biwk),]$meansalin) #0.27
# Summary: correlation does not appear to be an issue. Moderate correlation btwn temp/biwk


# Quick exploratory modeling
summary(lm(ages_CPUE ~ Year + biweekly + meantemp + meansalin, 
           data = abundance.biwk %>% filter(Species == "ARCS", age.put == "age0")))
summary(lm(ages_CPUE ~ Year + biweekly + meantemp + meansalin, 
           data = abundance.biwk %>% filter(Species == "ARCS", age.put == "age1")))

summary(lm(ages_CPUE ~ Year + biweekly + meantemp + meansalin, 
           data = abundance.biwk %>% filter(Species == "BDWF", age.put == "age0")))
summary(lm(ages_CPUE ~ Year + biweekly + meantemp + meansalin, 
           data = abundance.biwk %>% filter(Species == "BDWF", age.put == "age1")))


## View response & explanatory variable skew
hist((abundance.biwk %>% filter(Species == "ARCS", age.put == "age0"))$ages_CPUE) 
hist((abundance.biwk %>% filter(Species == "ARCS", age.put == "age1"))$ages_CPUE) 
hist((abundance.biwk %>% filter(Species == "BDWF", age.put == "age0"))$ages_CPUE) 
hist((abundance.biwk %>% filter(Species == "BDWF", age.put == "age1"))$ages_CPUE) 
# All four are very skewed

hist(abundance.biwk$meantemp) 
hist(abundance.biwk$meansalin) 
#not bad for predictor variables


boxcox(abundance.biwk$meantemp ~ 1, # 
       lambda = seq(0,1.5,0.1))
boxcox(abundance.biwk$meansalin ~ 1, # 
       lambda = seq(0,1.5,0.1))
# both are outside of 1, but not too far off, and also on either side of 1
# I'm making judgement call to leave variables untransformed




#############################
### LINEAR RANDOM EFFECTS ###
#############################

summary(lme(totalages ~  Year + meantemp + meansalin, 
            random=~1|biweekly, na.action = na.exclude,
            data = abundance.biwk %>% filter(Species == "ARCS", age.put == "age0")))

summary(lme(totalages ~ Station + Year + meantemp + meansalin, 
            random=~1|biweekly, na.action = na.exclude,
            data = abundance.biwk %>% filter(Species == "ARCS", age.put == "age1")))

summary(lme(totalages ~  Year + meantemp + meansalin, 
            random=~1|biweekly, na.action = na.exclude,
            data = abundance.biwk %>% filter(Species == "BDWF", age.put == "age0")))

summary(lme(totalages ~ Station + Year + meantemp + meansalin, 
            random=~1|biweekly, na.action = na.exclude,
            data = abundance.biwk %>% filter(Species == "BDWF", age.put == "age1")))




###############################
### EXPLORE MODEL CURVATURE ###
###############################

gam.modA0 <- gam(ages_CPUE ~ Year + s(meantemp) + s(meansalin), 
                 data = abundance.biwk %>% filter(Species == "ARCS", age.put == "age0"))
summary(gam.modA0)
AIC(gam.modA0) # change params and s() above to see how AIC responds
vis.gam(gam.modA0, view = c("meantemp", "Year"), plot.type="contour", color="terrain")
vis.gam(gam.modA0, view = c("meansalin", "Year"), plot.type="contour", color="terrain")
vis.gam(gam.modA0, view = c("meantemp", "meansalin"), plot.type="contour", color="terrain")
#Year signif, signif curvature in salinity


gam.modA1 <- gam(ages_CPUE ~ Year + s(meantemp) + s(meansalin), 
                 data = abundance.biwk %>% filter(Species == "ARCS", age.put == "age1"))
summary(gam.modA1)
AIC(gam.modA1)
vis.gam(gam.modA1, view = c("meantemp", "Year"), plot.type="contour", color="terrain")
vis.gam(gam.modA1, view = c("meantemp", "meansalin"), plot.type="contour", color="terrain")
vis.gam(gam.modA1, view = c("meansalin", "Year"), plot.type="contour", color="terrain")
# Year signif, no signif salin curvature
# GAM is slightly better than no curve


gam.modB0 <- gam(ages_CPUE ~ Year + s(meantemp) + s(meansalin), 
                 data = abundance.biwk %>% filter(Species == "BDWF", age.put == "age0"))
summary(gam.modB0)
AIC(gam.modB0)
vis.gam(gam.modB0, view = c("meantemp", "Year"), plot.type="contour", color="terrain")
vis.gam(gam.modB0, view = c("meantemp", "meansalin"), plot.type="contour", color="terrain")
vis.gam(gam.modB0, view = c("meansalin", "Year"), plot.type="contour", color="terrain")
#Year not signif, signif curvature
# GAM is ever so slightly better than no curve, but not much better


gam.modB1 <- gam(ages_CPUE ~ Year + s(meantemp) + s(meansalin), 
                 data = abundance.biwk %>% filter(Species == "BDWF", age.put == "age1"))
summary(gam.modB1)
AIC(gam.modB1)
vis.gam(gam.modB1, view = c("meantemp", "Year"), plot.type="contour", color="terrain")
vis.gam(gam.modB1, view = c("meantemp", "meansalin"), plot.type="contour", color="terrain")
vis.gam(gam.modB1, view = c("meansalin", "Year"), plot.type="contour", color="terrain")
# Year signif, signif curvature in salinity
# Better fit with GAM (del AIC of ~12)

# Summary, there is some justification to account for curvature, mostly for BDWF age1
# Deviance explained however increases drastically for each model when adding curvature

rm(gam.modA0, gam.modA1, gam.modB0, gam.modB1)




#############################
### GAMM - RANDOM EFFECTS ###
#############################

## LINEAR (for reference)
summary(lme(ages_CPUE ~ Year + biweekly + meantemp + meansalin - 1, 
            random=~1|Station, na.action = na.exclude, 
            data = abundance.biwk %>% filter(Species == "BDWF", age.put == "age1") ))

# summary(gamm(ages_CPUE ~ Year + biweekly + meantemp + meansalin, 
#             random = list(Station = ~1),
#             data = temp9)$gam)
# Could not get this code to run the random effect

## GAMM
test <- gam(ages_CPUE ~ Year + biweekly + s(meantemp) + s(meansalin) +
    s(Station, bs="re"), method = "REML",
    data = abundance.biwk %>% filter(Species == "BDWF", age.put == "age1") %>% 
    ungroup() %>% mutate(Station = as.factor(Station)))
            # this adds random slopes by Stn
            # https://r.789695.n4.nabble.com/mgcv-gamm-predict-to-reflect-random-s-effects-td3622738.html
summary(test)
vis.gam(test, view = c("meansalin", "meantemp"), plot.type="contour", color="terrain")

plot(residuals.gam(test))
rm(test)



############################################################################
##### BEGIN FJM SECTION #####
#Negative Binomial
datA0 <- abundance.biwk %>% filter(Species == "ARCS", age.put == "age0") %>% 
  ungroup() %>% mutate(Station = as.factor(Station)) %>%
  filter(!is.na(meansalin) & !is.na(meantemp)) # rm rows w/ missing values to be able to compare
# For comparing different models below (with and without temp/salinity), I am 
# eliminating the rows with missing values for these variables:

abundmodNB.A0.FJM1 <- mgcv::gam(ages_CPUE ~ Year + biweekly + s(meantemp, k=4) + 
                                  s(meansalin, k=4) + s(Station, bs="re"), 
                                family = "nb", link = "log", data = datA0) 
summary(abundmodNB.A0.FJM1)
plot(abundmodNB.A0.FJM1)

# Residual diagnostics
gam.check(abundmodNB.A0.FJM1)
# One extreme fitted value, but no major concerns about residual distribution

# Currently, the model assumes a linear trend over time and a linear trend
# within the season. That may be adequate but there will almost certainly 
# be other year-to-year variability not accounted for by the covariates 

# Hence, plot residuals against year and biweekly:
plot(datA0$Year, resid(abundmodNB.A0.FJM1))
plot(factor(datA0$Year), resid(abundmodNB.A0.FJM1))
# There is a clear pattern over time that is NOT captured by differences in
# temperature or salinity with much higher values in 2007-2009 and in some
# other years. 

# You could account for that through either a smooth temporal trend or
# a random year effect (see below)

plot(datA0$biweekly, resid(abundmodNB.A0.FJM1))
plot(factor(datA0$biweekly), resid(abundmodNB.A0.FJM1))
# There may also be a seasonal pattern, not quite clear. 

# One possibility of dealing with these apparent trends is to add a random
# effect for the year-to-year variability.
datA0$Year.fac <- factor(datA0$Year)
abundmodNB.A0.FJM2 <- mgcv::gam(ages_CPUE ~ s(Year.fac, bs="re") + biweekly + s(meantemp, k=4) + 
                                  s(meansalin, k=4) + s(Station, bs="re"), family = "nb", link = "log", 
                                data = datA0, na.action=na.exclude) 
summary(abundmodNB.A0.FJM2)
plot(abundmodNB.A0.FJM2)
# The model attributes basically all of the variability to year-to-year 
# variation and a linear trend over time, and neither temperature nor 
# salinity effects are significant any longer
# This is not too surprising because the 'biweekly' trend within a given
# year is probably stronly confounded with temperatuer and salinity if
# both change over time (in particular temperature)

# That confounding is an issue and you may have to fit two separate models,
# one with with a 'biweekly' trend and one with temperature and salinity
# and then compare the models:

# Linear seasonal trend:
abundmodNB.A0.FJM3 <- mgcv::gam(ages_CPUE ~ s(Year.fac, bs="re") + biweekly + s(Station, bs="re"), 
                                family = "nb", link = "log", data = datA0) 
summary(abundmodNB.A0.FJM3)
# Note that the random station effect is no longer significant
plot(abundmodNB.A0.FJM3)
visreg(abundmodNB.A0.FJM3)

# Smooth trends with temperature and salinity:
abundmodNB.A0.FJM4 <- mgcv::gam(ages_CPUE ~ s(Year.fac, bs="re") + s(meantemp, k=4) + 
                                  s(meansalin, k=4) + s(Station, bs="re"), 
                                family = "nb", link = "log", data = datA0) 
summary(abundmodNB.A0.FJM4)
visreg(abundmodNB.A0.FJM4)
AIC(abundmodNB.A0.FJM3, abundmodNB.A0.FJM4)


# Now a few other models to  compare against
abundmodNB.A0.FJM5 <- mgcv::gam(ages_CPUE ~ s(Year.fac, bs="re") + s(Station, bs="re"), 
                                family = "nb", link = "log", data = datA0) 
abundmodNB.A0.FJM6 <- mgcv::gam(ages_CPUE ~ biweekly + s(Station, bs="re"), 
                                family = "nb", link = "log", data = datA0) 
abundmodNB.A0.FJM7 <- mgcv::gam(ages_CPUE ~1, 
                                family = "nb", link = "log", data = datA0) 

AIC(abundmodNB.A0.FJM1, abundmodNB.A0.FJM2, abundmodNB.A0.FJM3, abundmodNB.A0.FJM4, 
    abundmodNB.A0.FJM5, abundmodNB.A0.FJM6, abundmodNB.A0.FJM7)

# The model with a seasonal trend fits substantially better based on AIC and
# percent deviance explained, suggesting that the changes in abundance within
# a season are largely a seasonal increase in abundance of age-0 Arctic cisco,
# rather than a response to temperature or salinity. 

visreg(abundmodNB.A0.FJM4)
# However, part of the interannual variability that is attributed to the 
# random year-to-year variations is almost certainly due to temperature
# or salinity. You could assess this separately by extracting the random 
# year effects and relating them to mean annual temperature / salinity:
# Estimate mean annual temp & salinity:
temp.fit <- lm(meantemp ~ Year.fac + factor(biweekly) - 1, data=datA0)
temps <- coef(temp.fit)[1:18]
sal.fit <- lm(meansalin ~ Year.fac + factor(biweekly) - 1, data=datA0)
sals <- coef(sal.fit)[1:18]
# Predicted year-to-year variability in CPUE (from random effects
# of the linear seasonal trend model):
cpue.re <- coef(abundmodNB.A0.FJM3)[3:20]
cor(temps, sals)  # Note some confounding!
A0.TS <- gam(cpue.re ~ s(temps, k=4) + s(sals, k=4))
visreg(A0.TS)
# Strong salinity effect but no temperature effect!
# For comparison:
A0.T <- gam(cpue.re ~ s(temps, k=4))
A0.S <- gam(cpue.re ~ s(sals, k=4))
AIC(A0.TS, A0.T, A0.S)
# Salinity only model fits much better than temperature only
# and no evidence that temperature add signficiantly to the model
visreg(A0.S)  # Fitted model on log-scale

# I think the reason that this is not picked up by the model that includes 
# temperature and salinity effects is that the large seasonal variability 
# masks the interannual effect. Since these are probably distinct effects,
# it makes sense to separate within-season effects of temp/salinity from
# the effects of interannual variability in temp/salinity.

# You could use the 'two-step' approach that I followed above, or you could 
# include mean annual temps in the overall model as a covariate (instead of
# biweekly means).

# Now to visualizing the fits that include temp/salinity:
range(datA0$meantemp)

# Set up data frame, picking some year and station:
# For the gam model, it seems like I can't predict for the "average" year and
# the "average" station (that is, with the random effects set to zero), but to
# illustrate the fit, you can use any year and station. With multiple explanatory
# variables, any fitted model is conditional on setting other terms to some 
# value (for example salinity at the mean). Since there are no interactions,
# the shape of the relationship, which is what matters, is always the same
# regardless of what salinity, year or station you pick:
x <- seq(min(datA0$meantemp), max(datA0$meantemp), length=100)
new.df <- data.frame(meantemp=x, meansalin = mean(datA0$meansalin),Year.fac="2001", Station="214") 
p1<-predict(abundmodNB.A0.FJM4, newdata=new.df, se=T, type="link") # Std. error on log-scale
lwr <- exp(p1$fit - 1.96*p1$se.fit)  # Back-transformed lower confidence limit
upr <- exp(p1$fit + 1.96*p1$se.fit)  # Back-transformed upper confidence limit
# Note that I am not using a bias correction for the log-normal response,
# so the predicted values are the predicted median at each temperature
plot(x,exp(p1$fit), type="l", ylim=range(0, max(upr)), 
     xlab="Temparature",ylab="Age-0 abundance")
lines(x,lwr,lty=2)
lines(x,upr,lty=2)

# This seems to agree with the 'visreg' predictions for temperature:
visreg(abundmodNB.A0.FJM4, scale="response")

# Though insignificant, it appears that there is some basis that age-0 ARCS are
# associated with higher salinity & lower temps. There is a strong East-West component
# as well. Highest predicted catch is at Stn 230 (closest to Mackenzie), and the 
# lowest catch at 220 (farthest west)

###################################################



# JTP Feb 9. Trying out Franz's code for the other species/ages too


datA1 <- abundance.biwk %>% filter(Species == "ARCS", age.put == "age1") %>% 
  ungroup() %>% mutate(Station = as.factor(Station)) %>%
  filter(!is.na(meansalin) & !is.na(meantemp)) # rm rows w/ missing values to be able to compare

abundmodNB.A1.FJM1 <- mgcv::gam(ages_CPUE ~ Year + biweekly + s(meantemp, k=4) + 
                                  s(meansalin, k=4) + s(Station, bs="re"), 
                                family = "nb", link = "log", data = datA1) 
summary(abundmodNB.A1.FJM1)
plot(abundmodNB.A1.FJM1)

# Residual diagnostics
gam.check(abundmodNB.A1.FJM1)
# No major concerns about residual distribution

# Currently, the model assumes a linear trend over time and a linear trend
# within the season. That may be adequate but there will almost certainly 
# be other year-to-year variability not accounted for by the covariates 

# Hence, plot residuals against year and biweekly:
plot(datA1$Year, resid(abundmodNB.A1.FJM1))
plot(factor(datA1$Year), resid(abundmodNB.A1.FJM1))
# Similar to age-0 ARCS, there is a clear pattern over time that is NOT captured 
# by differences in temperature or salinity with much higher values in 2009-2014 
# and in some other years. 
# Could account for that through either a smooth temporal trend or rand yr effect

plot(datA1$biweekly, resid(abundmodNB.A1.FJM1))
plot(factor(datA1$biweekly), resid(abundmodNB.A1.FJM1))
# There does not appear to be a seasonal pattern. 

# One possibility of dealing with these apparent trends is to add a random
# effect for the year-to-year variability.
datA1$Year.fac <- factor(datA1$Year)
abundmodNB.A1.FJM2 <- mgcv::gam(ages_CPUE ~ s(Year.fac, bs="re") + biweekly + s(meantemp, k=4) + 
                                  s(meansalin, k=4) + s(Station, bs="re"), family = "nb", link = "log", 
                                data = datA1, na.action=na.exclude) 
summary(abundmodNB.A1.FJM2)
plot(abundmodNB.A1.FJM2)
# The model attributes the variability to year-to-year variation, 
# a biweekly linear trend, temperature, and Station. 
# Salinity effects are no longer significant

# The confounding btwn biweekly and temp/salin is an issue. Fit separate models,
# one  with a 'biweekly' trend and one with temp/salinity then compare:

# Linear seasonal trend:
abundmodNB.A1.FJM3 <- mgcv::gam(ages_CPUE ~ s(Year.fac, bs="re") + biweekly + s(Station, bs="re"), 
                                family = "nb", link = "log", data = datA1) 
summary(abundmodNB.A1.FJM3)
plot(abundmodNB.A1.FJM3)

# Smooth trends with temperature and salinity:
abundmodNB.A1.FJM4 <- mgcv::gam(ages_CPUE ~ s(Year.fac, bs="re") + s(meantemp, k=4) + 
                                  s(meansalin, k=4) + s(Station, bs="re"), 
                                family = "nb", link = "log", data = datA1) 
summary(abundmodNB.A1.FJM4)
#visreg(abundmodNB.A1.FJM4)
abundmodNB.A1.FJM4

# Top
abundmodNB.A1.FJM5 <- mgcv::gam(ages_CPUE ~ s(Year.fac, bs="re") + biweekly + s(meantemp, k=4) + 
                                  s(Station, bs="re"), family = "nb", link = "log", 
                                data = datA1, na.action=na.exclude) 
abundmodNB.A1.FJM6 <- mgcv::gam(ages_CPUE ~ s(Year.fac, bs="re") + biweekly + 
                                  s(meansalin, k=4) + s(Station, bs="re"), family = "nb", link = "log", 
                                data = datA1, na.action=na.exclude) 
AIC(abundmodNB.A1.FJM1, # Year +                 biweekly + s(temp) + s(salin) + s(Station, bs="re")
    abundmodNB.A1.FJM2, # s(Year.fac, bs="re") + biweekly + s(temp) + s(salin) + s(Station, bs="re")
    abundmodNB.A1.FJM3, # s(Year.fac, bs="re") + biweekly +                      s(Station, bs="re")
    abundmodNB.A1.FJM4, # s(Year.fac, bs="re") +            s(temp) + s(salin) + s(Station, bs="re")
    abundmodNB.A1.FJM5, # s(Year.fac, bs="re") + biweekly + s(temp) +            s(Station, bs="re")
    abundmodNB.A1.FJM6) # s(Year.fac, bs="re") + biweekly +           s(salin) + s(Station, bs="re")

# The model with biweekly, year, temp, and station fits the data best



# JTP: New visualizing using ggplot
new.df <- data.frame(meantemp=seq(min(datA1$meantemp), max(datA1$meantemp), length=100), 
                     biweekly = 3, meansalin = mean(datA1$meansalin),Year.fac="2017", Station="218") 
predA1 <- predict(abundmodNB.A1.FJM5, newdata=new.df, se=T, type="link") # Std. error on log-scale
preddf_A1_temp <- data.frame(temp = seq(min(datA1$meantemp), max(datA1$meantemp), length=100), 
                             lwr = exp(predA1$fit - 1.96*predA1$se.fit),  # Back-transformed lower confidence limit, 
                             predval = exp(predA1$fit), 
                             upr = exp(predA1$fit + 1.96*predA1$se.fit))  # Back-transformed upper confidence limit)
rm(new.df)


# Pred 
ggplot(preddf_A1_temp, aes(x=temp, y = predval)) + 
  geom_line() + 
  geom_line(aes(y=lwr), lty=2) + geom_line(aes(y=upr), lty=2) + 
  labs(x = "Temperature (C)", y = "Predicted ARCS Age-1 CPUE") +
  theme_minimal()



#rm(new.df,preddf_A1_temp)

# This seems to agree with the 'visreg' predictions for temperature:
visreg(abundmodNB.A1.FJM5, scale="response")




#################################################
# BROAD WHITEFISH AGE0



datB0 <- abundance.biwk %>% filter(Species == "BDWF", age.put == "age0") %>% 
  ungroup() %>% mutate(Station = as.factor(Station)) %>%
  filter(!is.na(meansalin) & !is.na(meantemp)) # rm rows w/ missing values to be able to compare

abundmodNB.B0.FJM1 <- mgcv::gam(ages_CPUE ~ Year + biweekly + s(meantemp, k=4) + 
                                  s(meansalin, k=4) + s(Station, bs="re"), 
                                family = "nb", link = "log", data = datB0) 
summary(abundmodNB.B0.FJM1)
plot(abundmodNB.B0.FJM1)

# Residual diagnostics
gam.check(abundmodNB.B0.FJM1)
# One extreme fitted value, but no major concerns about residual distribution

# This model assumes linear trend over time and a linear trend within the season. 
# That may be adequate but there will almost certainly 
# be other year-to-year variability not accounted for by the covariates 

# Hence, plot residuals against year and biweekly:
plot(datB0$Year, resid(abundmodNB.B0.FJM1))
plot(factor(datB0$Year), resid(abundmodNB.B0.FJM1))
# There  might be a pattern over time separate from the effects of temp/salinity
# Not as clear as ARCS 0/1
# Test random year effects below to account for this. 

plot(datB0$biweekly, resid(abundmodNB.B0.FJM1))
plot(factor(datB0$biweekly), resid(abundmodNB.B0.FJM1))
# There does not appear to be a seasonal pattern. 

# One possibility of dealing with these apparent trends is to add a random
# effect for the year-to-year variability.
datB0$Year.fac <- factor(datB0$Year)
abundmodNB.B0.FJM2 <- mgcv::gam(ages_CPUE ~ s(Year.fac, bs="re") + biweekly + s(meantemp, k=4) + 
                                  s(meansalin, k=4) + s(Station, bs="re"), family = "nb", link = "log", 
                                data = datB0, na.action=na.exclude) 
summary(abundmodNB.B0.FJM2)
plot(abundmodNB.B0.FJM2)
# The model attributes the variability to year-to-year variation, 
# a biweekly linear trend, temperature, salinity, and Station. 

# The confounding btwn biweekly and temp/salin is an issue. Fit separate models,
# one  with a linear 'biweekly' trend and one with smoothed temp/salinity then compare:

# Linear seasonal trend:
abundmodNB.B0.FJM3 <- mgcv::gam(ages_CPUE ~ s(Year.fac, bs="re") + biweekly + s(Station, bs="re"), 
                                family = "nb", link = "log", data = datB0) 
summary(abundmodNB.B0.FJM3)
plot(abundmodNB.B0.FJM3)
visreg(abundmodNB.B0.FJM3)

# Smooth trends with temperature and salinity:
abundmodNB.B0.FJM4 <- mgcv::gam(ages_CPUE ~ s(Year.fac, bs="re") + s(meantemp, k=4) + 
                                  s(meansalin, k=4) + s(Station, bs="re"), 
                                family = "nb", link = "log", data = datB0) 
summary(abundmodNB.B0.FJM4)
visreg(abundmodNB.B0.FJM4)



abundmodNB.B0.FJM5 <- mgcv::gam(ages_CPUE ~ s(Year.fac, bs="re") + biweekly + s(meantemp, k=4) + 
                                  s(Station, bs="re"), family = "nb", link = "log", 
                                data = datB0, na.action=na.exclude) 
abundmodNB.B0.FJM6 <- mgcv::gam(ages_CPUE ~ s(Year.fac, bs="re") + biweekly + 
                                  s(meansalin, k=4) + s(Station, bs="re"), family = "nb", link = "log", 
                                data = datB0, na.action=na.exclude) 
AIC(abundmodNB.B0.FJM1, # Year +                 biweekly + s(temp) + s(salin) + s(Station, bs="re")
    abundmodNB.B0.FJM2, # s(Year.fac, bs="re") + biweekly + s(temp) + s(salin) + s(Station, bs="re")
    abundmodNB.B0.FJM3, # s(Year.fac, bs="re") + biweekly +                      s(Station, bs="re")
    abundmodNB.B0.FJM4, # s(Year.fac, bs="re") +            s(temp) + s(salin) + s(Station, bs="re")
    abundmodNB.B0.FJM5, # s(Year.fac, bs="re") + biweekly + s(temp) +            s(Station, bs="re")
    abundmodNB.B0.FJM6) # s(Year.fac, bs="re") + biweekly +           s(salin) + s(Station, bs="re")



summary(abundmodNB.B0.FJM6) # Top model
# The model with biweekly, year, station, and salinity fits the data best




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

# Pred 

ggplot(preddf_B0_sal, aes(x=salin, y = predval)) + 
  geom_line() + 
  geom_line(aes(y=lwr), lty=2) + geom_line(aes(y=upr), lty=2) + 
  labs(x = "Salinity (ppt)", y = "Predicted BDWF Age-0 CPUE") +
  theme_minimal()

#rm(new.df,preddf_B0_temp, preddf_B0_sal)

# This agrees with the 'visreg' predictions for salinity:
visreg(abundmodNB.B0.FJM6, scale="response")



#######################################################################

# NOW BDWF AGE1

datB0


datB1 <- abundance.biwk %>% filter(Species == "BDWF", age.put == "age1") %>% 
  ungroup() %>% mutate(Station = as.factor(Station)) %>%
  filter(!is.na(meansalin) & !is.na(meantemp)) # rm rows w/ missing values to be able to compare

abundmodNB.B1.FJM1 <- mgcv::gam(ages_CPUE ~ Year + biweekly + s(meantemp, k=4) + 
                                  s(meansalin, k=4) + s(Station, bs="re"), 
                                family = "nb", link = "log", data = datB1) 
summary(abundmodNB.B1.FJM1)
plot(abundmodNB.B1.FJM1)

# Residual diagnostics
gam.check(abundmodNB.B1.FJM1)
# No major concerns about residual distribution

# Currently, the model assumes a linear trend over time and a linear trend
# within the season. That may be adequate but there will almost certainly 
# be other year-to-year variability not accounted for by the covariates 

# Hence, plot residuals against year and biweekly:
plot(datB1$Year, resid(abundmodNB.B1.FJM1))
plot(factor(datB1$Year), resid(abundmodNB.B1.FJM1))
# There does not appear to be a clear pattern over time, but perhaps difficult to see

plot(datB1$biweekly, resid(abundmodNB.B1.FJM1))
plot(factor(datB1$biweekly), resid(abundmodNB.B1.FJM1))
# There does not appear to be a seasonal pattern. 

# Similar to previous sp/age, we'll add rand effects to account for year-to-year variability.
datB1$Year.fac <- factor(datB1$Year)
abundmodNB.B1.FJM2 <- mgcv::gam(ages_CPUE ~ s(Year.fac, bs="re") + biweekly + s(meantemp, k=4) + 
                                  s(meansalin, k=4) + s(Station, bs="re"), family = "nb", link = "log", 
                                data = datB1, na.action=na.exclude) 
summary(abundmodNB.B1.FJM2)
plot(abundmodNB.B1.FJM2)
# The model attributes the variability to year-to-year variation, 
# a biweekly linear trend, temperature, and Station. Salinity effects are not significant

# That confounding is an issue and you may have to fit two separate models,
# one with with a 'biweekly' trend and one with temperature and salinity
# and then compare the models:

# Linear seasonal trend:
abundmodNB.B1.FJM3 <- mgcv::gam(ages_CPUE ~ s(Year.fac, bs="re") + biweekly + s(Station, bs="re"), 
                                family = "nb", link = "log", data = datB1) 
summary(abundmodNB.B1.FJM3)
plot(abundmodNB.B1.FJM3)

visreg(abundmodNB.B1.FJM3)

# Smooth trends with temperature and salinity:
abundmodNB.B1.FJM4 <- mgcv::gam(ages_CPUE ~ s(Year.fac, bs="re") + meantemp + 
                                  meansalin + s(Station, bs="re"), 
                                family = "nb", link = "log", data = datB1) 
# Update 3/21: Dropping the smoothing terms (meantemp, k=4) + s(meansalin, k=4) 
# because the EDF was 1.001 for both. Turning to linear. Reduces AIC and makes interp easy
summary(abundmodNB.B1.FJM4)
visreg(abundmodNB.B1.FJM4)





abundmodNB.B1.FJM5 <- mgcv::gam(ages_CPUE ~ s(Year.fac, bs="re") + biweekly + s(meantemp, k=4) + 
                                  s(Station, bs="re"), family = "nb", link = "log", 
                                data = datB1, na.action=na.exclude) 
abundmodNB.B1.FJM6 <- mgcv::gam(ages_CPUE ~ s(Year.fac, bs="re") + biweekly + 
                                  s(meansalin, k=4) + s(Station, bs="re"), family = "nb", link = "log", 
                                data = datB1, na.action=na.exclude) 
AIC(abundmodNB.B1.FJM1, # Year +                 biweekly + s(temp) + s(salin) + s(Station, bs="re")
    abundmodNB.B1.FJM2, # s(Year.fac, bs="re") + biweekly + s(temp) + s(salin) + s(Station, bs="re")
    abundmodNB.B1.FJM3, # s(Year.fac, bs="re") + biweekly +                      s(Station, bs="re")
    abundmodNB.B1.FJM4, # s(Year.fac, bs="re") +            s(temp) + s(salin) + s(Station, bs="re")
    abundmodNB.B1.FJM5, # s(Year.fac, bs="re") + biweekly + s(temp) +            s(Station, bs="re")
    abundmodNB.B1.FJM6) # s(Year.fac, bs="re") + biweekly +           s(salin) + s(Station, bs="re")


# The model with year, temp, salinity, and station fits the data best
# Changed this model to be linear 3/21/2020


# VISUALIZE

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


ggplot(preddf_B1_temp, aes(x=temp, y = predval)) + 
  geom_line() + 
  geom_line(aes(y=lwr), lty=2) + geom_line(aes(y=upr), lty=2) + 
  geom_point(data = datB1, aes(x=meantemp, y=ages_CPUE)) +
  labs(x = "Temperature (C)", y = "Predicted BDWF Age-1 CPUE") +
  theme_minimal()

ggplot(preddf_B1_sal, aes(x=salin, y = predval)) + 
  geom_line() + 
  geom_line(aes(y=lwr), lty=2) + geom_line(aes(y=upr), lty=2) + 
  labs(x = "Salinity (ppt)", y = "Predicted BDWF Age-1 CPUE") +
  theme_minimal()
#rm(new.df,preddf_B1_temp, preddf_B1_sal)


# This seems to agree with the 'visreg' predictions for temperature:
visreg(abundmodNB.B1.FJM4, scale="response")
summary(abundmodNB.B1.FJM4)

abundmodNB.A0.FJM3 # s(Year.fac, bs = "re") + biweekly + s(Station, bs = "re")
abundmodNB.A1.FJM5 # s(Year.fac, bs = "re") + biweekly + s(meantemp, k = 4) + s(Station, bs = "re")
abundmodNB.B0.FJM6 # s(Year.fac, bs = "re") + biweekly + s(meansalin, k = 4) + s(Station, bs = "re")
abundmodNB.B1.FJM4 # s(Year.fac, bs = "re") + s(meantemp, k = 4) + s(meansalin, k = 4) + s(Station, bs = "re")




anova(abundmodNB.A0.FJM3)
anova(abundmodNB.A1.FJM5)
anova(abundmodNB.B0.FJM6) 
anova(abundmodNB.B1.FJM4)


anova(abundmodNB.B0.FJM6)
gam.vcomp(abundmodNB.B0.FJM6)

summary(abundmodNB.B1.FJM4)
# Below this is previous CPUE modeling attempts using GAM, Quasi-Poisson, and prev NegBin 

# abundmod.A0 <- gam(ages_CPUE ~ Year + biweekly + s(meantemp) + s(meansalin) +
#                      s(Station, bs="re"), method = "REML", 
#                    data = abundance.biwk %>% filter(Species == "ARCS", age.put == "age0") %>% 
#                      ungroup() %>% mutate(Station = as.factor(Station)))
# 
# abundmod.A1 <- gam(ages_CPUE ~ Year + biweekly + s(meantemp) + s(meansalin) +
#                      s(Station, bs="re"), method = "REML", 
#                    data = abundance.biwk %>% filter(Species == "ARCS", age.put == "age1") %>% 
#                      ungroup() %>% mutate(Station = as.factor(Station)))
# 
# abundmod.B0 <- gam(ages_CPUE ~ Year + biweekly + s(meantemp) + s(meansalin) +
#                      s(Station, bs="re"), method = "REML", 
#                    data = abundance.biwk %>% filter(Species == "BDWF", age.put == "age0") %>% 
#                      ungroup() %>% mutate(Station = as.factor(Station)))
# 
# abundmod.B1 <- gam(ages_CPUE ~ Year + biweekly + s(meantemp) + s(meansalin) +
#                      s(Station, bs="re"), method = "REML", 
#                    data = abundance.biwk %>% filter(Species == "BDWF", age.put == "age1") %>% 
#                      ungroup() %>% mutate(Station = as.factor(Station)))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ### TRY WITH QUASIPOISSON
# summary(gam(ages_CPUE ~ Year + biweekly + s(meantemp) + s(meansalin) +
#               s(Station, bs="re"), method = "REML", family="quasipoisson",link = "log",
#             data = abundance.biwk %>% filter(Species == "ARCS", age.put == "age0") %>% 
#               ungroup() %>% mutate(Station = as.factor(Station))))
# abundmodQPois.A0 <- gam(ages_CPUE ~ Year + biweekly + s(meantemp) + meansalin + #removed 
#                      s(Station, bs="re"), method = "REML", family="quasipoisson",link = "log",
#                    data = abundance.biwk %>% filter(Species == "ARCS", age.put == "age0") %>% 
#                      ungroup() %>% mutate(Station = as.factor(Station)))
# 
# summary(gam(ages_CPUE ~ Year + biweekly + s(meantemp) + s(meansalin) +
#       s(Station, bs="re"), method = "REML", family="quasipoisson",link = "log",
#     data = abundance.biwk %>% filter(Species == "ARCS", age.put == "age1") %>% 
#       ungroup() %>% mutate(Station = as.factor(Station))))
# abundmodQPois.A1 <- gam(ages_CPUE ~ Year + biweekly + s(meansalin) +
#                      s(Station, bs="re"), method = "REML", family="quasipoisson",link = "log",
#                    data = abundance.biwk %>% filter(Species == "ARCS", age.put == "age1") %>% 
#                      ungroup() %>% mutate(Station = as.factor(Station)))
# 
# summary(gam(ages_CPUE ~ Year + biweekly + s(meantemp) + s(meansalin) +
#               s(Station, bs="re"), method = "REML", family="quasipoisson", link = "log",
#             data = abundance.biwk %>% filter(Species == "BDWF", age.put == "age0") %>% 
#               ungroup() %>% mutate(Station = as.factor(Station))))
# abundmodQPois.B0 <- gam(ages_CPUE ~ Year + biweekly + meantemp + s(meansalin) +
#                      s(Station, bs="re"), method = "REML", family="quasipoisson", link = "log",
#                    data = abundance.biwk %>% filter(Species == "BDWF", age.put == "age0") %>% 
#                      ungroup() %>% mutate(Station = as.factor(Station)))
# 
# summary(gam(ages_CPUE ~ Year + biweekly + s(meantemp) + s(meansalin) +
#       s(Station, bs="re"), method = "REML", family="quasipoisson", link = "log",
#     data = abundance.biwk %>% filter(Species == "BDWF", age.put == "age1") %>% 
#       ungroup() %>% mutate(Station = as.factor(Station))))
# abundmodQPois.B1 <- gam(ages_CPUE ~ Year + biweekly + s(meantemp) + s(meansalin) +
#                      s(Station, bs="re"), method = "REML", family="quasipoisson", link = "log",
#                    data = abundance.biwk %>% filter(Species == "BDWF", age.put == "age1") %>% 
#                      ungroup() %>% mutate(Station = as.factor(Station)))
# 
# 
# #Negative Binomial
# summary(glmer.nb(ages_CPUE ~ Year + biweekly + s(meantemp) + s(meansalin) + Station + (1|Station),
#     data = abundance.biwk %>% filter(Species == "ARCS", age.put == "age0") %>% 
#       ungroup() %>% mutate(Station = as.factor(Station))))
# 
# 
# abundmodNB.A0 <- mgcv::gam(ages_CPUE ~ Year + biweekly + s(meantemp, k=4) + 
#                              s(meansalin, k=4) + s(Station, bs="re"), family = "nb", link = "log", 
#                            data = abundance.biwk %>% filter(Species == "ARCS", age.put == "age0") %>% 
#                              ungroup() %>% mutate(Station = as.factor(Station))) 
# abundmodNB.A1 <- mgcv::gam(ages_CPUE ~ Year + biweekly + s(meantemp, k=4) + 
#                              s(meansalin, k=4) + s(Station, bs="re"), family = "nb", link = "log", 
#                            data = abundance.biwk %>% filter(Species == "ARCS", age.put == "age1") %>% 
#                              ungroup() %>% mutate(Station = as.factor(Station))) 
# 
# summary(mgcv::gam(ages_CPUE ~ Year + biweekly + s(meantemp, k=4) + 
#                     s(meansalin, k=4) + s(Station, bs="re"), family = "nb", link = "log", 
#                   data = abundance.biwk %>% filter(Species == "BDWF", age.put == "age0") %>% 
#                     ungroup() %>% mutate(Station = as.factor(Station)))) 
# abundmodNB.B0 <- mgcv::gam(ages_CPUE ~ Year + biweekly + s(meantemp, k=4) + 
#                              s(meansalin, k=4) + s(Station, bs="re"), family = "nb", link = "log", 
#                            data = abundance.biwk %>% filter(Species == "BDWF", age.put == "age0") %>% 
#                              ungroup() %>% mutate(Station = as.factor(Station))) 
# 
# summary(mgcv::gam(ages_CPUE ~ Year + biweekly + s(meantemp, k=4) + 
#             s(meansalin, k=4) + s(Station, bs="re"), family = "nb", link = "log", 
#           data = abundance.biwk %>% filter(Species == "BDWF", age.put == "age1") %>% 
#             ungroup() %>% mutate(Station = as.factor(Station)))) 
# abundmodNB.B1 <- mgcv::gam(ages_CPUE ~ Year + biweekly + s(meantemp, k=4) + 
#                              s(meansalin, k=4) + s(Station, bs="re"), family = "nb", link = "log", 
#                            data = abundance.biwk %>% filter(Species == "BDWF", age.put == "age1") %>% 
#                              ungroup() %>% mutate(Station = as.factor(Station))) 
# 
# summary(abundmodNB.A0)
# summary(abundmodNB.A1)
# summary(abundmodNB.B0)
# summary(abundmodNB.B1)
# 
# 
# #A0: station effects don't really change anything. Y+B+T+S+S
# #A1: biweekly effects very slight
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # Explore model output and residuals
# explore_model <- function(model){ 
#   require(ggplot2)
#   #.name_sp_age <- substr(deparse(substitute(model)),10,11)
#   .name_sp_age <- strsplit(deparse(substitute(model)), split = ".", fixed = TRUE)[[1]][2]
#   print(summary(model))
#   plot(residuals.gam(model), main = paste0(.name_sp_age))
#   readline(prompt="Press [enter] to continue")
#   print(ggplot(data.frame(eta=predict(model,type="link"),pearson=residuals(model,type="pearson")),
#                aes(x=eta,y=pearson)) +
#           geom_point() + labs(title = .name_sp_age) +
#           theme_bw())
#   readline(prompt="Press [enter] to continue")
#   invisible(qqnorm(residuals(model)))
#   invisible(qqline(residuals(model), col = "steelblue"))
# }
# 
# explore_model(abundmod.A0)
# explore_model(abundmodQPois.A0)
# explore_model(abundmod.A1)
# explore_model(abundmodQPois.A1)
# explore_model(abundmod.B0)
# explore_model(abundmodQPois.B0)
# explore_model(abundmod.B1)
# explore_model(abundmodQPois.B1)
# 
# explore_model(abundmodNB.A0)
# explore_model(abundmodNB.A1)
# explore_model(abundmodNB.B0)
# explore_model(abundmodNB.B1)
# 
# 
# 
# 
# #Summary: Quasipoisson models substantially increase deviance explained
# # and residual plots look better for several ages
# 
# summary(abundmodQPois.A1)$dispersion #very high dispersion
# 
# 
# 
# 
# pointvals.CPUE <- abundance.biwk %>% filter(age.put == "age0" | age.put == "age1") %>% 
#   ungroup() %>% mutate(Station = as.factor(Station),
#                         Sp.Age = paste0(Species, "_", age.put)) %>% 
#   dplyr::select(-totalages, -Effort_NetHrs, -Species, -age.put) %>%
#   mutate(Sp.Age = recode(Sp.Age, "ARCS_age0" = "ARCS_Age0", "ARCS_age1" = "ARCS_Age1",
#                          "BDWF_age0" = "BDWF_Age0", "BDWF_age1" = "BDWF_Age1"),
#          pred.gam = ages_CPUE)
# 
# pointvals.CPUE2 <- abundance.biwk %>% filter(age.put == "age0" | age.put == "age1") %>%
#   group_by(Year, biweekly, Species, age.put) %>%
#   summarise(totalages = sum(totalages, na.rm = TRUE),
#             Effort_NetHrs = sum(Effort_NetHrs),
#             meantemp = mean(meantemp),
#             meansalin = mean(meansalin)) %>% 
#   mutate(totalages = replace_na(totalages, 0), 
#          ages_CPUE = totalages / (Effort_NetHrs / (672*4) )) # 672 is num hrs in full sample biwk, 4 stns
# 
# 
# 
# 
# 
# 
# # QUASIPOISSON
# pred_abundQP <- expand.grid(Year = 2001:2018, biweekly = seq(from=1, to = 4, by=0.5),
#                           Station = as.factor(c("230","214","218","220")),
#                           meansalin = seq(floor(min(growth.biwk$meansalin, na.rm = TRUE)),
#                                            ceiling(max(growth.biwk$meansalin, na.rm = TRUE)), by=1),
#                           meantemp = seq(floor(min(growth.biwk$meantemp, na.rm = TRUE)),
#                                           ceiling(max(growth.biwk$meantemp, na.rm = TRUE)), by=0.5))
# 
# pred_abundQP$ARCS_Age0 <- predict.gam(abundmodQPois.A0, type = "response", newdata = pred_abundQP)
# pred_abundQP$ARCS_Age1 <- predict.gam(abundmodQPois.A1, type = "response", newdata = pred_abundQP)
# pred_abundQP$BDWF_Age0 <- predict.gam(abundmodQPois.B0, type = "response", newdata = pred_abundQP)
# pred_abundQP$BDWF_Age1 <- predict.gam(abundmodQPois.B1, type = "response", newdata = pred_abundQP)
# 
# 
# 
# pred_abundQP <- gather(pred_abundQP, Sp.Age, preds, -c(Year, biweekly, Station, meansalin, meantemp) )
# pred_abundQP <- pred_abundQP %>%
#   mutate(pred.se = if_else(Sp.Age=="ARCS_Age0", 
#                            predict.gam(abundmodQPois.A0, type = "response", newdata = pred_abundQP, se = TRUE)$se.fit,
#                            if_else(Sp.Age=="ARCS_Age1", predict.gam(abundmodQPois.A1, type = "response",
#                                                                     newdata = pred_abundQP, se = TRUE)$se.fit,
#                                    if_else(Sp.Age=="BDWF_Age0", predict.gam(abundmodQPois.B0, type = "response",
#                                                                             newdata = pred_abundQP, se = TRUE)$se.fit,
#                                            if_else(Sp.Age=="BDWF_Age1", predict.gam(abundmodQPois.B1, type = "response",
#                                                                                     newdata = pred_abundQP, se = TRUE)$se.fit,
#                                                    0)))))
# 
# 
# ggplot(pred_abundQP %>% group_by(Sp.Age, meansalin) %>% 
#          summarise(pred.gam = mean(preds),
#                    pred.gam.se = mean(pred.se)), 
#        aes(x=meansalin, y=pred.gam)) + 
#   geom_line() +
#   scale_y_continuous("Predicted CPUE", limits = c(0, 6000), oob=scales::rescale_none) +
#   scale_x_continuous("Salinity (ppt)") +
#   geom_ribbon(aes(ymin=pred.gam-(1*pred.gam.se), ymax=pred.gam+(1*pred.gam.se)), alpha = 0.3) +
#   facet_wrap(. ~ Sp.Age) +
#   geom_point(data = pointvals.CPUE) +
#   ggtitle("QuasiPoisson") +
#   theme_sleek()
# 
# ggplot(pred_abundQP %>% group_by(Sp.Age, meantemp) %>% 
#          summarise(pred.gam = mean(preds),
#                    pred.gam.se = mean(pred.se)), 
#        aes(x=meantemp, y=pred.gam)) + 
#   geom_line() +
#   scale_y_continuous("Predicted CPUE", limits = c(0, 15000), oob=scales::rescale_none) +
#   scale_x_continuous("Water Temperature (C)") +
#   geom_ribbon(aes(ymin=pred.gam-(1*pred.gam.se), ymax=pred.gam+(1*pred.gam.se)), alpha = 0.3) +
#   facet_wrap(. ~ Sp.Age) +
#   geom_point(data = pointvals.CPUE, alpha =0.5) +
#   ggtitle("QuasiPoisson") +
#   theme_sleek()
# 
# 
# 
# 
# # NEGATIVE BINOMIAL
# pred_abund <- expand.grid(Year = 2001:2018, biweekly = seq(from = 1, to = 4, by = 0.5),
#                             Station = as.factor(c("230","214","218","220")),
#                             meansalin = seq(floor(min(abundance.biwk$meansalin, na.rm = TRUE)),
#                                             ceiling(max(abundance.biwk$meansalin, na.rm = TRUE)), by=1),
#                             meantemp = seq(floor(min(abundance.biwk$meantemp, na.rm = TRUE)),
#                                            ceiling(max(abundance.biwk$meantemp, na.rm = TRUE)), by=0.5))
# 
# 
# 
# 
# pred_abund$ARCS_Age0 <- predict.gam(abundmodNB.A0, type = "response", newdata = pred_abund)
# pred_abund$ARCS_Age1 <- predict.gam(abundmodNB.A1, type = "response", newdata = pred_abund)
# pred_abund$BDWF_Age0 <- predict.gam(abundmodNB.B0, type = "response", newdata = pred_abund)
# pred_abund$BDWF_Age1 <- predict.gam(abundmodNB.B1, type = "response", newdata = pred_abund)
# 
# pred_abund <- gather(pred_abund, Sp.Age, preds, -c(Year, biweekly, Station, meansalin, meantemp) )
# pred_abund <- pred_abund %>%
#   mutate(pred.se = if_else(Sp.Age=="ARCS_Age0", predict.gam(abundmodNB.A0, type = "response", 
#                                                             newdata = pred_abund, se = TRUE)$se.fit,
#                            if_else(Sp.Age=="ARCS_Age1", predict.gam(abundmodNB.A1, type = "response",
#                                                                     newdata = pred_abund, se = TRUE)$se.fit,
#                                    if_else(Sp.Age=="BDWF_Age0", predict.gam(abundmodNB.B0, type = "response",
#                                                                             newdata = pred_abund, se = TRUE)$se.fit,
#                                            if_else(Sp.Age=="BDWF_Age1", predict.gam(abundmodNB.B1, type = "response",
#                                                                                     newdata = pred_abund, se = TRUE)$se.fit,
#                                                    0)))))
# 
# 
# 
# # Not final figures. See .RMD for final figures
# ggplot(pred_abund %>% group_by(Sp.Age, meansalin) %>% 
#          summarise(pred.gam = mean(preds),
#                    pred.gam.se = mean(pred.se)), 
#        aes(x=meansalin, y=pred.gam)) + 
#   geom_line() +
#   scale_y_continuous("Predicted CPUE") + # , limits = c(0, 6000), oob=scales::rescale_none
#   scale_x_continuous("Salinity (ppt)") +
#   geom_ribbon(aes(ymin=pred.gam-(1*pred.gam.se), ymax=pred.gam+(1*pred.gam.se)), alpha = 0.3) +
#   facet_wrap(. ~ Sp.Age, scales = "free_y") +
#   geom_point(data = pointvals.CPUE, alpha =0.5) +
#   ggtitle("Negative Binomial") +
#   theme_sleek()
# 
# ggplot(pred_abund %>% group_by(Sp.Age, meantemp) %>% 
#          summarise(pred.gam = mean(preds),
#                    pred.gam.se = mean(pred.se)), 
#        aes(x=meantemp, y=pred.gam)) + 
#   geom_line() +
#   scale_y_continuous("Predicted CPUE") + # , limits = c(0, 15000), oob=scales::rescale_none
#   scale_x_continuous("Water Temperature (C)") +
#   geom_ribbon(aes(ymin=pred.gam-(1*pred.gam.se), ymax=pred.gam+(1*pred.gam.se)), alpha = 0.3) +
#   facet_wrap(. ~ Sp.Age, scales = "free_y") +
#   geom_point(data = pointvals.CPUE, alpha =0.5) +
#   ggtitle("Negative Binomial") +
#   theme_sleek()
# 
# # There's currently a problem with NegBin predicted values
# # I'm guessing that we need to account for this by using a zero-inflated model
# 
# 
# 
# 
# 
