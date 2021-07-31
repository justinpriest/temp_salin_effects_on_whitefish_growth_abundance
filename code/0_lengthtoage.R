# This is an unaltered copy of JTP's script that parsed out likely ages based upon lengths.
# The original script is from my file in "Grad School\Slope Project\2018ThesisAnalysis\lengthtoage.R"
# Do not re-run this script unless something goes drastically wrong. It is only included here for 
#  understanding how/what decisions were made to assign ages and which info was pasted in "lengthtoage.xlsx"


# This script checked how the lengths could be used to assume a likely age, based for three whitefish species and
# varying for each year. Basically for each year, we used a mixture model (assuming normal dist) to generate three
# parameters for each species, year, and age class: alpha (y-int of mm length on day 180), beta (growth rate, mm/day),
# and sigma (variance for that age class).

# Section 1 is just exploratory, then the values were manually entered into an excel document. The starting params for 
# regmixEM varied GREATLY between each species/year. Sometimes a "good" fit could be found by setting sigma/beta/lambda
# for each year. There was no consistent way to produce results, especially to constrain beta (growth), to positive values.
# A good fit was determined if growth was positve, and visually to see if it bisected the cloud. I assumed a linear growth 
# trend over the ~65 day growing season.

# Section 2 applied the excel doc of age class length and growth, then corrected for any obvious errors. If there 
# was a clear issue, I manually set values based on expected values, previous/post years (if a cloud appears/disappears
# between years, could use that info to inform judgment), as well as visual assessment

library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(mixtools)
library(readxl)


#DATA IMPORT AND CLEANUP
#setwd("C:/Users/justi/Desktop/Grad School/Slope Project/2018ThesisAnalysis")
load("../Data/PrudhoeCatch&LengthDataset_2001-2018_Version8.Rdata")

pru_lengths <- all.len

pru_lengths <- pru_lengths[pru_lengths$Length!=-9999,]
pru_lengths <- pru_lengths[pru_lengths$Length>0,]



#################
### SECTION 1 ###
#################
############# Run EM mixture models for all three species. These values were then manually written into "lengthtoage.xlsx" 
#################

#Explore data and test out some trends
bdwf2016 <- pru_lengths %>% filter(Year == 2016, Length < 250, Species == "BDWF")
plot(bdwf2016$Length ~ bdwf2016$day.of.year)

#4 mode test
bdwfregtest1 <- (regmixEM(y=bdwf2016$Length, x=(bdwf2016$day.of.year - 180), 
                         beta = matrix(c(40, .6, 95, 1, 127, 1.5, 174, .75), ncol = 4), 
                         sigma = c(5, 5, 5, 5), k=4))
summary(bdwfregtest1)

bdwfregtest <- regmixEM(y=bdwf2016$Length, x=(bdwf2016$day.of.year - 180), 
                         beta = matrix(c(40, .6, 95, 1, 127, 1.5, 174, .75, 220, 0.5), ncol = 5), 
                         sigma = c(5, 5, 5, 5, 5), k=5)
summary(bdwfregtest)

#plot and see if it works
ggplot(bdwf2016, aes(x=(day.of.year - 180), y=Length)) + 
  geom_point(colour = "dark blue", size = 1.5, alpha = 0.5) +
  scale_y_continuous(limits = c(0,250)) + 
  geom_abline(intercept = bdwfregtest$beta[1,1], slope = bdwfregtest$beta[2,1]) +
  geom_abline(intercept = bdwfregtest$beta[1,2], slope = bdwfregtest$beta[2,2]) +
  geom_abline(intercept = bdwfregtest$beta[1,3], slope = bdwfregtest$beta[2,3]) +
  geom_abline(intercept = bdwfregtest$beta[1,4], slope = bdwfregtest$beta[2,4]) 



bdwf2017 <- pru_lengths %>% filter(Year == 2017, Length < 250, Length > 45, Species == "BDWF")
plot(Length ~ day.of.year, data = bdwf2017)
abline(89-(180*0.87), 0.87)
abline(27-(180*0.864), 0.864)
abline(220-(180*-2.66), -2.66)


#4 mode
bdwfregtest2013 <- regmixEM(y=bdwf2013$Length, x=(bdwf2013$day.of.year - 180), 
                            beta = matrix(c(40, .6, 100, 1, 160, 1.5, 220, 0.5), ncol = 4), 
                            sigma = c(5, 5, 5, 5), k=4)

bdwfregtest2016 <- regmixEM(y=bdwf2016$Length, x=(bdwf2016$day.of.year - 180), #lambda = c(0.003, 0.427, 0.54, 0.03),
                            beta = matrix(c(30, .6, 80, 1, 120, 1, 155, 1.5, 220, 0.5), ncol = 5), 
                            sigma = c(3, 5, 10, 10, 10), k=5)
summary(bdwfregtest2016)


#### Actual working section
#### by modifying params in here, I got values for the excel doc. JTP Aug 2018
#### I wrote these values down into the excel doc and manually had to change some values

for(i in 2018:2018){
  .tempdf <- pru_lengths %>% filter(Year == i, Length < 250, Length > 45, Species == "BDWF")
  .summ <- summary(regmixEM(y=.tempdf$Length, x=(.tempdf$day.of.year - 180), 
           beta = matrix(c(30, .5, 80, 1, 160, 1.5, 220, 1), ncol = 4), 
           lambda = c(0.01, 0.53, 0.45, 0.01),
           sigma = c( 1, 4, 4, 4), k=4))
  print(.summ)
  plot(Length ~ day.of.year, data = .tempdf)
}
abline(89-(180*.5), .5)
abline(144-(180*.53), .53)
abline(204-(180*.27), .27)
abline(35-(180*.5), .5)
abline(-6-(180*1.25), 1.25)

#now test for ARCS
for(i in 2018:2018){
  .tempdf <- pru_lengths %>% filter(Year == i, Length < 250, Length > 45, Species == "ARCS")
  .summ <- summary(regmixEM(y=.tempdf$Length, x=(.tempdf$day.of.year - 180), 
                            beta = matrix(c(50, 0.5, 90, 1, 220, 1), ncol = 3), 
                            #lambda = c(0.01, 0.01, 0.75, 0.23),
                            sigma = c( 10, 4, 4), k=3))
  print(.summ)
  plot(Length ~ day.of.year, data = .tempdf)
}

abline(45-(180*.7), .7)
abline(124-(180*.74), .74)
abline(178-(180*.50), .5)
abline(222-(180*.32), .32)

#For certain two or three year groups, by combining the years, we could see age classes across years.
# so while indiv years would fail model, year groups provided enough resolution
arcs2005_2006 <- pru_lengths %>% filter(Year == 2005 | Year == 2006, Length < 250, Length > 45, Species == "ARCS")
#plot(Length ~ day.of.year, data = arcs2005_2006)

arcs2005_2006$doy.comb <- arcs2005_2006$day.of.year
#arcs2005_2006[arcs2005_2006$Year == 2006,"doy.comb"] <- arcs2005_2006 %>% filter(Year == 2006) %>% mutate(doy.comb = day.of.year + 62)
arcs2005_2006[arcs2005_2006$Year == 2006,"doy.comb"] <- arcs2005_2006[arcs2005_2006$Year == 2006,"day.of.year"] +62
#plot(Length ~ doy.comb, data = arcs2005_2006)
summary(regmixEM(y=arcs2005_2006$Length, x=(arcs2005_2006$doy.comb - 180), 
                 beta = matrix(c(30, 0.5, 90, 1, 160, 1, 220, 1), ncol = 4), 
                 #lambda = c(0.01, 0.01, 0.75, 0.23),
                 sigma = c( 10, 4, 4, 4), k=4))
plot(Length ~ day.of.year, data = arcs2006)
abline(31-(180*1.01)+62, 1.01)


arcs2009_10 <- pru_lengths %>% filter(Year == 2009 | Year == 2010, 
                                           Length < 250, Length > 45, Species == "ARCS")
#plot(Length ~ day.of.year, data = arcs2005_2006)

arcs2009_10$doy.comb <- arcs2009_10$day.of.year
#arcs2005_2006[arcs2005_2006$Year == 2006,"doy.comb"] <- arcs2005_2006 %>% filter(Year == 2006) %>% mutate(doy.comb = day.of.year + 62)
arcs2009_10[arcs2009_10$Year == 2010,"doy.comb"] <- arcs2009_10[arcs2009_10$Year == 2010,
                                                                               "day.of.year"] +62
# arcs2008_2009_10[arcs2008_2009_10$Year == 2010,"doy.comb"] <- arcs2008_2009_10[arcs2008_2009_10$Year == 2010,
#                                                                                "day.of.year"] +124

summary(regmixEM(y=arcs2009_10$Length, x=(arcs2009_10$doy.comb - 180), 
                 beta = matrix(c(50, 1, 95, 1, 150, 1), ncol = 3), 
                 lambda = c(0.3, 0.65, 0.05),
                 sigma = c(3, 10, 3), k=3))
plot(Length ~ day.of.year, data = arcs2006)
abline(31-(180*1.01)+62, 1.01)


#################
### SECTION 2 ###
#################
############# Assign lengths to putative ages based on species, length, year, and day 
#################


# Now that the mixture model is complete, import these values. Manually set blank and wrong values
bdwfbreaks <- read_excel("lengthtoage.xlsx", sheet = "bdwf", col_types = "numeric")
bdwfbreaks <- bdwfbreaks[,-c(1,9,15,21,27)] # drop blank columns
bdwfbreaks[1,c(8, 13, 18)] <- c(5, 30, 0.5) #for 2001 age0, manually add sig=5, yint=30, growth(slope)=0.5
bdwfbreaks[3,c(8, 13, 18)] <- c(5, 30, 0.5) #for 2003 age0, manually add sig=5, yint=30, growth(slope)=0.5
bdwfbreaks[5,c(8, 13, 18)] <- c(1, 30, 0.1) #for 2005 age0, add really low line (even though no age0 caught)
bdwfbreaks[6,c(11, 16, 21)] <- c(5, 195, 0.75) #for 2006 age3, manually change sig=5, yint=195, growth(slope)=0.75
bdwfbreaks[6,c(12, 17, 22)] <- c(5, 230, 0.65) #for 2006 age4, manually change sig=5, yint=230, growth(slope)=0.65
bdwfbreaks[9,c(8, 13, 18)] <- c(5, 30, 0.7) #for 2009 age0, manually add sig=5, yint=30, growth(slope)=0.7
bdwfbreaks[12,c(10, 15, 20)] <- c(5, 150, 0.75) #for 2012 age2, manually add sig=5, yint=150, growth(slope)=0.75
bdwfbreaks[14,c(8, 13, 18)] <- c(5, 25, 0.5) #for 2014 age0, manually add sig=5, yint=25, growth(slope)=0.5
bdwfbreaks[8,c(11, 16, 21)] <- c(3, 210, 0.75) #for 2012 age3, manually add sig=3, yint=210, growth(slope)=0.75
bdwfbreaks[12,c(11, 16, 21)] <- c(3, 190, 0.7) #for 2012 age3, manually add sig=3, yint=190, growth(slope)=0.7
bdwfbreaks[17,c(11, 16, 21)] <- c(5, 185, 0.6) #for 2017 age3, manually add sig=5, yint=190, growth(slope)=0.7
bdwfbreaks[17,c(12, 17, 22)] <- c(7, 220, 0.5) #for 2017 age4, manually add sig=7, yint=190, growth(slope)=0.7
#bdwfbreaks <- bdwfbreaks[-18,] # remove 2018 for now until we get a complete season


# now run a quick plot of the values from above for a subset of years
# if param values don't work, change above values manually
for(i in 2018:2018){
  print(
  ggplot(pru_lengths %>% filter(Year == i & Species == "BDWF"), aes(x=day.of.year, y=Length)) + 
    geom_point(colour = "dark blue", size = 1.5, alpha = 0.5) +
    scale_y_continuous(limits = c(0,250)) +
    #scale_x_datetime(limits = c(as_datetime("2015-06-30 00:00"), as_datetime("2015-09-03 00:00"))) +
    labs(title=paste0("BDWF lengths ", i), y="Length (mm)", x="Date") +
    geom_abline(aes(intercept=(alpha0 - (180*beta0)),slope=beta0), 
                data = bdwfbreaks %>% filter(Year == i)) +
    geom_abline(aes(intercept=(alpha1 - (180*beta1)),slope=beta1), 
                data = bdwfbreaks %>% filter(Year == i)) + 
    geom_abline(aes(intercept=(alpha2 - (180*beta2)),slope=beta2), 
                data = bdwfbreaks %>% filter(Year == i)) +
    geom_abline(aes(intercept=(alpha3 - (180*beta3)),slope=beta3), 
                data = bdwfbreaks %>% filter(Year == i)) +
    geom_abline(aes(intercept=(alpha4 - (180*beta4)),slope=beta4), 
                data = bdwfbreaks %>% filter(Year == i))
  )
  readline()
}
#previous loop is to plot our data and make sure that everything looks good. 


################
# Now find out the cutoff lengths at which a fish is more likely one age than another, by doy and year
#check that there are no NAs in alpha0:3, beta0:3, sigma0:3
#if no age0 are caught in a certain year, add artifical low "regression" line

nrow(bdwfbreaks)
breakpoints.bdwf <- data.frame()
for(i in 1:nrow(bdwfbreaks)){
  for(j in c(1,65)){
    # this looks for the slope on day 1, then again on day 65 (approx season duration)
    # next it assigns a normal distribution based on mean and variance for lengths under 250mm
    bins = 1:250
    .p1 <- diff(pnorm(bins, bdwfbreaks$alpha0[i] + ((j)*bdwfbreaks$beta0[i]), bdwfbreaks$sigma0[i]))
    .p2 <- diff(pnorm(bins, bdwfbreaks$alpha1[i] + ((j)*bdwfbreaks$beta1[i]), bdwfbreaks$sigma1[i]))
    .p3 <- diff(pnorm(bins, bdwfbreaks$alpha2[i] + ((j)*bdwfbreaks$beta2[i]), bdwfbreaks$sigma2[i]))
    .p4 <- diff(pnorm(bins, bdwfbreaks$alpha3[i] + ((j)*bdwfbreaks$beta3[i]), bdwfbreaks$sigma3[i]))
    
    .probs <- cbind(.p1,.p2,.p3,.p4)
    
    .break <- as.data.frame(.probs / apply(.probs,1,sum))
    
    .break[.break <  0.001] <- 0 # this eliminates the tiny fractions which threw it off
    if(j<2){
      .break <- .break[40:(length(bins)-1),] # this restricts it to looking above 40 mm for cutoff
      breakpoints.bdwf[i,1] <- which.max((.break[,2]>.break[,1])) + 40
      breakpoints.bdwf[i,2] <- which.max((.break[,3]>.break[,2])) + 40
      breakpoints.bdwf[i,3] <- which.max((.break[,4]>.break[,3])) + 40
      # these are the break points at which a length is more likely to be one age than another
    } else{
      .break <- .break[80:(length(bins)-1),] # this restricts it to looking above 80 mm for cutoff
      breakpoints.bdwf[i,4] <- which.max((.break[,2]>.break[,1])) + 80
      breakpoints.bdwf[i,5] <- which.max((.break[,3]>.break[,2])) + 80
      breakpoints.bdwf[i,6] <- which.max((.break[,4]>.break[,3])) + 80 
    }
  }
}
breakpoints.bdwf[2,6] <- 195 #manually had to revise upwards
breakpoints.bdwf[18,3] <- 173 #shifted breakpoint line up 5 mm based on visual fit
breakpoints.bdwf[18,6] <- 202
#Note: manually fix any issue years here!

ggplot(pru_lengths %>% filter(Year == 2018, Species == "BDWF", Length <= 250), aes(x=day.of.year, y = Length)) + 
  geom_point(alpha=0.5) + geom_segment(aes(x=180, y=173, xend = 244, yend = 202))


breakpoints.bdwf$age01cut_slope <- (breakpoints.bdwf$V4-breakpoints.bdwf$V1)/64
breakpoints.bdwf$age12cut_slope <- (breakpoints.bdwf$V5-breakpoints.bdwf$V2)/64
breakpoints.bdwf$age23cut_slope <- (breakpoints.bdwf$V6-breakpoints.bdwf$V3)/64
breakpoints.bdwf



# for loop
# run all years and make cuts for all 65 doy,
# assign under age01 cut to age 0, assign between 01 and 12 cuts to age 1, etc
allbreaks.bdwf <- data.frame(year= "", doy= "", age01cut= "", age12cut= "", stringsAsFactors = FALSE)
allbreaks.bdwf <- sapply( allbreaks.bdwf, as.numeric )
cuts <- data.frame(age01cut=rep(0,68), age12cut=rep(0,68), age23cut=rep(0,68))
doy <- vector()
for(j in c(1:nrow(bdwfbreaks))){
  #get values for each day of the year (doy)
  for(i in seq(-2, 65)){ #doy of year. starts at -2 because some years sampling started on doy 178
    doy[i+3] <- i + 180 # i + 3 to compensate for starting at doy178
    cuts$age01cut[i+3] <- breakpoints.bdwf[j,1] + ((i)*breakpoints.bdwf$age01cut_slope[j])
    cuts$age12cut[i+3] <- breakpoints.bdwf[j,2] + ((i)*breakpoints.bdwf$age12cut_slope[j])
    cuts$age23cut[i+3] <- breakpoints.bdwf[j,3] + ((i)*breakpoints.bdwf$age23cut_slope[j])
    #can implement this later but there's a neg slope which needs to be fixed
  }
  
  .yearbreaks <- cbind(year = rep(j+2000, length(doy)), doy, cuts)
  
  allbreaks.bdwf <- bind_rows(allbreaks.bdwf, .yearbreaks)
  
}


bdwfallages <- pru_lengths %>% filter(Length < 250, Species == "BDWF")
bdwfallages <- left_join(bdwfallages, allbreaks.bdwf, by = c("Year" = "year", "day.of.year" = "doy"))
bdwfallages$age.put <- if_else(bdwfallages$Length <= bdwfallages$age01cut, "age0", 
                           if_else(bdwfallages$Length <= bdwfallages$age12cut, "age1", 
                                   if_else(bdwfallages$Length <= bdwfallages$age23cut, "age2", "age3plus")))

bdwfallages$broodyr <- if_else(bdwfallages$age.put == "age0", bdwfallages$Year - 1, 
                               if_else(bdwfallages$age.put == "age1", bdwfallages$Year - 2, 
                                       if_else(bdwfallages$age.put == "age2", bdwfallages$Year - 3, 
                                               bdwfallages$Year - 4))) #beware any NAs


agesummary_bdwf <- bdwfallages %>% group_by(Year, age.put) %>% summarise(total=sum(totcount))

agesummary_bdwf <- spread(agesummary_bdwf, age.put, total)
agesummary_bdwf$age0[is.na(agesummary_bdwf$age0)] <- 0
agesummary_bdwf



ggplot(bdwfallages %>% filter(Year >= 2013 & Year <= 2018), aes(x=day.of.year, y=Length, color = age.put)) + 
  geom_point(size = 1.5, alpha = 0.5) +
  scale_y_continuous(limits = c(0,250)) +
  #scale_x_datetime(limits = c(as_datetime("2015-06-30 00:00"), as_datetime("2015-09-03 00:00"))) +
  labs(title=paste0("BDWF lengths "), y="Length (mm)", x="Date") + 
  facet_wrap(~Year, ncol = 6)

ggplot(bdwfallages %>% filter(Year >= 2013 & Year <= 2018), aes(x=day.of.year, y=Length, color = as.factor(broodyr))) + 
  geom_point(size = 1.5, alpha = 0.5) +
  scale_y_continuous(limits = c(0,250)) +
  #scale_x_datetime(limits = c(as_datetime("2015-06-30 00:00"), as_datetime("2015-09-03 00:00"))) +
  labs(title=paste0("BDWF lengths "), y="Length (mm)", x="Date") + 
  facet_wrap(~Year, ncol = 6)



################
## convert these to total number of ages per LengthGroup

unmeasuredfish.bdwf <- allcatch %>% filter(Species == "BDWF", LG == 1 | LG == 2) %>% group_by(Year, EndDate, Net, LG) %>% 
  summarise(totcount=sum(totcount, na.rm = TRUE)) %>% 
  left_join(all.len %>%
              mutate(Net = paste0(Station, Side)) %>%
              filter( Species == "BDWF") %>% group_by(Year, EndDate, Net, Group) %>% 
              summarise(Lengthcount=sum(totcount, na.rm = TRUE)), 
            by = c("Year" = "Year", "EndDate" = "EndDate", "Net" = "Net", "LG" = "Group")) %>%
  replace(., is.na(.), 0) %>%
  mutate(unmeasured = totcount - Lengthcount) 



allcatch.bdwfages <- unmeasuredfish.bdwf %>% left_join(bdwfallages %>%
                                                         mutate(Net = paste0(Station, Side)) %>%
                                                         #filter(Year == 2018) %>% 
                                                         group_by(Year, EndDate, Net, Group, age.put) %>% 
                                                         summarise(agecount=sum(totcount, na.rm = TRUE)), 
                                                       by = c("Year" = "Year", "EndDate" = "EndDate", "Net" = "Net", "LG" = "Group")) %>%
  mutate(unleng_ages = round(unmeasured*(agecount / Lengthcount)),
         totalages = agecount + unleng_ages) %>% filter(totcount > 0)
# there are some rows with NA ages. As best as I can tell, these are from days where there were totals but no lengths 
# (so we can't partition out estimated ages). 

allcatch.bdwfages %>% group_by(Year, age.put) %>% summarise(totals = sum(totalages)) %>% spread(age.put, value = totals )
allcatch %>% filter(Species == "BDWF", LG == 3) %>% group_by(Year) %>% summarise(sum(totcount))


ggplot(allcatch.bdwfages %>% filter(age.put != is.na(age.put)) %>% 
       group_by(Year, age.put) %>% summarise(totals = sum(totalages)), 
       aes(x=Year, y = totals, color = age.put)) + geom_line() + facet_wrap(~age.put, ncol = 2, scales = "free")


############### ARCS ###############
#Now all of the above but with ARCS
####################################

arcsbreaks <- read_excel("lengthtoage.xlsx", sheet = "arcs", col_types = "numeric")
arcsbreaks <- arcsbreaks[,-c(1,9,15,21,27)]
arcsbreaks[1,c(8, 13, 18)] <- c(4, 30, 0.7) # 2001 age0 
arcsbreaks[2,c(8, 13, 18)] <- c(5, 30, 0.7) # 2002 age0. no age 0 but can't have blank line
arcsbreaks[2,c(11, 16, 21)] <- c(5, 175, 0.5) #for 2002 age 3, yint based on prev year
arcsbreaks[3,c(8, 13, 18)] <- c(5, 30, 0.7) # 2003 age0. no age 0 but can't have blank line
arcsbreaks[3,c(9, 14, 19)] <- c(4, 70, 0.7) #for 2003 age1, manually add sig=4, yint=70, growth(slope)=0.7
arcsbreaks[3,c(10, 15, 20)] <- c(6, 125, 0.5) #for 2003 age2, manually add sig=6, yint=125, growth(slope)=0.5
arcsbreaks[3,c(11, 16, 21)] <- c(4, 170, 0.6) #for 2003 age3, manually add sig=4, yint=170, growth(slope)=0.6
arcsbreaks[3,c(12, 17, 22)] <- c(4, 200, 0.4) #for 2003 age4, manually add sig=4, yint=200, growth(slope)=0.4
arcsbreaks[4,c(8, 13, 18)] <- c(5, 50, 0.7) # 2004 age0 
arcsbreaks[4,c(9, 14, 19)] <- c(5, 95, 0.7) # 2004 age1
arcsbreaks[4,c(10, 15, 20)] <- c(6, 135, 0.7) # 2004 age2
arcsbreaks[4,c(11, 16, 21)] <- c(4, 170, 0.6) # 2004 age3
arcsbreaks[4,c(12, 17, 22)] <- c(7, 225, 0.5) # 2004 age4
arcsbreaks[5,c(9, 14, 19)] <- c(6, 95, 0.7) # 2005 age1
arcsbreaks[5,c(10, 15, 20)] <- c(6, 125, 0.7) # 2005 age2
arcsbreaks[5,c(11, 16, 21)] <- c(4, 165, 0.6) # 2005 age3
arcsbreaks[5,c(12, 17, 22)] <- c(7, 205, 0.5) # 2005 age4
arcsbreaks[6,c(8, 13, 18)] <- c(5, 30, 0.7) # 2006 age0 #artificially low on purpose
arcsbreaks[6,c(10, 15, 20)] <- c(4, 140, 0.7) # 2006 age2
arcsbreaks[6,c(11, 16, 21)] <- c(6, 165, 0.6) # 2006 age3
arcsbreaks[6,c(12, 17, 22)] <- c(7, 205, 0.5) # 2006 age4
arcsbreaks[7,c(8, 13, 18)] <- c(6, 30, 1.1) # 2007 age0 
arcsbreaks[7,c(10, 15, 20)] <- c(3, 150, 0.9) # 2007 age2
arcsbreaks[7,c(11, 16, 21)] <- c(6, 190, 0.75) # 2007 age3
arcsbreaks[7,c(12, 17, 22)] <- c(7, 225, 0.5) # 2007 age4
arcsbreaks[8,c(8, 13, 18)] <- c(5.5, 45, 0.8) # 2008 age0 
arcsbreaks[8,c(9, 14, 19)] <- c(5, 95, 0.7) # 2008 age1
arcsbreaks[8,c(10, 15, 20)] <- c(5, 155, 0.9) # 2008 age2
arcsbreaks[8,c(11, 16, 21)] <- c(6, 200, 0.75) # 2008 age3
arcsbreaks[8,c(12, 17, 22)] <- c(7, 230, 0.5) # 2008 age4
arcsbreaks[9,8] <- 4 # 2009 age0 
arcsbreaks[9,c(9, 14, 19)] <- c(5, 95, 0.7) # 2009 age1
arcsbreaks[9,c(10, 15, 20)] <- c(5, 170, 0.7) # 2009 age2
arcsbreaks[9,c(11, 16, 21)] <- c(6, 215, 0.75) # 2009 age3
arcsbreaks[10,c(8, 13, 18)] <- c(4, 15, 0.95) # 2010 age0 
arcsbreaks[10,c(9, 14, 19)] <- c(5, 90, 0.7) # 2010 age1
arcsbreaks[10,c(10, 15, 20)] <- c(5, 140, 0.7) # 2010 age2
arcsbreaks[10,c(11, 16, 21)] <- c(3, 200, 0.4) # 2010 age3
arcsbreaks[10,c(12, 17, 22)] <- c(7, 225, 0.3) # 2010 age4
arcsbreaks[11,8] <- 5 # 2011 age0 sigma = 5
arcsbreaks[11,c(9, 14, 19)] <- c(6, 90, 0.9) # 2011 age1
arcsbreaks[11,c(10, 15, 20)] <- c(5, 145, 0.8) # 2011 age2
arcsbreaks[11,c(11, 16, 21)] <- c(5, 215, 0.6) # 2011 age3
arcsbreaks[12,8] <- 5 # 2012 age0, change sigma
#arcsbreaks[12,c(9, 14, 19)] <- c(5, 90, 0.9) # 2012 age1
arcsbreaks[12,c(10, 15, 20)] <- c(5.5, 155, 0.8) # 2012 age2
arcsbreaks[12,c(11, 16, 21)] <- c(5, 210, 0.55) # 2012 age3
arcsbreaks[13,c(8, 13, 18)] <- c(5, 40, 0.7) # 2013 age0 
arcsbreaks[13,c(9, 14, 19)] <- c(6, 115, 0.75) # 2013 age1
arcsbreaks[13,c(10, 15, 20)] <- c(5.5, 163, 0.55) # 2013 age2
arcsbreaks[13,c(11, 16, 21)] <- c(5, 210, 0.55) # 2013 age3
arcsbreaks[14,c(8, 13, 18)] <- c(33, 25, 0.9) # 2014 age0 
arcsbreaks[15,c(8, 13, 18)] <- c(5, 30, 0.7) # 2015 age0 
arcsbreaks[15,c(9, 14, 19)] <- c(4, 100, 0.7) # 2015 age1
arcsbreaks[15,c(10, 15, 20)] <- c(6, 140, 0.7) # 2015 age2
arcsbreaks[15,c(11, 16, 21)] <- c(5, 200, 0.5) # 2015 age3
arcsbreaks[15,c(12, 17, 22)] <- c(6, 225, 0.5) # 2015 age4
arcsbreaks[16,c(8, 13, 18)] <- c(1, 30, 0.5) # 2016 age0 #even though none, add line
arcsbreaks[16,c(9, 14, 19)] <- c(5, 100, 0.75) # 2016 age1
arcsbreaks[16,c(10, 15, 20)] <- c(5, 140, 0.85) # 2016 age2
arcsbreaks[16,c(11, 16, 21)] <- c(5, 175, 0.85) # 2016 age3
arcsbreaks[16,c(12, 17, 22)] <- c(6, 225, 0.65) # 2016 age4
arcsbreaks[17,c(8, 13, 18)] <- c(5, 25, 0.95) # 2017 age0 
arcsbreaks[17,c(9, 14, 19)] <- c(5, 95, 0.85) # 2017 age1
arcsbreaks[17,c(10, 15, 20)] <- c(5, 150, 0.85) # 2017 age2
arcsbreaks[17,c(11, 16, 21)] <- c(3, 190, 0.75) # 2017 age3
arcsbreaks[17,c(12, 17, 22)] <- c(6, 220, 0.75) # 2017 age4
arcsbreaks[18,c(8, 13, 18)]  <- c(1, 20,  0.4)  # 2018 age0 #there aren't any really
arcsbreaks[18,c(9, 14, 19)]  <- c(8, 90,  0.6)  # 2018 age1
arcsbreaks[18,c(10, 15, 20)] <- c(5, 150, 0.75) # 2018 age2
arcsbreaks[18,c(11, 16, 21)] <- c(5, 190, 0.85) # 2018 age3
arcsbreaks[18,c(12, 17, 22)] <- c(6, 225, 0.65) # 2018 age4


# now run a quick plot of the values from above for a subset of years
# if param values don't work, change above values manually
for(i in 2018:2018){
  print(
    ggplot(pru_lengths %>% filter(Year == i & Species == "ARCS"), aes(x=day.of.year, y=Length)) + 
      geom_point(colour = "dark blue", size = 1.5, alpha = 0.5) +
      scale_y_continuous(limits = c(0,250)) +
      #scale_x_datetime(limits = c(as_datetime("2015-06-30 00:00"), as_datetime("2015-09-03 00:00"))) +
      labs(title=paste0("ARCS lengths ", i), y="Length (mm)", x="Date") +
      geom_abline(aes(intercept=(alpha0 - (180*beta0)),slope=beta0),
                  data = arcsbreaks %>% filter(Year == i)) +
      geom_abline(aes(intercept=(alpha1 - (180*beta1)),slope=beta1),
                  data = arcsbreaks %>% filter(Year == i)) + 
      geom_abline(aes(intercept=(alpha2 - (180*beta2)),slope=beta2), 
                  data = arcsbreaks %>% filter(Year == i)) +
      geom_abline(aes(intercept=(alpha3 - (180*beta3)),slope=beta3), 
                  data = arcsbreaks %>% filter(Year == i)) +
      geom_abline(aes(intercept=(alpha4 - (180*beta4)),slope=beta4), 
                  data = arcsbreaks %>% filter(Year == i))
  )
  #readline()
}


################
# Now find out the cutoff lengths at which a fish is more likely one age than another, by doy and year
#check that there are no NAs in alpha0:3, beta0:3, sigma0:3
#if no age0 are caught in a certain year, add artifical low "regression" line

nrow(arcsbreaks)
breakpoints.arcs <- data.frame()
for(i in 1:nrow(arcsbreaks)){
  for(j in c(1,65)){
    # this looks for the slope on day 1, then again on day 65 (approx season duration)
    # next it assigns a normal distribution based on mean and variance for lengths under 250mm
    bins = 1:250
    .p1 <- diff(pnorm(bins, arcsbreaks$alpha0[i] + ((j)*arcsbreaks$beta0[i]), arcsbreaks$sigma0[i]))
    .p2 <- diff(pnorm(bins, arcsbreaks$alpha1[i] + ((j)*arcsbreaks$beta1[i]), arcsbreaks$sigma1[i]))
    .p3 <- diff(pnorm(bins, arcsbreaks$alpha2[i] + ((j)*arcsbreaks$beta2[i]), arcsbreaks$sigma2[i]))
    .p4 <- diff(pnorm(bins, arcsbreaks$alpha3[i] + ((j)*arcsbreaks$beta3[i]), arcsbreaks$sigma3[i]))
    
    .probs <- cbind(.p1,.p2,.p3,.p4)
    
    .break <- as.data.frame(.probs / apply(.probs,1,sum))
    
    .break[.break <  0.001] <- 0 # this eliminates the tiny fractions which threw it off
    if(j<2){
      .break <- .break[40:(length(bins)-1),] # this restricts it to looking above 40 mm for cutoff
      breakpoints.arcs[i,1] <- which.max((.break[,2]>.break[,1])) + 40
      breakpoints.arcs[i,2] <- which.max((.break[,3]>.break[,2])) + 40
      breakpoints.arcs[i,3] <- which.max((.break[,4]>.break[,3])) + 40
      # these are the break points at which a length is more likely to be one age than another
    } else{
      .break <- .break[80:(length(bins)-1),] # this restricts it to looking above 80 mm for cutoff
      breakpoints.arcs[i,4] <- which.max((.break[,2]>.break[,1])) + 80
      breakpoints.arcs[i,5] <- which.max((.break[,3]>.break[,2])) + 80
      breakpoints.arcs[i,6] <- which.max((.break[,4]>.break[,3])) + 80 
    }
  }
}
#Note: manually fix any issue years here!
breakpoints.arcs[18,2] <- 124 #revise down a few mm
breakpoints.arcs[18,5] <- 169

breakpoints.arcs$age01cut_slope <- (breakpoints.arcs$V4-breakpoints.arcs$V1)/64
breakpoints.arcs$age12cut_slope <- (breakpoints.arcs$V5-breakpoints.arcs$V2)/64
breakpoints.arcs$age23cut_slope <- (breakpoints.arcs$V6-breakpoints.arcs$V3)/64
breakpoints.arcs



# for loop
# run all years and make cuts for all 65 doy,
# assign under age01 cut to age 0, assign between 01 and 12 cuts to age 1, etc
allbreaks.arcs <- data.frame(year= "", doy= "", age01cut= "", age12cut= "", stringsAsFactors = FALSE)
allbreaks.arcs <- sapply( allbreaks.arcs, as.numeric )
cuts <- data.frame(age01cut=rep(0,68), age12cut=rep(0,68), age23cut=rep(0,68))
doy <- vector()
for(j in c(1:18)){
  #get values for each day of the year (doy)
  for(i in seq(-2, 65)){ #doy of year. starts at -2 because some years sampling started on doy 178
    doy[i+3] <- i + 180 # i + 3 to compensate for starting at doy178
    cuts$age01cut[i+3] <- breakpoints.arcs[j,1] + ((i)*breakpoints.arcs$age01cut_slope[j])
    cuts$age12cut[i+3] <- breakpoints.arcs[j,2] + ((i)*breakpoints.arcs$age12cut_slope[j])
    cuts$age23cut[i+3] <- breakpoints.arcs[j,3] + ((i)*breakpoints.arcs$age23cut_slope[j])
    #can implement this later but there's a neg slope which needs to be fixed
  }
  
  .yearbreaks <- cbind(year = rep(j+2000, length(doy)), doy, cuts)
  
  allbreaks.arcs <- bind_rows(allbreaks.arcs, .yearbreaks)
  
}

arcsallages <- pru_lengths %>% filter(Length < 250, Species == "ARCS")
arcsallages <- left_join(arcsallages, allbreaks.arcs, by = c("Year" = "year", "day.of.year" = "doy"))
arcsallages$age.put <- if_else(arcsallages$Length <= arcsallages$age01cut, "age0", 
                               if_else(arcsallages$Length <= arcsallages$age12cut, "age1", 
                                       if_else(arcsallages$Length <= arcsallages$age23cut, "age2", "age3plus")))

arcsallages$broodyr <- if_else(arcsallages$age.put == "age0", arcsallages$Year - 1, 
                               if_else(arcsallages$age.put == "age1", arcsallages$Year - 2, 
                                       if_else(arcsallages$age.put == "age2", arcsallages$Year - 3, 
                                               arcsallages$Year - 4))) #beware any NAs


agesummary_arcs <- arcsallages %>% group_by(Year, age.put) %>% summarise(total=sum(totcount))

agesummary_arcs <- spread(agesummary_arcs, age.put, total)
agesummary_arcs$age0[is.na(agesummary_arcs$age0)] <- 0
agesummary_arcs




################
## convert these to total number of ages per LengthGroup

unmeasuredfish.arcs <- allcatch %>% filter(Species == "ARCS", LG == 1 | LG == 2) %>% group_by(Year, EndDate, Net, LG) %>% 
  summarise(totcount=sum(totcount, na.rm = TRUE)) %>% 
  left_join(all.len %>%
              mutate(Net = paste0(Station, Side)) %>%
              filter( Species == "ARCS") %>% group_by(Year, EndDate, Net, Group) %>% 
              summarise(Lengthcount=sum(totcount, na.rm = TRUE)), 
            by = c("Year" = "Year", "EndDate" = "EndDate", "Net" = "Net", "LG" = "Group")) %>%
  replace(., is.na(.), 0) %>%
  mutate(unmeasured = totcount - Lengthcount) 



allcatch.arcsages <- unmeasuredfish.arcs %>% left_join(arcsallages %>%
                                                         mutate(Net = paste0(Station, Side)) %>%
                                                         #filter(Year == 2018) %>% 
                                                         group_by(Year, EndDate, Net, Group, age.put) %>% 
                                                         summarise(agecount=sum(totcount, na.rm = TRUE)), 
                                                       by = c("Year" = "Year", "EndDate" = "EndDate", "Net" = "Net", "LG" = "Group")) %>%
  mutate(unleng_ages = round(unmeasured*(agecount / Lengthcount)),
         totalages = agecount + unleng_ages) %>% filter(totcount > 0)
# there are some rows with NA ages. As best as I can tell, these are from days where there were totals but no lengths 
# (so we can't partition out estimated ages). 

allcatch.arcsages %>% group_by(Year, age.put) %>% summarise(totals = sum(totalages)) %>% spread(age.put, value = totals )
allcatch %>% filter(Species == "ARCS", LG == 3) %>% group_by(Year) %>% summarise(LG3 = sum(totcount))
allcatch %>% filter(Species == "ARCS") %>% group_by(Year, LG) %>% summarise(tot = sum(totcount)) %>% spread(LG, tot)



#save(arcsallages, bdwfallages, allcatch.arcsages, allcatch.bdwfages, file = "AgeAssignment_ARCS_BDWF_2001-2018.Rdata")



ggplot(arcsallages %>% filter(Year >= 2013 & Year <= 2018), aes(x=day.of.year, y=Length, color = age.put)) + 
  geom_point(size = 1.5, alpha = 0.5) +
  scale_y_continuous(limits = c(0,250)) +
  #scale_x_datetime(limits = c(as_datetime("2015-06-30 00:00"), as_datetime("2015-09-03 00:00"))) +
  labs(title=paste0("ARCS lengths "), y="Length (mm)", x="Date") + 
  facet_wrap(~Year, ncol = 6)

ggplot(arcsallages %>% filter(Year >= 2007 & Year <= 2012), aes(x=day.of.year, y=Length, color = as.factor(broodyr))) + 
  geom_point(size = 1.5, alpha = 0.5) +
  scale_y_continuous(limits = c(0,250)) +
  #scale_x_datetime(limits = c(as_datetime("2015-06-30 00:00"), as_datetime("2015-09-03 00:00"))) +
  labs(title=paste0("ARCS lengths "), y="Length (mm)", x="Date") + 
  facet_wrap(~Year, ncol = 6)




allcatch %>% filter(LG == 1 | LG == 2, Species == "ARCS") %>% group_by(Year, LG) %>%
  summarise(totals = sum(totcount)) %>% spread(LG, totals)


all.len %>% filter(Group == 1 | Group == 2, Species == "ARCS") %>% group_by(Year, Group) %>%
  summarise(totals = length(Count)) %>% spread(Group, totals)
#next step is adding the 2017 data to the 2001-16 sets and resaving it
temp <- allcatch %>% filter(Species == "ARCS", Year == 2011) 
sum(temp$totcount)
temp1 <- all.len %>% filter(Group == 1 | Group == 2, Species == "ARCS", Year == 2014) 




all.len$Net <- paste0(all.len$Station, all.len$Side)
temp.len <- all.len %>% filter(Group == 1 | Group == 2, Species == "ARCS") %>% 
  group_by(Year, EndDate, day.of.year, Net, Species, Group) %>% summarise(len.totals = sum(totcount))

catchandlensumm <- left_join(allcatch %>% filter(LG == 1 | LG == 2, Species == "ARCS") %>% select(-Morts), temp.len, by =
            c("Year", "EndDate", "day.of.year", "Net", "Species", "LG" = "Group"))
catchandlensumm$extracount <- catchandlensumm$totcount - catchandlensumm$len.totals
sum(catchandlensumm$extracount < 0, na.rm = TRUE)
temp <- catchandlensumm %>% filter(extracount <0)
sum(is.na(catchandlensumm$Year))

#fix remaining minor issues, mostly from 2013

test2 <- left_join(temp.len, allcatch %>% filter(LG == 1 | LG == 2, Species == "ARCS") %>% select(-Morts), by =
            c("Year", "EndDate", "day.of.year", "Net", "Species", "Group" = "LG"))
test2$diff <- test2$len.totals - test2$totcount
head(test2 %>% filter(is.na(diff)), n=20)
#Aug 23: working this section. Then run report for BDWF too and fix those


arcsallages$Net <- paste0(arcsallages$Station, arcsallages$Side)
arcsdailytotals <- arcsallages %>% filter(Length < 250) %>% group_by(Year, day.of.year, Net, Species, Group, age.put) %>%
  summarise(indivtots = length(Species)) %>% spread(age.put, indivtots)

arcsdailytotals$daytots <- rowSums(arcsdailytotals[,6:9], na.rm = TRUE)
head(catchandlensumm, n = 20)

#multiply into percentages
arcsdailytotals$age0perc <- arcsdailytotals$age0 / arcsdailytotals$daytots
arcsdailytotals$age1perc <- arcsdailytotals$age1 / arcsdailytotals$daytots
arcsdailytotals$age2perc <- arcsdailytotals$age2 / arcsdailytotals$daytots
arcsdailytotals$age3plusperc <- arcsdailytotals$age3plus / arcsdailytotals$daytots
arcsdailytotals[is.na(arcsdailytotals)] <- 0

#add in total caught by LG 
arcsdailytotals <- left_join(arcsdailytotals, allcatch %>% filter(LG == 1 | LG == 2, Species == "ARCS") %>% select(-Morts), 
                             c("Year", "day.of.year", "Net", "Species", "Group" = "LG"))

arcsdailytotals$age0.exp <- arcsdailytotals$totcount * arcsdailytotals$age0perc
arcsdailytotals$age1.exp <- arcsdailytotals$totcount * arcsdailytotals$age1perc
arcsdailytotals$age2.exp <- arcsdailytotals$totcount * arcsdailytotals$age2perc
arcsdailytotals$age3plus.exp <- arcsdailytotals$totcount * arcsdailytotals$age3plusperc

(arcsannualtotals <- arcsdailytotals[,c(1:5, 15, 17:20)] %>% 
  gather(key = age, value = countss, -Year, -day.of.year, -Net, -Species, -Group, -EndDate) %>% 
  group_by(Year, age) %>% summarize(count = sum(countss, na.rm = TRUE)) %>% spread(age, count) )
#DONE! These are the total ages for all LG1 and LG2 ARCS. Does NOT include LG3 fish



############### LSCS ###############
#Now all of the above but with LSCS
####################################

lscsbreaks <- read_excel("2018ThesisAnalysis/lengthtoage.xlsx", sheet = "lscs")
lscsbreaks <- lscsbreaks[,-c(1,9,15,21,27)]
lscsbreaks[1,c(8, 13, 18)] <- c(4, 30, 0.4) # 2001 age0, no fish but don't want blanks
lscsbreaks[1,c(9, 14, 19)] <- c(4, 70, 0.5) # 2001 age1, 
lscsbreaks[1,c(10, 15, 20)] <- c(4, 113, 0.5) # 2001 age2, yint and growth rate taken by averaging all other years 
lscsbreaks[1,c(11, 16, 21)] <- c(4, 143, 0.55) # 2001 age3, yint and growth rate taken by averaging all other years
lscsbreaks[1,c(12, 17, 22)] <- c(4, 190, 0.55) # 2001 age4, yint and growth rate taken by averaging all other years
lscsbreaks[2,c(8, 13, 18)] <- c(4, 35, 0.45) # 2002 age0
lscsbreaks[2,c(9, 14, 19)] <- c(4, 65, 0.6) # 2002 age1, yint and growth rate taken by averaging all other years
lscsbreaks[2,c(10, 15, 20)] <- c(4, 110, 0.55) # 2002 age2, yint and growth rate taken by averaging all other years 
lscsbreaks[2,c(11, 16, 21)] <- c(4, 135, 0.55) # 2002 age3, yint and growth rate taken by averaging all other years
lscsbreaks[2,c(12, 17, 22)] <- c(4, 180, 0.5) # 2002 age4, yint and growth rate taken by averaging all other years
lscsbreaks[3,c(8, 13, 18)] <- c(3, 30, 0.45) # 2003 age0
lscsbreaks[3,c(11, 16, 21)] <- c(4, 145, 0.55) # 2003 age3, yint was 156, changed to 145 and incr growth
lscsbreaks[3,c(12, 17, 22)] <- c(4, 195, 0.5) # 2003 age4, yint and growth rate taken by averaging all other years

lscsbreaks[4,c(8, 13, 18)] <- c(3, 25, 0.6) # 2004 age0
lscsbreaks[4,c(9, 14, 19)] <- c(4, 65, 0.75) # 2004 age1
lscsbreaks[4,c(10, 15, 20)] <- c(6, 110, 0.7) # 2004 age2,
lscsbreaks[4,c(11, 16, 21)] <- c(4, 150, 0.55) # 2004 age3
lscsbreaks[4,c(12, 17, 22)] <- c(4, 180, 0.6)

lscsbreaks[5,c(8, 13, 18)] <- c(3, 30, 0.6) # 2005 age0
lscsbreaks[5,c(9, 14, 19)] <- c(4, 75, 0.6) # 2005 age1, 
lscsbreaks[5,c(10, 15, 20)] <- c(5, 108, 0.7) # 2005 age2, 
lscsbreaks[5,c(11, 16, 21)] <- c(7, 155, 0.65) # 2005 age3, 
lscsbreaks[5,c(12, 17, 22)] <- c(6, 210, 0.55) # 2005 age4,

lscsbreaks[6,c(8, 13, 18)] <- c(3, 35, 0.6) # 2006 age0
lscsbreaks[6,c(9, 14, 19)] <- c(4, 71, 0.75) # 2006 age1
#lscsbreaks[6,c(10, 15, 20)] <- c(5, 108, 0.7) # 2006 age2, 
lscsbreaks[6,c(11, 16, 21)] <- c(4.5, 155, 0.65) # 2006 age3,
lscsbreaks[6,c(12, 17, 22)] <- c(6, 185, 0.55) # 2006 age4,

lscsbreaks[7,c(8, 13, 18)] <- c(3, 45, 0.6) # 2007 age0
lscsbreaks[7,c(9, 14, 19)] <- c(4, 80, 0.6) # 2007 age1
lscsbreaks[7,c(10, 15, 20)] <- c(5, 125, 0.6) # 2007 age2
lscsbreaks[7,c(11, 16, 21)] <- c(5, 160, 0.65) # 2007 age3
lscsbreaks[7,c(12, 17, 22)] <- c(6, 195, 0.55) # 2007 age4

lscsbreaks[8,c(8, 13, 18)] <- c(3, 30, 0.55) # 2008 age0
lscsbreaks[8,c(9, 14, 19)] <- c(4, 65, 0.7) # 2008 age1
lscsbreaks[8,c(10, 15, 20)] <- c(5, 110, 0.7) # 2008 age2
lscsbreaks[8,c(11, 16, 21)] <- c(4.5, 155, 0.65) # 2008 age3
lscsbreaks[8,c(12, 17, 22)] <- c(6, 195, 0.55) # 2008 age4

#Note: 2009 values were just left as fairly benign "default" values
lscsbreaks[9,c(8, 13, 18)] <- c(3, 30, 0.55) # 2009 age0
lscsbreaks[9,c(9, 14, 19)] <- c(4, 75, 0.75) # 2009 age1
lscsbreaks[9,c(10, 15, 20)] <- c(4, 115, 0.7) # 2009 age2
lscsbreaks[9,c(11, 16, 21)] <- c(5, 155, 0.65) # 2009 age3
lscsbreaks[9,c(12, 17, 22)] <- c(6, 195, 0.55) # 2009 age4

lscsbreaks[10,c(8, 13, 18)] <- c(3, 30, 0.55) # 2010 age0
lscsbreaks[10,c(9, 14, 19)] <- c(4, 80, 0.75) # 2010 age1
lscsbreaks[10,c(10, 15, 20)] <- c(5, 127, 0.75) # 2010 age2
lscsbreaks[10,c(11, 16, 21)] <- c(5, 175, 0.7) # 2010 age3
lscsbreaks[10,c(12, 17, 22)] <- c(6, 215, 0.55) # 2010 age4

lscsbreaks[11,c(8, 13, 18)] <- c(3, 50, 0.55) # 2011 age0
lscsbreaks[11,c(9, 14, 19)] <- c(4, 90, 0.75) # 2011 age1
lscsbreaks[11,c(10, 15, 20)] <- c(4, 130, 0.7) # 2011 age2
lscsbreaks[11,c(11, 16, 21)] <- c(5, 175, 0.65) # 2011 age3
lscsbreaks[11,c(12, 17, 22)] <- c(6, 215, 0.55) # 2011 age4

lscsbreaks[12,c(8, 13, 18)] <- c(3, 35, 0.55) # 2012 age0
lscsbreaks[12,c(9, 14, 19)] <- c(4, 80, 0.75) # 2012 age1
lscsbreaks[12,c(10, 15, 20)] <- c(6, 125, 0.7) # 2012 age2
lscsbreaks[12,c(11, 16, 21)] <- c(5, 175, 0.65) # 2012 age3
lscsbreaks[12,c(12, 17, 22)] <- c(6, 215, 0.55) # 2012 age4

lscsbreaks[13,c(8, 13, 18)] <- c(3, 45, 0.6) # 2013 age0
lscsbreaks[13,c(9, 14, 19)] <- c(4, 90, 0.75) # 2013 age1
lscsbreaks[13,c(10, 15, 20)] <- c(5, 128, 0.8) # 2013 age2
lscsbreaks[13,c(11, 16, 21)] <- c(6, 170, 0.7) # 2013 age3
lscsbreaks[13,c(12, 17, 22)] <- c(6, 220, 0.55) # 2013 age4

lscsbreaks[14,c(8, 13, 18)] <- c(3, 45, 0.4) # 2014 age0
lscsbreaks[14,c(9, 14, 19)] <- c(4, 90, 0.45) # 2014 age1
lscsbreaks[14,c(10, 15, 20)] <- c(5, 140, 0.5) # 2014 age2
lscsbreaks[14,c(11, 16, 21)] <- c(6, 170, 0.55) # 2014 age3
lscsbreaks[14,c(12, 17, 22)] <- c(6, 205, 0.5) # 2014 age4

lscsbreaks[15,c(8, 13, 18)] <- c(3, 30, 0.4) # 2015 age0
lscsbreaks[15,c(9, 14, 19)] <- c(4, 75, 0.55) # 2015 age1
lscsbreaks[15,c(10, 15, 20)] <- c(5, 130, 0.55) # 2015 age2
lscsbreaks[15,c(11, 16, 21)] <- c(7, 170, 0.55) # 2015 age3
lscsbreaks[15,c(12, 17, 22)] <- c(6, 215, 0.5) # 2015 age4

lscsbreaks[16,c(8, 13, 18)] <- c(3, 45, 0.4) # 2016 age0
lscsbreaks[16,c(9, 14, 19)] <- c(4, 75, 0.55) # 2016 age1
lscsbreaks[16,c(10, 15, 20)] <- c(4, 120, 0.55) # 2016 age2
lscsbreaks[16,c(11, 16, 21)] <- c(7, 170, 0.55) # 2016 age3
lscsbreaks[16,c(12, 17, 22)] <- c(6, 215, 0.5) # 2016 age4

lscsbreaks[17,c(8, 13, 18)] <- c(3, 35, 0.7) # 2017 age0
lscsbreaks[17,c(9, 14, 19)] <- c(5, 85, 0.9) # 2017 age1
lscsbreaks[17,c(10, 15, 20)] <- c(4, 125, 0.8) # 2017 age2
lscsbreaks[17,c(11, 16, 21)] <- c(3, 170, 0.55) # 2017 age3
lscsbreaks[17,c(12, 17, 22)] <- c(6, 205, 0.5) # 2017 age4

# now run a quick plot of the values from above for a subset of years
# if param values don't work, change above values manually
for(i in 2017:2017){
  print( 
    ggplot(pru_lengths %>% filter(Year == i, Species == "LSCS", Group == 1 | Group == 2), aes(x=day.of.year, y=Length)) + 
      #sample_n(pru_lengths %>% filter(Year == i, Species == "LSCS", Group == 1 | Group == 2),3000)
      geom_point(colour = "dark blue", size = 1.5, alpha = 0.5) +
      scale_y_continuous(limits = c(0,250)) +
      labs(title=paste0("LSCS lengths ", i), y="Length (mm)", x="Date") +
      geom_abline(aes(intercept=(alpha0 - (180*beta0)),slope=beta0),
                  data = lscsbreaks %>% filter(Year == i)) +
      geom_abline(aes(intercept=(alpha1 - (180*beta1)),slope=beta1),
                  data = lscsbreaks %>% filter(Year == i)) + 
      geom_abline(aes(intercept=(alpha2 - (180*beta2)),slope=beta2), 
                  data = lscsbreaks %>% filter(Year == i)) +
      geom_abline(aes(intercept=(alpha3 - (180*beta3)),slope=beta3), 
                  data = lscsbreaks %>% filter(Year == i)) +
      geom_abline(aes(intercept=(alpha4 - (180*beta4)),slope=beta4), 
                  data = lscsbreaks %>% filter(Year == i))
  ) 
  #readline()
}


