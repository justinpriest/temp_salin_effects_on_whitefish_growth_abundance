# DATA IMPORT SUMMARY

# In these script files, there are shorthand species codes that are used
# ARCS = Arctic Cisco = Coregonus autumnalis
# BDWF = Broad Whitefish = Coregonus nasus


# This is the summary of the lengthtoage.R file which is where ages were originally partitioned.
# This script recreates the output from that file, but refer to that file for more info regarding decisions.

# Script lengthtoage.R checked how the lengths could be used to assume a likely age, based for three whitefish species and
# varying for each year. Basically for each year, I used a mixture model (assuming normal dist) to generate three
# parameters for each species, year, and age class: alpha (y-int of mm length on day 180), beta (growth rate, mm/day),
# and sigma (variance for that age class).

# The starting params for regmixEM from package mixtools varied GREATLY between species/year. 
# Sometimes a "good" fit could be found by setting sigma/beta/lambda for each year. 
# There was no consistent way to produce results, especially to constrain beta (growth), to positive values.
# A good fit was determined if growth was positve, and visually to see if it bisected the cloud. I assumed a linear growth 
# trend over the ~65 day growing season.

# This script applies the Excel doc of age class length and growth, then corrects for any obvious errors. If there 
# was a clear issue, I manually set values based on expected values, previous/post years (if a cloud appears/disappears
# between years, could use that info to inform judgment), as well as visual assessment

library(tidyverse)
#library(tidyr)
#library(tibble)
library(lubridate)
library(readxl)
library(here)


########## SECTION 1 ##########
## IMPORT LENGTH, CATCH DATA ##

# Read in the length, catch, and environmental data, plus species lookup table




indivfish.bdwfages <- read_csv("data/indivfish_bdwfages.csv", guess_max = 70000) %>%
  mutate(Side = as.factor(Side),
         StartDateTime = as.POSIXct(StartDateTime, tz="America/Anchorage")-hours(8),
         EndDateTime = as.POSIXct(EndDateTime, tz="America/Anchorage")-hours(8)) 
# Needed to correct for the time being 8 hrs off (UTC issue)


bdwflargelength <- read_csv("data/bdwflargelength.csv")


unmeasuredfish.bdwf <- read_csv("data/unmeasuredfish_bdwf.csv")



bdwflargecatch <- read_csv("data/bdwflargecatch.csv")








########## SECTION 2 ##########
###### LENGTH-AGE ASSIGNMENT #####
# Determine which lengths correspond to which putative age,
# based on species, length, year, and day 


############ BROAD WHITEFISH ############
# Assign ages to Broad Whitefish (BDWF) #


# Import values from the mixture model. Manually set blank and wrong values
bdwfbreaks <- read_excel(here::here("data/lengthtoage.xlsx"), sheet = "bdwf", col_types = "numeric")
bdwfbreaks <- data.frame(bdwfbreaks) # to maintain compatibility with old code
bdwfbreaks <- bdwfbreaks[,-c(1,9,15,21,27)] # drop blank columns
bdwfbreaks[1,c(8, 13, 18)] <- c(5, 30, 0.5) #for 2001 age0, manually add sig=5, yint=30, growth(slope)=0.5
bdwfbreaks[3,c(8, 13, 18)] <- c(5, 30, 0.5) #for 2003 age0, manually add sig=5, yint=30, growth(slope)=0.5
bdwfbreaks[3,c(9, 14, 19)] <- c(6.5, 85, 0.6) #for 2003 age1, manually add sig=6.5, yint=85, growth(slope)=0.6
bdwfbreaks[3,c(10, 15, 20)] <- c(7, 137, 0.73) #for 2003 age2, manually add sig=7, yint=137, growth(slope)=0.73
bdwfbreaks[3,c(11, 16, 21)] <- c(7, 200, 0.54) #for 2003 age3, manually add sig=7, yint=200, growth(slope)=0.54
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

bdwfbreaks[3,c(10, 15, 20)]
bdwfbreaks[3,c(11, 16, 21)]

################
# Now find out the cutoff lengths at which a fish is more likely one age than another, by doy and year
# Check that there are no NAs in alpha0:3, beta0:3, sigma0:3
# If no age0 are caught in a certain year, add artifical low "regression" line

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


# Note! indivfish.bdwfages is only for LG1 & 2!
indivfish.bdwfages <- left_join(indivfish.bdwfages, allbreaks.bdwf, by = c("Year" = "year", "day.of.year" = "doy"))
indivfish.bdwfages$age.put <- if_else(indivfish.bdwfages$Length <= indivfish.bdwfages$age01cut, "age0", 
                               if_else(indivfish.bdwfages$Length <= indivfish.bdwfages$age12cut, "age1", 
                                       if_else(indivfish.bdwfages$Length <= indivfish.bdwfages$age23cut, "age2", "age3plus")))

indivfish.bdwfages$broodyr <- if_else(indivfish.bdwfages$age.put == "age0", indivfish.bdwfages$Year - 1, 
                               if_else(indivfish.bdwfages$age.put == "age1", indivfish.bdwfages$Year - 2, 
                                       if_else(indivfish.bdwfages$age.put == "age2", indivfish.bdwfages$Year - 3, 
                                               indivfish.bdwfages$Year - 4))) #beware any NAs


agesummary_bdwf <- indivfish.bdwfages %>% group_by(Year, age.put) %>% summarise(total=sum(totcount))

agesummary_bdwf <- spread(agesummary_bdwf, age.put, total)
agesummary_bdwf$age0[is.na(agesummary_bdwf$age0)] <- 0


# Note! only measured fish, not totals
agesummary_bdwf <- agesummary_bdwf %>% 
  add_column(age3plusLG3=bdwflargelength$age3plusLG3)




################
## convert these to total number of ages per LengthGroup



allcatch.bdwfages <- unmeasuredfish.bdwf %>% left_join(indivfish.bdwfages %>%
                                                         mutate(Net = paste0(Station, Side)) %>%
                                                         #filter(Year == 2018) %>% 
                                                         group_by(Year, EndDate, Net, Group, age.put) %>% 
                                                         summarise(agecount=sum(totcount, na.rm = TRUE)), 
                                                       by = c("Year" = "Year", "EndDate" = "EndDate", "Net" = "Net", "LG" = "Group")) %>%
  mutate(unleng_ages = round(unmeasured*(agecount / Lengthcount)),
         totalages = agecount + unleng_ages,
         Species = "BDWF") %>% 
  dplyr::select(Species, everything()) %>%
  filter(totcount > 0) %>% #Next, add the LG3 fish into the df
  full_join(bdwflargecatch) %>% 
  mutate(totalages = ifelse(LG == 3, totcount, totalages),
         age.put = ifelse(LG == 3, "age3plus", age.put))



# Trying to create imputed values for days where no lengths were taken, but totals were recorded
# The following code takes LG1&2 totals, and finds days where there were totals but no lengths.
# Next, for each LG, Net, and Day it takes the average proportion of each age for the prev/following day
# There were two days where this average proportion didn't add up to 100%. 
# For these, I manually check and the value was supposed to be 100% age1, so I manually changed these.
# Lastly, I just cleaned up the vestigial columns and put it back to 'long' data format
allcatch.bdwfages <- allcatch.bdwfages %>% filter(!is.na(age.put) | LG == 3, totcount > 0) %>% 
  #there were also entries for LG3 fish with no totalcatch (placeholder during data entry). Removed these lines
  full_join(allcatch.bdwfages %>% filter(LG != 3) %>% group_by(EndDate, Net, LG, age.put) %>% 
              summarise(totcount = totcount, ageprops=agecount/Lengthcount) %>% 
              spread(age.put, value = ageprops) %>% ungroup() %>% 
              rename(notrecorded = `<NA>`) %>% 
              replace(., is.na(.), 0) %>%
              arrange(LG, Net, EndDate) %>% 
              mutate(grouping=sprintf("%04s-%04s-%04s", LG, Net, EndDate),
                     notrecorded = age0+age1+age2+age3plus) %>%
              mutate(age0 = ifelse(notrecorded==0, (dplyr::lag(age0, order_by=grouping) + dplyr::lead(age0, order_by=grouping))/2, age0),
                     age1 = ifelse(notrecorded==0, (dplyr::lag(age1, order_by=grouping) + dplyr::lead(age1, order_by=grouping))/2, age1),
                     age2 = ifelse(notrecorded==0, (dplyr::lag(age2, order_by=grouping) + dplyr::lead(age2, order_by=grouping))/2, age2),
                     age3plus = ifelse(notrecorded==0, (dplyr::lag(age3plus, order_by=grouping) + dplyr::lead(age3plus, order_by=grouping))/2, age3plus),
                     totalprop = age0 + age1 + age2 + age3plus,
                     age1 = ifelse(totalprop == 0.5, 1, age1), #account for the two days. 
                     #Note: if using this in future years, this is just a shortcut and won't necessarily work!!
                     newprop = age0 + age1 + age2 + age3plus) %>% 
              filter(notrecorded ==  0) %>%
              dplyr::select(-c(notrecorded, grouping, totalprop, newprop)) %>%
              gather(age.put, ageprop, -EndDate, -Net, -LG, -totcount) %>%
              mutate(Year = year(EndDate),
                     totalages = round(totcount * ageprop)) %>% 
              filter(totalages > 0),
            by = c("EndDate", "Year", "Net", "LG", "totcount", "age.put", "totalages")) %>% # end full join
  mutate(age.put = ifelse(LG==3, "age3plus", age.put), # now clean up remaining data
         Species = "BDWF",
         Station = as.factor(substr(Net, 1, 3))) %>%
  dplyr::select(-ageprop) %>% #drop column
  dplyr::select(Species, Year, EndDate, Net, Station, everything()) # order correctly


# S U C C E S S F U L    E X E C U T I O N #





################## ARCTIC CISCO ##################
#Now all of the above but with Arctic Cisco (ARCS)
##################################################

arcsbreaks <- read_excel(here::here("data/lengthtoage.xlsx"), sheet = "arcs", col_types = "numeric")
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
arcsbreaks[7,c(8, 13, 18)] <- c(7, 25, 1.2) # 2007 age0 
arcsbreaks[7,c(10, 15, 20)] <- c(3, 150, 0.9) # 2007 age2
arcsbreaks[7,c(11, 16, 21)] <- c(6, 190, 0.75) # 2007 age3
arcsbreaks[7,c(12, 17, 22)] <- c(7, 225, 0.5) # 2007 age4
arcsbreaks[8,c(8, 13, 18)] <- c(5.5, 45, 0.8) # 2008 age0 
arcsbreaks[8,c(9, 14, 19)] <- c(5, 95, 0.7) # 2008 age1
arcsbreaks[8,c(10, 15, 20)] <- c(5, 155, 0.9) # 2008 age2
arcsbreaks[8,c(11, 16, 21)] <- c(6, 200, 0.75) # 2008 age3
arcsbreaks[8,c(12, 17, 22)] <- c(7, 230, 0.5) # 2008 age4
arcsbreaks[9,8] <- 4 # 2009 age0 
arcsbreaks[9,c(9, 14, 19)] <- c(6, 92, 0.7) # 2009 age1
arcsbreaks[9,c(10, 15, 20)] <- c(5, 170, 0.7) # 2009 age2
arcsbreaks[9,c(11, 16, 21)] <- c(6, 215, 0.75) # 2009 age3
arcsbreaks[10,c(8, 13, 18)] <- c(3.5, 15, 0.95) # 2010 age0 
arcsbreaks[10,c(9, 14, 19)] <- c(5.5, 90, 0.7) # 2010 age1
arcsbreaks[10,c(10, 15, 20)] <- c(5, 140, 0.7) # 2010 age2
arcsbreaks[10,c(11, 16, 21)] <- c(3, 200, 0.4) # 2010 age3
arcsbreaks[10,c(12, 17, 22)] <- c(7, 225, 0.3) # 2010 age4
arcsbreaks[11,c(8, 13, 18)] <- c(6.5, 5, 1.25) # 2011 age0 
arcsbreaks[11,c(9, 14, 19)] <- c(5.5, 85, 1) # 2011 age1
arcsbreaks[11,c(10, 15, 20)] <- c(5, 145, 0.8) # 2011 age2
arcsbreaks[11,c(11, 16, 21)] <- c(5, 215, 0.6) # 2011 age3
arcsbreaks[12,8] <- 5 # 2012 age0, change sigma
#arcsbreaks[12,c(9, 14, 19)] <- c(5, 90, 0.9) # 2012 age1
arcsbreaks[12,c(10, 15, 20)] <- c(5.5, 155, 0.8) # 2012 age2
arcsbreaks[12,c(11, 16, 21)] <- c(5, 210, 0.55) # 2012 age3
arcsbreaks[13,c(8, 13, 18)] <- c(3, 40, 0.7) # 2013 age0 
arcsbreaks[13,c(9, 14, 19)] <- c(6, 115, 0.75) # 2013 age1
arcsbreaks[13,c(10, 15, 20)] <- c(5.5, 163, 0.55) # 2013 age2
arcsbreaks[13,c(11, 16, 21)] <- c(5, 210, 0.55) # 2013 age3
arcsbreaks[14,c(8, 13, 18)] <- c(4, 10, 1.3) # 2014 age0 
arcsbreaks[15,c(8, 13, 18)] <- c(5, 30, 0.7) # 2015 age0 
arcsbreaks[15,c(9, 14, 19)] <- c(4, 100, 0.7) # 2015 age1
arcsbreaks[15,c(10, 15, 20)] <- c(7, 135, 0.7) # 2015 age2
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




################
# Now find out the cutoff lengths at which a fish is more likely one age than another, by doy and year
#check that there are no NAs in alpha0:3, beta0:3, sigma0:3
#if no age0 are caught in a certain year, add artifical low "regression" line

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

# Note! indivfish.arcsages is only for LG1 & 2!
indivfish.arcsages <- pru_lengths %>% filter(Length < 250, Species == "ARCS")
indivfish.arcsages <- left_join(indivfish.arcsages, allbreaks.arcs, by = c("Year" = "year", "day.of.year" = "doy"))
indivfish.arcsages$age.put <- if_else(indivfish.arcsages$Length <= indivfish.arcsages$age01cut, "age0",  # Putative age
                               if_else(indivfish.arcsages$Length <= indivfish.arcsages$age12cut, "age1", 
                                       if_else(indivfish.arcsages$Length <= indivfish.arcsages$age23cut, "age2", "age3plus")))

indivfish.arcsages$broodyr <- if_else(indivfish.arcsages$age.put == "age0", indivfish.arcsages$Year - 1,  # Year eggs laid
                               if_else(indivfish.arcsages$age.put == "age1", indivfish.arcsages$Year - 2, 
                                       if_else(indivfish.arcsages$age.put == "age2", indivfish.arcsages$Year - 3, 
                                               indivfish.arcsages$Year - 4))) #beware any NAs


agesummary_arcs <- indivfish.arcsages %>% group_by(Year, age.put) %>% summarise(total=sum(totcount))
agesummary_arcs <- spread(agesummary_arcs, age.put, total)
agesummary_arcs$age0[is.na(agesummary_arcs$age0)] <- 0

agesummary_arcs <- agesummary_arcs %>% 
  add_column(age3plusLG3=(pru_lengths %>% filter(Length >= 250, Species == "ARCS") %>% 
                          group_by(Year) %>% summarise(age3plusLG3=sum(totcount)))$age3plusLG3)


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



allcatch.arcsages <- unmeasuredfish.arcs %>% left_join(indivfish.arcsages %>%
                                                         mutate(Net = paste0(Station, Side)) %>%
                                                         #filter(Year == 2018) %>% 
                                                         group_by(Year, EndDate, Net, Group, age.put) %>% 
                                                         summarise(agecount=sum(totcount, na.rm = TRUE)), 
                                                       by = c("Year" = "Year", "EndDate" = "EndDate", "Net" = "Net", "LG" = "Group")) %>%
  mutate(unleng_ages = round(unmeasured*(agecount / Lengthcount)),
         totalages = agecount + unleng_ages,
         Species = "ARCS") %>% 
  dplyr::select(Species, everything()) %>%
  filter(totcount > 0) %>% #Next, add the LG3 fish into the df
  full_join(allcatch %>% filter(Species == "ARCS", LG == 3, totcount > 0) %>% group_by(Year, EndDate, Net, LG) %>% 
            summarise(totcount=sum(totcount, na.rm = TRUE)) %>% 
            mutate(Species = "ARCS", 
                   totalages = totcount)) 


# There are some rows with NA ages. These are from days where there were totals but no lengths 
# (so we can't partition out estimated ages if there are no lengths). 

# Create imputed values for days where no lengths were taken, but totals were recorded
# The following code takes LG1&2 totals, and finds days where there were totals but no lengths.
# Next, for each LG, Net, and Day it takes the average proportion of each age for the prev/following day
# There were two days where this average proportion didn't add up to 100%. 
# For these, I manually check and the value was supposed to be 100% age1, so I manually changed these.
# Lastly, I just cleaned up the vestigial columns and put it back to 'long' data format
allcatch.arcsages <- allcatch.arcsages %>% filter(!is.na(age.put) | LG == 3) %>%
  full_join(allcatch.arcsages %>% filter(LG != 3) %>% group_by(EndDate, Net, LG, age.put) %>% 
  summarise(totcount = totcount, ageprops=agecount/Lengthcount) %>% 
  spread(age.put, value = ageprops) %>% ungroup() %>% 
  rename(notrecorded = `<NA>`) %>% 
  replace(., is.na(.), 0) %>%
  arrange(LG, Net, EndDate) %>% 
  mutate(grouping=sprintf("%04s-%04s-%04s", LG, Net, EndDate),
         notrecorded = age0+age1+age2+age3plus) %>%
  mutate(age0 = ifelse(notrecorded==0, (dplyr::lag(age0, order_by=grouping) + dplyr::lead(age0, order_by=grouping))/2, age0),
         age1 = ifelse(notrecorded==0, (dplyr::lag(age1, order_by=grouping) + dplyr::lead(age1, order_by=grouping))/2, age1),
         age2 = ifelse(notrecorded==0, (dplyr::lag(age2, order_by=grouping) + dplyr::lead(age2, order_by=grouping))/2, age2),
         age3plus = ifelse(notrecorded==0, (dplyr::lag(age3plus, order_by=grouping) + dplyr::lead(age3plus, order_by=grouping))/2, age3plus),
         totalprop = age0 + age1 + age2 + age3plus,
         age1 = ifelse(totalprop == 0.5, 1, age1), #account for the two days. 
         #Note: if using this in future years, this is just a shortcut and won't necessarily work!!
         newprop = age0 + age1 + age2 + age3plus) %>% 
  filter(notrecorded ==  0) %>%
  dplyr::select(-c(notrecorded, grouping, totalprop, newprop)) %>%
  gather(age.put, ageprop, -EndDate, -Net, -LG, -totcount) %>%
  mutate(Year = year(EndDate),
         totalages = round(totcount * ageprop)) %>% 
  filter(totalages > 0),
  by = c("EndDate", "Year", "Net", "LG", "totcount", "age.put", "totalages")) %>% # end full join
  mutate(age.put = ifelse(LG==3, "age3plus", age.put), # now clean up remaining data
         Species = "ARCS",
         Station = as.factor(substr(Net, 1, 3))) %>%
  dplyr::select(-ageprop) %>% #drop column
  dplyr::select(Species, Year, EndDate, Net, Station, everything()) # order correctly





# CLEANUP
rm(bins, doy, i, j, cuts,
   allbreaks.arcs, allbreaks.bdwf,
   unmeasuredfish.arcs, unmeasuredfish.bdwf,
   arcsbreaks, bdwfbreaks,
   breakpoints.arcs, breakpoints.bdwf)




# FUNCTIONS TO USE
addbiwknum <- function(dataframename){
  dataframename <- dataframename %>% 
    mutate(day.of.year = yday(EndDate),
           biweekly = ifelse(day.of.year <= 196, 1, # July 15 and before 
                             ifelse(day.of.year > 196 & day.of.year <= 212, 2, #btwn July 16 and 31
                                    ifelse(day.of.year > 212 & day.of.year <= 227, 3, #Aug 1 - 15
                                           ifelse(day.of.year > 227, 4, NA))))) # Aug 16 and after 
}

addweeknum <- function(dataframename){
  dataframename <- dataframename %>% 
    mutate(day.of.year = yday(EndDate),
           week = ifelse(day.of.year <= 187, 1, 
                         ifelse(day.of.year > 187 & day.of.year <= 194, 2, 
                                ifelse(day.of.year > 194 & day.of.year <= 201, 3, 
                                       ifelse(day.of.year > 201 & day.of.year <= 208, 4, 
                                              ifelse(day.of.year > 208 & day.of.year <= 215, 5,
                                                     ifelse(day.of.year > 215 & day.of.year <= 222, 6,
                                                            ifelse(day.of.year > 222 & day.of.year <= 229, 7,
                                                                   ifelse(day.of.year > 229 & day.of.year <= 236, 8,
                                                                          ifelse(day.of.year > 236, 9, NA))))))))))
  
}


