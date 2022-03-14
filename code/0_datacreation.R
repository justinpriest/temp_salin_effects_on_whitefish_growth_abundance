library(tidyverse)
#library(tidyr)
#library(tibble)
library(lubridate)
library(readxl)
library(here)


########## SECTION 1 ##########
## IMPORT LENGTH, CATCH DATA ##

# Read in the length, catch, and environmental data, plus species lookup table
load(here::here("data/PrudhoeCatch&LengthDataset_2001-2018_Version12.Rdata"))

#Will need this later
all.len <- all.len %>% mutate(Net = paste0(Station, Side))

# Clean up dataframes
allcatch <- allcatch %>% mutate(Net = factor(Net), Station = factor(substr(Net, 1, 3)))
env_allyears <- env_allyears %>% mutate(Station = factor(Station)) %>% filter(Station != 231)

allcatch <- allcatch %>% add_column(Month = month(allcatch$EndDate), .after = 2)
env_allyears <- env_allyears %>% add_column(Month = month(env_allyears$Date), .after = 2)


pru_lengths <- all.len # work in a copy of the data just in case

pru_lengths <- pru_lengths[pru_lengths$Length!=-9999,]
pru_lengths <- pru_lengths[pru_lengths$Length>0,]







# Note! indivfish.bdwfages is only for LG1 & 2!
indivfish.bdwfages <- pru_lengths %>% filter(Length < 250, Species == "BDWF")
write_csv(indivfish.bdwfages, "data/indivfish_bdwfages.csv")


indivfish.arcsages <- pru_lengths %>% filter(Length < 250, Species == "ARCS")
write_csv(indivfish.arcsages, "data/indivfish_arcsages.csv")




bdwflargelength <- pru_lengths %>% 
  filter(Length >= 250, Species == "BDWF") %>% 
  group_by(Year) %>% 
  summarise(age3plusLG3=sum(totcount))

write_csv(bdwflargelength, "data/bdwflargelength.csv")


arcslargelength <- pru_lengths %>% 
  filter(Length >= 250, Species == "ARCS") %>% 
  group_by(Year) %>% 
  summarise(age3plusLG3=sum(totcount))

write_csv(arcslargelength, "data/arcslargelength.csv")




unmeasuredfish.bdwf <- allcatch %>% filter(Species == "BDWF", LG == 1 | LG == 2) %>% group_by(Year, EndDate, Net, LG) %>% 
  summarise(totcount=sum(totcount, na.rm = TRUE)) %>% 
  left_join(all.len %>%
              mutate(Net = paste0(Station, Side)) %>%
              filter( Species == "BDWF") %>% group_by(Year, EndDate, Net, Group) %>% 
              summarise(Lengthcount=sum(totcount, na.rm = TRUE)), 
            by = c("Year" = "Year", "EndDate" = "EndDate", "Net" = "Net", "LG" = "Group")) %>%
  replace(., is.na(.), 0) %>%
  mutate(unmeasured = totcount - Lengthcount) 
write_csv(unmeasuredfish.bdwf, "data/unmeasuredfish_bdwf.csv")


unmeasuredfish.arcs <- allcatch %>% filter(Species == "ARCS", LG == 1 | LG == 2) %>% group_by(Year, EndDate, Net, LG) %>% 
  summarise(totcount=sum(totcount, na.rm = TRUE)) %>% 
  left_join(all.len %>%
              mutate(Net = paste0(Station, Side)) %>%
              filter( Species == "ARCS") %>% group_by(Year, EndDate, Net, Group) %>% 
              summarise(Lengthcount=sum(totcount, na.rm = TRUE)), 
            by = c("Year" = "Year", "EndDate" = "EndDate", "Net" = "Net", "LG" = "Group")) %>%
  replace(., is.na(.), 0) %>%
  mutate(unmeasured = totcount - Lengthcount) 
write_csv(unmeasuredfish.arcs, "data/unmeasuredfish_arcs.csv")




bdwflargecatch <- allcatch %>% 
  filter(Species == "BDWF", LG == 3) %>% 
  group_by(Year, EndDate, Net, LG) %>% 
  summarise(totcount=sum(totcount, na.rm = TRUE))
write_csv(bdwflargecatch, "data/bdwflargecatch.csv")


arcslargecatch <- allcatch %>% 
  filter(Species == "ARCS", LG == 3) %>% 
  group_by(Year, EndDate, Net, LG) %>% 
  summarise(totcount=sum(totcount, na.rm = TRUE))
write_csv(arcslargecatch, "data/arcslargecatch.csv")


# Environmental Data
write_csv(env_allyears, "data/env_allyears.csv")




# Effort summary 
effort <- full_join(all.len %>% distinct(EndDate, Net), 
                    allcatch %>% distinct(EndDate, Net), 
                    by = c("EndDate", "Net")) %>% 
  left_join(all.len %>%
              dplyr::select(EndDate, Net, StartDateTime, EndDateTime), 
            by = c("EndDate", "Net")) %>% 
  distinct(EndDate, Net, StartDateTime, EndDateTime) %>%
  #this first mutate MANUALLY adds in skipped dates
  mutate(StartDateTime=replace(StartDateTime, EndDate=="2018-07-10" & Net == "230N", 
                               as.POSIXct("2018-07-09 09:25")), 
         EndDateTime=replace(EndDateTime, EndDate=="2018-07-10" & Net == "230N", 
                             as.POSIXct("2018-07-10 08:55")), 
         
         StartDateTime=replace(StartDateTime, EndDate=="2018-07-24" & Net == "220W", 
                               as.POSIXct("2018-07-23 09:15")), 
         EndDateTime=replace(EndDateTime, EndDate=="2018-07-24" & Net == "220W", 
                             as.POSIXct("2018-07-24 14:45"))) %>% 
  mutate(Year = year(EndDate),
         Effort_NetHrs = as.numeric(EndDateTime - StartDateTime),
         Station = substr(Net, 1,3)) %>%
  arrange(EndDate, Net, Station)
# A few NAs occur when we have catch but no lengths for that day 
# (because the times are recorded in the length dataframe!)
write_csv(effort, "data/effort.csv")



