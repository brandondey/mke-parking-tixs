# Author: Brandon Dey
#
# Date: 5/14/18
#
# Purpose: 
#   This script imports data from various sources and does some basic cleaning. 
#   Exports "./Data/Clean/cleanedtickets_etc.Rdata", which is all the cleaned objects.
#
# Highlights: 
#   
#
# ^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^
# Environment
# ^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^

# Load libraries
library(foreign)
library(tidyverse) # loadd ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr, forcats
library(data.table)
library(lubridate)
library(openxlsx)



##_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^
#Grooming
##_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^

# First fix the time issue: i didn't import the issuetm into stata correctly.
# Note that Stata records datetime values as the number of milliseconds since 01jan1960.

tickets <- read.dta("./Data/Clean/2012 MKE Parking Ticket.dta", 
                    convert.factors = FALSE) # Don't convert Stata value labels to R factors. If T, there's an error: factor level [39] is duplicated
blocks <- read.dbf("./Data/Raw/oct15nogeo.dbf") #This is block data.
neighborhoods <- read.dbf("./Data/Raw/neighborhood.dbf") #This is neighborhood data

fromstata_dttm <- read.dta("./Data/Raw/for_dttm_merge_R.dta") 
fromstata_dttm[, "newtm"] <- fromstata_dttm[,"issuetm"] + (as.POSIXct("1960-01-01 00:00:00 CST") - fromstata_dttm[fromstata_dttm$tixno=='662538111',"issuetm"]) #convert garbled issustm to milliseconds since 1960. 
fromstata_newtms <- select(fromstata_dttm, tixno,newtm)#If I ever want to cleave the date off do: t <- strftime(times, format="%H:%M:%S")
tickets <- merge(x = tickets, y =fromstata_newtms, by = "tixno", all.x = T) #rejoin to orginal dataset.
tickets <- unique(tickets) # remove duplicates from the original dataset.

# Transform block data. 
blocks %>% 
  mutate(BLOCK_ST = pmin(LO_ADD_L, LO_ADD_R), 
         BLOCK_END = pmax(HI_ADD_L, HI_ADD_R)) -> blocks

# select and arrange needed variables
tickets <- tickets %>% 
  select(-issuetm, -time, -UniqueID, -feelslike)
tickets <- tickets[,c("tixno","tixdesc", "origfee", "location", "Lat", "Long", "street", "nodirstreet", "direction", "issuedt", "newtm", "month", "calday", "dow", "hour", "Temp", "windmph", "gustmph", "visibilitymiles", "snowin")]  

glimpse(tickets)


# Summarize at location/day,hour level. Only includes hours and days when a tic was issued.
tix_locations_daily <- tickets %>% 
  mutate(day = issuedt) %>%
  group_by(location, day, hour) %>%
  summarise(n(), 
            lat = max(Lat), 
            long = max(Long)) %>%
  arrange(location, day, hour)
names(tix_locations_daily) <- c("location", "day", "hour", "tickets", "lat", "long")

##_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^
# Build and test approach on the area surrounding UWM
##_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^

timestart <- as.Date("2012-01-01") #take a smaller sample to make wait times on this slow machine bearable. (And then take a sample of that)
timeend <- as.Date("2012-03-31")

# create df of ticketet locations around UWM.
# #N of Locust, S of Newton, east of about Oakland. I have an eastern bound (even though Lake Michigan is there because my geocoder went a little haywire on some addresses. Found some in Europe...)
uwm <- tix_locations_daily %>%
  mutate(long = as.numeric(long), 
         lat = as.numeric(lat)) %>% 
  filter((long >=-87.889 & long <= -87.863) & (lat >= 43.070 & lat <= 43.085)) %>% 
  arrange(location, day, hour) 

# summarize UWM data @ location level  
uwm_topaddresses <- uwm %>%
  filter(day >= timestart & day <= timeend) %>%
  group_by(location) %>%
  summarise(tickets = sum(tickets), 
            lat = max(lat), 
            long = max(long)) %>%
  arrange(desc(tickets)) 

# rank and filter to top 100 addresses around UWM.
uwm_topaddresses <- uwm_topaddresses %>%
  mutate(rank = (1:nrow(uwm_topaddresses))) %>%
  filter(rank <=100)

locations_vec_uwm <- uwm_topaddresses$location

# Create df of all days and times, for all locations in sample
days <- seq(timestart, timeend, by = "days") 
times <- c(0:23)

# define the observational unit as a single location in a single hour on a single day
adh_uwm <- expand.grid(locations_vec_uwm, days, times, KEEP.OUT.ATTRS = T) 
names(adh_uwm) <- c("location", "day", "hour")
adh_uwm <- arrange(adh_uwm, location, day, hour)


# separate address into component variables: house num, direction, street, street suffix
uwm_topaddresses <- uwm_topaddresses %>%
  separate(location, into = c("house_num", "direction", "street_name", "suffix"), sep = " ")

uwm_street_names <- unique(uwm_topaddresses$street_name)

# get all blocks in uwm_topaddresses
blocks %>% filter(STREET %in% uwm_street_names) -> uwm_blocks

# get custom functions
load("./Scripts/MKE_Parking_Functions.rds")

# get all the addresses on blocks in uwm_blocks. This creates a df called address_dictionary with all the addresses on every block in uwm_blocks
get_block_addresses(uwm_blocks)

# create variable of whole address to look up in geocoder census_geocoder.
address_dictionary %>% 
  mutate(whole_address = paste(location, block_street_direction, 
                               block_street, block_street_suffix, sep = " ")) -> address_dictionary


##_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^
# Geocode lots of addresses around UWM. It takes about 2 seconds to geocode an address.
##_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^

uwm_geocoded <- lapply(address_dictionary$whole_address, FUN = census_geocoder, secondary = "Milwaukee", type = F, state = "WI")

# how many uwm addresses couldn't be geocoded?
sum(unlist(lapply(uwm_geocoded, FUN = is.null)))

# squash list of dataframes into a single one.
flattened_uwm_geocoded <- plyr::rbind.fill(uwm_geocoded)

# create var called location that matches same format as location var in adh_uwm
flattened_uwm_geocoded %>% # don't worry about the "Expected 1 pieces" warning. From the second "," after MKE in addresses.
  separate(col = address, into = "location", sep = ",") -> flattened_uwm_geocoded

# convert to character to join
adh_uwm$location <- as.character(adh_uwm$location)

# join all lat/long coordinates. left join in case the address wasn't geocoded.
adh_uwm <- left_join(adh_uwm, flattened_uwm_geocoded, by = "location")

# remove lat long columns in uwm
uwm <- uwm[, c("location","day", "hour", "tickets")]

# join the ticketed addresses with the day/time df to show ticket history at each ticketed spot over time
adh_uwm <- merge(x = uwm, y = adh_uwm, by = c("hour", "day","location"), all.y = T) %>%
  mutate(tickets = ifelse(is.na(tickets), 0, tickets)) %>% # NAs to 0
  arrange(location, day, hour)

# create address vars without suffix so I can join block df to adh_uwm
gsub("\\s*\\w*$", "", address_dictionary$whole_address) -> address_dictionary$whole_address_no_suffix
gsub("\\s*\\w*$", "", adh_uwm$location) -> adh_uwm$location_no_suffix

# Join ticket data with block data
adh_uwm %>%
  inner_join(address_dictionary, by = c( "location_no_suffix" = "whole_address_no_suffix")) %>%
  select(-contains(".")) -> adh_uwm

# fix date times, finally.
read.xlsx("/users/brandondey/documents/projects/Data Science/MKE_Parking_Tickets/Data/Clean/2012 tickets issued.xlsx", 
          sheet = 1, 
          detectDates = T) -> raw_tix

raw_tix$issue_dttm <- as.Date(raw_tix$ISSUEDATE) + lubridate::seconds(raw_tix$ISSUETIME*24*60*60)

summary(raw_tix)

raw_tix %>% 
  select(ISSUENO, issue_dttm, VIODESCRIPTION) -> raw_tix 

tickets %>% 
  merge(raw_tix, 
        by.x = "tixno", by.y = "ISSUENO") %>%
  select(-newtm, -snowin, -visibilitymiles, -gustmph, -windmph, -temp) -> tickets

colnames(tickets) <- tolower(colnames(tickets))
tickets <- unique(tickets)



# save objects 
save(
    adh_uwm,  
    blocks, 
    neighborhoods, 
    tickets, 
    tix_locations_daily, 
    uwm, 
    uwm_topaddresses, 
    locations_vec_uwm,
    timestart, 
    timeend,
    uwm_blocks,
    address_dictionary,
    uwm_geocoded,
    file = "./Data/Clean/cleanedtickets_etc.Rdata")
