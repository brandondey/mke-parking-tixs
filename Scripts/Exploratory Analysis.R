# Highlights: 
#   count number of NULL elements in a list using base::is.null() %>% lapply() %>% base::unlist() %>% sum()


# Load libraries
library(foreign)
library(tidyverse) # ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr, forcats
library(ggmap) # cite ggmap: citation('ggmap')
library(data.table)
library(lubridate)
library(geosphere)
library(plyr) # for rbind.fill
library(gmapsdistance) # to get driving times between locations

# load cleaned objects
load(file = "./Data/Clean/cleanedtickets_etc.Rdata")

# how many uwm addresses couldn't be geocoded?
sum(unlist(lapply(uwm_geocoded, FUN = is.null)))

# squash list of dataframes into a single one.
flattened_uwm_geocoded <- plyr::rbind.fill(uwm_geocoded)

# create var called location that matches same format as location var in adh_uwm
flattened_uwm_geocoded %>%
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


# adh_uwm is what I think i'll be predicting on.




# join block data
head(adh_uwm)
head(address_dictionary)




##_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^
#Exploratory Analysis.
##_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^
# Use ggplot with ggmap to plot the lat/long. https://stackoverflow.com/questions/23130604/plot-coordinates-on-map

mapgilbert <- get_map(location = c(Longitude = mean(uwm$long), Latitude = mean(uwm$lat)+.001), zoom = 15, # Get the base map from maps.google api
                      maptype = "roadmap", scale = 2)

#ggmap(mapgilbert) #base map

title <- "All Parking Tickets around UWM"
ggmap(mapgilbert) + # plot the map with all uwm points on it
  geom_point(data = uwm, aes(x = long, y = lat, alpha = 0.6), fill = "red", size = .75, shape = 21) +
  guides(alpha = F, size = F, fill = F) +
  ggtitle(title) +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
ggsave(title, plot = last_plot(), device = "jpeg", path = "./Plots")


# top 100 uwm points on it
title <- "Top 100 Ticketed Addresses around UWM"
ggmap(mapgilbert) +
  geom_point(data = uwm_topaddresses, aes(x = long, y = lat, alpha = 0.6, size = tickets), fill = "red", shape = 21) +
  guides(alpha = F, size = F, fill = F) + 
  ggtitle(title) +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
ggsave(title, plot = last_plot(), device = "jpeg", path = "./Plots")



# find the main offenders by # of issued violations 
length(unique(tickets$location))
tickets_summary <- tickets %>%
  group_by(tixdesc) %>%
  summarise(n= n(), 
            totfees = sum(origfee), 
            earliest_tod = min(newtm),
            latest_tod = max(newtm), 
            distinct_addresses = length(unique(location))) %>%
  arrange(desc(n))

ggplot(tickets, aes(x=newtm)) + geom_histogram(bins = 60) + facet_wrap("tixdesc") #dist of times by all tixdesc




## ^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_
#Where's the meter maid? 
##1. (How often can I not answer this?) I.e., How many hours per day, on average, does the meter maid drop off the map? (This is what I might need to estimate)
##2. I need to estimate the meter maid jurisdictions. Say, tickets can't be issued in the same 2 minute window more than 
##### d distance apart. Those tickets that are were issued by a different meter maid. All tickets issued 
##3. How many blocks, on average, were not ticketed?

## ^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_
day_by_hour <- spread(uwm,key = hour, value = tickets) %>%
  arrange(location, day) %>%
  setDT() %>%
  select(-lat, -long, -location)

day_by_hour_day <- day_by_hour[, lapply(.SD, sum, na.rm=TRUE), by=day ] %>% #summarize by day and hour
  mutate(month = month(ymd(day), label = T),
         quarter = quarter(day)) 

day_by_hour_day <- gather(day_by_hour_day, key = hour, value = tickets, -day, -month, -quarter)

day_by_hour_day <- day_by_hour_day %>%
  group_by(day) %>%
  arrange(day, hour)

day_by_hour_day_viz <- day_by_hour_day %>%
  group_by(hour, quarter, month) %>%
  summarise(tickets = sum(tickets),
            no_tickets = ifelse(sum(tickets) == 0,1,0)) %>%
  arrange(month) 

day_by_hour_day_viz$hour <- as.numeric(day_by_hour_day_viz$hour)

hour_plot <- ggplot(day_by_hour_day_viz, aes(x = hour, y = tickets)) + 
  geom_histogram(stat = "identity") 

hour_plot + facet_grid(month~.)
hour_plot + facet_grid(quarter~.)

sum(day_by_hour_day_viz$no_tickets) #there were only 3 hours in 2012 that a parking ticket was not issued in the UWM area!  

#FUNCTIONS CAME FROM HERE

# How many blocks don't have a ticket? A lot will not be good be cause I may need to simulate if/when the mm patrols those areas. 


##_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^
#Then do this for ALL addresses in a neighborhood, not just the ticketed addresses.
##_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^


print("put code here")


##_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^
#Are they coming my way?!
##_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^





## ^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_
#Scratch work
## ^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_

#this much data will be a problem
#research how to speed this up: Wickam's advanced R: http://adv-r.had.co.nz/
#or: https://rstudio-pubs-static.s3.amazonaws.com/72295_692737b667614d369bd87cb0f51c9a4b.html
#or: https://cran.r-project.org/web/views/HighPerformanceComputing.html


## ^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_
# Lat/long info
# http://www.findlatitudeandlongitude.com/?loc=Milwaukee%2C+WI+53244%2C+USA#.WVxr09PyuRs
# https://en.wikipedia.org/wiki/Geographic_coordinate_system
# Tested Lat/long coordinates
#long >=-87.890797 & lat >= 43.070859 # 130,459
#>=-87.89079666137695 #588572          
#>=-87.89139747619629 #585466
## ^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_

#8760*length(locations$location)/ length(tickets$tixno)



tix_locations <- tickets %>% #this is collapsing all tickets, it makes no distinction of tixdesc.
  group_by(location, hour) %>%
  summarise(n(), 
            lat = max(Lat), 
            long = max(Long))
names(tix_locations) <- c("location", "hour", "tickets", "lat", "long")


tix_times_t <- merge(x = tix_times, y = tix_locations, by = c("hour", "location"), all.x = T) 
tix_times_t <- tix_times_t %>% 
  arrange(location, hour) %>%
  mutate(tickets = ifelse(is.na(tickets) ==T,0,tickets)) # replace NA with 0


# Our old address on oakland
tix_times_t[tix_times_t$location=="2636 N OAKLAND AVE",]
ggplot(tix_times_t[tix_times_t$location=="2636 N OAKLAND AVE",c("hour", "tickets")],aes(x=hour, y = tickets)) + geom_bar(stat = "identity")

# The heights of the bar graph can not be used as the basis of the strict risk of getting a ticket becuase
#getting a ticket is a function of whether a car is parked illegally AND if there's a meter maid nearby.
# I need to esimtate if there will be a car parked in every spot in every hour evenutally, 
# but for now just get the location of the nearest ticket in the last hour for every hour and location. 

#First get lat/long for ALL locations, not just the ones with tickets
locations <- tickets %>% 
  group_by(location) %>%
  summarise(Lat = max(Lat), 
            Long = max(Long))
tix_times_t <- merge(x = tix_times_t, y = locations, by = "location", all.x = T)
tix_times_t <- tix_times_t %>% 
  mutate(lat = ifelse(is.na(lat)==T,Lat, lat),
         long = ifelse(is.na(long)==T,Long, long)) %>%
  select(-Long, -Lat)


#tix_times_t_daily <- merge(x = tix_times_t_daily, y = locations, by = "location", all.x = T)
#tix_times_t_daily <- tix_times_t_daily %>% 
#  mutate(lat = ifelse(is.na(lat)==T,Lat, lat),
#         long = ifelse(is.na(long)==T,Long, long)) %>%
#  select(-Long, -Lat, -issuedt)%>%
#  mutate(tickets = ifelse(is.na(tickets) ==T,0,tickets)) # replace NA with 0



