# Author: Brandon Dey
#
# Date: 5/14/18
#
# Purpose: 
#   Explore ticket data
#
# Highlights: 
#   Count number of NULL elements in a list using base::is.null() %>% lapply() %>% base::unlist() %>% sum()
#   Squash list of dataframes into one df using plyr::rbind.fill()
#
#
# Working To Do list:
#   Write get_nearest_ticket
#   Test on uwm_adh (top 100 addresses around UWM for roughly the first quarter of 2012)
#   Run on all UWM addresses, not just the top 100 spots
#   Get vars:
#       whether ticketed address is on the same street as input address
#         and whether it’s on the same side of street
#           number of addresses between ticketed address and input address’s side of street
#   Visualize
#   Model?

# ^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^
# Environment
# ^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^

# Load libraries
library(foreign)
library(tidyverse) # ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr, forcats
library(plyr) # for rbind.fill
library(ggmap) # cite ggmap: citation('ggmap')
library(lubridate)

library(gmapsdistance) # to get driving times between locations

library(data.table)
library(geosphere)

# load cleaned objects
load(file = "./Data/Clean/cleanedtickets_etc.Rdata")


  
# ^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^
# Visualize Area around UWM
# ^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^

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
# Archive
## ^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_


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
