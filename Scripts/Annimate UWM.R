# Author: Brandon Dey
#
# Date: 5/14/18
#
# Purpose: This script annimates all ticketed addresse on locust in timeframe
#   
#
# Highlights: 
#   
#

# ^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^
# Environment
# ^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^

# Load libraries
library(tidyverse)
library(ggmap) # cite ggmap: citation('ggmap')
library(gganimate)

# load cleaned objects for testing functions
load(file = "./Data/Clean/cleanedtickets_etc.Rdata")

glimpse(adh_uwm)

adh_uwm %>%
  filter(day >= "2012-01-01" & day <= "2012-01-15") -> locust

summary(locust)

# Get the base map from maps.google API
gg_map_base <- get_map(location = c(Longitude = mean(locust$lon), 
                                    Latitude = mean(locust$lat) + 0.001), 
                       zoom = 15, 
                       maptype = "roadmap", scale = 2)

# base map
ggmap(gg_map_base) 

title <- "Locust Annimation"
ggmap(gg_map_base) + 
  
  geom_point(data = locust, 
             aes(x = as.numeric(as.character(lon)), 
                 y = as.numeric(as.character(lat)), 
                 frame = day + hour,
                 fill = hour, 
                 alpha = tickets), 
             size = 1, 
             shape = 21) +
  
  scale_alpha("tickets", range = c(.1, 1)) +
  
  geom_jitter() +
  
  guides(alpha = F, 
         size = F, 
         fill = F) +
  
  labs(title = title) +
  theme(plot.title = element_text(hjust = 0.5)) -> animate_me

# Annimate
gganimate(animate_me,  
          interval = 1.5,
          cumulative = F # Each image generates with new data. Pic doesn't grow cumulatively
          )




