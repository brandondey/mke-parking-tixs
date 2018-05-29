# Author: Brandon Dey
#
# Date: 5/14/18
#
# Purpose: 
#   Working script as I develop get_Nearest_ticket()
#
# Highlights: 
#   
#

# ^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^
# Environment
# ^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^

library(gmapsdistance)
library(ggmap) # cite ggmap: citation('ggmap')
library(Imap)
library(gganimate)

#load cleaned objects for testing functions
load(file = "./Data/Clean/cleanedtickets_etc.Rdata")


## ARCHIVE -- Or needs to be vetted of usefulness
# Get a unique vector of addresses around uwm. Should be 100.
adh_uwm %>%
  mutate(gmap_coordinates = paste(lat,lon, sep = "+"),
         lat = as.numeric(as.character(lat)), 
         lon = as.numeric(as.character(lon))
  ) -> adh_uwm

adh_uwm %>%
  select(gmap_coordinates, whole_address) %>%
  unique() %>%
  arrange(gmap_coordinates) -> unique_coordinates

glimpse(unique_coordinates)

# Get drive time and distance between top addresses around UWM.
# Can't use departure time and date because gmapsdistance requires future dates/times only.

gmap_me_coords <- head(unique_coordinates$gmap_coordinates, 10)

top_uwm_gmap <- gmapsdistance(origin = gmap_me_coords, gmap_me_coords, 
                              mode = "driving", 
                              shape = "long")

# The distance is returned in meters and the time in seconds.
# Plot 
data.frame(lat = lapply(str_split(gmap_me_coords, pattern = "[+]"), `[[`, 1) %>% unlist, 
           lon = lapply(str_split(gmap_me_coords, pattern = "[+]"), `[[`, 2) %>% unlist) -> gmap_coords_df

gmap_coords_df %>%
  mutate(lat = as.numeric(as.character(lat)), 
         lon = as.numeric(as.character(lon))) -> gmap_coords_df


# Get the base map from maps.google api
gg_map_base <- get_map(location = c(Longitude = mean(gmap_coords_df$lon), 
                                    Latitude = mean(gmap_coords_df$lat) + 0.001), 
                       zoom = 15, 
                       maptype = "roadmap", scale = 2)
# base map
ggmap(gg_map_base) 

title <- "Ten Locations around UWM"
ggmap(gg_map_base) + # plot the map with all uwm points on it
  geom_point(data = gmap_coords_df, aes(x = lon, 
                                        y = lat, 
                                        alpha = 0.6), 
             fill = "red", 
             size = 1, 
             shape = 21) +
  
  guides(alpha = F, size = F, fill = F) +
  ggtitle(title) +
  theme(plot.title = element_text(hjust = 0.5))

dev.off()

ggsave(title, 
       height = 10, 
       width = 10,
       device = "jpeg", 
       path = "./Plots")

# Get variables in a sensible order
adh_uwm %>%
  select(whole_address, location_no_suffix, day, hour, tickets, lat, lon, everything()) -> adh_uwm


gmap_addr <- unique_coordinates %>%
  inner_join(unique(top_uwm_gmap$Time %>% select(or)), 
             by = c("coordinates" = "or"))


adh_uwm %>%
  inner_join(gmap_addr, by = "whole_address") -> gmap_tix_adh

glimpse(gmap_tix_adh)


glimpse(gmap_coords_df)
glimpse(gmap_tix_adh)

ggmap(gg_map_base) + 
  
  geom_point(data = sample_n(gmap_tix_adh, 1000), 
             aes(x = as.numeric(as.character(lon)), 
                 y = as.numeric(as.character(lat)), 
                 frame = day,
                 fill = hour,
                 alpha = 0.3), 
             size = 1, 
             shape = 21) +
  
  geom_jitter() +
  
  guides(alpha = F, 
         size = F, 
         fill = F) +
  
  labs(title = title) +
  theme(plot.title = element_text(hjust = 0.5)) -> animate_me

# Try to annimate these ten addresses 
gganimate(animate_me,  interval = .6)