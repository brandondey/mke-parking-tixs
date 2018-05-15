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

# Load libraries
library(tidyverse)
library(gmapsdistance)
library(ggmap) # cite ggmap: citation('ggmap')

# load cleaned objects for testing functions
load(file = "./Data/Clean/cleanedtickets_etc.Rdata")

# Get a unique vector of addresses around uwm. Should be 100.
adh_uwm %>%
  unite(coordinates, lat, lon, sep = "+") %>%
  select(coordinates, whole_address) %>%
  unique() %>%
  arrange(coordinates) -> unique_coordinates

glimpse(unique_coordinates)

# Get drive time and distance between top addresses around UWM.
# Can't use departure time and date because gmapsdistance requires future dates/times only.

gmap_me_coords <- head(unique_coordinates$coordinates, 10)

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
  geom_point(data = gmap_coords_df, aes(x = lon, y = lat, 
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

# get_Nearest_ticket() function finds the nearest ticketed address from an input and spits out 
# its location and distance from input address.

# Input: a whole_address in an hour on a given day. 
# Output: 
#   Exact time and address of <nearest> ticket in the last <twelve> hours
#   Driving time and distance between input and output
#  

top_uwm_gmap

get_Nearest_ticket <- function(distance_matrix, addresses) {
  
  for (c in 1:ncol(distance_matrix)) {
    
    closest_rank <- match(min(distance_matrix[distance_matrix[,c]!=0,c]), distance_matrix[,c])
    distance <- distance_matrix[closest_rank]
    closest_location <- addresses[addresses$rank==closest_rank, "location"]
    row <- cbind(closest_rank, distance, closest_location)
    if(c == 1){
      df_to_add  <<- row
    } else {
      df_to_add <<- data.frame(rbind(df_to_add, row))
    } #end if
    print(row)
  }# end for
  
  # create new data frame of addresses with distance to closest ticket. 
  # columns must be the same to cbind()
  
  #names(df_to_add) <<- c("closest_rank", "distance", "closest_location")
  
  addresses_new <<- cbind(addresses, df_to_add)
  
}#end function
