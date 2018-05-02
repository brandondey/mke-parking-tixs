# this script contains all the functions I wrote custom for the MKE Parking Ticket project.
# it saves each function as an object in a .rds in ./Scripts/MKE_Parking_Functions.rds



library(tidyverse)
require(geosphere)

# load cleaned objects for testing functions
load(file = "./Data/Clean/cleanedtickets_etc.Rdata")

#Calculate distances between all the top 100 ticketed addreses around UWM.
d <- uwm_topaddresses[,c("long", "lat")]
distances <- geosphere::distm(x = d, fun = distHaversine) #to limit use: d[1:10,]. # distHaversine assumes the earth is a perfect sphere. It's not. It's eppiloidal.
distances <- distances*3.28084 # convert meters to feet


dim(distances)


# this function finds the nearest ticketed address from an input and spits out 
# the location and distance from the address.
get_nearest_ticket <- function(distance_matrix, addresses) {
  
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

# remove objects created outside function scope
rm(df_to_add, addresses_new, row)

#test get_nearest_ticket
get_nearest_ticket(distances, uwm_topaddresses)






if ( exists("address_dictionary") )  {
  rm(address_dictionary)
} 
get_block_addresses <- function( block_df ){
# create a dataframe called address_dictionary with all addresses between where a block starts and ends. 
#
# args: block_df. data frame where each row is a block. Needs five columns: BLOCK_ST, BLOCK_END, STREET, LENGTH
#
# FIX: 
# shorten: map2: block_list <- map2(uwm_blocks$BLOCK_ST, uwm_blocks$BLOCK_END, .f = seq)
# output dataframe with name of inputted dataframe_output or something.
# make faster

  for(a in 1:nrow(block_df)){
    all_addresses_on_block <- data.frame(location = seq(block_df[a, "BLOCK_ST"], block_df[a, "BLOCK_END"]), 
                                         block_street_direction = block_df[a, "DIR"], 
                                         block_street = block_df[a,"STREET"], 
                                         block_street_suffix = block_df[a, "STTYPE"],
                                         block_start = block_df[a, "BLOCK_ST"] , 
                                         block_end = block_df[a, "BLOCK_END"],
                                         block_lowest_addr_left_side = block_df[a, "LO_ADD_L"],
                                         block_length = block_df[a,"LENGTH"])
    if( a == 1 ){
      address_dictionary <<- data.frame(all_addresses_on_block) 
    } else {
      address_dictionary <<- data.frame(rbind(address_dictionary, 
                                              all_addresses_on_block))
    }#end if
  }#end for
  #new_block_data <- address_dictionary
}#end function






# census_geocoder from: https://github.com/SigmaMonstR/census-geocoder
census_geocoder <- function(address, type, secondary, state){
# Overview
#   census_geocoder gets lattitude and longitude coordinates for an address by using the US Census Bureau's geocoder. (For free.)
#   runs for a single address at a time.
#   e.g.: census_geocoder(address = "463 W LOCUST ST", secondary = "Milwaukee", type = F, state = "WI")
#
# Args:
#   address: house number and street address
#   type: have zipcode? if so, use type = "z", if not, anything works.
#   secondary: city
#   state: as abbreviation
#
# returns
#  data frame
  
  library(jsonlite)
  library(RCurl)
  
  addy <- paste("street=", gsub(" ", "+", address), sep = "")
  if (type == "z") {
    wild <- paste("zip=", gsub(" ", "+", secondary), sep = "")
  } else {
    wild <- paste("city=", gsub(" ", "+", secondary), sep = "")
  }
  
  state <- paste("state=", gsub(" ", "+", state), sep = "") 
  string <-  paste("https://geocoding.geo.census.gov/geocoder/geographies/address?", addy, "&", wild, "&", state, "&benchmark=4&vintage=4&format=json", sep = "")
  json_file <- fromJSON(getURL(string))
  
  # Check if there are results
  if(length(json_file$result$addressMatches$coordinates) > 0){
    
    # If not, kick back an empty dataframe
    if(is.null(json_file$result$addressMatches$coordinates$x[1]) == TRUE){
      print("no result")
      return(data.frame(
        address = "",
        lat = "",
        lon = ""))
      
    } else{
      
      # Address, lat, lon (keep first match)
      return(data.frame(
        address = as.character(data.frame(json_file$result$addressMatches$matchedAddress)[1, ]),
        lat = as.character(json_file$result$addressMatches$coordinates$y[1]),
        lon = as.character(json_file$result$addressMatches$coordinates$x[1])))
      
    }
  }
}


# save functions
save(get_block_addresses, 
     get_nearest_ticket,
     census_geocoder,
     file = "./Scripts/MKE_Parking_Functions.rds")

# load("./Scripts/MKE_Parking_Functions.rds")





# ARCHIVE
# FOR LOOP TO SPLIT ADDRESSES INTO COMPONENT PARTS: house number, direction, street name, suffix
#
# split_addresses <- sapply(uwm_topaddresses$location, FUN = strsplit, split = " ") # Split location field on spaces and stuff into a list.
# house_num <- numeric(100) # create numeric vector of 100 elements
# direction <- character(100) #create character vector of 100 elements
# street_name <- character(100) #create character vector of 100 elements
# 
# for (a in 1:length(split_addresses)) {
#   # works now, but address gotchas might break it, eg:
#   # no direction
#   # doesn't start with house number
#   # no road type
#   house_num[a] <- split_addresses[[a]][1] # house number is 1st element of sublist
#   direction[a] <- split_addresses[[a]][2] # direction is 2nd element of sublist
#   street_name[a] <- split_addresses[[a]][3] # street name is 3rd element of sublist
# }
# # combine back to df 
# uwm_topaddresses <- cbind(uwm_topaddresses, house_num, direction, street_name)





# THIS TESTS GET_BLOCK_ADDRESSES ON A RANDOM SAMPLE OF ADDRESSES
# 
# block_sample <- blocks %>%
#   select(RCD_NBR, STREET, LO_ADD_L, LO_ADD_R, HI_ADD_L, HI_ADD_R) %>%
#   sample_n(1000) %>%
#   mutate(startof_block = pmin(LO_ADD_L, LO_ADD_R), # row wise min
#          endof_block = pmax(HI_ADD_L, HI_ADD_R)) %>%
#   select(-HI_ADD_L, -HI_ADD_R, -LO_ADD_L, -LO_ADD_R)
# # test get_block_addresses
# get_block_addresses(block_sample)