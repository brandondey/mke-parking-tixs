# Author: Brandon Dey
#
# Date: 5/28/18
#
# Purpose: 
#   Working script as I develop get_Nearest_ticket()
# 
#
# Input: a whole_address in an hour on a given day. 
# Output: 
#   Exact time and address of <nearest> ticket in the last <twelve> hours
#   Driving time and distance between input and output
#  
# Highlights: 
#   

# ^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^
# Environment
# ^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^_^

# Load libraries
library(tidyverse)
library(gmapsdistance)
library(ggmap) # cite ggmap: citation('ggmap')
library(gganimate)
library(Imap)
library(openxlsx)
library(lubridate)
library(sqldf)

#load cleaned objects for testing functions
load(file = "./Data/Clean/cleanedtickets_etc.Rdata")


plan_crow_flights <- function(df, id, units = "miles")
  # help from: http://www.nagraj.net/notes/calculating-geographic-distance-with-r/
{
  # About:
  #   plan_crow_flights() calculates as a crow flies (geodesic) distance between coordinates. 
  #   (Popular among crows planning their migration)
  # 
  # Args:
  #   df: dataframe
  #   id: var name as a string that uniquely identifies what a lat/long coordinate represents. 
  #       A place? An address? 
  #   units: units distance is calculated in. Passed to (and so options restricted by) Imap::gdist
 
  if(all((names(df) %in% id) == F)) {
    stop("Error: Your id variable isn't in the dataframe. Try adding it and rerunning.")
  }
  
  require(tibble)
  require(Imap)
  
  # calculate crow distance with Imap::gdist()
  dist_list <- list() 
  
  for (i in 1:nrow(df)) {
    dist_list[[i]] <- gdist(
                            lat.1 = df$lat, 
                            lon.1 = df$lon,
                            lat.2 = df$lat[i], 
                            lon.2 = df$lon[i],
                            units = units)
  }
  
  # unlist and convert to a named matrix 
  
  dist_mat <- sapply(dist_list, unlist)
  colnames(dist_mat) <- df[, which(colnames(df) %in% id )]
  rownames(dist_mat) <- df[, which(colnames(df) %in% id )]
  
  # convert matrix to df to get address as var
  tibble::rownames_to_column(data.frame(dist_mat), 
                             var = id) -> dist_df
  return(dist_df)
  }


plan_crow_flights(uwm_addrs, 
                  id = "whole_address")



# get nearby tickets and find their addresses within timeframe
# then get distances 
# then find closest on via get_nearest_address
# then the num spots between there and here
# then street vars like side, same side, etc.

glimpse(adh_uwm)
glimpse(tickets)

scour_historic_tix <- function(time_series_df, tix_lookup, hrs_within = 24)
{
# About: 
#   scour_historic_tix() scours a time series df and gets all the tickets (and accompanying info)
#   issued within a certain number of hours of a given time and address.
#
# Args:
#   time_series_df: df at hour/day/address level. 
#   tix_lookup: df of all tickets issued
#   hrs_within: number of hours to get tickets issued before hour in time series.

  
  
  }

adh_uwm %>% summary

# fix issue time and date in tickets
tickets$issue_dttm <-  as.POSIXct(tickets$issue_dttm) 

# add begining hour to tix time series
adh_uwm %>% mutate(hour_start = as.POSIXct(as.Date(day) + hours(hour)), 
                   hour_end = as.POSIXct(as.Date(day) + hours(hour + 1))) -> adh_uwm

# get all tickets issued within the hour
sqldf::sqldf("select ts.*, t.tixno, t.issue_dttm, t.location
            
              from adh_uwm ts 
             
             left join tickets t 
              on t.hour = ts.hour
              and t.issuedt = ts.day
              and t.issue_dttm between ts.hour_start and ts.hour_end
              limit 100
             ") %>% 
  select(hour_start, tickets, whole_address, tixno, issue_dttm, location) %>% head









get_Nearest_address <- function(distance_matrix) 
{
  # About
  #   get_Nearest_address() finds the closest address to i given a set of i, j1, j2, where 
  #   i is an address and jn is an address around it. Addresses with tickets in the last T 
  #   hours are fed to this function. (typically)
  #
  # args: 
  #   distance_matrix. could be either a data frame or matrix object with Assumes the first 
  #   column of distance_matrix is the whole address. Each subsequent column is one of these 
  #   addresses, and the values are the distance in miles between it and row address.
  #
  # Notes: 
  #   
  
  # create data frame 
  address_lookup <- data.frame(address = NA, 
                                   nearest_address = NA, 
                                   miles_away = NA)
  
  for (i in 1:nrow(dist_mat)) {
      
      # find the nearest address  
      match(min(dist_df[dist_df[ , i+1] != 0, i+1]), 
            dist_df[ , i+1]) -> row_id_of_nearest_address
      
      # then store all the info about it
      address_lookup[i, 1] <- dist_df[i,1]
      address_lookup[i, 2] <- dist_df[row_id_of_nearest_address, 1]
      address_lookup[i, 3] <- dist_df[row_id_of_nearest_address, i+1]
      
      
      # print status report. Uncomment to see how this runs in real time. 
      # print(
      #   paste("The closest address to", address_lookup[i,1], 
      #             "is", address_lookup[i,2], "at", round(address_lookup[i, 3], 3),
      #             "miles away (", round(address_lookup[i, 3]*5280), "ft)")
      #   )
      
    } # end for
  
  return(address_lookup)
  
} # end function









