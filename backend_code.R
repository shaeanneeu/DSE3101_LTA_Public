library(readr)
library(httr)
library(geosphere)
library(jsonlite)
library(lubridate)
library(tidyr)
library(tidyverse)
library(readxl)
library(fuzzyjoin)

current_dir <- getwd()

tokens_dir <- file.path(current_dir, "Tokens")
datasets_dir <- file.path(current_dir, "Datasets")

api_key_filepath <- file.path(tokens_dir, "lta_api_token.csv")
api_key_file <- read_csv(api_key_filepath)
api_key <- api_key_file$AccountKey
headers <- c(AccountKey = api_key)

bus_stops_filepath <- file.path(datasets_dir, "bus_stops.csv")
bus_stops_file_exists <- file.exists(bus_stops_filepath)

### Getting bus stops information
if (!bus_stops_file_exists) {
  bus_stops_url <- "http://datamall2.mytransport.sg/ltaodataservice/BusStops"
  skip_number <- 0
  df <- data.frame() # Create an empty data frame to store the data

  while (TRUE) {
    url <- paste0(bus_stops_url, "?$skip=", skip_number)
    temp <- GET(url, add_headers(headers))
    res <- content(temp)

    temp_df <- do.call(rbind, res$value)
    temp_df <- as.data.frame(temp_df)
    if (nrow(temp_df) == 0) {
      break # Exit the loop if there's no more data
    }

    df <- rbind(df, temp_df) # Append the data to the main data frame
    skip_number <- skip_number + 500
  }

  flat_df <- unnest(df, cols = c(BusStopCode, RoadName, Description, Latitude, Longitude))
  write.csv(flat_df, file = bus_stops_filepath, row.names = FALSE) # Save the complete data frame as 'bus_stops.csv'
  print("File downloaded as bus_stops.csv")
} else {
  print("File exists")
}

bus_stops <- read_csv(bus_stops_filepath)
bus_stops

### Instagram Tokens

ig_token_key_filepath <- file.path(tokens_dir, "ig_token.csv")
ig_token_file_exists <- file.exists(ig_token_key_filepath)
client_secret_filepath <- file.path(tokens_dir, "ig_client_secret.csv")
client_secret_token_file <- read_csv(client_secret_filepath)
client_secret <- client_secret_token_file$SecretCode

if (!ig_token_file_exists) {
  ### Instagram API
  code <- "AQADhvoWdu482cDGfQocmNrSry7rh4sH7a0bhLojRoSawjZ0zB0eL5JnH-E-SpY5oWtDojARxK2s_QZmQwaagiABS9W4sO5gqZ1xZ3k1_hn5Jnlj3d4f1W2pEbl4cei8neDAzxJ48Df-uOswufuYxencxTbF61uJVL1z85-6UEYnbQ7Zw4Jk8-ZBhsQZMV7VEemGFTgTBWldLLCflAnpavXZz1DpNG_ZzrnENcmS93R0YA"

  params <- list(
    client_id = "1074040483264515", client_secret = client_secret, code = code,
    grant_type = "authorization_code", redirect_uri = "https://jiakaihoo.wixsite.com/pandacon"
  )

  ### Short_lived token
  request <- POST(url = "https://api.instagram.com/oauth/access_token", body = params)
  requestJSON <- content(request)
  short_lived_access_token <- requestJSON$access_token

  ### Long_lived token
  params <- list(
    access_token = short_lived_access_token, client_secret = client_secret,
    grant_type = "ig_exchange_token"
  )

  request <- GET(url = "https://graph.instagram.com/access_token", query = params)
  requestJSON <- content(request)
  long_lived_access_token <- requestJSON$access_token
  current_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  token_df <- data.frame(
    access_token = c(long_lived_access_token),
    time = current_time
  )
  ## Writing to csv file
  write.csv(token_df, ig_token_key_filepath, row.names = FALSE)
} else {
  print("File exists")
}

ig_token_file <- read_csv(ig_token_key_filepath)
ig_token <- ig_token_file$access_token

### Retrieval of locations
food_locations_file_name <- file.path(datasets_dir, "food_locations.csv")
food_locations_file_name_exists <- file.exists(food_locations_file_name)

if (!food_locations_file_name_exists) {
  params <- list(access_token = token, fields = "id,caption,media_url,permalink,timestamp")
  food_df <- data.frame()

  request <- GET(url = "https://graph.instagram.com/me/media", query = params)
  requestJSON <- content(request)
  temp_df <- do.call(rbind, lapply(requestJSON$data, as.data.frame))
  food_df <- rbind(food_df, temp_df)

  # params = list(access_token = token, fields = 'id,media_url,permalink,timestamp')
  # request <- GET(url = 'https://graph.instagram.com/17966518561288167/children', query = params)
  # requestJSON <- content(request)
  # temp_df = do.call(rbind, lapply(requestJSON$data, as.data.frame))
  # food_df = rbind(food_df, temp_df)

  ## Loops using the API [The next link is tagged in the JSON if it exists]
  while (TRUE) {
    if (!is.null(requestJSON$paging$`next`)) {
      print(requestJSON$paging$`next`)
      request <- GET(url = requestJSON$paging$`next`)
      requestJSON <- content(request)
      temp_df <- do.call(rbind, lapply(requestJSON$data, as.data.frame))

      food_df <- rbind(food_df, temp_df)
    } else {
      break
    }
  }

  ## Filter for only ratings >=7 and removes unnecessary food reviews
  above_7 <- sapply(food_df$caption, function(row) {
    ### Non-wanted cause of various reasons, i.e. cheap/closed/private dining
    phrases <- c(
      "Chagee", "Evviva", "Sakeya", "Chaffic", "\\{Closed\\}", "Heytea", "HEYTEA", "Fruce", "Whale Tea", "\\[Delivery\\]",
      "Pots and Pans", "Fragrance", "Fardello", "CHICHA"
    )
    match <- str_extract(row, ":\\s\\d")
    if (is.na(match)) {
      return(FALSE) # If ":" is not found
    } else {
      test_int <- substr(match, nchar(match), nchar(match))
      # Extract the substring after the first ":"
      # Check if the number is greater than 7
      if (test_int >= 7) {
        if (str_detect(row, paste(phrases, collapse = "|"))) {
          return(FALSE)
        } else {
          return(TRUE)
        }
      } else {
        return(FALSE)
      }
    }
  })

  ## Modifications of the dataframe to be stored so it can be merged with the rest of the attractions
  wanted_food_df <- food_df[above_7, ]
  wanted_food_df_destination <- sapply(wanted_food_df$caption, function(row) {
    destination <- str_extract(row, str_extract(row, "^[^[:\\[\\]]]+"))
    return(destination)
  })
  wanted_food_df$destination <- wanted_food_df_destination
  wanted_food_df$type <- "food"
  wanted_food_df$"latitude & longitude" <- NA
  wanted_food_df <- wanted_food_df[, c("destination", "type", "latitude & longitude", "permalink", "timestamp", "id")]

  # Save the result
  write.csv(wanted_food_df, food_locations_file_name, row.names = FALSE)
} else {
  print("File exists")
}

food_locations <- read_csv(food_locations_file_name)

## Manually add coordinates for location
food_locations_wcoords_filepath <- file.path(datasets_dir, "food_locations_wcoords.csv")
food_locations_mod <- read_csv(food_locations_wcoords_filepath)

## Total locations
tourist_locations_wcoords_filepath <- file.path(datasets_dir, "Singapore Tourist Attractions.csv")
tourist_locations <- read_csv(tourist_locations_wcoords_filepath)

## Bus Routes
bus_routes_filepath <- file.path(datasets_dir, "bus_routes.csv")
bus_routes_file_exists <- file.exists(bus_routes_filepath)

if (!bus_routes_file_exists) {
  bus_routes_url <- "http://datamall2.mytransport.sg/ltaodataservice/BusRoutes"
  skip_number <- 0
  df <- data.frame() # Create an empty data frame to store the data

  while (TRUE) {
    url <- paste0(bus_routes_url, "?$skip=", skip_number)
    temp <- GET(url, add_headers(headers))
    res <- content(temp)
    temp_df <- do.call(rbind, res$value)
    temp_df <- as.data.frame(temp_df)

    if (nrow(temp_df) == 0) {
      break # Exit the loop if there's no more data
    }

    df <- rbind(df, temp_df) # Append the data to the main data frame
    skip_number <- skip_number + 500
  }
  ## Add a flat_df function since the df are in lists of one to let them save into a csv
  flat_df <- unnest(df, cols = c(
    ServiceNo, Operator, Direction, StopSequence, BusStopCode, Distance, WD_FirstBus, WD_LastBus,
    SAT_FirstBus, SAT_LastBus, SUN_FirstBus, SUN_LastBus
  ))
  write.csv(flat_df, file = bus_routes_filepath, row.names = FALSE) # Save the complete data frame as 'bus_stops.csv'
} else {
  print("File exists")
}

bus_routes <- read_csv(bus_routes_filepath)
bus_routes

## local file to store the bus route of each bus
bus_routes_consolidated_filepath <- file.path(datasets_dir, "bus_stops_wroutes.csv")
bus_routes_consolidated_filepath_exists <- file.exists(bus_routes_consolidated_filepath)

if (!bus_routes_consolidated_filepath_exists) {
  consolidated_file <- bus_routes %>%
    left_join(bus_stops, by = "BusStopCode") %>%
    select(ServiceNo, Direction, StopSequence, BusStopCode, Description, Latitude, Longitude)

  write.csv(consolidated_file, file = "Bus_stops_wroutes.csv", row.names = FALSE)

  bus_stops_wroutes <- read_csv(bus_routes_consolidated_filepath)
} else {
  print("bus routes consolidated exists")
}

bus_stops_wroutes <- read_csv(bus_routes_consolidated_filepath)

## Google uses lat long
# Test datacase
MBS <- c(1.2839454495737048, 103.85937544472274)
busstop_near_MBS <- c(1.2806844615667559, 103.85507356421047)

## Distance Function
calculate_distance <- function(row, lat, long) {
  point <- as.numeric(c(row["Longitude"], row["Latitude"]))
  distance <- distVincentySphere(point, c(long, lat))
  return(distance)
}

## Finding nearest bus_stop in 500m
closest_bus_stop <- function(lat, long, bus_stops_wroutes) {
  temp <- as.data.frame(bus_stops_wroutes)
  temp$dist <- apply(temp, 1, function(row) calculate_distance(row, lat, long))
  ### Finds bus_stops in 500m radius
  # print(temp)
  ### Selecting closest bus stops for each bus
  wanted_bus_stops <- which(temp$dist <= 500)
  wanted <- temp[wanted_bus_stops, c("ServiceNo", "Direction", "BusStopCode", "Description", "dist")]
  ### Having only the closest distance busstop for each bus
  wanted <- wanted %>%
    group_by(ServiceNo, Direction) %>%
    slice_min(dist)

  return(wanted)
} # get the busstopcode and roadname with the index of shortest distance, and also tell the user how far he/she is from the nearest bus stop

# test closest bus stop function and calculate distance function with MBS bus stop
# closest_stops <- closest_bus_stop(test_coordinates[2], test_coordinates[1], bus_stops_wroutes)
# closest_stops

## Adding closest bus-stops to attractions to local file so it takes less time with api_calls later
tourist_locations_wproximalbusstops_filepath <- file.path(datasets_dir, "tourist_locations_wproximalbusstops.csv")
tourist_locations_wproximalbusstops_filepath_exists <- file.exists(tourist_locations_wproximalbusstops_filepath)

total_results_df <- data.frame(bus_stops = character())
if (!tourist_locations_wproximalbusstops_filepath_exists) {
  for (i in 1:nrow(tourist_locations)) {
    location <- tourist_locations[i, ]
    print(location)
    temp_df <- closest_bus_stop(as.numeric(location[4]), as.numeric(location[3]), bus_stops_wroutes)
    result_df <- data.frame(bus_stops = paste(unique(temp_df$BusStopCode), collapse = ","))
    total_results_df <- rbind(total_results_df, result_df)
  }
  tourist_locations$ProximalBusStops <- unlist(total_results_df)
  write.csv(tourist_locations, tourist_locations_wproximalbusstops_filepath, row.names = FALSE)
} else {
  print("tourist_locations_wproximalbusstops_name_exists")
}

tourist_locations_wproximalbusstops <- read_csv(tourist_locations_wproximalbusstops_filepath)

# Function to filter buses in closest_stops based on estimated arrival time
bus_approaching <- function(row, headers, time_threshold) {
  bus_arrival_url <- "http://datamall2.mytransport.sg/ltaodataservice/BusArrivalv2"
  temp <- GET(bus_arrival_url, add_headers(headers),
    query = list(BusStopCode = row["BusStopCode"], ServiceNo = row["ServiceNo"])
  )
  res <- content(temp)
  if (length(res$Services) == 0) {
    return(FALSE)
  }
  # print(paste("Next Bus Estimated Arrival:", res$Services[[1]]$NextBus$EstimatedArrival, ";", "Seats Availability:", res$Services[[1]]$NextBus$Load))
  # print(as.POSIXct(res$Services[[1]]$NextBus$EstimatedArrival, format = "%Y-%m-%dT%H:%M:%S"))
  if (as.POSIXct(res$Services[[1]]$NextBus$EstimatedArrival, format = "%Y-%m-%dT%H:%M:%S") - Sys.time() < as.difftime(time_threshold, units = "mins")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# temp_bus_approaching <- closest_stops
# temp_bus_approaching$approaching <- apply(temp_bus_approaching, 1, function(row) bus_approaching(row, headers, 5))
# temp_bus_approaching<- temp_bus_approaching[temp_bus_approaching$approaching == TRUE,]

# Getting bus_stops_visited
bus_stops_visited <- function(bus_stops_wroutes, ServiceNo, Direction, BusStopCode, time_alloted) {
  ## Testing variables
  # ServiceNo = 174
  # Direction = 1
  # BusStopCode = 21161
  # time_alloted <- as.difftime(50, units = "mins")
  
  # print(BusStopCode)
  # print(time_alloted)
  bus_stops_reached <- list()
  running_total <- 0
  running_totals <- list()
  origin_time <- NULL
  OriginBusStopCode <- BusStopCode
  
  ## Check if no BusStops pass
  if (is.na(BusStopCode)){
    return(data.frame())
  }

  
  bus_stops_sequence <- bus_stops_wroutes[bus_stops_wroutes$BusStopCode == BusStopCode &
    bus_stops_wroutes$ServiceNo == ServiceNo, ]$StopSequence
  bus_stops_check <- bus_stops_wroutes[bus_stops_wroutes$ServiceNo == ServiceNo &
    bus_stops_wroutes$Direction == Direction &
    bus_stops_wroutes$StopSequence >= bus_stops_sequence, ]

  bus_arrival_url <- "http://datamall2.mytransport.sg/ltaodataservice/BusArrivalv2"
  # df_test <- list()

  # print(bus_stops_check)
  # Iterate through each bus's ETA, see if any bus_stops cannot arrive on time
  for (BusStopCode in bus_stops_check$BusStopCode) {
    # print(ServiceNo)
    # print(running_total)

    if (running_total <= time_alloted) {
      temp <- GET(bus_arrival_url, add_headers(headers),
        query = list(BusStopCode = BusStopCode, ServiceNo = ServiceNo)
      )
      res <- content(temp)
      if (length(res$Services) == 0) {
        return(NA)
      }
      res <- res$Services[[1]]
      # df_test <- c(df_test, list(res))

      ## Gets first time
      if (is.null(origin_time)) {
        origin_time <- as.POSIXct(res$NextBus$EstimatedArrival, format = "%Y-%m-%dT%H:%M:%S")
        start_time <- origin_time
        bus_stops_reached <- rbind(bus_stops_reached, BusStopCode)
        running_totals <- rbind(running_totals, running_total)
        bus_load <- res$NextBus$Load
        # print(bus_load)
        # print(origin_time)
        next
      } else {
        time_diff <- difftime(as.POSIXct(res$NextBus$EstimatedArrival, format = "%Y-%m-%dT%H:%M:%S"), origin_time, units = "mins")
        # Check if the time difference is within the acceptable range
        if (time_diff >= as.difftime(30, units = "secs")) {
          # print(time_diff)
          origin_time <- as.POSIXct(res$NextBus$EstimatedArrival, format = "%Y-%m-%dT%H:%M:%S")
          running_total <- running_total + time_diff
        } else {
          time_diff <- difftime(as.POSIXct(res$NextBus2$EstimatedArrival, format = "%Y-%m-%dT%H:%M:%S"), origin_time, units = "mins")
          # print(time_diff,1)
          origin_time <- as.POSIXct(res$NextBus$EstimatedArrival, format = "%Y-%m-%dT%H:%M:%S")
          running_total <- running_total + time_diff
        }
        bus_stops_reached <- rbind(bus_stops_reached, BusStopCode)
        running_totals <- rbind(running_totals, running_total)
      }
    } else {
      break
    }
  }

  # print(bus_stops_reached)
  # print(running_totals)
  bus_stops_reached_wtime <- cbind(as.data.frame(bus_stops_reached), as.data.frame(running_totals))
  names(bus_stops_reached_wtime) <- c("BusStopCode", "ETA")

  bus_stops_reached_wtime <- bus_stops_reached_wtime %>%
    filter(ETA <= time_alloted)

  bus_stops_reached_wtime$ServiceNo <- ServiceNo
  bus_stops_reached_wtime$OriginBusStopCode <- OriginBusStopCode
  bus_stops_reached_wtime$BusStartTime <- start_time
  bus_stops_reached_wtime$Load <- bus_load

  return(bus_stops_reached_wtime)
}

# bus_stops_reached_wtime = cbind(as.data.frame(c('01619','03071','01529','02111')),as.data.frame(c(0,10,20,15)))
# names(bus_stops_reached_wtime) <- c('BusStopCode',"ETA")
#

# tourist_location_reached_mod <- data.frame()
# bus_stops_reached_all = bus_stops_reached_wtime

## Checking for tourist location matches

tourist_location_check <- function(tourist_locations_wproximalbusstops, bus_stops, bus_stops_reached_all) {
  tourist_location_reached_mod <- data.frame()
  for (i in 1:nrow(bus_stops_reached_all)) {
    bus_stop <- bus_stops_reached_all[i, ]
    matching_rows <- which(grepl(bus_stop[[1]], tourist_locations_wproximalbusstops$ProximalBusStops))
    tourist_location_reached <- tourist_locations_wproximalbusstops[matching_rows, ]
    tourist_location_reached$BusStopCode <- bus_stop[[1]]
    tourist_location_reached$ETA <- bus_stop[[2]]
    tourist_location_reached$ServiceNo <- bus_stop[[3]]
    tourist_location_reached$OriginBusStopCode <- bus_stop[[4]]
    tourist_location_reached$BusStartTime <- bus_stop[[5]]
    tourist_location_reached$Load <- bus_stop[[6]]
    tourist_location_reached_mod <- rbind(tourist_location_reached_mod, tourist_location_reached)
  }

  tourist_location_reached_mod$BusStopCode <- as.character(tourist_location_reached_mod$BusStopCode)
  tourist_location_reached_mod$ETA <- as.double(tourist_location_reached_mod$ETA)
  tourist_location_reached_mod$ServiceNo <- as.character(tourist_location_reached_mod$ServiceNo)
  tourist_location_reached_mod$OriginBusStopCode <- as.character(tourist_location_reached_mod$OriginBusStopCode)
  tourist_location_reached_mod$Load <- as.character(tourist_location_reached_mod$Load)

  # Bugtesting
  # return(tourist_location_reached_mod)
  # tourist_location_reached_mod = a

  tourist_location_reached_mod_ordered <- tourist_location_reached_mod %>%
    select(-ProximalBusStops) %>%
    inner_join(bus_stops, by = c(BusStopCode = "BusStopCode")) %>%
    select(-RoadName) %>%
    rename(
      DestinationLatitude = Latitude.x,
      DestinationLongitude = Longitude.x,
      DestinationBusStopCode = BusStopCode,
      DestinationBusStopName = Description,
      DestinationBusStopLatitude = Latitude.y,
      DestinationBusStopLongitude = Longitude.y
    ) %>%
    inner_join(bus_stops, by = c(OriginBusStopCode = "BusStopCode")) %>%
    select(-RoadName) %>%
    rename(
      OriginBusStopName = Description,
      OriginBusStopLatitude = Latitude,
      OriginBusStopLongitude = Longitude
    ) %>%
    mutate(BusEndTime = BusStartTime + as.difftime(ETA, unit = "mins")) %>%
    group_by(Destination) %>%
    mutate(ETA_min = min(BusEndTime)) %>%
    ungroup() %>%
    filter(ETA_min == BusEndTime) %>%
    select(-ETA_min) %>%
    mutate(TravelMode = ifelse(BusStartTime == BusEndTime, "Walk", "Bus")) %>%
    group_by(Destination) %>%
    slice(1)


  return(tourist_location_reached_mod_ordered)
}

wanted_attractions <- function(lat, long, wanted_types, time_interval) {
  ## Required data
  # api_key <- "3x+HF5oTQ0eDUnVeFjakIQ=="
  # headers <- c(AccountKey = api_key)
  # bus_stops_wroutes <- read_csv("bus_stops_wroutes.csv")
  # tourist_locations_wproximalbusstops <- read_csv("tourist_locations_wproximalbusstops.csv")
  bus_stops_reached_all <- data.frame()

  ## Function calls
  ## Getting closest stops to user
  closest_stops <- closest_bus_stop(lat, long, bus_stops_wroutes)

  ## Checking for bus in 5 minutes
  closest_stops$approaching <- apply(closest_stops, 1, function(row) bus_approaching(row, headers, 5))
  closest_stops <- closest_stops[closest_stops$approaching == TRUE, ]

  ## Checking what busstops have been reached
  for (i in 1:nrow(closest_stops)) {
    row <- closest_stops[i, ]
    bus_stops_reached <- bus_stops_visited(bus_stops_wroutes, row$ServiceNo, row$Direction, row$BusStopCode, 
                                           as.difftime(time_interval - 10,unit = "mins"))
    # print(bus_stops_reached)
    bus_stops_reached_all <- rbind(bus_stops_reached_all, bus_stops_reached)
  }
  # Filtering bus_stops that don't pass criteria
  bus_stops_reached_all <- bus_stops_reached_all %>%
    filter(if_any(everything(), ~ !is.na(.)))

  ## Checking which tourist_location is Ok
  all_attractions <- (tourist_location_check(tourist_locations_wproximalbusstops, bus_stops, bus_stops_reached_all))
  all_attractions_filtered <- all_attractions %>%
    filter(Type %in% wanted_types)

  return(all_attractions_filtered)
}

## Testrun
# test_1 <- c(1.3050779239689276, 103.83175851424681)
# test_2 = c(1.28, 103.8)
# a <- wanted_attractions(test_1[1], test_1[2],c("Food","Shopping","Heritage"),20)

# write_csv(a,"test_df.csv")
