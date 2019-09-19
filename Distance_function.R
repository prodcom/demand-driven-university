##########----------------------------------------------------------------------------------------------------##########
#         Function to find the distance of the closest destination from each origin
##########----------------------------------------------------------------------------------------------------##########


distance <- function(origin_postcodes, destination_coordinates, csv_name, API_key){

  Origin <- na.omit(unique(origin_postcodes))
  Origin <- ifelse(nchar(Origin) <4,paste0("0",Origin),Origin) # put a zero at the start of postcode if less than 4 digits Google maps won't recoginise it as a postcode otherwise
  
  
  if (file.exists(csv_name)) { Origin_Coord <- read_csv(csv_name, col_names = TRUE) } else {
      Origin_Coord <- geocode(paste(Origin,"Australia"), key = API_key)
      Origin_Coord <- cbind(Origin,Origin_Coord)
      colnames(Origin_Coord)[1] <- "Origin"
      write_csv(Origin_Coord, "University_locations_Coord.csv") } 
  
  colnames(Origin_Coord)[1] <- "Origin"
  Origin <- unique(Origin_Coord)
  
  Destination_locations <- unlist(lapply(destination_coordinates[,1], as.character)) 
  Distance <- expand.grid(as.character(Origin$Origin),Destination_locations) # get all combinations of origin & destination
  colnames(Distance)[1] <- "Origin_location"
  colnames(Distance)[2] <- "Destination_location"
  
  Distance$Origin_lon <- with(Origin,lon[match(Distance$Origin_location,Origin)])  
  Distance$Origin_lat <- with(Origin,lat[match(Distance$Origin_location,Origin)])  
  
  colnames(destination_coordinates)[1] <- "Destination_location"
  Distance$Destination_lon <- with(destination_coordinates,lon[match(Distance$Destination_location,Destination_location)])  
  Distance$Destination_lat <- with(destination_coordinates,lat[match(Distance$Destination_location,Destination_location)])  
  
  Destination_lon_lat <- cbind(Distance$Destination_lon,Distance$Destination_lat)
  Origin_lon_lat  <- cbind(Distance$Origin_lon,Distance$Origin_lat)
  
  Distance$Distance <- distGeo(matrix(c(Distance$Origin_lon, Distance$Origin_lat), ncol = 2),
                                    matrix(c(Distance$Destination_lon, Distance$Destination_lat), ncol = 2))
  
 # Distance_pivot <- cast(Distance[,c("Origin_location","Destination_location","Distance")], Origin_location ~ Destination_location)
  
  Distance_pivot <- Distance %>% select(Origin_location, Destination_location, Distance) %>% spread(key = Destination_location, value = Distance)
  
  Distance$Closest_destination <- apply(Distance_pivot %>% select(-Origin_location), 1, FUN=min,na.rm = TRUE) 
  Distance$Closest_destination <- sapply(Distance$Closest_destination,  function(x) {ifelse(x == "Inf", NA, x)}) # Replace Inf with NA
  Distance$Origin_location <- as.numeric(as.character(Distance$Origin_location))
  
  Closest_destination <- as.numeric(with(Distance,Closest_destination[match(origin_postcodes,Origin_location)]) ) 
  return(Closest_destination)
  }
  