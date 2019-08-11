#Test function after client uploads their spot

library(googleway)
library(data.table)

rm(list = ls())

lat <- 54.8808546373476
lon <- -3.09306037239926

wc_fnc <- function(lat,lon){

output_wc <- fread("ACTUAL.csv")

#Distance from road

lonlatvec <- paste(lon, lat, sep = ",")
url <- paste("http://router.project-osrm.org/nearest/v1/driving/", lonlatvec, "?number=1", sep = "")
temp <- jsonlite::fromJSON(url)
nearest_road <- temp$waypoints$distance / 1000
rm(lonlatvec, url, temp)

#Nearest to pub
key <- "AIzaSyBDJ0zvWo4HjanSao_8wqd18Nrr4qLoE_Y"

gpp <- google_places(location = c(lat,lon),
                     search_string = "pubs",
                     radius = 5000,
                     key = key)
gpp <- gpp$results
gpp$geometry$location$Dist <- NA
for(i in 1:nrow(gpp$geometry)){
  gpp$geometry$location$Dist[i] <- distHaversine(c(gpp$geometry$location$lng[i],gpp$geometry$location$lat[i]),c(lon, lat)) 
}

pub_name <- gpp$name[which(gpp$geometry$location$Dist == min(gpp$geometry$location$Dist))]
pub_postcode <- gpp$formatted_address[which(gpp$geometry$location$Dist == min(gpp$geometry$location$Dist))]
pub_dist <- gpp$geometry$location$Dist[which(gpp$geometry$location$Dist == min(gpp$geometry$location$Dist))] / 1000
pub_rating <- gpp$rating[which(gpp$geometry$location$Dist == min(gpp$geometry$location$Dist))]
rm(i,gpp)

#Photo Popularity 
api.key <- "21b8e31463a001c649a03438497e7e3b"
secret <- "9e2aef3abcd7ac5c"
userid <- "99792943"

url <- paste("https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=21b8e31463a001c649a03438497e7e3b&lat=",
               lat,"&lon=",lon,"&radius=0.5&format=json&nojsoncallback=1", sep = "")
temp <- jsonlite::fromJSON(url)
photo_pop <- nrow(as.data.table(temp$photos$photo))
rm(api.key,secret,userid,temp, url)

#Highest elevation
ele <- google_elevation(data.frame(lat = lat,
                                   lon = lon),key = key)
elevation <- ele$results$elevation
rm(ele)

#Nearest to someone else
output_wc$Dist <- NA
for(i in 1:nrow(output_wc)){
  output_wc$Dist[i] <- distHaversine(c(output_wc$lon[i],output_wc$lat[i]),c(lon, lat)) 
}

nearest_wc_distance <- output_wc$Dist[which(output_wc$Dist == min(output_wc$Dist))]
nearest_wc_lat <-  output_wc$lat[which(output_wc$Dist == min(output_wc$Dist))]
nearest_wc_lon <-  output_wc$lon[which(output_wc$Dist == min(output_wc$Dist))]

print(paste("Your Wildcamping location (",round(lat,4),", ",round(lon,4),
            ") is ", round(nearest_road,2), "km from the nearest road, ",
            round(pub_dist, 2), "km from the nearest pub (", pub_name, 
            "). It has had ", photo_pop, " picture(s) taken there and is at an elevation of ",
            round(elevation), "m", sep = ""))
}

wc_fnc(54.489112,-2.897755)

