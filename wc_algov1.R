#Algo (scale distance to watebody, river, road etc)
library(rgdal) 
library(plotKML)
library(data.table)
library(jsonlite)
library(geosphere)
library(ggmap)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(rgeos)
library(maptools)

#Do for each np in UK 
#Create box, randomly select in between
setwd("Documents/wcai/")
#LE 50.0657, -5.7125
#CP 53.973341, -2.531102
rm(list = ls())
#set.seed(101)

#s.lat <- 54.218558
#n.lat <- 54.757797
#w.lon <- -2.590859
#e.lon <- -1.728227

# #LD
# s.lat <- 54.218558
# n.lat <- 54.757797
# w.lon <- -3.348916
# e.lon <- -2.590859
#output_wc <- NULL
data(wrld_simpl)
output_wc <- fread("ACTUAL.csv")
for(ta in 1:200){
  #md <- cbind(lat = rnorm(75) + 53.97, lon = rnorm(75) + -2.53)
  #md <- cbind(lat = runif(45,-2,2) + 53.97, lon = runif(45,-2,2) + -2.53)
  md <- cbind(lat = runif(45,-2,2) + 52, lon = runif(45,-2,2) + -2.9)
  md <- as.data.table(md)
  #op <- NULL
  
  pts <- SpatialPoints(md[,c("lon", "lat")], proj4string=CRS(proj4string(wrld_simpl)))
  
  ## Find which points fall over land
  ii <- !is.na(over(pts, wrld_simpl)$FIPS)
  md <- md[ii == TRUE]
  rm(ii,pts)
  
  #for(i in 1:nrow(md)){
  #  url <- paste("https://api.onwater.io/api/v1/results/", md$lat[i], ",", md$lon[i], "?access_token=NN2ViyKB8KQkzHH_dJMe", sep = "")
  #  temp <- jsonlite::fromJSON(url)
  #  if(temp$water == FALSE){
  #    op <- rbind(op,md[i,])
  #  }
  #  Sys.sleep(4)
  #}
  
  #md <- op
  #rm(op, url, temp, i)
  
  #md <- NULL
  #for(i in 1:5000){
  #  md$lat[i] <- runif(1,s.lat,n.lat)
  #  md$lon[i] <- runif(1,w.lon,e.lon)
  #}
  
  plot(md$lon,md$lat)
  
  #Remove data that is within 100meters
  x <- 0
  for(i in 1:nrow(md)){
    x <- x + 1
    a <- md[-x,]
    b <- md[x,]
    dist <- distHaversine(b[,c("lon", "lat")],a[,c("lon", "lat")]) 
    if(any(dist < 100)){
      md <- a
      x <- x - 1
    }
  }
  
  plot(md$lon,md$lat)
  
  colnames(md)[2] <- "lon"
  colnames(md)[1] <- "lat"
  
  md_select <- md
  
  md_select$ID <- paste(md_select$lon, md_select$lat, sep = ",")
  
  #rm(a,b,dist,e.lon,i,x,w.lon,n.lat,s.lat)
  #################
  #Start API calls#
  #################
  
  print(paste("Analysing ", nrow(md_select), " potential wildcamping spots...", sep =""))
  md_select$NEAREST_RD <- NA
  for(i in 1:nrow(md_select)){
    Sys.sleep(4)
    latlonvec <- md_select$ID[i]
    url <- paste("http://router.project-osrm.org/nearest/v1/driving/", latlonvec, "?number=1", sep = "")
    temp <- jsonlite::fromJSON(url)
    if(nrow(temp$waypoints) > 0){
      md_select$NEAREST_RD[i] <- temp$waypoints$distance
    }
  }
  
  rm(url,temp,i, latlonvec)
  md_select <- na.omit(md_select)
  print("Distance to road...")
  quantile(md_select$NEAREST_RD)
  md_select_nr <- subset(md_select, NEAREST_RD < quantile(md_select$NEAREST_RD)[2])
  md_select <- md_select[order(-NEAREST_RD),]
  
  # if(quantile(md_select$NEAREST_RD)[4] < 500){
  # md_select <- subset(md_select, NEAREST_RD > quantile(md_select$NEAREST_RD)[4])
  # } else {
  md_select <- subset(md_select, NEAREST_RD > 500)
  #   md_select <- head(md_select, n = 800)
  # }
  
  print(paste(nrow(md_select), " potential sites", sep = ""))
  
  #Get elevation of a 4x4m boundary box
  r <- 0.0005
  df <- r/69
  dl <- df / cos(md_select$lat)
  md_select$slat <- md_select$lat - df 
  md_select$nlat <- md_select$lat + df 
  md_select$wlon <- md_select$lon - dl 
  md_select$elon <- md_select$lon + dl 
  rm(r,df,dl)
  md_select$OVERALL_ELEVATION <- NA
  for(i in 1:nrow(md_select)){
    Sys.sleep(2)
    longlatvec <- (paste(md_select$lat[i],",",md_select$lon[i], "|",
                         md_select$slat[i],",",md_select$elon[i], "|",
                         md_select$slat[i],",",md_select$wlon[i], "|",
                         md_select$nlat[i],",",md_select$elon[i], "|",
                         md_select$nlat[i],",",md_select$wlon[i], sep =""))
    
    url <- (paste("https://api.open-elevation.com/api/v1/lookup?locations=",longlatvec, sep = ""))
    temp <- jsonlite::fromJSON(url)
    md_select$OVERALL_ELEVATION[i] <- var(temp$results$elevation)
  }
  rm(temp,url,i,longlatvec)
  md_select <- subset(md_select, OVERALL_ELEVATION == 0)
  print(paste(nrow(md_select), " potential sites", sep = ""))
  
  #Get natural features
  #print(paste("Analysing ", nrow(md_select), " natural features...", sep = ""))
  # md_select$NEARBY_NAME <- "NONE"
  # for(i in 1:nrow(md_select)){
  #   url <- paste("https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=",md_select$lat[i],
  #                ",",md_select$lon[i],"&radius=500&types=natural_feature",
  #                "&key=AIzaSyDtdEB-R-YwkUHN9FNxE4JjfUBlz6S99ik", sep = "")
  #   temp <- jsonlite::fromJSON(url)
  #   if(temp$status != "ZERO_RESULTS"){
  #     md_select$NEARBY_NAME[i] <- temp$results$name
  #   }
  # }
  # 
  # print(table(md_select$NEARBY_NAME))
  # rm(temp,url,i)
  # 
  # md_select$NEARBY_FLAG <- ifelse(md_select$NEARBY_NAME != "NONE",1,0)
  
  print(paste(nrow(md_select), " potential sites", sep = ""))
  #print(paste(nrow(subset(md_select,NEARBY_FLAG == 1)), " potential sites of interest", sep = ""))
  
  #Calculate proximity to sea
  # WGS84 long/lat
  wgs.84    <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  # ESRI:54009 world mollweide projection, units = meters
  # see http://www.spatialreference.org/ref/esri/54009/
  mollweide <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  sp.points <- SpatialPoints(md_select[,c("lon","lat")], proj4string=CRS(wgs.84))
  coast <- readOGR("ne_10m_coastline/ne_10m_coastline.shp")
  
  #coast  <- readOGR(dsn=".",layer=file.choose(),p4s=wgs.84)
  coast.moll <- spTransform(coast,CRS(mollweide))
  point.moll <- spTransform(sp.points,CRS(mollweide))
  
  test <- 1:length(sp.points)  # random sample of ten points
  md_select$PROXIMITY_TO_SEA <- sapply(test,function(i)gDistance(point.moll[i],coast.moll)) / 1000   # distance in km
  rm(test,coast,coast.moll,point.moll, mollweide,sp.points,wgs.84)
  
  #Calculate proximity to lakes
  # WGS84 long/lat
  wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  # ESRI:54009 world mollweide projection, units = meters
  # see http://www.spatialreference.org/ref/esri/54009/
  mollweide <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  sp.points <- SpatialPoints(md_select[,c("lon","lat")], proj4string=CRS(wgs.84))
  coast <- readOGR("ne_10m_lakes_europe/ne_10m_lakes_europe.shp")
  
  #coast  <- readOGR(dsn=".",layer=file.choose(),p4s=wgs.84)
  coast.moll <- spTransform(coast,CRS(mollweide))
  point.moll <- spTransform(sp.points,CRS(mollweide))
  
  test <- 1:length(sp.points)  # random sample of ten points
  md_select$PROXIMITY_TO_LAKE <- sapply(test,function(i)gDistance(point.moll[i],coast.moll)) / 1000   # distance in km
  rm(test,coast,coast.moll,point.moll, mollweide,sp.points,wgs.84)
  
  #Calculate proximity to rivers
  # WGS84 long/lat
  wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  # ESRI:54009 world mollweide projection, units = meters
  # see http://www.spatialreference.org/ref/esri/54009/
  mollweide <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  sp.points <- SpatialPoints(md_select[,c("lon","lat")], proj4string=CRS(wgs.84))
  coast <- readOGR("ne_10m_rivers_europe/ne_10m_rivers_europe.shp")
  
  #coast  <- readOGR(dsn=".",layer=file.choose(),p4s=wgs.84)
  coast.moll <- spTransform(coast,CRS(mollweide))
  point.moll <- spTransform(sp.points,CRS(mollweide))
  
  test <- 1:length(sp.points)  # random sample of ten points
  md_select$PROXIMITY_TO_RIVER <- sapply(test,function(i)gDistance(point.moll[i],coast.moll)) / 1000   # distance in km
  rm(test,coast,coast.moll,point.moll, mollweide,sp.points,wgs.84)
  
  #Save progress
  write.csv(md_select, "temp.csv")
  
  #Run Flikr
  api.key <- "21b8e31463a001c649a03438497e7e3b"
  secret <- "9e2aef3abcd7ac5c"
  userid <- "99792943"
  
  #Define radius (in km)
  r <- 0.5
  
  #Flikr search - could potentially do specific tags. Analysis required
  md_temp <- md_select
  md_select$PHOTO_POPULARITY <- 0
  for(i in 1:nrow(md_select)){
    url <- paste("https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=21b8e31463a001c649a03438497e7e3b&lat=",
                 md_select$lat[i],"&lon=",md_select$lon[i],"&radius=",r,"&format=json&nojsoncallback=1", sep = "")
    temp <- jsonlite::fromJSON(url)
    temp <- as.data.table(temp$photos$photo)
    Sys.sleep(2)
    if(nrow(temp) > 0){
      temp$ID <- paste(temp$owner,temp$title)
      temp <- temp[!duplicated(temp$ID), ]
      md_select$PHOTO_POPULARITY[i] <- nrow(temp)
    }
  }
  rm(api.key,i,r,secret,temp,url,userid)
  print(paste(nrow(subset(md_select, PHOTO_POPULARITY > 0)), " popular locations.."), sep = "")
  
  #View calculator
  #Get elevation of a 200x200m boundary box
  r <- 0.04
  df <- r/69
  dl <- df / cos(md_select$lat)
  md_select$slat <- md_select$lat - df 
  md_select$nlat <- md_select$lat + df 
  md_select$wlon <- md_select$lon - dl 
  md_select$elon <- md_select$lon + dl 
  rm(r,df,dl)
  md_select$OVERALL_ELEVATION <- NA
  for(i in 1:nrow(md_select)){
    Sys.sleep(5)
    longlatvec <- (paste(md_select$lat[i],",",md_select$lon[i], "|",
                         md_select$slat[i],",",md_select$elon[i], "|",
                         md_select$slat[i],",",md_select$wlon[i], "|",
                         md_select$nlat[i],",",md_select$elon[i], "|",
                         md_select$nlat[i],",",md_select$wlon[i], sep =""))
    
    url <- paste("https://api.open-elevation.com/api/v1/lookup?locations=",longlatvec, sep = "")
    temp <- jsonlite::fromJSON(url)
    md_select$OVERALL_ELEVATION[i] <- temp$results$elevation[1]
    md_select$ELEVATION_100_SE[i] <- temp$results$elevation[2]
    md_select$ELEVATION_100_SW[i] <- temp$results$elevation[3]
    md_select$ELEVATION_100_NE[i] <- temp$results$elevation[4]
    md_select$ELEVATION_100_NW[i] <- temp$results$elevation[5]
  }
  rm(temp,url,i,longlatvec)
  
  #Save progress
  write.csv(md_select, "temp.csv")
  
  #Calculate close view
  md_select$ELEVATION_100_SE <- ifelse(md_select$ELEVATION_100_SE < 0,0, md_select$ELEVATION_100_SE)
  md_select$ELEVATION_100_SW <- ifelse(md_select$ELEVATION_100_SW < 0,0, md_select$ELEVATION_100_SW)
  md_select$ELEVATION_100_NE <- ifelse(md_select$ELEVATION_100_NE < 0,0, md_select$ELEVATION_100_NE)
  md_select$ELEVATION_100_NW <- ifelse(md_select$ELEVATION_100_NW < 0,0, md_select$ELEVATION_100_NW)
  
  md_select$EAST_SIDE_SHIELDED <- ifelse(md_select$OVERALL_ELEVATION < md_select$ELEVATION_100_SE &
                                           md_select$OVERALL_ELEVATION > md_select$ELEVATION_100_SW &
                                           md_select$OVERALL_ELEVATION < md_select$ELEVATION_100_NE &
                                           md_select$OVERALL_ELEVATION > md_select$ELEVATION_100_NW,1,0)
  md_select$WEST_SIDE_SHIELDED <- ifelse(md_select$OVERALL_ELEVATION > md_select$ELEVATION_100_SE &
                                           md_select$OVERALL_ELEVATION < md_select$ELEVATION_100_SW &
                                           md_select$OVERALL_ELEVATION > md_select$ELEVATION_100_NE &
                                           md_select$OVERALL_ELEVATION < md_select$ELEVATION_100_NW,1,0)
  md_select$SOUTH_SIDE_SHIELDED <- ifelse(md_select$OVERALL_ELEVATION < md_select$ELEVATION_100_SE &
                                            md_select$OVERALL_ELEVATION < md_select$ELEVATION_100_SW &
                                            md_select$OVERALL_ELEVATION > md_select$ELEVATION_100_NE &
                                            md_select$OVERALL_ELEVATION > md_select$ELEVATION_100_NW,1,0)
  md_select$NORTH_SIDE_SHIELDED <- ifelse(md_select$OVERALL_ELEVATION > md_select$ELEVATION_100_SE &
                                            md_select$OVERALL_ELEVATION > md_select$ELEVATION_100_SW &
                                            md_select$OVERALL_ELEVATION < md_select$ELEVATION_100_NE &
                                            md_select$OVERALL_ELEVATION < md_select$ELEVATION_100_NW,1,0)
  md_select$SOUTH_GRADIENT <- md_select$OVERALL_ELEVATION - rowMeans(md_select[,c("ELEVATION_100_SE", "ELEVATION_100_SW")])
  md_select$NORTH_GRADIENT <- md_select$OVERALL_ELEVATION - rowMeans(md_select[,c("ELEVATION_100_NE", "ELEVATION_100_NW")])
  md_select$EAST_GRADIENT <- md_select$OVERALL_ELEVATION - rowMeans(md_select[,c("ELEVATION_100_NE", "ELEVATION_100_SE")])
  md_select$WEST_GRADIENT <- md_select$OVERALL_ELEVATION - rowMeans(md_select[,c("ELEVATION_100_NW", "ELEVATION_100_SW")])
  md_select$OVERALL_GRADIENT <- md_select$OVERALL_ELEVATION - rowMeans(md_select[,c("ELEVATION_100_SE", "ELEVATION_100_SW","ELEVATION_100_NE", "ELEVATION_100_NW")])
  
  md_select$OVERALL_HIGHEST <-
    ifelse(
      md_select$OVERALL_ELEVATION > md_select$ELEVATION_100_SE &
        md_select$OVERALL_ELEVATION > md_select$ELEVATION_100_SW &
        md_select$OVERALL_ELEVATION > md_select$ELEVATION_100_NE &
        md_select$OVERALL_ELEVATION > md_select$ELEVATION_100_NW,
      1,
      0
    )
  
  md_select$PROXIMITY_TO_SEA <-
    ifelse(md_select$PROXIMITY_TO_SEA < 500, 1, 0)
  md_select$PROXIMITY_TO_LAKE <-
    ifelse(md_select$PROXIMITY_TO_LAKE < 500, 1, 0)
  md_select$PROXIMITY_TO_RIVER <-
    ifelse(md_select$PROXIMITY_TO_RIVER < 500, 1, 0)
  
  md_select$WC_ALGO <-
    ((md_select$OVERALL_HIGHEST * 0.3) + 1) * (log(md_select$NEAREST_RD + 2)) *
    (log(md_select$PHOTO_POPULARITY + 2)) * (md_select$PROXIMITY_TO_SEA + 1) * (md_select$PROXIMITY_TO_LAKE + 1) * 
    (md_select$PROXIMITY_TO_RIVER + 1)
  
  md_select <- md_select[order(-WC_ALGO),]
  output_wc <- rbind(output_wc,md_select)
  #md_sub <- subset(md_select, select = c(lat, lon, WC_ALGO))
  print(ta)
  write.csv(output_wc, "ACTUAL.csv", row.names = F)
  plot(x = output_wc$lon, y = output_wc$lat)
  Sys.sleep(60*3)
}

#May want to scale
#md_select <- subset(md_select, select = c("lon", "lat", "WC_ALGO"))
#output_wc$WC_ALGO <- (output_wc$WC_ALGO - min(output_wc$WC_ALGO)) / (max(output_wc$WC_ALGO) - min(output_wc$WC_ALGO))

# m <- leaflet(output_wc) %>% 
#   addTiles() %>% 
#   addHeatmap(
#   lng = ~lon,
#   lat = ~lat,
#   intensity = ~WC_ALGO,
#   blur = 20,
#   max = 0.05,
#   radius = 5
# )
# m <- addControlGPS(m, options = gpsOptions(position = "topleft", activate = TRUE, 
#                                                autoCenter = TRUE, 
#                                                setView = TRUE))
# activateGPS(m)

#Dark Theme
output_wc$ID <- paste(round(output_wc$lat,6), round(output_wc$lon,6), sep = ", ")
m <- leaflet(output_wc) %>% 
  #addSearchOSM() %>%
  addTiles(group = "OSM") %>%
  #addTiles(group = "OSM",
  #         options = providerTileOptions(minZoom = 8, maxZoom = 20)) %>%
  addProviderTiles(providers$CartoDB.DarkMatter,group = "Dark Theme") %>% 
  
  addHeatmap(
    lng = ~lon,
    lat = ~lat,
    intensity = ~WC_ALGO,
    blur = 20,
    max = 0.05,
    radius = 10,
    group = "Heat Map"
  ) %>%
  addMarkers(
    lng = ~lon,
    lat = ~lat,
    popup = ~ID, group = "Normal Map", 
    clusterOptions = markerClusterOptions()) %>%
  addLayersControl(
    baseGroups = c("Dark Theme", "Open Street Map"), 
    overlayGroups = c("Heat Map", "Normal Map"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% hideGroup("Normal Map")
#m

#library(htmlwidgets)
saveWidget(m, file="indexv1.html")


#may be worth removing 0s for no photos
# add markers for best locations (circle when zoomed out)


####

#three camps
#24 geographical regions
#apart from LDN
#split regions into 4 (one control) equal spread
#apply simple reinforcement

#3rf models
#different epsilon

#no need for complex ml models
#allows you to test multiple campaigns at once
## add double function same as TF
rm(list = ls())
score <- c(-2,-1,0,1,2)

strat <- 1:5
timeline <- 1:5

op <- expand.grid(strat,timeline)
colnames(op)[1] <- "s"
colnames(op)[2] <- "t"
op$SCORE <- 0.5
op$IT <- 0
op <- as.data.table(op)

rl <- function(ep){
  n1 <- runif(1,0,1)
  op <- op[order(-SCORE),]
  if(n1 < (1-ep)){
    pick <- op[1,]
    outcome <- sample(score,1)
    op$SCORE[1] <- op$SCORE[1] + outcome
    op$IT[1] <- op$IT[1] + 1
  }  else {
    n2 <- round(runif(1,1,nrow(op)))
    outcome <- sample(score,1)
    op$SCORE[n2] <- op$SCORE[n2] + outcome
    op$IT[n2] <- op$IT[n2] + 1
  }
}

ep <- 0.95
for(i in 1:100000){
  n1 <- runif(1,0,1)
  op <- op[order(-SCORE),]
  if(n1 < (1-ep)){
    pick <- op[1,]
    outcome <- sample(score,1)
    op$SCORE[1] <- op$SCORE[1] + outcome
    op$IT[1] <- op$IT[1] + 1
  }  else {
    n2 <- round(runif(1,1,nrow(op)))
    outcome <- sample(score,1)
    op$SCORE[n2] <- op$SCORE[n2] + outcome
    op$IT[n2] <- op$IT[n2] + 1
  }
  if(i %% 2000 == 0 & ep > 0.05){
    ep <- ep - 0.01
  }
}

op$ID <- 1:nrow(op)
op$TS <- op$SCORE / op$IT

plot(x = op$ID, y = op$SCORE, type = "h")
plot(x = op$ID, y = op$IT, type = "h")
plot(x = op$ID, y = op$TS, type = "h")
