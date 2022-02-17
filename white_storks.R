# white storks stopping sites during migration & analysing their spectral characteristics

# research question: why do storks migrate to spain and not to africa anymorer in the last years?

library(move)
# logging in 
login <- movebankLogin(user= "SvenjaDobe", password = getPass::getPass())


# getting study meta data: 

study.west <- searchMovebankStudies("LifeTrack White Stork SW Germany", login = login)


storks.meta.W <- getMovebankStudy(study.west[1],login = login)
storks.indis.W <-  getMovebankAnimals(study.west[1], login= login)

# download study data: 
## all individuals: moveStack
#storks.data <- getMovebankData(study.west[1],login = login)
#storks.data.ul <- unlist(storks.data)
#plot(study.data.ul)


## specific individuals: redrunner, chompy, sierit

storks <- getMovebankData(study.west[1],animalName = c(storks.indis.W[storks.indis.W$tag_local_identifier == 3339, "local_identifier"],storks.indis.W[storks.indis.W$tag_local_identifier == 2561, "local_identifier"], (storks.indis.W[storks.indis.W$tag_local_identifier == 3647, "local_identifier"])),login = login, removeDuplicatedTimestamps=TRUE)

redrunner <- getMovebankData(study.west[1],animalName = storks.indis.W[storks.indis.W$tag_local_identifier == 3339, "local_identifier"],login = login, removeDuplicatedTimestamps=TRUE)
chompy <- getMovebankData(study.west[1],animalName = storks.indis.W[storks.indis.W$tag_local_identifier == 3647, "local_identifier"],login = login, removeDuplicatedTimestamps=TRUE)
sierit <- getMovebankData(study.west[1],animalName = storks.indis.W[storks.indis.W$tag_local_identifier == 2561, "local_identifier"],login = login, removeDuplicatedTimestamps=TRUE)

## subset to year 2014
storks <- storks[storks$timestamp >= "2014-06-03 12:15:47 UTC" & storks$timestamp <= "2015-06-03 12:15:47 UTC"]
#storks <- storks[storks$timestamp <= "2014-06-03 12:15:47 UTC"]

plot(storks)


# convert to data frame with sf

library(sf)
storks.df <- as.data.frame(storks)
storks.sf <- st_as_sf(storks.df,coords = c("location_lat", "location_long"), crs= st_crs(storks))

library(ggplot2)
ggplot(storks.sf)+
  geom_sf(aes(color=tag_id))

#mapview::mapview(storks.sf)


# coords, timestamps and lags:
storks.coords <-  coordinates(storks)
storks.times <- timestamps(storks)
storks.lags <- timeLag(storks, units="mins")

#

library(circular)
storks.speed <- speed(storks) # m/s
storks.speed.na <- append(NA,storks.speed)

storks.heading <- angle(storks)
storks.heading.na <- append(NA,storks.heading)

storks.angle <- turnAngleGc(storks)
storks.angle.na <- append(NA,storks.angle)
storks.angle.na <- append(storks.angle.na,NA)

storks.azimuth <- as.circular(storks)
storks.azimuth.na <- append(NA,storks.azimuth)

plot(storks.speed,storks.heading)
windrose(storks.speed,storks.heading)
plot(storks.times,storks.heading.na)

plot(storks.speed.na,storks.angle.na)
windrose(storks.angle.na,storks.speed.na)

plot(storks.speed.na,storks.azimuth)



### 1. fancy moveVIS plot:

library(moveVis)
library(raster)



move_data <- align_move(storks, res = 1, unit = "days")
plot(move_data)

frames <- frames_spatial(move_data, path_colours = c("green","red","blue"),
                         map_service = "osm", map_type = "watercolor", alpha = 0.5, r_times=storks.times)%>%
  add_labels(x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow() %>%
  add_scalebar() %>%
  add_timestamps(move_data, type = "label") %>%
  add_progress()

length(frames) # number of frames
frames[[1]] 
frames[[365]]




#You may recognize that the map seems to be distorted which becomes mainly visible when looking at the map labels.
#By default, moveVis calculates an equidistant map extent (squared), with y and x axis measuring equal surface distances. 
#In addition, moveVis reprojects the default base maps to the projection of your movement data. 
#The default base maps are originally projected as WGS 84 / Pseudo Mercator (epsg:3857), also referred to as Web Mercator, 
#and thus are squeezed or stretched onto the projection grid of the movement data (in this case WGS 84 / LatLon).

#To represent the base map in its original projection, just reproject your movement data to the WGS 84 / Pseudo Mercator projection and disable the calculation of an equidistant extent:
  
#  move_data <- sp::spTransform(move_data, crs("+init=epsg:3857"))
#frames <- frames_spatial(move_data, path_colours = c("red","green","blue","orange"),
                         #map_service = "osm", map_type = "streets", map_res = 0.8, equidistant = F)
#frames[[100]] # display one of the frames

#Finally, animate the newly created frames:
  
animate_frames(frames, out_file = "storks2.gif", width = 700, height = 500, res = 80)


# finding stopping sites - Segmentation: 

## Azimuth:
## only for redrunner:

redrunner.speed <- speed(redrunner)
redrunner.heading <- angle(redrunner)
plot(redrunner.heading,redrunner.speed)

redrunner.azimuth <- as.circular(redrunner.heading, rotation="clock", units="degrees", type="angles",mundolo = "asis", zwro = 0, template = "geographic")
circular::windrose(redrunner.azimuth,redrunner.speed)
plot(redrunner.azimuth,redrunner.speed)

##only for chompy:
chompy.speed <- speed(chompy)
chompy.heading <- angle(chompy)
plot(chompy.heading,chompy.speed)

chompy.azimuth <- as.circular(chompy.heading, rotation="clock", units="degrees", type="angles",mundolo = "asis", zwro = 0, template = "geographic")
plot(chompy.azimuth,chompy.speed)
circular::windrose(chompy.azimuth,chompy.speed)


##only for sierit:
sierit.speed <- speed(sierit)
sierit.heading <- angle(sierit)
plot(sierit.heading,sierit.speed)

sierit.azimuth <- as.circular(sierit.heading, rotation="clock", units="degrees", type="angles",mundolo = "asis", zwro = 0, template = "geographic")
plot(sierit.azimuth,sierit.speed)
circular::windrose(sierit.azimuth,sierit.speed)


## for all of them: 
storks.ul <- unlist(storks)


library(circular)
storks.speed <- speed(storks) # m/s
storks.speed <- unlist(storks.speed)

storks.heading <- angle(storks)
storks.heading <- unlist(storks.heading)

plot(storks.heading,storks.speed)


storks.azimuth <- as.circular(storks.heading, rotation="clock", units="degrees", type="angles",mundolo = "asis", zwro = 0, template = "geographic")
circular::windrose(storks.azimuth,storks.speed)

storks.azimuth.ul <- unlist(storks.azimuth)
plot(storks.azimuth.ul,storks.speed)

length(storks.speed.ul)
length(storks.azimuth)
length(storks.heading.ul)


## penalizsed contrast: trying to minimize contrasts between segments -> laviell()
## BCPA: Behavioral Change Point Analysis: sliding window over persistence measure (autocorrelation)
## Net Square Dispalcement (NSD): distance from starting point, correlating with speed and turning angle

storks.nsd <- sp::spDistsN1(storks,storks[1,])
plot(storks.nsd)

ggplot(data.frame(storks.nsd,storks$timestamp))+
  geom_point(aes(x=storks.timestamp,y=storks.nsd))

chompy.nsd <- sp::spDistsN1(chompy,chompy[1,])
plot(chompy$timestamp,chompy.nsd)

ggplot(data.frame(chompy.nsd,chompy$timestamp))+
  geom_point(aes(x=chompy.timestamp,y=chompy.nsd),color="green")+
  ggtitle("NSD Chompy")+
  xlab("")


redrunner.nsd <- sp::spDistsN1(redrunner,redrunner[1,])
                               
ggplot(data.frame(redrunner.nsd,redrunner$timestamp))+
  geom_point(aes(x=redrunner.timestamp,y=redrunner.nsd),color="red")+
  ggtitle("NSD Redrunner")+
  xlab("")

sierit.nsd <- sp::spDistsN1(sierit,sierit[1,])
ggplot(data.frame(sierit.nsd,sierit$timestamp))+
  geom_point(aes(x=sierit.timestamp,y=sierit.nsd),color="blue")+
  ggtitle("NSD Sierit")+
  xlab("")

