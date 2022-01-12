#extracting all information for analysis
# Once all configurations of files have been run in ichthyop and produced .nc files of larval dispersal
# the following code extracts the relevant information from each .nc file 
# and stores that information as in a single csv file.

library(ncdf4)
library(lubridate)
library(lubridate)
library(ncdf4)

# NOTE: Duplicate runs can occur when using ichthyop so it is worth checking that all .nc file are unique
# before running this code.

################
#### STEP 1 ####
################

# Creating a file with the names of all .nc files produced by ichthyop
# Files should be stored in folder call output containing separate folders for all .nc for each year 

#Location of files
dirpath  <- "D:/Ocean models/output/"
#List of years simulated
Years <- list.files(dirpath)
#Blank list to fill with file names
filenames <- list()

#Loop that open up all folders and read file names inside them
for(i in 1:length(Years)){
  yearpath <- paste0(dirpath, Years[i], "/")
  monthpath <-paste0(yearpath,list.files(yearpath),"/")
  tmplist <- list()
  for(j in 1:length(monthpath)){
    tmplist[[j]] <- paste0(monthpath[j], list.files(monthpath[j]))
  }
  tempnames <- do.call("c", tmplist)
  filenames[[i]] <- tempnames
}

#Combining all extracted file names into a single vector
filenames <- do.call("c", filenames)

#List of dimension recordin the nc file
names(nc$dim)

t0 <- vector()
for(i in 1:length(filenames)){
  ncfile <- filenames[i]
  nc <- nc_open(ncfile)
  t0[i] <- as.numeric(ncvar_get(nc, varid = "time", start = c(1), count = c(1)))
  print(paste0(round(i/length(filenames)*100),"%"))
  nc_close(nc)
}
filenames <- filenames[order(t0)]
t0 <- sort(t0)
starttimes <- as_datetime(t0, origin = "1900-01-01 00:00:00 UTC")

write(x = filenames, "Backwards_fileorder.txt")
filenames <- readLines("fileorder.txt")
ncfile <- filenames[1]
nc_open(ncfile)

################
#### STEP 2 ####
################

# Extracting a subset of information contained within each .nc file and saving it in one data base.
# This data base is still likely to be 2-4Gb.


#reading in preparpared file names
#info <- read.table("Forwards_fileorder.txt",header = T,)
info <- read.table("fileorder.txt", header = T, sep = " ")
summary(info)

info$filenames <- as.character(info$filenames)
nc <- nc_open(info$filenames[1])
#Number of particles in simulation
length(nc$dim$drifter$vals)
# In case one wishes to consider only a subset of all time records
# Index of the first and last time records
firsttime <- 1
lasttime <- length(nc$dim$time$vals)
nbtime <- (lasttime - firsttime)+1

#Subset of the drifters
#Index of the first and last drifters
firstdrifter <- 1
lastdrifter <- 10000
nbdrifters <- (lastdrifter-firstdrifter)+1

tmpinfo <- list()
#Loop to extract all needed data
nc <- nc_open(info$filenames[1])
startdate <- as.character(info$startdate[1])
#Number of paricles tranported at d40
d40 <- ncvar_get(nc, "zone", start = c(2, firstdrifter, lasttime), count = c(1, nbdrifters, 1))+1
d40 <- sum(d40)
zones <- ncvar_get(nc, "zone", start = c(2, firstdrifter, firsttime), count = c(1, nbdrifters, nbtime))+1
#Number of paricles tranported at time time between d1 and d40
d1.40 <- ncvar_get(nc, "zone", start = c(2, firstdrifter, firsttime), count = c(1, nbdrifters, nbtime))+1
d1.40 <- max.col(d1.40, ties.method = "first")
d1.40 <- length(d1.40[d1.40>1])

#Cummulative number of transportation
cumulative <- ncvar_get(nc, "zone", start = c(2, firstdrifter, firsttime), count = c(1, nbdrifters, nbtime))+1
cumulative <- rowSums(cumulative)
cumulative <- sum(cumulative)
#Storing information
tmpinfo[[1]] <- cbind(startdate, d40, d1.40, cumulative)
nc_close(nc)

for(i in 2:length(info$filenames)){
  nc <- nc_open(info$filenames[i])
  #Number of paricles tranported at d40
  startdate <- as.character(info$startdate[i])
  d40 <- ncvar_get(nc, "zone", start = c(2, firstdrifter, lasttime), count = c(1, nbdrifters, 1))+1
  d40 <- sum(d40)
  #Number of paricles tranported at time time between d1 and d40
  d1.40 <- ncvar_get(nc, "zone", start = c(2, firstdrifter, firsttime), count = c(1, nbdrifters, nbtime))+1
  d1.40 <- max.col(d1.40, ties.method = "first")
  d1.40 <- length(d1.40[d1.40>1])
  #Cummulative number of transportation
  cumulative <- ncvar_get(nc, "zone", start = c(2, firstdrifter, firsttime), count = c(1, nbdrifters, nbtime))+1
  cumulative <- rowSums(cumulative)
  cumulative <- sum(cumulative)
  #Storing information
  tmpinfo[[i]] <- cbind(startdate, d40, d1.40, cumulative)
  print(paste0(round(i/length(info$filenames)*100),"%", " ", i, " out of ", length(info$filenames)))
  nc_close(nc)
}

df <- do.call("rbind", tmpinfo)

df <- as.data.frame(df)
summary(df)


df$Year <- year(df$startdate)

df$startdate <- as_datetime(df$startdate, origin = "1900-01-01 00:00:00")
df$doy <- strftime(df$startdate, format = "%j")

write.csv(df, file = "Data/All_data.txt",)

################
#### STEP 3 ####
################

#Further simplification of the data can be done using the following code, which summarises the data 
#according to data rather in individual simulate larvae.

All <- read.csv("Data/All_data.csv", header = T)


head(All)
str(All)

# Converting sim type to factor
All$Sim.type <- as.factor(All$Sim.type)
# Converting time0 to factor
All$time0 <- as.factor(All$time0)

head(All)

#Summerising the data by sim type and date
sum.data <- group_by(All, Sim.type) %>%
  group_by(time0, add = T) %>%
  summarise(d1.40 = sum(zoneA),
            d40 = sum(zonei))

write.table(sum.data, file = "R/Larval dispersal/Sum_data.txt")


############
## Step 4 ##
############

# Adding in the bathematry data for the strarting position all all particles.

#Cummulative recruiment code
library(ncdf4)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(maps)
library(mapdata)
library(patchwork)
library(raster)


#Reading in All data sets
All.df <- read.table("All_data.txt", header = T)

#Setting origin for time/date
ic.origin <- as_datetime("1900-01-01 00:00:00")

#Saving oringal format of simulation
All.df$Simulation <- as.factor(All.df$Sim.type)
#All.df$Sim.type <- All.df$Simulation
All.df$Sim.type <- as.numeric(All.df$Sim.type)
#Converting simulation data to understanble format
All.df$Sim.type[All.df$Sim.type==1] <- "No VM"
All.df$Sim.type[All.df$Sim.type==2] <- "VM"

All.df$time0 <- as.numeric(as.character(All.df$time0))
All.df$Origin <- lubridate::as_datetime(x = All.df$time0, origin = ic.origin, tz = "UTC")


#Creating a single origin column (This takes a while)

#Creating columns for months and years (Origin)
All.df$Year <-  year(All.df$Origin)
All.df$Month <- month(All.df$Origin)
All.df$doy <- strftime(All.df$Origin, format = "%j")

#Checking format is correct
head(All.df)

#Converting time into a factor
All.df$time0 <- factor(All.df$time0)

All$time0 <- as.factor(All$time0)
summary(All$time0)

head(All)

#### Adding information about depth of start
#Poluyygon extent
UK.poly <- cbind(c(-4.9,-3.9,-5.54,-5.89,-4.9), c(50.8,50.2,49.8,50.15,50.8))
Fr.poly <- cbind(c(-5.0,-5.0,-2.6,-2.6,-5.0), c(47.5,49.0,49.0,47.5,47.5))
simulation.extent <- cbind(lon = c(-7.5, -1.0, -1.0, -7.5), lat = c(51.0, 51.0, 47.0, 47.0))

#Raster data
bathy <- brick("E4_2020.nc", varname = "elevation", nl =1)
crs(bathy) <- '+proj=longlat +datum=WGS84 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84'
plot(bathy)
bathy.sim <- bathy[[1]]

bathy.sim <- crop(bathy[[1]], extent(SpatialPoints(simulation.extent[c(1,3),])))
plot(bathy.sim)

#extracting bathy data for the starting locations of each particle
simple.sf <- cbind(All$lon0, All$lat0)

bathy.data <- extract(bathy.sim[[1]], simple.sf)
bathy.data[1:1000]

#Adding data to 
All$bathy <- bathy.data

#Write file
write.csv(All, file = "All_bathy_data.csv")

#Create summary data
#Summerising the data by sim type and date
sum.data <- group_by(All, Sim.type) %>%
  group_by(time0, add = T) %>%
  summarise(d1.40 = sum(zoneA),
            d40 = sum(zonei))

write.table(sum.data, file = "Sum_bathy_data")