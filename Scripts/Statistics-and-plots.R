## Code for creating statistics and plots for Patterson et al 2022

library(patchwork)

# Adding folder paths

plot.folder <- "~/R/Larval dispersal/Plots/"
setwd("~/R/Larval dispersal/")

#Reading data for all particles
All <- read_csv("All_bathy_data.csv", col_names = T)
str(All)
#Reading in summary data for all particles and all particles under -30 meter
sum.data <- read.table("Sum_data.txt", header = T)
sum.data.bathy <- read.table("Sum_bathy_data.txt", sep = ",")

#########################################################
#### Simple stats around the proportion of particles ####
#########################################################

#Those that underwent a recruitment event

head(sum.data.bathy)
str(sum.data.bathy)
length(unique(sum.data.bathy$time0))
sum.data.bathy %>%
  group_by(by = Sim.type) %>%
  summarise(e = mean(num.bathy.30), s.d = sd(num.bathy.30), mx = max(num.bathy.30), mn = min(num.bathy.30))

summary(sum.data.bathy$num.bathy.30)
sd(sum.data.bathy$num.bathy.30)

length(na.omit(All$bathy[All$Sim.type=="No VM"&All$bathy>=-30]))
length(na.omit(All$bathy[All$Sim.type=="No VM"&All$bathy>=-30&All$zoneA==1]))
(570/1693919)*100

#Correlation plots between Recruitment events and proportion of particles initially placed in water shallower than 30 meters

#Accross all years
par(mfrow=c(2,1))
plot(sum.data.bathy$d1.40.b, sum.data.bathy$num.bathy.30,
     ylab = "Number of particles starting at <30 meters depth",
     xlab = "Recuriment events")
plot(sum.data.bathy$d1.40, sum.data.bathy$num.bathy.30,
     ylab = "Number of particles starting at <30 meters depth",
     xlab = "Recuriment events")

par(mfrow=c(1,1))

#Within each year

ggplot(data = sum.data.bathy) +
  geom_point(aes(d1.40.b, num.bathy.30)) +
  facet_wrap(~Sim.type*Year)

ggplot(data = sum.data.bathy) +
  geom_point(aes(d1.40, num.bathy.30)) +
  facet_wrap(~Sim.type*Year)

#Corrolation tests
cor.test(sum.data.bathy$d1.40.b, sum.data.bathy$num.bathy.30, method = "spearman")
cor.test(sum.data.bathy$d1.40.b, sum.data.bathy$num.bathy.30, method = "spearman")
cor.test(sum.data.bathy$d1.40, sum.data.bathy$num.bathy.30, methods = "spearman")

sum.data.bathy$p1.40.b <- (sum.data.bathy$d1.40.b/sum.data.bathy$num.bathy.30)*100


#Those that underwent a recruitment event but did not originate from -30
head(sum.data)
str(sum.data)

#Creating a month, day of year and year column
sum.data$p1.40 <- (sum.data$d1.40/10000)*100
ic.origin <- as_datetime("1900-01-01 00:00:00")
sum.data$Origin <- as_datetime(sum.data$time0, origin = ic.origin, tz = "UTC")

# Adding columns
sum.data$Year <-  year(sum.data$Origin)
sum.data$Month <- month(sum.data$Origin)
sum.data$doy <- strftime(sum.data$Origin, format = "%j")
sum.data$date.doy <- sum.data$Origin-years(year(sum.data$Origin))

plot(sum.data.bathy$d1.40.b, sum.data.bathy$num.bathy.30)

cor.test(sum.data.bathy$d1.40.b, sum.data.bathy$num.bathy.30, method = "spearman")

sum.data.bathy$p1.40.b <- (sum.data.bathy$d1.40.b/sum.data.bathy$num.bathy.30)*100


################
#### Table 1 ###
################

#All years
All.stats <- group_by(sum.data.bathy, Sim.type) %>%
  summarise(Year = "All Years", mn.d1.40b = mean(p1.40.b), med = median(p1.40.b), sd.d1.40b = sd(p1.40.b), mx = max(p1.40.b))
#grouped by all years
sum.stats <- group_by(sum.data.bathy, Sim.type) %>%
  group_by(Year, add = T) %>%
  summarise(mn.d1.40b = mean(p1.40.b), med= median(p1.40.b), sd.d1.40b = sd(p1.40.b), mx = max(p1.40.b))

sum.stats <- rbind(as.data.frame(sum.stats), as.data.frame(All.stats))
sum.stats[,3:6] <- signif(sum.stats[,3:6], digits = 2)

sum.stats

All.stats <- group_by(sum.data.bathy, Sim.type) %>%
  summarise(Year = "All Years", mn.d1.40b = mean(d1.40.b), med = median(d1.40.b), sd.d1.40b = sd(d1.40.b), mx = max(d1.40.b))
#grouped by all years
sum.stats <- group_by(sum.data.bathy, Sim.type) %>%
  group_by(Year, add = T) %>%
  summarise(mn.d1.40b = mean(d1.40.b), med= median(d1.40.b), sd.d1.40b = sd(d1.40.b), mx = max(d1.40.b))

sum.stats <- rbind(as.data.frame(sum.stats), as.data.frame(All.stats))
sum.stats[,3:6] <- signif(sum.stats[,3:6], digits = 2)

sum.stats


#Highest recruitment of the bathy data with No VM
sum.data.bathy$Origin[which(sum.data.bathy$d1.40.b == max(sum.data.bathy$d1.40.b))]

# Which VM simulation underwent a recruitment event
sum.data.bathy$Origin[sum.data.bathy$Sim.type=="VM"][which(sum.data.bathy$d1.40.b[sum.data.bathy$Sim.type=="VM"] == max(sum.data.bathy$d1.40.b[sum.data.bathy$Sim.type=="VM"]))]

#How many No VM simulation had no recruitment events
length(which(sum.data.bathy$d1.40.b[sum.data.bathy$Sim.type=="No VM"] == 0)) / length(sum.data.bathy$d1.40.b[sum.data.bathy$Sim.type=="No VM"]) *100

head(All)

#######################################################
#### Figure One: Map with trajectories of particles
#######################################################
library(ncdf4)

#Simulation with the highest amount of connectivity.
#Specific plotted particles where narrowed down and then selected based on visual appeal.
sum.data.bathy[which(sum.data.bathy$d1.40.b == max(sum.data.bathy$d1.40.b)),]

# Use any .nc file produced by your simulations
nc <- nc_open("mars3d_2014__ichthyop-run202001201103_s20.nc")

#Map of simulation area
UK.poly <- cbind(c(-4.9,-3.9,-5.54,-5.89,-4.9), c(50.8,50.2,49.8,50.15,50.8))
Fr.poly <- cbind(c(-5.0,-5.0,-2.6,-2.6,-5.0), c(47.5,49.0,49.0,47.5,47.5))
simulation.extent <- cbind(lon = c(-7.5, -1.0, -1.0, -7.5), lat = c(51.0, 51.0, 47.0, 47.0))
plot(simulation.extent, col = "white", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab = "Longitude",
     ylab = "Latitude")
world <- map_data("worldHires")
world <- world[world$long>=-9&world$long<=-1&world$lat>=46&world$lat<=52,]
map("worldHires",add=TRUE,fill = TRUE, col = "grey90")#regions = c("france", "UK"))
polygon(simulation.extent, lwd = 4)
polygon(UK.poly)
polygon(Fr.poly)


#Parameter for particle trajectory extraction
firstdrifter <- 1
lastdrifter <- 10000
nbdrifters <- lastdrifter-firstdrifter+1
# Index of the first and last time records
#firsttime <- 1
#lasttime <- 61
firsttime <- 1
lasttime <-  length(nc$dim$time$vals)
nbtime <- lasttime-firsttime+1
nc$var$depth
#Under.30 <- which(ncvar_get(nc,'depth',c(firstdrifter,firsttime),c(lastdrifter,1)) >= -30)

#Particle that leaves simulated area
lowestlat <- order(ncvar_get(nc,'lat',c(firstdrifter,lasttime),c(lastdrifter,1)))[2]
loni <- ncvar_get(nc,'lon',c(lowestlat,firsttime),c(1,nbtime))
lati <- ncvar_get(nc,'lat',c(lowestlat,firsttime),c(1,nbtime))
points(loni,lati,type='l',col = "black", lwd = 2)
points(loni[lasttime],lati[lasttime],type='p',pch=20, cex = 4, col = "black")
text("A", x = loni[lasttime]-0.2,y = lati[lasttime]+0.2, col = "black", cex = 1.5)


#Particle that enters and then leaves the UK Polygon
#Backwards particle
otly <- order(ncvar_get(nc,'lat',c(firstdrifter,lasttime),c(lastdrifter,1)), decreasing = T)[170]
loni <- ncvar_get(nc,'lon',c(otly,firsttime),c(1,nbtime))-0.5
lati <- ncvar_get(nc,'lat',c(otly,firsttime),c(1,nbtime))
points(loni,lati,type='l', lwd = 2, col = "black") #col= "grey")
points(loni[lasttime],lati[lasttime],type='p',pch=20, cex = 4, col = "black") #col = "grey")
text("C", x = loni[lasttime]-0.3,y = lati[lasttime]+0.1, cex = 1.5, col = "black") #col = "grey")

#Particle that enters and then leaves polygon
highestlat <- order(ncvar_get(nc,'lat',c(firstdrifter,lasttime),c(lastdrifter,1)), decreasing = T)[235]
loni <- ncvar_get(nc,'lon',c(highestlat,firsttime),c(1,nbtime))
lati <- ncvar_get(nc,'lat',c(highestlat,firsttime),c(1,nbtime))
lines(loni,lati,type='l', lwd = 2, col = "grey") #col= "grey50")
#arrows(x0 = loni[lasttime*0.5-200]-0.5, y0 = lati[lasttime*0.5-200],x1 = loni[lasttime*0.5]-0.4, y1 = lati[lasttime*0.5], col= "black", lwd = 2)
points(loni[lasttime],lati[lasttime],type='p',pch=20, cex = 4, col = "grey") #"grey50",)
text("B", x = loni[lasttime]-0.2,y = lati[lasttime]+0.1, cex = 1.5, col = "grey") #"grey50")

#Tidy up enviroment
rm(list = ls()[!ls()%in%c("All", "sum.data", "sum.data.bathy", "plot.folder", "UK.poly", "Fr.poly", "simulation.extent")])

#################################################################
#### Figure 2: Connectivity between Brittany and the UK over time
#################################################################

#Colour palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
plot(1:length(cbPalette), rep(1,length(cbPalette)), col = cbPalette, pch =  20, cex = 20)
cbPalette.up <- cbPalette[c(1,2,3,4,7)]
nbdrifters <- 10000
head(sum.data.bathy)
paste(sum.data.bathy)
sum.data.bathy$Origin <- as_datetime(sum.data.bathy$Origin)
sum.data.bathy$date.doy <- sum.data.bathy$Origin-years(year(sum.data.bathy$Origin))

p <- ggplot(sum.data.bathy) +
  geom_path(data = sum.data, aes(x = date.doy, y = p1.40), col = "grey", size = 1.5) +
  geom_path(aes(x = date.doy, y = p1.40.b), col = "black", size = 1.5) +
  facet_wrap(~Sim.type*Year, nrow = 2) +
  theme_bw() +
  xlab("Date") +
  ylab("Recruitment events (%)") +
  #scale_colour_manual(values=cbPalette.up) +
  labs(colour = "Year", linetype = "Simulation") +
  theme(legend.key.size = unit(3,"line"))+
  theme(text = element_text(size = 25),
        axis.text=element_text(size=15),
        legend.text=element_text(size=20),
        legend.title=element_text(size=25),
        axis.title=element_text(size=30))

png(filename = paste0(plot.folder,"Figure 2 Connectivity between VM and No VM and years.png"),
    width = 1200*1.5,
    height = 400*2,
    units = "px")
p
dev.off()

###############################################################
#### Figure S2 - the end trajectory of all particles
###############################################################
All.rc.30 <- All[All$zoneA==1&All$bathy>=-30,]

All.rc.30 <- na.omit(All.rc.30)

All.rc.30$date.doy <- All.rc.30$Origin-years(year(All.rc.30$Origin))
head(All.rc.30)

world <- map_data("worldHires", region = c("UK", "France"))
world <- rbind(world[world$group=="8",], world[world$group=="1",])
world <- world[world$long>=-9&world$long<=-1&world$lat>=47.5&world$lat<=51,]

world <- map_data("worldHires")
world <- world[world$long>=-9&world$long<=-1&world$lat>=46&world$lat<=52,]

p <- ggplot(All.rc.30[All.rc.30$Sim.type=="No VM",]) +
  geom_segment(aes(x= lon0, y = lat0, xend = loni, yend=lati, col = mntmp), alpha = 0.1, size = 1) +
  geom_point(aes(lon0, lat0, col = mntmp), alpha = 0.5, size = 3) +
  geom_point(aes(loni, lati, col = mntmp), alpha = 0.5, size = 3) +
  geom_path(data = world, aes(x = long, y = lat, group = group)) +
  geom_path(data = data.frame(Fr.poly), aes(X1, X2)) +
  geom_path(data = data.frame(UK.poly), aes(X1, X2)) +
  facet_wrap(~Year, drop = FALSE) +
  theme(legend.key.size = unit(3,"line"))+
  xlim(-6, -2) +
  ylim(47.4,51) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text = element_text(size = 40),
        axis.text=element_text(size=25),
        legend.text=element_text(size=25),
        legend.title=element_text(size=25),
        axis.title=element_text(size=38),
        axis.text.x = element_text(size =  30),
        axis.text.y = element_text(size =  30),
        legend.position = c(0.89,0.25)) +
  scale_colour_gradient(low="blue", high="red") +
  labs(colour = "Mean temperature (°C)", x = "Longtitude", y = "Latitude")


png(filename = paste0(plot.folder,"Figure S2-C_erythropus habitat map with connecting points_v2.png"),
    width = 1200,
    height = 1500,
    units = "px")

p

dev.off()

###############################################################
#### Large scale map of end point for all particles
###############################################################

p <- ggplot(All) +
  #geom_bin2d(aes(loni, lati)) +
  facet_wrap(~Year*Sim.type) +
  geom_bin2d(aes(x = loni, y = lati), col = "grey28") +
  geom_path(data = world, aes(x = long, y = lat, group = group)) +
  geom_path(data = data.frame(Fr.poly), aes(X1, X2)) +
  geom_path(data = data.frame(UK.poly), aes(X1, X2)) +
  ylim(c(47,51)) +
  xlim(c(-7.5,-1)) +
  scale_fill_gradientn(colours = c("white","blue","black"), values = c(-0, 0.2, 1), breaks =c(0, 20000, 40000)) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(text=element_text(size = 30))

png(filename = paste0(plot.folder,"C_erythropus Large scale map of end point for all particles.png"),
    width = 3000,
    height = 3200,
    units = "px")

p

dev.off()

p <- ggplot(All) +
  #geom_bin2d(aes(loni, lati)) +
  facet_wrap(~Year*Sim.type) +
  geom_density_2d(aes(x = loni, y = lati)) +
  geom_path(data = world, aes(x = long, y = lat, group = group)) +
  geom_path(data = data.frame(Fr.poly), aes(X1, X2)) +
  geom_path(data = data.frame(UK.poly), aes(X1, X2)) +
  ylim(c(47,51)) +
  xlim(c(-7.5,-1)) +
  #scale_fill_gradientn(colours = c("white","blue","black"), values = c(-0, 0.2, 1), breaks =c(0, 20000, 40000)) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(text=element_text(size = 30))

png(filename = paste0(plot.folder,"C_erythropus Large scale density map of end point for all particles v2.png"),
    width = 3000,
    height = 3200,
    units = "px")

p

dev.off()

##################################################################
#Figure S2 the end point of all particles across all simulations##
##################################################################

head(All)
rm("p", "q", "r")

#No VM
p <- ggplot(All[All$Sim.type=="No VM",]) +
  facet_wrap(~Year, nrow = 1) +
  geom_bin2d(aes(x = loni, y = lati), col = "black") +
  geom_path(data = world, aes(x = long, y = lat, group = group)) +
  geom_path(data = data.frame(Fr.poly), aes(X1, X2)) +
  geom_path(data = data.frame(UK.poly), aes(X1, X2)) +
  ylim(c(47,51)) +
  xlim(c(-7.5,-1)) +
  scale_fill_gradientn(colours = c("white","blue","black"), values = c(-0, 0.2, 1), breaks =c(1, 20000, 40000)) +
  theme_bw() +
  ggtitle("Calculated final position for all particles", subtitle = "(a) No vertical migration") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(text=element_text(size = 30))


#With Vertical Migration
q <- ggplot(All[All$Sim.type=="VM",]) +
  facet_wrap(~Year, nrow = 1) +
  geom_bin2d(aes(x = loni, y = lati), col = "grey20") +
  geom_path(data = world, aes(x = long, y = lat, group = group)) +
  geom_path(data = data.frame(Fr.poly), aes(X1, X2)) +
  geom_path(data = data.frame(UK.poly), aes(X1, X2)) +
  ylim(c(47,51)) +
  xlim(c(-7.5,-1)) +
  scale_fill_gradientn(colours = c("white","blue","black"), values = c(-0, 0.2, 1), breaks =c(1, 20000, 40000)) +
  theme_bw() +
  ggtitle("", subtitle = "(b) Vertical migration") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(text=element_text(size = 30))

png(filename = paste0(plot.folder,"C_erythropus end points for all particles across all simulations.png"),
    width = 8000/2,
    height = 2800/2,
    units = "px")

p/q

dev.off()


################
####  FIG S1  ###
################

#Density plot of recruiment events at day 1-40
bin.wth <- 25
scalelim <- c(0, 600)
Re <- All[All$zoneA=="1",]
head(Re)
#"Forwards "End point"
a <- ggplot() +
  geom_bin2d(data = Re[Re$Sim.type=="No VM",], aes(x = loni, y = lati), bins = bin.wth, col = "black") +
  facet_wrap(~Year, nrow = 1) +
  geom_path(data = data.frame(UK.poly), aes(X1, X2)) +
  geom_path(data = world[world$region=="UK"&world$long<=-2.5&world$lat<=51,], aes(x = long, y = lat, group = group)) +
  scale_fill_gradientn(colours = c("white","blue","black"),values = c(0, 0.2, 1), breaks = c(1,200,400,600), limits = c(0,750)) +
  theme_bw() +
  ggtitle("No vertical migration", subtitle = expression(paste(bold("(a)"), " Calculated final position"))) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(text=element_text(size = 30))
#Forwards intial conditions
b <- ggplot() +
  geom_bin2d(data = Re[Re$Sim.type=="No VM",], aes(x = lon0, y = lat0), bins = bin.wth, col = "black") +
  facet_wrap(~Year, nrow = 1) +
  geom_path(data = data.frame(Fr.poly), aes(X1, X2)) +
  geom_path(data = world[world$region=="France"&world$long<=-2.5,], aes(x = long, y = lat, group = group)) +
  scale_fill_gradientn(colours = c("white","blue","black"),values = c(0, 0.2, 1), breaks = c(1,200,400,600), limits = c(0,750)) +
  theme_bw() +
  ggtitle("", subtitle = expression(paste(bold("(b)"), " Intial conditions"))) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(text=element_text(size = 30))

#"VM "End point"
e <- ggplot() +
  geom_bin2d(data = Re[Re$Sim.type=="VM",], aes(x = loni, y = lati), bins = bin.wth, col = "black") +
  facet_wrap(~Year, nrow = 1) +
  geom_path(data = data.frame(UK.poly), aes(X1, X2)) +
  geom_path(data = world[world$region=="UK"&world$long<=-2.5&world$lat<=51,], aes(x = long, y = lat, group = group)) +
  scale_fill_gradientn(colours = c("white","blue","black"),values = c(0, 0.2, 1), breaks = c(1,200,400,600), limits = c(0,750)) +
  theme_bw() +
  ggtitle("Vertical Migration", subtitle = expression(paste(bold("(c)"), " Calculated final position"))) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(text=element_text(size = 30))
#VM initial conditions
f <- ggplot() +
  geom_bin2d(data = Re[Re$Sim.type=="VM",], aes(x = lon0, y = lat0), bins = bin.wth, col = "black") +
  facet_wrap(~Year, nrow = 1) +
  geom_path(data = data.frame(Fr.poly), aes(X1, X2)) +
  geom_path(data = world[world$region=="France"&world$long<=-2.5,], aes(x = long, y = lat, group = group)) +
  scale_fill_gradientn(colours = c("white","blue","black"),values = c(0, 0.2, 1), breaks = c(1,200,400,600), limits = c(0,750)) +
  theme_bw() +
  ggtitle("", subtitle = expression(paste(bold("(d)"), " Intial conditions"))) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(text=element_text(size = 30))



#png("C:/Users/chris/Dropbox/Degree Work/Masters by Research/Work folder/Write ups/Parts of write up for Masters/Larval dispersal model - Chapter 1/2_Results/Figures/Density plot for recuriment events d1-40 rescaled.png",
png(paste0(plot.folder,"Density plot for recuriment events F VM d1-40 rescaled.png"),
    width = 1000*4, 
    height=1000*2)
(a/b/e/f)
dev.off() 


#################################################

#No VM and the initial starting location of all particles
p <- ggplot(All[All$Sim.type=="No VM"&All$zoneA==1,]) +
  facet_wrap(~Year, nrow = 1) +
  geom_bin2d(aes(x = loni, y = lati), col = "grey20") +
  geom_path(data = world, aes(x = long, y = lat, group = group)) +
  geom_path(data = data.frame(Fr.poly), aes(X1, X2)) +
  geom_path(data = data.frame(UK.poly), aes(X1, X2)) +
  ylim(c(49.75,51)) +
  xlim(c(-6.5,-3)) +
  scale_fill_gradientn(colours = c("white","blue","black"), values = c(-0, 0.2, 1), breaks =c(1, 200, 400)) +
  theme_bw() +
  ggtitle("Calculated Final Postision", subtitle = "(a) No vertical migration") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(text=element_text(size = 30))


#With Vertical Migration and the intial starting location of all particles
q <- ggplot(All[All$Sim.type=="VM"&All$zoneA==1,]) +
  facet_wrap(~Year, nrow = 1) +
  geom_bin2d(aes(x = loni, y = lati), col = "grey20") +
  geom_path(data = world, aes(x = long, y = lat, group = group)) +
  geom_path(data = data.frame(Fr.poly), aes(X1, X2)) +
  geom_path(data = data.frame(UK.poly), aes(X1, X2)) +
  ylim(c(47.25,49.25)) +
  xlim(c(-5.25,-2.25)) +
  scale_fill_gradientn(colours = c("white","blue","black"), values = c(-0, 0.2, 1), breaks =c(1, 200, 400)) +
  theme_bw() +
  ggtitle("", subtitle = "(a) Vertical migration") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(text=element_text(size = 30))


#No VM and the initial starting location of all particles
r <- ggplot(All[All$Sim.type=="No VM"&All$zoneA==1,]) +
  facet_wrap(~Year, nrow = 1) +
  geom_bin2d(aes(x = lon0, y = lat0), col = "grey20") +
  geom_path(data = world, aes(x = long, y = lat, group = group)) +
  geom_path(data = data.frame(Fr.poly), aes(X1, X2)) +
  geom_path(data = data.frame(UK.poly), aes(X1, X2)) +
  ylim(c(47.25,49.25)) +
  xlim(c(-5.25,-2.25)) +
  scale_fill_gradientn(colours = c("white","blue","black"), values = c(-0, 0.2, 1), breaks =c(1, 200, 400)) +
  theme_bw() +
  ggtitle("Intial position of particles that underwent a recuriment event", subtitle = "(c) No vertical migration") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(text=element_text(size = 30))


#With Vertical Migration and the intial starting location of all particles
s <- ggplot(All[All$Sim.type=="VM"&All$zoneA==1,]) +
  facet_wrap(~Year, nrow = 1) +
  geom_bin2d(aes(x = lon0, y = lat0), col = "grey20") +
  geom_path(data = world, aes(x = long, y = lat, group = group)) +
  geom_path(data = data.frame(Fr.poly), aes(X1, X2)) +
  geom_path(data = data.frame(UK.poly), aes(X1, X2)) +
  ylim(c(47.25,49.25)) +
  xlim(c(-5.25,-2.25)) +
  scale_fill_gradientn(colours = c("white","blue","black"), values = c(-0, 0.2, 1), breaks =c(1, 200, 400)) +
  theme_bw() +
  ggtitle("", subtitle = "(d) Vertical migration") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(text=element_text(size = 30))


##########
#Figure S3
##########
Backwards <- read.table("All_Backwards_data.txt", header = T)
head(Backwards)
#Creating a month, day of year and year column
ic.origin <- as_datetime("1900-01-01 00:00:00")
Backwards$Origin <- as_datetime(Backwards$time0, origin = ic.origin, tz = "UTC")
Backwards$Year <- year(Backwards$Origin)
Backwards$doy <- as.numeric(strftime(Backwards$Origin, format = "%j"))
Backwards$date <- Backwards$Origin-years(year(Backwards$Origin))

simulation.extent <- cbind(lon = c(-7.5, -1.0, -1.0, -7.5), lat = c(51.0, 51.0, 47.0, 47.0))
boxes<-data.frame(maxlat = 51,minlat = 47,maxlong = -7.5,minlong = -1, id="1")
boxes<-transform(boxes, laby=(maxlat +minlat )/2, labx=(maxlong+minlong )/2)

p <- ggplot(Backwards) +
  geom_bin2d(data = Backwards, aes(x = loni, y = lati), bins = bin.wth, col = "black") +
  geom_rect(data=boxes, aes(xmin=minlong , xmax=maxlong, ymin=minlat, ymax=maxlat ), color="black", fill="transparent") +
  facet_wrap(~Year) +
  geom_path(data = world, aes(x = long, y = lat, group = group)) +
  geom_path(data = data.frame(Fr.poly), aes(X1, X2)) +
  geom_path(data = data.frame(UK.poly), aes(X1, X2)) +
  ylim(c(47,51)) +
  xlim(c(-7.5,-1)) +
  scale_fill_gradientn(colours = c("white","blue","black"), values = c(-0, 0.1, 1), breaks =c(1, 30000, 60000)) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(text=element_text(size = 50))

png(paste0(plot.folder,"Density plot for final position of backwards particles.png"),
    width = 1000*2, 
    height=1000*1.7)
p
dev.off() 


####
# Figure S4: Starting densities of particles
#######


head(All)
high.rc.b <- sum.data.bathy$Origin[which(sum.data.bathy$d1.40.b == max(sum.data.bathy$d1.40.b))[1]]
sum.data.bathy$time0[which(sum.data.bathy$d1.40.b == max(sum.data.bathy$d1.40.b))]

All.22.08.2014 <- All[All$Origin==high.rc.b,]
All.22.08.2014.NVM <- All.22.08.2014[All.22.08.2014$Sim.type=="No VM",]
plot(All.22.08.2014.NVM$lon0[All.22.08.2014$bathy>-30], All.22.08.2014.NVM$lat0[All.22.08.2014$bathy>-30])

All.subset <- All[All$time0>=3617690400&All$time0<=3617863200&All$Sim.type=="No VM",]

All.b <- All[All$bathy>=-30,]
All.b <- na.omit(All.b)
rad.dates <- c(sample(unique(All.b$Origin), 8), sum.data.bathy[which(sum.data.bathy$d1.40.b == max(sum.data.bathy$d1.40.b)),]$Origin[1])
All.b.subset <- All.b[All.b$Origin %in% rad.dates,]

All.b.subset <- arrange(All.b.subset, zoneA)

p <- ggplot(All.b.subset) +
  geom_point(aes(x = lon0, y = lat0, col = as.factor(zoneA))) +
  facet_wrap(~as.factor(Origin)) +
  geom_path(data = world, aes(x = long, y = lat, group = group)) +
  geom_path(data = data.frame(Fr.poly), aes(X1, X2)) +
  #geom_path(data = data.frame(UK.poly), aes(X1, X2)) +
  ylim(c(47.4,49.1)) +
  xlim(c(-5.1,-2.4)) +
  scale_color_manual(values=c("#999999", "#E69F00"), labels = c("No", "Yes")) +#, "#56B4E9"))
  theme_bw() +
  labs(color='Recruited') +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(text=element_text(size = 50))

png(paste0(plot.folder,"Starting positions of random subset of dates.png"),
    width = 1000*2, 
    height=1000*2)
p
dev.off() 

p <- ggplot(All.b.subset) +
  geom_point(aes(x = lon0, y = lat0, col = as.factor(zoneA))) +
  #geom_point(aes(x = loni, y = lati, col = as.factor(zoneA))) +
  facet_wrap(~as.factor(Origin)) +
  geom_path(data = world, aes(x = long, y = lat, group = group)) +
  geom_path(data = data.frame(Fr.poly), aes(X1, X2)) +
  geom_path(data = data.frame(UK.poly), aes(X1, X2)) +
  ylim(c(47,51)) +
  xlim(c(-7.5,-1)) +
  ggtitle("", subtitle = "intial position") +
  scale_color_manual(values=c("#999999", "#E69F00"), labels = c("No", "Yes")) +#, "#56B4E9"))
  theme_bw() +
  labs(color='Recruited') +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(text=element_text(size = 50))

q <- ggplot(All.b.subset) +
  #geom_point(aes(x = lon0, y = lat0, col = as.factor(zoneA))) +
  geom_point(aes(x = loni, y = lati, col = as.factor(zoneA))) +
  facet_wrap(~as.factor(Origin)) +
  geom_path(data = world, aes(x = long, y = lat, group = group)) +
  geom_path(data = data.frame(Fr.poly), aes(X1, X2)) +
  geom_path(data = data.frame(UK.poly), aes(X1, X2)) +
  ylim(c(47,51)) +
  xlim(c(-7.5,-1)) +
  ggtitle("", subtitle = "Final position") +
  scale_color_manual(values=c("#999999", "#E69F00"), labels = c("No", "Yes")) +#, "#56B4E9"))
  theme_bw() +
  labs(color='Recruited') +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(text=element_text(size = 50))

png(paste0(plot.folder,"Starting and end positions of random subset of dates.png"),
    width = 2000*2, 
    height=1000*2)
p+q
dev.off() 
