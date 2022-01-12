# Creating text configuration files for ichthyop v3.3 for vertical migration simulation using
# the times of sunrise and sunset

#install.packages("suncalc")
library(lubridate)
library(suncalc)

#Creating a list of start times for the simulation
startdate <- as_datetime("2012-09-11 10:00:00") #startdate
enddate <- as_datetime("2012-12-31 10:00:00")   #enddate
mid.lat <- mean(c(51,47))                       #centre lat of simulated area
mid.long <- mean(c(-1,-7.5))                    #centre lon of simulated area
years.sim <-  c(2006:2016)#c(2012,2013,2014,2015,2016)        #Years to be simulated

#A while loop which creates every date needed within a given year.
idate <- startdate
datetime.range <- startdate
while (idate != enddate){
  idate <- idate + days(1)
  datetime.range <- c(datetime.range, idate)
}
datetime.range

#For loop for adding further simulations dates
first.years.dtime <- datetime.range
for(i in 1:(length(years.sim)-1)){
  iyear <- first.years.dtime + years(i)
  datetime.range <- c(datetime.range, iyear)
}

date.range <- as_date(datetime.range) #Creating a date from dtime


sunsettimes <- getSunlightTimes(date.range, lat = mid.lat, lon = mid.long) #getting dates
sunsettimes$dtime <- datetime.range      #Creating a column with time and date
sunsettimes <- sunsettimes[,c(1,6,7,18)] #Removing superfloues unformation
sunsettimes$year <- year(sunsettimes$dtime)
sunsettimes$month <- format(sunsettimes$dtime, "%m")
sunsettimes$day <- format(sunsettimes$dtime, "%d")
sunsettimes$time <- format(sunsettimes$dtime, "%H:%M")
#Overridding sunset and sunrise time to a form icthypop can understand
sunsettimes$sunrise <- format(sunsettimes$sunrise, "%H:%M")
sunsettimes$sunset <- format(sunsettimes$sunset, "%H:%M")
#Creating filenames
cf.folder <- "C:Ocean-models/ichthyop_3.3.3/ichthyop_3.3.3/cfg/Vertical_migration/All_cfg/"
sunsettimes$cfg.name <- paste("Vertical_migration_cfg",sunsettimes$year , sunsettimes$month, sunsettimes$day, sep = "_")
sunsettimes$cfg.name <- paste0(cf.folder,sunsettimes$cfg.name,".xml")

#Creating a column for the start time of each simulation
sunsettimes$simulation.start.time <- paste("year", sunsettimes$year, 
                                           "month", sunsettimes$month, 
                                           "day", sunsettimes$day, 
                                           "at", sunsettimes$time)
#Creating a output file location. Must be in the format used by the operating system the simulation will be run under.
default.output <- "/mnt/c/Ocean-models/ichthyop_3.3.3/ichthyop_3.3.3/output/Vertical migration/"
sunsettimes$output.folder <- paste0(default.output, sunsettimes$year)

#Creating input file location
default.input <- "/mnt/d/Ocean models/Input/"
sunsettimes$input.folder <- paste0(default.input, sunsettimes$year)
#Creating Fileprefix
default.prefix <- "MARS-3D-VM-"
sunsettimes$prefix <- paste0(default.prefix, sunsettimes$date)

#Reading in Outline of default ichthyop config file. Build this default config file within ichthyop v3.3
#Specifying the depths that vertical migration will occur over.
text.file <- readLines("Outline_for_single_VM_config_file.xml")
#Break up of text to fill in
text.1 <- text.file[1:13]
text.2 <- text.file[15:77]
text.3 <- text.file[79:84]
text.4 <- text.file[86:115]
text.5 <- text.file[117:604]
text.6 <- text.file[606:612]
text.7 <- text.file[614:1265]

#Test file writing- DO NOT remove spaces before <value>
temp.text <- c(text.1,
               paste0("        <value>",sunsettimes$simulation.start.time[1],"</value>"),
               text.2,
               paste0("        <value>",sunsettimes$prefix[1],"</value>"),
               text.3,
               paste0("        <value>",sunsettimes$output.folder[1],"</value>"),
               text.4,
               paste0("        <value>",sunsettimes$input.folder[1],"</value>"),
               text.5,
               paste0("        <value>",sunsettimes$sunrise[1],"</value>"),
               text.6,
               paste0("        <value>",sunsettimes$sunset[1],"</value>"),
               text.7)
writeLines(temp.text,"Testtext.xml")

#For loop for creating files
for(i in 1:length(sunsettimes$cfg.name)){
  temp.text <- c(text.1,
                 paste0("        <value>",sunsettimes$simulation.start.time[i],"</value>"),
                 text.2,
                 paste0("        <value>",sunsettimes$prefix[i],"</value>"),
                 text.3,
                 paste0("        <value>",sunsettimes$output.folder[i],"</value>"),
                 text.4,
                 paste0("        <value>",sunsettimes$input.folder[i],"</value>"),
                 text.5,
                 paste0("        <value>",sunsettimes$sunrise[i],"</value>"),
                 text.6,
                 paste0("        <value>",sunsettimes$sunset[i],"</value>"),
                 text.7)
  writeLines(temp.text, con = sunsettimes$cfg.name[i])
}
              