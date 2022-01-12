#Downloading all files from 2006 to 2011
rm(list =ls())
#.libPaths("C:/Users/cwp206/R/win-library/3.6")
#install.packages("threadr")
#install.packages("RCurl")
#install.packages("remotes")
#remotes::install_github("skgrange/threadr")
library(threadr)
library(RCurl)

#url where files are located
url <- "ftp://ext-marc_mangae:sP5VD5TePgadk2e4@ftp.ifremer.fr/MARC_F1-MARS3D-MANGAE2500/best_estimate/"
#Output location, make sure there is a folder for each year within this location

                                               #
output.location <- "D:/Ocean models/Input/"   #############
                                               #
#Sequence of years to be sequenced
years <- c(2006:2011)

#Loop to extracted the url for every file that wants to be downloaded
filenames <- list()
for(i in 1:length(unique(years))){
  #Making each url for each year
  url.tmp <- paste0(url, years[i], "/")
  #extracting url names for each year
  filenames[[i]] <- getURL(url.tmp, ftp.use.epsv = FALSE,dirlistonly = TRUE)
  #This is extracted as a long list of charaters so needs to be split up
  filenames[[i]] <- strsplit(filenames[[i]],"\r\n")
  print(url.tmp)
}

#Creating a data frame with list of urls for files
filenames <- unlist(filenames, use.names = F)
filenames <- as.data.frame(filenames)
colnames(filenames) <- "URL"

#Extracting each year
filenames$year <- substr(filenames$URL, 27, 30)
filenames$month <- substr(filenames$URL, 31, 32)
head(filenames)

#Column for each files output
filenames$Out.path <- paste0(output.location, filenames$year,"/", filenames$URL)

#removing files that won't be needed, in this case we only want from August onwards
filenames <- filenames[as.numeric(filenames$month)>=8,]

#specific  year to be extracted
ext.year <- 2006

#store.filenames <- filenames
#filenames <- store.filenames

filenames <- filenames[c(1:8, 4000:4006),]

filenames$URL.d <- paste0(url, filenames$year, "/", filenames$URL)


#Loop for downloading files that checks to see which files have already been downloaded

years.rep <- rep(1:length(unique(filenames$year)), 5)


for(j in years.rep){
  
  Year.j <- unique(filenames$year)[j]
  
  for(i in 1:length(filenames$URL[filenames$year==Year.j])){
    #name file to be downloaded
    file.i <- filenames$URL[filenames$year==Year.j][i]
    url.i <- filenames$URL.d[filenames$year==Year.j][i]
    output.i <- filenames$Out.path[filenames$year==Year.j][i]
    #names of files already extracted
    extracted.files <-list.files(paste0(output.location, Year.j))
    #Asks if the file i is already present in the data set
    if(file.i %in% extracted.files){
      #if it is then skips file
      print(paste0("All ready downloaded: ", file.i))
    } else {
      #If not then file is downloaded
      download_ftp_file(url.i, output.i, verbose = T, curl = T)
      #checking file downloaded
      extracted.files <-list.files(paste0(output.location, Year.j))
      if(file.i %in% extracted.files){
        print(paste0("Downloaded: ", file.i))
      } else print(paste0("File failed to download: ", file.i))
      
    }
  }
  
}


