
#################################
###  Author : Ross Searle         
###  Date : Fri Feb 19 12:18:37 2021                      
###  Project : TERN SMIPS V2
###  Purpose : This script downloads raster surfaces of precipitation, tmax and Tmin from the CSIRO DigiScape climate data repository
###  Digiscape climate THREDDS catalogue root - https://data-cbr.it.csiro.au/thredds/catalog/catch_all/Digiscape_Climate_Data_Portal/awap/catalog.html
#################################

library(raster)
library(httr)

# see below for loop to demo this function

getClimateRasters <- function(today){
  
  # some required constants
  vars <- c('precip', 'tmin', 'tmax' )
  otherBits <- c('total', 'mean','mean')
  ids <- c('IDCKZRDAT0','IDCKZNDA90','IDCKZXDA90')
  
  
    ### iterate through the download and transform of the raster data sets
    for (i in 1:length(vars)) {
      
      var <- vars[i]
      dayName <- paste0(ids[i], '_', var, '_', otherBits[i], '_r005_', today, '_', today)
      url <- paste0(urlRoot, '/',var, '/data/', dayName, '.nc' )
      dPath <- paste0(downloadRoot, '/', dayName, '.nc')
      
      print(paste0('Attempting to download ', dayName))
      
      resp <- GET(url)
    
      if (http_error(resp)) {
          stop(
           print(paste0('There was a problem with the download of ', var, ' on ', today, '  URL - ', url)),
            call. = FALSE
          )
      }
      
      if (http_type(resp) != "application/x-netcdf") {
        stop("API did not return a file in netCDF format", call. = FALSE)
      }
      
      rawFile <- paste0(tempdir(), '/', dayName, '.nc')
      bin <- content(resp, 'raw')
      writeBin(bin, rawFile)
      suppressWarnings(r <- raster(rawFile))
      r2 <- resample(r, templateR)
      r3 <- mask(r2, templateR)
      writeRaster(r3, paste0(downloadRoot, '/', var, '_',today, '.tif'), overwrite=T)
      unlink(rawFile)
      plot(r3)
      print(paste0(dayName,' was successfully downloaded'))
      
    }
}


### These variables need to be set to match the values for the system on which this script is running

templateR <- raster('C:/Projects/SMIPS/Climate/soil_thickness_corrected.tif')
urlRoot <- 'http://data-cbr.it.csiro.au/thredds/fileServer/catch_all/Digiscape_Climate_Data_Portal/awap'
downloadRoot <- 'c:/temp/clim'



dts <- seq(as.Date("2021/2/1"), as.Date("2021/2/18"), "days")

for (j in 1:length(dts)) {
  today <-format(dts[j], format="%Y%m%d")
  getClimateRasters(today)
  
}
