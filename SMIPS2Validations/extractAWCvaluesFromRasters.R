library(raster)
library(RSQLite)
library(jsonlite)
library(stringr)
library(stringr)

rasterOptions(datatype="FLT4S", timer=TRUE, format='GTiff',progress="text",chunksize=1e+09,maxmemory=1e+09, overwrite=TRUE) # maxmemory = max no of cells to read into memory

awcRootDir <- 'Y:/Ross/TERN/AWC'



urlsitdf <- "http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorLocations?sensortype=Soil-Moisture&usr=ross.searle@csiro.au&pwd=S4QQBMk74zhnBnEpTcd6iLwlUredn6kekLkjFL"
sitdf <- fromJSON(urlsitdf)
locs <- sitdf
coordinates(locs) <- ~ Longitude + Latitude
plot(locs)


products <- c('ApsoilRF', 'PedoRasterMaps', 'MeasuredTextures')
MTdepths <- c('005', '015', '030', '060', '100', '200')
RFdepths <- c('0', '5', '15', '30', '60', '100')
pedodepths <- c('1', '2', '3', '4', '5', '6')
props <- c('LL15', 'DUL')

outDF <- data.frame()

for (i in 1:length(props)) {
  prop = props[i]
  
  for (j in 1:length(MTdepths)) {
    print(paste0(prop, ' : ', MTdepths[j] ))
    r1 <- raster(paste0(awcRootDir, '/ApsoilRF/Maps/', prop, '_', RFdepths[j], '_mean.tif'))
    valsRF = extract(r1, locs)
    odf1 <- data.frame(sid=locs$SiteID, product='RF', depth=MTdepths[j], prop=prop, val=valsRF*100)
    r2 <- raster(paste0(awcRootDir, '/MeasuredTextures/Maps/', prop, '_', MTdepths[j], '_mean.tif'))
    valsMT = extract(r2, locs)
    odf2 <- data.frame(sid=locs$SiteID, product='MT', depth=MTdepths[j], prop=prop, val=valsMT)
    r3 <- raster(paste0(awcRootDir, '/PedoRasterMaps/Maps/', prop, '_', pedodepths[j], '.tif'))
    valsPM = extract(r3, locs)
    odf3 <- data.frame(sid=locs$SiteID, product='PM', depth=MTdepths[j], prop=prop, val=valsPM)
    outDF <- rbind(outDF, odf1, odf2, odf3)
  }
  
  
}


write.csv(outDF, 'Y:/Ross/TERN/AWC/SMProbes_AWC_Drills.csv', row.names = F)
outDF[outDF$sid == 'opSID_31710', ]


