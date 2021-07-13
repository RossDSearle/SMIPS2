library(raster)
library(RSQLite)
library(jsonlite)

rasterOptions(datatype="FLT4S", timer=TRUE, format='GTiff',progress="text",chunksize=1e+09,maxmemory=1e+09, overwrite=TRUE) # maxmemory = max no of cells to read into memory

awcRootDir <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m'



urlsitdf <- "http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorLocations?sensortype=Soil-Moisture&usr=ross.searle@csiro.au&pwd=S4QQBMk74zhnBnEpTcd6iLwlUredn6kekLkjFL"
sitdf <- fromJSON(urlsitdf)
locs <- sitdf
coordinates(locs) <- ~ Longitude + Latitude
plot(locs)

smipsRootDir <- '//fs1-cbr.nexus.csiro.au/{lw-sm-forecast}/work/processed/delivery/SMIPS'

versions <- c('v1.0.0')
products <- c('totalbucket')

yrs <- c('2015','2016','2017','2018','2019','2020','2021')


version = versions[1]
product = products[1]
inDir <- paste0(smipsRootDir, '/', version, '/', product)

fls <- list.files(path=inDir, pattern = '.tif$', full.names = T, recursive = T)
stk <- stack(fls)


urlsitdf <- "http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorLocations?usr=ross.searle@csiro.au&pwd=S4QQBMk74zhnBnEpTcd6iLwlUredn6kekLkjFL"
sitdf <- fromJSON(urlsitdf)


for (i in 1:length(fls)) {
  r<-raster(fls[i])
  extract(r, locs)
  
}