library(raster)
library(RSQLite)
library(jsonlite)
library(stringr)
library(stringr)

rasterOptions(datatype="FLT4S", timer=TRUE, format='GTiff',progress="text",chunksize=1e+09,maxmemory=1e+09, overwrite=TRUE) # maxmemory = max no of cells to read into memory

awcRootDir <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m'



urlsitdf <- "http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorLocations?sensortype=Soil-Moisture&usr=ross.searle@csiro.au&pwd=S4QQBMk74zhnBnEpTcd6iLwlUredn6kekLkjFL"
sitdf <- fromJSON(urlsitdf)
locs <- sitdf
coordinates(locs) <- ~ Longitude + Latitude
plot(locs)

smipsRootDir <- '//fs1-cbr.nexus.csiro.au/{lw-sm-forecast}/work/processed/delivery/SMIPS'

versions <- c('v1.0.0','v1.0.1','v1.0.2','v1.0.3','v1.0.4','v1.0.5','v1.0.6','v1.0.7','v1.0.8','v1.0.9','v1.1.0','v1.1.1')
products <- c('totalbucket', 'bucket1', 'bucket2', 'deepD', 'runoff')

versions <- c('v1.0.1','v1.0.3','v1.0.5','v1.0.6','v1.0.7','v1.0.8','v1.0.9','v1.1.0','v1.1.1')
products <- c('totalbucket', 'bucket1', 'bucket2')


yrs <- c('2015','2016','2017','2018','2019','2020','2021')


dbfile <-  paste0('e:/projects/SMIPS', '/SMIPS_Drills.db')
con <- dbConnect(RSQLite::SQLite(),dbfile )

sql <- 'DROP TABLE DrillData'
RSQLite::dbSendStatement(con, sql)

ctsql <- 'CREATE TABLE DrillData (
  sid      VARCHAR,
  model  VARCHAR,
  version VARCHAR,
  productType VARCHAR,
  date     DATETIME,
  value    DOUBLE,
  PRIMARY KEY (
    sid ASC,
    model ASC,
    version ASC,
    productType ASC,
    date ASC
  )
);'
RSQLite::dbSendStatement(con, ctsql)
idx1 <- 'CREATE INDEX idx1 ON DrillData (sid);'
RSQLite::dbSendStatement(con, idx1)
idx2 <- 'CREATE INDEX idx2 ON DrillData (model);'
RSQLite::dbSendStatement(con, idx2)
idx3 <- 'CREATE INDEX idx3 ON DrillData (version);'
RSQLite::dbSendStatement(con, idx3)
idx4 <- 'CREATE INDEX idx4 ON DrillData (date);'
RSQLite::dbSendStatement(con, idx4)
idx5 <- 'CREATE INDEX idx5 ON DrillData (productType);'
RSQLite::dbSendStatement(con, idx5)




####  Populate the DB from the raster drills

model = 'SMIPS2'

for (cv in 1:length(versions) ){
  version = versions[cv]
  
  for (cp in 1:length(products) ){
      product = products[cp]
      
      inDir <- paste0(smipsRootDir, '/', version, '/', product)
      
      fls <- list.files(path=inDir, pattern = '.tif$', full.names = T, recursive = T)

        for (i in 1:length(fls)) {
          print(paste0(i, ' of ', length(fls), ' : ', product, ' - ', version))
          f <- fls[i]
          fb <- basename(f)
          fb2 <- str_remove(fb, '.tif')
          bits <- str_split(fb2, '_')
          ds <- bits[[1]][4]
          dt <- paste0(str_sub(ds,1,4), '-', str_sub(ds,5,6), '-', str_sub(ds,7,8))
          
          r<-raster(f)
          vals = extract(r, locs)
          odf <- data.frame(sid=locs$SiteID, model=model, version=version, productType=product, date=dt, value=vals, stringsAsFactors = F)
          idxs <- which(!is.na(odf$value))
          odf <- odf[idxs,]
          dbAppendTable(conn = con, name = 'DrillData', value = odf)
        }
  }
}



dbDisconnect(con)
