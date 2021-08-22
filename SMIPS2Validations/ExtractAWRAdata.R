
#################################
###  Author : Ross Searle         
###  Date : Wed Aug 18 12:33:45 2021                      
###  Project : SMIPS2
###  Purpose : Extract data from the BoM AWRA site, generate a netCDF store, drill at probe locations and dump into sqlite DB
#################################

library(XML)
library(XML2)
library(stringr)
library(jsonlite)
library(rCurl)
library(raster)
library(rgdal)

source('C:/Users/sea084/Dropbox/RossRCode/myFunctions/GeneralUtils.R')
source('C:/Users/sea084/Dropbox/RossRCode/myFunctions/VectorUtils.R')
source('C:/Users/sea084/Dropbox/RossRCode/myFunctions/DateTimeUtils.R')


rasterOptions(datatype="FLT4S", timer=TRUE, format='GTiff',progress="text",chunksize=1e+08,maxmemory=1e+09, overwrite=TRUE) # maxmemory = max no of cells to read into memory


library(raster)
library(RCurl)
library(ncdf4)
library(stringr)
library(httr)
library(jsonlite)
library(RSQLite)

rasterOptions(datatype="FLT4S", timer=TRUE, format='GTiff',progress="text",chunksize=1e+08,maxmemory=1e+09, overwrite=TRUE) 

source('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/AusSoilsDSM/generalFunctions/DateTimeUtils.R')

#outDir <- 'c:/temp'
#awncpath <- 'C:/Projects/SMIPS/SMIPSAnalysis/AWRA/sm_pct_2017_Actual_day.nc'



######## Get data from the AWRA website  #########

downloadRoot <- 'C:/Projects/SMIPS/SMIPS2_Validations/AWRA/Downloads'

att <- 's0_pct' #0-10cm
#att <- 'sm_pct' #0-90cm
att <- 'ss_pct' #10-90cm

yrs <- seq(2017, 2021, 1)

downloadPath <- paste0(downloadRoot, '/', att)
if(!dir.exists(downloadPath)){dir.create(downloadPath, recursive = T)}


##### Download Data From BOM #######

for(i in 1:length(yrs)){
  
  yr <- yrs[i]
  
  numDays <- days.in.year(yr)
  
  for(k in 1:numDays){
    
    print(paste0(yr, ' : ', k))
    
    out <- tryCatch(
      {
        outFile <- paste0(downloadPath, '/', yr, '_', k, '.txt')
        
        if(!file.exists(outFile)){
            #j <- RCurl::getURL(paste0('http://www.bom.gov.au/jsp/awra/thredds/dodsC/AWRA/values/day/sm_pct_', yr, '.nc.ascii?sm_pct[', k-1, ':1:',k-1,']')) 
            url <- paste0('http://www.bom.gov.au/jsp/awra/thredds/dodsC/AWRACMS/values/day/', att, '_', yr, '.nc.ascii?', att, '[', k-1, ':1:',k-1,']')
            
            #download.file(url = url, destfile = outFile, quiet = F)
            GET(url,user_agent("Mozilla/5.0"), write_disk(outFile, overwrite = T))
        }
        
        #cat(j, file = outFile)
        
      }, error=function(e){})
  }
}



######## Dump rasters into a NetCDF store  #########

templateR<- raster()
infile <- 'C:/Projects/SMIPS/SMIPS2_Validations/AWRA/Downloads/ss_pct/2017_1.txt'
dt <- read.table(infile, skip=12, nrows = 681 , sep = ',')
dt2 <- dt[,-1]
m <- as.matrix(dt2)
r <- raster(nrows=681, ncols=841, xmn=111.975, xmx= 154.025, ymn=-44.025, ymx=-9.975, crs=CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'),  vals=m)
plot(r)
NAvalue(r) <- -999

templateR <- r


outDir <- 'C:/Projects/SMIPS/SMIPS2_Validations/AWRA'
outFileName <- paste0(outDir, '/AWRA_', att, '.nc')


Longvector = seq(112, 154, by = 0.05)
Latvector = seq(-44, -10, by = 0.05)

daysInYrs <- days.in.year(yrs)
numDays <- sum(daysInYrs)


dimSD <- daysSinceDate('1899-12-31', paste0(yrs[1], '-01-01') )
dimED <- daysSinceDate('1899-12-31', paste0(yrs[length(yrs)],'-12-31') )

ds = seq(from=dimSD, to=dimED, by=1)
dimX = ncdim_def("Long", "degrees_north", Longvector)
dimY = ncdim_def("Lat", "degrees_south", Latvector)
dimT = ncdim_def("time", "days since 1899-12-31", seq(dimSD, dimED), calendar = 'gregorian', longname='time')
var3d = ncvar_def( att, "units", list(dimX,dimY,dimT), -999,  prec="double", compression=1)

nc = nc_create(outFileName, list(var3d))

dayCnt =1
for(i in 1:length(yrs)){
  
  yr <- yrs[i]

  numDays <- days.in.year(yr)

    for(k in 1:numDays){

      print(paste0(yr, ' : ', k))
      
      out <- tryCatch(
        {
         
          infile <- paste0(downloadPath, '/', yr, '_', k, '.txt')
            r <- raster(templateR)
            NAvalue(r) <- -999
            dt <- read.table(infile, skip=12, nrows = 681 , sep = ',')
            dt2 <- dt[,-1]
            m <- as.matrix(dt2)
            r[] <- m
            rm2 <- flip(r, direction='y')
            ncvar_put( nc, var3d, rm2[], start=c(1,1,dayCnt), count=c(-1,-1,1))
            
            
        }, error=function(e){})
      
      dayCnt <- dayCnt + 1
      
    }
    
}

nc_close(nc)




######## drill probe locations in the NetCDF store  #########

url <- "http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorLocations?sensortype=Soil-Moisture&usr=ross.searle@csiro.au&pwd=S4QQBMk74zhnBnEpTcd6iLwlUredn6kekLkjFL"

locs <- fromJSON(url)
head(locs)
locs$SiteID

brk <- brick(outFileName)


outDir <- 'C:/Projects/SMIPS/SMIPS2_Validations/AWRA/ProbeDrills'
for(i in 1: nrow(locs)){

    print(paste0('Extracting data for : ', locs$SiteID[i]))
    v <- t(extract(brk, rbind(c( locs$Longitude[i], locs$Latitude[i]))))
    
    vs <- as.numeric(v[,1])
    dts <- brk@z
    df<-data.frame( dts, vs, stringsAsFactors = F)
    df$Date <- as.character(df$Date)
    colnames(df) <- c('Date', "Value")
    write.csv(df, paste0(outDir, '/AWRA$', att, '$', locs$SiteID[i], '.csv'), row.names = F)
  }






######## Make an SQLite DB to store drills in  #########

dbfile <-  paste0('C:/Projects/SMIPS/SMIPS2_Validations/SMIPS_Drills/AWRA_Drills.db')
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




######## Dump drills into the SQLite DB  #########

dbfile <-  paste0('C:/Projects/SMIPS/SMIPS2_Validations/SMIPS_Drills/AWRA_Drills.db')
con <- dbConnect(RSQLite::SQLite(),dbfile )

fls <- list.files(outDir, full.names = T)

for (i in 1:length(fls)) {
  print(i)
  f <- read.csv(fls[i], stringsAsFactors = F)
  fn1 <- basename(fls[i])
  bits <- str_split(fn1, '[$]')
  sid <- str_remove( bits[[1]][3], '.csv')
  odf <- data.frame(sid=sid, model='AWRA', version=1, productType=att, date=f$Date, value=f$Value, stringsAsFactors = F)
  idxs <- which(!is.na(odf$value))
  odf <- odf[idxs,]
  dbAppendTable(conn = con, name = 'DrillData', value = odf)
  
  
}

dbDisconnect(con)



