library(raster)
library(RSQLite)
library(jsonlite)
library(xts)

rasterOptions(datatype="FLT4S", timer=TRUE, format='GTiff',progress="text",chunksize=1e+09,maxmemory=1e+09, overwrite=TRUE) # maxmemory = max no of cells to read into memory
#awcRootDir <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m'
#smipsRootDir <- '//fs1-cbr.nexus.csiro.au/{lw-sm-forecast}/work/processed/delivery/SMIPS'


#dbfile <-  paste0(smipsRootDir, '/SMIPS_Drills.db')
dbfile <-  paste0('e:/projects/SMIPS', '/SMIPS_Drills.db')

#sid='Cosmoz_10';model='SMIPS2'; version='v1.0.0'; productType='totalbucket';startDate='2015-11-20';endDate='2015-11-25'

#getModelTSfromDBasDF(sid='Cosmoz_21',model='SMIPS2', version='v1.0.0', productType='totalbucket', startDate='2015-11-20', endDate='2020-11-25')

#ts <- getModelTSfromDBasTS(sid='Cosmoz_21',model='SMIPS2', version='v1.0.0', productType='totalbucket', startDate='2015-11-20', endDate='2020-11-25')
#plot(ts)


getModelTSfromDBasDF <- function(sid=NULL, model=NULL, version=NULL, productType=NULL, startDate=NULL, endDate=NULL){
 
  con <- dbConnect(RSQLite::SQLite(),dbfile ) 
  
  sql <- paste0("select * from drillData where sid='", sid, 
                                            "' and model = '", model, 
                                            "' and version = '", version, 
                                            "' and productType = '", productType, 
                                            "' and date >= '", startDate, 
                                            "' and date <= '", endDate, "'",
                                            " order by date"  )
  res <- dbSendQuery(con, sql)
  df <- dbFetch(res)
  dbClearResult(res)
  RSQLite::dbDisconnect(con)
  
  return(df)
}

getModelTSfromDBasTS <- function(sid=NULL, model=NULL, version=NULL, productType=NULL, startDate=NULL, endDate=NULL){
  
  df <- getModelTSfromDBasDF(sid=sid, model=model, version=version, productType=productType, startDate=startDate, endDate=endDate)
  
  ts <- xts(x=df$value, order.by = as.Date(df$date))
  colnames(ts)<-'Val'
  return(ts)
}