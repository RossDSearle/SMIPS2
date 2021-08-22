library(stringr)
library(xts)
library(dygraphs)
library(hexbin)
library(RColorBrewer)
library(httr)

source('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/SMIPS2/SMIPS2Validations/Utils.R')

mVersions <- c('v1.0.0','v1.0.1','v1.0.2','v1.0.3','v1.0.4','v1.0.5','v1.0.6','v1.0.7','v1.0.8','v1.0.8','v1.1.0','v1.1.1','v1.1.2','v1.1.3')
HQprobes <- read.csv('C:/Projects/SMIPS/HQProbeDumps/allHighQualityProbeSites.csv', stringsAsFactors = F)
l=nrow(HQprobes)

url <- "http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorLocations?sensortype=Soil-Moisture&usr=ross.searle@csiro.au&pwd=S4QQBMk74zhnBnEpTcd6iLwlUredn6kekLkjFL"

locs <- fromJSON(url)
head(locs)
locs$SiteID


idxs <- which(grepl('Cosm',locs$SiteID))
cos <- locs[idxs,]
cos$SiteID

pbs <- getDBProbeSiteNames()
idxs <- which(grepl('Cosm',pbs$sid))
cos <- sort(pbs[idxs,])


sid='Cosmoz_11'; dataType='Soil-Moisture';depth=0; startDate='2017-01-01'; depth=20; endDate='2021-07-14';modelVer='v1.0.7'



modelTS <- getModelDataTS(sid=sid, version=modelVer, productType='totalbucket', startDate=startDate, endDate=endDate)
modelTS <- clipTS(modelTS)
head(modelTS)

rawProbeTS <- getProbeDataTS(sid=sid, productType='Soil-Moisture', depth=0, startDate=startDate, endDate=endDate)
head(rawProbeTS)

mRaw <- merge.xts(rawProbeTS*5, modelTS)
colnames(mRaw) <- c('rawProbe', 'Model')
dygraph(mRaw, main=sid) %>% dyRangeSelector()
rawFit<-fitStatsx(obsVal=mRaw$rawProbe, modelVal=mRaw$Model, att=sid, xName='RawProbe', yName='Model', verbose = F)

soilInfo <- getSoilInfo(sid)
thickness=500
volTS <- getLayerVolumeticTS(type='Available',rawProbeVals=rawProbeTS, soilParams=soilInfo, probeDepth=20, layerWidth = thickness)
volTS <- rollmean(volTS,10)

mVolTS <- merge.xts(volTS, modelTS)
colnames(mVolTS) <- c('Probes', 'Model')

dygraph(mVolTS, main=paste0('Total Available Water (mm), - ', sid)) %>% dyRangeSelector()
fs <- fitStatsx(obsVal=mVolTS$Probes, modelVal=mVolTS$Model, att=sid, xName='AvailWaterMM', yName='Model', verbose = F)





##  Cosmoz api docs - https://cosmoz.csiro.au/builder

url <- 'https://landscapes-cosmoz-api.tern.org.au/rest/stations/21/calibration'
url <- 'https://landscapes-cosmoz-api.tern.org.au/rest/stations/21'

cStations <- fromJSON('https://landscapes-cosmoz-api.tern.org.au/rest/stations')
csids <- cStations$stations[,1:2]

sNum = 11

url <- paste0('https://landscapes-cosmoz-api.tern.org.au/rest/stations/', sNum, '/observations?startdate=', startDate, 'T23:59:59.000Z&enddate=', endDate, 'T23:59:59.000Z&count=0')
df <- fromJSON(url)
head(df)

ts <- xts(x=df$observations$soil_moist_filtered, order.by=as.POSIXct(df$observations$time))
dygraph(ts, main=paste0(sid)) %>% dyRangeSelector()
head(ts)
odf <- data.frame(ts[,1], index(ts))
SMts <- apply.daily(as.xts(ts),FUN=mean)
plot(SMts)
dygraph(SMts, main=paste0(sid)) %>% dyRangeSelector()
mRaw <- merge.xts(rawProbeTS*5, SMts)








sid='Cosmoz_11'; dataType='Soil-Moisture';depth=0; startDate='2017-01-01';endDate='2021-07-14'
sid='VicAg_Speed'; dataType='Soil-Moisture';depth=0; startDate='2017-01-01';endDate='2021-07-14'
sid='VicAg_Hamilton'; dataType='Soil-Moisture';depth=0; startDate='2017-01-01';endDate='2021-07-14'
sid='VicAg_LakeBolac'; dataType='Soil-Moisture';depth=0; startDate='2017-01-01';endDate='2021-07-14'
sid='SFS_10'; dataType='Soil-Moisture';depth=0; startDate='2017-01-01';endDate='2021-07-14'



getModelTypes()




statDF <- data.frame(sid=character(l),
                     m_0R2 = numeric(l), m_0CC = numeric(l),
                     m_1R2 = numeric(l), m_1CC = numeric(l),
                     m_2R2 = numeric(l), m_2CC = numeric(l),
                     m_3R2 = numeric(l), m_3CC = numeric(l),
                     m_4R2 = numeric(l), m_4CC = numeric(l),
                     m_5R2 = numeric(l), m_5CC = numeric(l),
                     m_6R2 = numeric(l), m_6CC = numeric(l),
                     m_7R2 = numeric(l), m_7CC = numeric(l),
                     m_8R2 = numeric(l), m_8CC = numeric(l),
                     m_9R2 = numeric(l), m_9CC = numeric(l),
                     m_10R2 = numeric(l), m_10CC = numeric(l),
                     m_11R2 = numeric(l), m_11CC = numeric(l)
)

for (h in 1:length(mVersions)) {
  


for (g in 1:nrow(HQprobes)) {
  
  sid = HQprobes$SiteID[g]

      modelTS <- getModelDataTS(sid=sid, version=mVersions[h], productType='totalbucket', startDate=startDate, endDate=endDate)
      modelTS <- clipTS(modelTS)
     # head(modelTS)
    #  summary(modelTS)
      
      modelTS <- rollmean(modelTS,30)
      
      dps <- getDBProbeDepths(siteName=sid)
      
      if(nrow(dps)>0){
      
      upperList <- numeric(length = nrow(dps)+1)
      cnt=0
      for (i in 1:nrow(dps)) {
        
        rawProbeTS <- getProbeDataTS(sid=sid, productType='Soil-Moisture', depth=dps$depth[i], startDate=startDate, endDate=endDate)
        #summary(rawProbeTS)
        
        if(nrow(rawProbeTS) > 0){
          
          if(dps$depth[i] <= 900){
              cnt=cnt+1
              
              if(dps$depth[i]==0){
                pd <- 20
                dd <- 200
              }else{
                pd <- dps$depth[i]
                dd <- dps$depth[i]
              }
              
              thickness <- dd - max(upperList)
              upperList[i+1] <- dd
              #print(thickness)
              
              mRaw <- merge.xts(rawProbeTS, modelTS)
              colnames(mRaw) <- c('rawProbe', 'Model')
              dygraph(mRaw, main=sid) %>% dyRangeSelector()
              rwaFit<-fitStatsx(obsVal=mRaw$rawProbe, modelVal=mRaw$Model, att=sid, xName='RawProbe', yName='Model', verbose = F)
              
              soilInfo <- getSoilInfo(sid)
              
              volTS <- getLayerVolumeticTS(type='Available',rawProbeVals=rawProbeTS, soilParams=soilInfo, probeDepth=pd, layerWidth = thickness)
              volTS <- rollmean(volTS,10)
              #m <- merge.xts(volTS, modelTS * 0.1)
              #colnames(m) <- c('AvailWaterMM', 'Model')
              #dygraph(m, main=sid) %>% dyRangeSelector()
              #vbn<-fitStatsx(obsVal=m$AvailWaterMM, modelVal=m$Model, att=sid, xName='AvailWaterMM', yName='Model', verbose = F)
              
              #upperList[i+1] <- dps$depth[i]
              
              if(cnt==1){
                ots <- volTS
              }else{
                ots <- merge.xts(ots, volTS)
              }
          }
        }
        else{
          print(paste0('No data for ', sid, ' Depth = ', dps$depth[i]))
        }
      }
      
      
      rs <- rowSums(ots, na.rm = T)
      sumTS <- xts(x=rs, order.by = index(ots))
      mSum <- merge.xts(sumTS, modelTS)
      colnames(mSum) <- c('Probes', 'Model')
      
      dygraph(mSum, main=paste0('Total Available Water (mm), - ', sid)) %>% dyRangeSelector()
      fs <- fitStatsx(obsVal=mSum$Probes, modelVal=mSum$Model, att=sid, xName='AvailWaterMM', yName='Model', verbose = F)

      statDF[g, 1] <- sid
      statDF[g, (h*2)] <- fs[3,2]
      statDF[g, ((h*2)+1)] <- fs[4,2]
      
      print(paste0(sid, ' Model = ', mVersions[h], ' R2 = ',  fs[3,2], ' CCC = ',  fs[4,2]))
}
      
}


}

statDF <- sapply(statDF[, seq(from=2, to=24, by=1)], as.numeric )
head(statDF)
str(statDF)
write.csv(statDF, 'C:/Projects/SMIPS/SMIPS2_Validations/TotalVolumetricValidationStats.csv', row.names = F)

statDF <- read.csv('C:/Projects/SMIPS/SMIPS2_Validations/TotalVolumetricValidationStatsEdited.csv', stringsAsFactors = F)
str(statDF)
head(statDF)

r2 <- statDF[, seq(from=2, to=24, by=2)]
r2RowMeans <- rowMeans(r2)

cc <- statDF[, seq(from=3, to=25, by=2)]
ccRowMeans <- rowMeans(cc)

r2c <- statDF[, seq(from=2, to=24, by=2)]
r2ColMeans <- colMeans(r2c)

ccc <- statDF[, seq(from=3, to=25, by=2)]
ccColMeans <- colMeans(ccc)

ccc2 <- statDF[, seq(from=2, to=25, by=1)]
allMeans <- colMeans(ccc2)
all2 <- t(data.frame(c(NA, as.numeric(round(as.numeric(allMeans),2), NA, NA ))))
colnames(all2) <- colnames(statDF)

statDF$meanR2 <- as.numeric(r2RowMeans)
statDF$meanCC <- as.numeric(ccRowMeans)
write.csv(statDF, 'C:/Projects/SMIPS/SMIPS2_Validations/TotalVolumetricValidationStatsWithSummaries.csv', row.names = F)


##### did some edits and some formatting in excel

library(readxl)
library(sf)

df <- read_xlsx('C:/Projects/SMIPS/SMIPS2_Validations/TotalVolumetricValidationStatsWithSummaries.xlsx')

pts <- data.frame(sid=HQprobes$SiteID, HQprobes$Longitude, HQprobes$Latitude)
spt <- merge(pts, df, by.x='sid', by.y='sid')
head(spt)
colnames(spt)[2:3] <- c('Longitude', 'Latitude')
spdf <- st_as_sf(x = spt, coords = c('Longitude', 'Latitude'))
plot(st_geometry(spdf))

st_write(spdf, 'C:/Projects/SMIPS/SMIPS2_Validations/TotalVolumetricValidationStatsWithSummaries.shp')



