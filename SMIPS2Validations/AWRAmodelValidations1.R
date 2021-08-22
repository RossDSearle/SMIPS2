library(stringr)
library(xts)
library(dygraphs)
library(hexbin)
library(RColorBrewer)

source('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/SMIPS2/SMIPS2Validations/Utils.R')


mVersions <- c('v1.0.0','v1.0.1','v1.0.2','v1.0.3','v1.0.4','v1.0.5','v1.0.6','v1.0.7','v1.0.8','v1.0.8','v1.1.0','v1.1.1')
HQprobes <- read.csv('C:/Projects/SMIPS/HQProbeDumps/allHighQualityProbeSites.csv', stringsAsFactors = F)
l=nrow(HQprobes)

DrillDBfile <-  paste0('C:/Projects/SMIPS/SMIPS2_Validations/SMIPS_Drills/AWRA_Drills.db')


sid='Cosmoz_11'; dataType='Soil-Moisture';depth=0; startDate='2017-01-01';endDate='2021-07-14'
sid='VicAg_Speed'; dataType='Soil-Moisture';depth=0; startDate='2017-01-01';endDate='2021-07-14'
sid='VicAg_Hamilton'; dataType='Soil-Moisture';depth=0; startDate='2017-01-01';endDate='2021-07-14'
sid='VicAg_LakeBolac'; dataType='Soil-Moisture';depth=0; startDate='2017-01-01';endDate='2021-07-14'
sid='SFS_10'; dataType='Soil-Moisture';depth=0; startDate='2017-01-01';endDate='2021-07-14'

getModelTypes()




statDF <- data.frame(sid=character(l),
                     m_0R2 = numeric(l), m_0CC = numeric(l)
                     
)



for (g in 1:nrow(HQprobes)) {
  
  sid = HQprobes$SiteID[g]

      modelTS <- getModelDataTS(sid=sid, model='AWRA', version='1.0', productType='ss_pct', startDate=startDate, endDate=endDate)
      modelTS <- modelTS * 100
      modelTS <- clipTS(modelTS)

      #modelTS <- rollmean(modelTS,30)
      
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
              rawFit<-fitStatsx(obsVal=mRaw$rawProbe, modelVal=mRaw$Model, att=sid, xName='RawProbe', yName='Model', verbose = F)
              
              soilInfo <- getSoilInfo(sid)
              
              volTS <- getLayerVolumeticTS(type='Available',rawProbeVals=rawProbeTS, soilParams=soilInfo, probeDepth=pd, layerWidth = thickness)
             
              
              # volTS <- rollmean(volTS,10)
             
              
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
      statDF[g, 2] <- fs[3,2]
      statDF[g, 3] <- fs[4,2]
      
      print(paste0(sid, ' Model = AWRA', ' R2 = ',  fs[3,2], ' CCC = ',  fs[4,2]))
  }
      
}


write.csv(statDF, 'C:/Projects/SMIPS/SMIPS2_Validations/AWRAValidationStats.csv', row.names = F)

statDF <- read.csv('C:/Projects/SMIPS/SMIPS2_Validations/AWRAValidationStats.csv', stringsAsFactors = F)
str(statDF)
head(statDF)

mean(statDF$m_0R2)
nrow(statDF)

smipsdfSum <- read.csv( 'C:/Projects/SMIPS/SMIPS2_Validations/TotalVolumetricValidationStatsEdited.csv', stringsAsFactors = F)
head(smipsdfSum)
nrow(smipsdfSum)
hqPrbs <- data.frame(smipsdfSum$sid)
colnames(hqPrbs) <- c('sid')
head(hqPrbs)

mdf <- merge(hqPrbs, statDF)
nrow(mdf)
mean(mdf$m_0R2)

write.csv(statDF, )

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



