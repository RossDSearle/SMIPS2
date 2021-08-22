library(stringr)
library(xts)
library(dygraphs)
library(hexbin)
library(RColorBrewer)

source('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/SMIPS2/SMIPS2Validations/Utils.R')

mVersions <- c('v1.0.0','v1.0.1','v1.0.2','v1.0.3','v1.0.4','v1.0.5','v1.0.6','v1.0.7','v1.0.8','v1.0.8','v1.1.0','v1.1.1','v1.1.2','v1.1.3')

HQprobes <- read.csv('C:/Projects/SMIPS/HQProbeDumps/allHighQualityProbeSites.csv', stringsAsFactors = F)
l=nrow(HQprobes)

DrillDBfile <-  paste0('C:/Projects/SMIPS/SMIPS2_Validations/SMIPS_Drills/SMIPS_Drills.db')

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
                     m_11R2 = numeric(l), m_11CC = numeric(l),
                     m_12R2 = numeric(l), m_12CC = numeric(l),
                     m_13R2 = numeric(l), m_13CC = numeric(l)
 )


# statDF <- data.frame(sid=character(l),
#                      m_12R2 = numeric(l), m_12CC = numeric(l),
#                      m_13R2 = numeric(l), m_13CC = numeric(l)
# )



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

write.csv(statDF, 'C:/Projects/SMIPS/SMIPS2_Validations/TotalVolumetricValidationStats_Extras.csv', row.names = F)





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



