library(stringr)
library(xts)
library(dygraphs)
library(hexbin)
library(RColorBrewer)
library(httr)
library(leaflet)

source('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/SMIPS2/SMIPS2Validations/Utils.R')

mVersions <- c('v1.0.0','v1.0.1','v1.0.2','v1.0.3','v1.0.4','v1.0.5','v1.0.6','v1.0.7','v1.0.8','v1.0.8','v1.1.0','v1.1.1','v1.1.2','v1.1.3')
HQprobes <- read.csv('C:/Projects/SMIPS/HQProbeDumps/allHighQualityProbeSites.csv', stringsAsFactors = F)
l=nrow(HQprobes)
DrillDBfile <-  paste0('C:/Projects/SMIPS/SMIPS2_Validations/SMIPS_Drills/smips_Drills.db')


url <- "http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorLocations?sensortype=Soil-Moisture&usr=ross.searle@csiro.au&pwd=S4QQBMk74zhnBnEpTcd6iLwlUredn6kekLkjFL"

locs <- fromJSON(url)
idxs <- which(grepl('Cosm',locs$SiteID))
cos <- locs[idxs,]
cos$SiteID


##  Cosmoz api docs - https://cosmoz.csiro.au/builder

url <- 'https://landscapes-cosmoz-api.tern.org.au/rest/stations/21/calibration'
url <- 'https://landscapes-cosmoz-api.tern.org.au/rest/stations/21'

cStations <- fromJSON('https://landscapes-cosmoz-api.tern.org.au/rest/stations')
str(cStations)
csids <- cStations$stations[,c(1,2,30)]

sNum = 11
sid = 'Cosmoz_11'
mVersion = 'v1.0.7'


mVersions <- c('v1.0.0','v1.0.1','v1.0.2','v1.0.3','v1.0.4','v1.0.5','v1.0.6','v1.0.7','v1.0.8','v1.0.8','v1.1.0','v1.1.1','v1.1.2','v1.1.3')

l=17
statDF <- data.frame(sid=character(l),
                     Raw0R2 = numeric(l), Raw0CC = numeric(l),  Adjusted0R2 = numeric(l), Adjusted0CC = numeric(l),
                     Raw1R2 = numeric(l), Raw1CC = numeric(l),  Adjusted1R2 = numeric(l), Adjusted1CC = numeric(l),
                     Raw2R2 = numeric(l), Raw2CC = numeric(l),  Adjusted2R2 = numeric(l), Adjusted2CC = numeric(l),
                     Raw3R2 = numeric(l), Raw3CC = numeric(l), Adjusted3R2 = numeric(l), Adjusted3CC = numeric(l),
                     Raw4R2 = numeric(l), Raw4CC = numeric(l),  Adjusted4R2 = numeric(l), Adjusted4CC = numeric(l),
                     Raw5R2 = numeric(l), Raw5CC = numeric(l), Adjusted5R2 = numeric(l), Adjusted5CC = numeric(l),
                     Raw6R2 = numeric(l), Raw6CC = numeric(l), Adjusted6R2 = numeric(l), Adjusted6CC = numeric(l),
                     Raw7R2 = numeric(l), Raw7CC = numeric(l), Adjusted7R2 = numeric(l), Adjusted7CC = numeric(l),
                     Raw8R2 = numeric(l), Raw8CC = numeric(l), Adjusted8R2 = numeric(l), Adjusted8CC = numeric(l),
                     Raw9R2 = numeric(l), Raw9CC = numeric(l), Adjusted9R2 = numeric(l), Adjusted9CC = numeric(l),
                     Raw10R2 = numeric(l), Raw10CC = numeric(l), Adjusted10R2 = numeric(l), Adjusted10CC = numeric(l),
                     Raw11R2 = numeric(l), Raw11CC = numeric(l), Adjusted11R2 = numeric(l), Adjusted11CC = numeric(l),
                     Raw12R2 = numeric(l), Raw12CC = numeric(l), Adjusted12R2 = numeric(l), Adjusted12CC = numeric(l),
                     Raw13R2 = numeric(l), Raw13CC = numeric(l), Adjusted13R2 = numeric(l), Adjusted13CC = numeric(l)
)

#h=8

for (h in 1:length(mVersions)) {

  
  mVersion <-mVersions[h]
  
for (i in 1:17) {

print(i)
      sname <- csids$site_name[i]
      print(sname)
      sid <- paste0('Cosmoz_', csids$site_no[i])
      cid <- csids$site_no[i]
      
      url <- paste0('https://landscapes-cosmoz-api.tern.org.au/rest/stations/', cid, '/observations?startdate=', startDate, 'T23:59:59.000Z&enddate=', endDate, 'T23:59:59.000Z&count=0')
      df <- fromJSON(url)
      
      if(length(df$observations)>0){
            rawCosTS <- apply.daily(xts(x=df$observations$soil_moist_filtered, order.by=as.Date(df$observations$time)),FUN=mean)
            rawCosTS <- rollmean(rawCosTS,10)
            rawCRainTS <- apply.daily(xts(x=df$observations$rainfall, order.by=as.Date(df$observations$time)),FUN=sum)
            crTS <- merge.xts(rawCosTS,rawCRainTS)
            
           # dygraph(crTS, main=paste0(sid, ' - ', sname)) %>% dyRangeSelector()
           # showSiteMap(sid, zoom=5)
            
            modelTS <- getModelDataTS(sid=sid, version=mVersion, productType='totalbucket', startDate=startDate, endDate=endDate)
            modelTS <- clipTS(modelTS)
            #volTmodelTSS <- rollmean(modelTS,10)
      
            
            soilInfo <- getSoilInfo(sid=sid)
            
            rawCompTS <- merge.xts(rawCosTS, modelTS)
            colnames(rawCompTS) <- c('rawCoz', 'rawModel')
          #  dygraph(rawCompTS, main=paste0(sid)) %>% dyRangeSelector()
            print('Raw TS Fit Stats')
            rawfs <- fitStatsx(obsVal =rawCompTS$rawCoz, modelVal = rawCompTS$rawModel )
            print(rawfs)

            depthTS <-  apply.daily(xts(x=df$observations$effective_depth, order.by=as.Date(df$observations$time)),FUN=mean)
          #  dygraph(depthTS, main=paste0(sid)) %>% dyRangeSelector()
            
            #####   clip to lower limit
            rawAvailCoz <- rawCosTS-(soilInfo$modLL*100)
            rawAvailCoz[rawAvailCoz<0] <- 0
            
            #dygraph(rawAvailCoz, main=paste0(sid)) %>% dyRangeSelector()
            depWtCoz <- rawAvailCoz*(depthTS/10)
            #depWtCoz <- rawAvailCoz*(17/90)
            #dygraph(depWtCoz)
            
            # mv <- mean(depthTS)
            # mTS <- merge.xts(modelTS*(mv/90), depWtCoz)
            # #mv <- median(depthTS)
            mTS <- merge.xts(modelTS*(20/90), depWtCoz)
            colnames(mTS) <- c('dw_Model', 'dw_Coz')
           # dygraph(mTS, main=paste0(sid, ' - ', sname)) %>% dyRangeSelector()
            fs <- fitStatsx(obsVal=mTS$dw_Coz, modelVal = mTS$dw_Model )
            
            
            statDF[i, 1] <- sid
            statDF[i, ((h*4)+1)-3] <- rawfs[3,2]
            statDF[i, ((h*4)+2)-3] <- rawfs[4,2]

            statDF[i, ((h*4)+3)-3] <- fs[3,2]
            statDF[i, ((h*4)+4)-3] <- fs[4,2]
            
            # statDF[i, 1] <- sid
            # statDF[i, 2] <- rawfs[3,2]
            # statDF[i, 3] <- rawfs[4,2]
            # 
            # statDF[i, 4] <- fs[3,2]
            # statDF[i, 5] <- fs[4,2]
            
            
            
            print("Adjusted Stats")
            print(paste0(sid, ' Model = ', mVersions[h], ' R2 = ',  fs[3,2], ' CCC = ',  fs[4,2]))
            
            #showSiteMap(sid, zoom=10)
            
      }
     
}

}

write.csv(statDF, 'C:/Projects/SMIPS/SMIPS2_Validations/SMIPS_validations_Full_Final.csv')

######   clip to upper limit
# rawCosTS[rawCosTS>(soilInfo$modDUL*100)] <- (soilInfo$modDUL*100)
# rawAvailCoz <- rawCosTS-(soilInfo$modLL*100)
# rawAvailCoz[rawAvailCoz<0] <- 0
 
  