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

sid='VicAg_LakeBolac'
sid='VicAg_Hamilton'
sid='SFS_6'
sid='VicAg_Birchip'

newTs <- compareModelAtSite(sid=sid, dataType='Soil-Moisture',depth=0, startDate='2017-01-01',endDate='2021-07-14',mVersion='v1.1.2')
dygraph(newTs, main=paste0('Total Available Water (mm), - ', sid, ' V=',mVersion)) %>% dyRangeSelector()
colnames(newTs) <- c('Probes', 'FullSMOS')
oldTs <- compareModelAtSite(sid=sid, dataType='Soil-Moisture',depth=0, startDate='2017-01-01',endDate='2021-07-14',mVersion='v1.0.7')
colnames(oldTs) <- c('Probes', 'halfSMOS')
dygraph(oldTs, main=paste0('Total Available Water (mm), - ', sid, ' V=',mVersion)) %>% dyRangeSelector()
cts <- merge.xts(oldTs, newTs)
dygraph(cts, main=paste0('Total Available Water (mm), - ', sid)) %>% dyRangeSelector()


showSiteMap(sid)


    
    
 
  