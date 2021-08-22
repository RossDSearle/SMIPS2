library(stringr)
library(xts)

source('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/SMIPS2/SMIPS2Validations/Utils.R')

#sid='Cosmoz_10'; dataType='Soil-Moisture';depth=0; startDate='2016-01-01';endDate='2021-07-14'

probesAPI <- getAllProbeInfo()
probesDB <- getDBProbeSiteNames()

swParamsAll <- data.frame(sid=character(), depth=numeric(), minProbe=numeric(), minProbeDate=character(), 
                          maxProbe=numeric(), maxProbeDate=character(), thickness=numeric(),
                          modLL=numeric(), modDUL=numeric())

for (k in 1:nrow(probesDB)) {
  
  print(k)
      sid=probesDB$sid[k]
      pdf <- getAllProbeDataTS(sid=sid)
      
      if(!is.null(pdf)){
    
      awcs <- getAWCParamsForSite(sid)

        swp <- list()
        swp$LL15 <-  awcs$LL15_Mean
        swp$DUL <-  awcs$DUL_Mean
        slgaSpline <- splineVals(pl=swp, sid=sid)
        
        spdfProbe <- data.frame(sid=character(), depth=numeric(), minProbe=numeric(), minProbeDate=character(), 
                                maxProbe=numeric(), maxProbeDate=character(), thickness=numeric(),modLL=numeric(), modDUL=numeric())

        upperDepth=0
        
        for (j in 1:ncol(pdf)) {
          
          colname <- colnames(pdf)[j]
          depth <- as.numeric(str_split(colname, '_')[[1]][2])
          if(depth==0){depth=20}
          
          ts <- pdf[,j]
          qts <- quantile(ts, probs = c(0.05, 0.95), na.rm = T, names = TRUE)
          
          minVal <- as.numeric(qts[1])
          idx <- which.min(abs(ts-minVal))
          minDt <- index(ts[idx])
          
          maxVal <- as.numeric(qts[2])
          idx <- which.min(abs(ts-maxVal))
          maxDt <- index(ts[idx])
          
          slgall <- slgaSpline$LL15$est_1cm[slgaSpline$LL15$est_1cm$UD == (depth/10),]$SPLINED_VALUE / 100
          slgaul <- slgaSpline$DUL$est_1cm[slgaSpline$DUL$est_1cm$UD == (depth/10),]$SPLINED_VALUE / 100
          
          width=depth-upperDepth
          upperDepth=depth
          
          ol <- data.frame(sid=sid, depth=depth, minProbe=minVal, minProbeDate=minDt, maxProbe=maxVal, maxProbeDate=maxDt,
                           thickness=width, modLL=slgall, modDUL=slgaul)
          spdfProbe <- rbind(spdfProbe, ol)
          
        }
        swParamsAll <- rbind(swParamsAll, spdfProbe)
      }
}


write.csv(swParamsAll, 'C:/Projects/SMIPS/SMIPS2_Validations/ProbeSoilInfo/ProbeSoilInfo.csv',row.names = F)
