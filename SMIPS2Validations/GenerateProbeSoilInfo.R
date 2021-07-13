library(xts)
library(dygraphs)
library(jsonlite)
library(mpspline2)

getSLGAInfo <- function(loc){
  
  url <- paste0('http://www.asris.csiro.au/ASRISApi/api/SLGA/simple/Drill?longitude=', loc$Longitude, '&latitude=', loc$Latitude,'&layers=ALL&kernal=0&json=true')
  resp <- fromJSON(url)
  
  soilAtts <- resp$SoilAttributes$`<attributeName>k__BackingField`
  SLGAvals <- resp$SoilAttributes$SoilLayers
  
  outDF <- data.frame(Depths=c(2.5, 10, 22.5, 45, 80, 150))
  for (j in 1:(length(soilAtts)-2)) {
    attVals <- SLGAvals[[j]]
    outDF <- cbind(outDF, attVals$value)
  }
  
  outDF <- cbind(outDF, SLGAvals[[11]]$value)
  outDF <- cbind(outDF, SLGAvals[[12]]$value)
  
  colnames(outDF) <- c('Depth', soilAtts[1:10], 'RegolithDepth', 'SoilDepth')
  
  
  outDF$LL15 <- 17.40-(10.05 * outDF$`BULK-DENSITY`)+(0.34 * outDF$CLAY)-(0.02 * outDF$SAND)+(0.18*outDF$SILT)	
  outDF$DUL <- 48.98-(21.86 * outDF$`BULK-DENSITY`)+(0.36 * outDF$CLAY)-(0.06 * outDF$SAND)+(0.19 * sqrt(outDF$SILT)) + (2.90 * sqrt(outDF$SOC))
  outDF$EstAWC <- (outDF$DUL - outDF$LL15)
  
  return(outDF)
}

getModelledAWC <- function(loc){
  
  ptsLoc <- loc
  coordinates(ptsLoc) <- ~Longitude+Latitude
  
  fls <- list.files(awcRasterPath, full.names = T, pattern = '.tif$')
  idxs <- which(grepl('_CoV', fls))
  awcAll <- fls[-idxs]
  
  stk <- stack(awcAll)
  d <- extract(stk, ptsLoc, df=T)
  
  odf <- data.frame(ptsLoc$SiteID, d)
  return(odf)
}

splineVals <- function(pl){
  
  ol <- vector("list", length = length(pl))
  templt <- data.frame("SID" = mv[,1],
                           "UD" = c( 0, 5, 15, 30, 60, 100),
                           "LD" = c( 5, 15, 30, 60, 100, 200),
                           stringsAsFactors = FALSE)
  
 for (n in 1:length(pl)) {
   
   pdf <- data.frame(templt, VAL=pl[n])
   colnames(pdf) <- c('SID','UD', 'LD', 'VAL' )
   spline <- mpspline_tidy(obj = pdf, var_name = 'VAL')
   ol[[n]] <- spline
 }
  names(ol) <- names(pl)
  return(ol)
}


probeRoot <- 'C:/Projects/EP/RegionalMoistureMaps/Probes/ManuallyTidied'
awcRasterPath <- 'C:/Projects/EP/RegionalMoistureMaps/WaterRasters'

url <- "http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorLocations?sensorgroup=EPARF"
probes <- fromJSON(url)

ModelledAWCVals <- getModelledAWC(probes)

mv <- ModelledAWCVals[1,]

probePaths <- list.files(probeRoot, full.names = T)

swParamsAll <- data.frame(sid=character(), depth=numeric(), minProbe=numeric(), minProbeDate=Date(), maxProbe=numeric(), maxProbeDate=Date(), thickness=numeric(),
                     modLL=numeric(), modDUL=numeric(), slgaLL=numeric(), slgaDUL=numeric()
)


for (i in 1:length(probePaths)) {
  
  probePath <- probePaths[i]
  probeID <- str_remove(basename(probePath), '.rds' )
  probeRec <- probes[probes$SiteID == probeID,]
  probeTS <- readRDS(probePath)
  colnames(probeTS) <- str_replace_all(colnames(probeTS), 'Soil.Moisture_', 'Depth=')
 
  metDF <- read.csv(paste0('c:/Projects/EP/RegionalMoistureMaps/Climate/', probeID, '.csv'))
  metTS <- xts(metDF[, -1], order.by = as.Date(metDF$Date))
  mgts <- merge.xts(probeTS, metTS$rain, join = 'left')
  
  p <- dygraph(mgts, main = probeRec$SiteName)  %>% dyRangeSelector(dateWindow = c(start(mgts), end(mgts)))
  p
  
  mv <- ModelledAWCVals[ModelledAWCVals$ptsLoc.SiteID == probeID,]
  slgav <- getSLGAInfo(loc=probeRec)

  swp <- list()
  swp$LL15 <-  c( mv$LL0_5,  mv$LL5_15,  mv$LL15_30 , mv$LL30_60 , mv$LL60_100 , mv$LL100_200 )
  swp$DUL <- c( mv$DUL05,  mv$DUL5_15,  mv$DUL15_30, mv$DUL30_60, mv$DUL60_100, mv$DUL100_200)
  modSpline <- splineVals(swp)
  
  swp <- list()
  swp$LL15 <-  slgav$LL15
  swp$DUL <-  slgav$DUL
  slgaSpline <- splineVals(pl=swp)
  slgaSpline
  
   upperDepth=0
  
   spdfProbe <- data.frame(sid=character(), depth=numeric(), minProbe=numeric(), minProbeDate=Date(), maxProbe=numeric(), maxProbeDate=Date(), thickness=numeric(),
                          modLL=numeric(), modDUL=numeric(), slgaLL=numeric(), slgaDUL=numeric())
  
  for (j in 1:ncol(probeTS)) {

    colname <- colnames(probeTS)[j]
    depth <- as.numeric(str_split(colname, '=')[[1]][2])
    print(depth)
    
    idx <- which.min(probeTS[,j])
    minVal <- as.numeric(probeTS[,j][idx])
    minDt <- index(probeTS[,j][idx])
    
    idxm <- which.max(probeTS[,j])
    maxVal <- as.numeric(probeTS[,j][idxm])
    maxDt <- index(probeTS[,j][idxm])
    
    modll <- modSpline$LL15$est_1cm[modSpline$LL15$est_1cm$UD == (depth/10),]$SPLINED_VALUE
    modul <- modSpline$DUL$est_1cm[modSpline$DUL$est_1cm$UD == (depth/10),]$SPLINED_VALUE
    
    slgall <- slgaSpline$LL15$est_1cm[slgaSpline$LL15$est_1cm$UD == (depth/10),]$SPLINED_VALUE / 100
    slgaul <- slgaSpline$DUL$est_1cm[slgaSpline$DUL$est_1cm$UD == (depth/10),]$SPLINED_VALUE / 100
    
    width=depth-upperDepth
    upperDepth=depth
    
    ol <- data.frame(sid=probeID, depth=depth, minProbe=minVal, minProbeDate=minDt, maxProbe=maxVal, maxProbeDate=maxDt,
                     thickness=width, modLL=modll, modDUL=modul, slgaLL=slgall, slgaDUL=slgaul)
    spdfProbe <- rbind(spdfProbe, ol)
    
  }
   swParamsAll <- rbind(swParamsAll, spdfProbe)
  
}


write.csv(swParamsAll, 'C:/Projects/EP/RegionalMoistureMaps/Probes/AllProbeSoilParams.csv')






#######  Probably van delete below


url <- "http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorLocations?sensorgroup=EPARF"
probes <- fromJSON(url)

probeSoilInfo <- read.csv('C:/Projects/EP/RegionalMoistureMaps/Probes/AllProbeSoilParams.csv', stringsAsFactors = F)

probePath <- probePaths[30]
probeID <- str_remove(basename(probePath), '.rds' )
print(probeID)
probeRec <- probes[probes$SiteID == probeID,]
probeTS <- readRDS(probePath)
colnames(probeTS) <- str_replace_all(colnames(probeTS), 'Soil.Moisture_', 'Depth=')

probeSoil <- probeSoilInfo[probeSoilInfo$sid == probeID,]



soilAWCValues <- data.frame(probeSoil$sid, probeSoil$depth, probeSoil$modLL, probeSoil$modDUL)
colnames(soilAWCValues) <- c('sid', 'depth', 'll', 'dul')
#dt <- '2020-08-09'


dts <- seq.Date(as.Date('2020-01-09'), as.Date('2021-01-09'),1 )

f=100
f=200


for (f in 1:length(dts)) {
  dt <- dts[f]
  dayVals <- probeTS[dt]
  soilInfo <- probeSoilInfo[probeSoilInfo$sid==probeID,]
  pw <- getProfileWater(rawProbeVals = dayVals, soilParams = soilInfo)
  dfBucket <-getBucket(SiteID=probeID, soilAWCValues=soilAWCValues, volVals = pw, soilInfo = soilInfo)
  show(plotPAW(dfBucket = dfBucket, title=probeRec$SiteName, yscale=10))
  Sys.sleep(0.1)
}


bucket=dfBucket

getTotalAvailableSoilWater <- function(bucket){
    
    lvw_mm <- (bucket$VolWater - bucket$LL) / 100 * bucket$Thickness
    lawc_mm <- (bucket$DUL - bucket$LL) / 100 * bucket$Thickness
    l = list()
    l$AvailWater_mm = sum(lvw_mm)
    l$AWC_mm = sum(lawc_mm)
    return(l)
  
}

getProfileWater <- function(rawProbeVals, soilParams){
    volVals <- data.frame(depth=numeric(nrow(soilParams)), Vol=numeric(nrow(soilParams)))
    volVals[,] <- NA
    for (i in 1:nrow(soilParams)) {
      rec <- soilParams[i, ]
      volVal <- getVolumetricValue(pParams=soilParams, depth=rec$depth, probeVal=as.numeric(rawProbeVals[,i]) )
      volVals[i,1] <- rec$depth
      volVals[i, 2] <- volVal
    }
    return(volVals)
}



plotPAW <- function(dfBucket, title='PAW', yscale=10){
  
  sbx <- c(dfBucket$LL, rev(dfBucket$DUL))
  sby <- c(dfBucket$Depths, rev(dfBucket$Depths)) /yscale 
  svw <- c(dfBucket$LL,  rev(dfBucket$VolWater)) 
  # xm <-c(bp$ll,  rev(volVals$Vol))* 100
  # ym <- c(bp$depth, rev(bp$depth)) 
  
  
  plot( 0, type="n", main=paste(title), 
        xlab='Volumteric Soil Moisture (cm/cm)', ylab='Soil Depth (cm)',
        yaxs = "i", xaxs = "i", xlim = c(min(dfBucket$LL)-5, max(dfBucket$DUL) + 5), ylim = rev(range(c(0,110))),
        cex.lab = 1.5
  )
  
  polygon(sbx,sby,
          col=c("navajowhite3"),
          border=c("navajowhite3"),
          lwd=1, lty=c("solid"))
  
  polygon(svw,sby,
          col=c("lightskyblue"),
          border=c("lightskyblue"),
          lwd=1, lty=c("solid"))
}




getBucket <- function(SiteID, soilAWCValues, volVals, soilInfo){

  # bp<- soilAWCValues
  # x <- c(bp$ll, rev(bp$dul)) * 100
  # y <- c(bp$depth, rev(bp$depth)) 
  # xm <-c(bp$ll,  rev(volVals$Vol))* 100
  # ym <- c(bp$depth, rev(bp$depth)) 
  # dfBucket <- data.frame(LL=x,DUL=y)
  # dfWater <- data.frame(LL=xm,SW=ym)
  # dfDepths <- data.frame(Depth=soilInfo$depth, Thickness=soilInfo$thickness)
  # outDF <- c(dfBucket, dfWater, dfDepths)
  # return(outDF)
  
  bp<- soilAWCValues
  LL <- bp$ll * 100
  DUL <- bp$dul * 100
  Depths <- bp$depth
  VolWater <- volVals$Vol * 100
  Thickness= soilInfo$thickness
  outDF <- data.frame(LL,DUL, VolWater, Depths,Thickness)
  return(outDF)
  
}

getVolumetricValue <- function(pParams, depth, probeVal){
  
  rec <- pParams[pParams$depth == depth, ]
  minp <- rec$minProbe
  maxp <- rec$maxProbe
  dul <- rec$modDUL
  ll <- rec$modLL
  
  mmperpv <-  (dul-ll)/ (maxp - minp)
  dv <- dayVals$Soil.Moisture_1000
  vol <- ll + (mmperpv * (probeVal-minp))
  return(vol)
}






