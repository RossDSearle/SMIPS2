library(raster)
library(imager)
library(png)
library(stringr)
library(rasterVis)
library(xts)
library(lubridate)


outDir <- 'e:/temp/smips/'
SMIn <- paste0('O:/processed/delivery/SMIPS')

dts <- seq.Date(as.Date('2017-01-01'), as.Date('2021-08-10') , 1)
colfunc <- colorRampPalette(c("brown", 'lightyellow', "darkblue"))

brk <- brick("E:/Projects/SMIPS/AWRA_ss_pct.nc")
totAWC <- raster('Y:/Ross/TERN/AWC/SMIPS/SMIPS_Mean_AWC_Total_Resample.tif')



for (i in 1:length(dts)) {
  
  print(paste0(i, ' of ', length(dts)))
  
  dt <- dts[i]  
  year(dt)
  bits <- str_split(dt, '-')
  yr <- bits[[1]][1]
  mn <- bits[[1]][2]
  dy <- bits[[1]][3]
  
  ##### Read in the SM rasters for the day
  
  nid <- match(paste0('X', yr, '.',mn, '.', dy), names(brk))
  ra<-brk[[nid]]
  #plot(ra)
 
  r7 <- raster(paste0(SMIn,'/v1.0.7/totalbucket/', yr, '/smips_totalbucket_mm_',yr, mn, dy, '.tif') )
  
  smipsIndex <- (r7/totAWC)
  writeRaster(smipsIndex, paste0(outDir, '/smipsIndex/smips_totalbucketIndex_',yr, mn, dy, '.tif'))
  
  ras <- resample(ra, totAWC)
  writeRaster(ras, paste0(outDir, '/awra/awra_',yr, mn, dy, '.tif'))
  
  compR <- ras-smipsIndex
  writeRaster(compR, paste0(outDir, '/compare/awraSmips_',yr, mn, dy, '.tif'))
  
}





