library(raster)
library(imager)
library(png)
library(stringr)
library(rasterVis)
library(xts)
library(lubridate)


dataRoot <- 'C:/Projects/EP/RegionalMoistureMaps'

outDir <- 'e:/temp/movie'
SMIn <- paste0('O:/processed/delivery/SMIPS')
AwraIn <- paste0('C:/Projects/SMIPS/SMEstimates/BOM/AWRANetCDFs/geotifsNew/sm_pct')
fls <- list.files(SMIn, full.names = T, recursive = T, pattern = '.tif')

dts <- seq.Date(as.Date('2016-01-01'), as.Date('2020-12-31') , 1)
colfunc <- colorRampPalette(c("brown", 'lightyellow', "darkblue"))

for (i in 1:length(dts)) {

  print(paste0(i, ' of ', length(dts)))
  png(filename = paste0(outDir, '/image-', i, '.png'), width = 1000, height = 1000)
  
  split.screen(matrix(c(0,1,  0,0.1,  0,1,  0.1,0.55,  0,1,  0.55,1), ncol=4, byrow = T)) # Makes Screen 1 and 2
  split.screen(c(1,2), screen=2) # Makes Screen 3 and 4
  split.screen(c(1,2), screen=3) # Makes Screen 3 and 4
  
  dt <- dts[i]  
  year(dt)
  bits <- str_split(dt, '-')
  yr <- bits[[1]][1]
  mn <- bits[[1]][2]
  dy <- bits[[1]][3]
  
  r1 <- raster(paste0(SMIn,'/v1.0.1/totalbucket/', yr, '/smips_totalbucket_mm_',yr, mn, dy, '.tif') )
  r2 <- raster(paste0(SMIn,'/v1.0.3/totalbucket/', yr, '/smips_totalbucket_mm_',yr, mn, dy, '.tif') )
  r3 <- raster(paste0(SMIn,'/v1.0.5/totalbucket/', yr, '/smips_totalbucket_mm_',yr, mn, dy, '.tif') )
  r4 <- raster(paste0(SMIn,'/v1.0.8/totalbucket/', yr, '/smips_totalbucket_mm_',yr, mn, dy, '.tif') )
 
  screen(4); par(mar = c(0.2, 0.2, 0.9, 0.2)); image(r1, col=colfunc(20),zlim=c(0,150),useRaster=T, main='ML-rain AWRA-Etp SMOS-smoothed',  axes=FALSE, xlab='', ylab='')
  screen(5); par(mar = c(0.2, 0.2, 0.9, 0.2)); image(r2, col=colfunc(20),zlim=c(0,150),useRaster=T, main='SILO-Rain AWRA-Etp SMOS-normal', legend=FALSE, axes=FALSE, box=FALSE, xlab='', ylab='')
  screen(6); par(mar = c(0.2, 0.2, 0.9, 0.2)); image(r3, col=colfunc(20),zlim=c(0,150),useRaster=T, main='ML-rain SILO-FAO56 SMOS-smoothed', legend=FALSE, axes=FALSE, box=FALSE, xlab='', ylab='')
  screen(7); par(mar = c(0.2, 0.2, 0.9, 0.2)); image(r4, col=colfunc(20),zlim=c(0,150),useRaster=T, main='AWRA-rain SILO-FAO56 SMOS-smoothed', legend=FALSE, axes=FALSE, box=FALSE, xlab='', ylab='')
 
  # par(mfrow=c(2,2))
  # image(r1, col=colfunc(20),zlim=c(0,150),useRaster=T, main='ML-rain AWRA-Etp SMOS-smoothed', legend=FALSE, axes=FALSE, box=FALSE, xlab='', ylab='')
  
  screen(1);par(mar = c(0, 0, 0, 0))
  
  x <- c(1, length(dts))
  y <- c(1.5, 1.5)
  plot.new(); plot.window(xlim=c(1, length(dts)),ylim=c(1,2) )
  lines (x, y1, col='gray', lwd=5)
  points(i, 1.5, pch=19, cex=2.2, col='blue')
  text(1, 1.2, '2016', cex = 1, font=2)
  text(365, 1.2, '2017', cex = 1, font=2)
  text(730, 1.2, '2018', cex = 1, font=2)
  text(1095, 1.2, '2019', cex = 1, font=2)
  text(1460, 1.2, '2020', cex = 1, font=2) 
  text(1825, 1.2, '2021', cex = 1, font=2)
  
  dev.off()
  close.screen(all = TRUE)    
  
}



args <- paste0(' -r 10  -i ',  'e:/temp/movie/image-%d.png -q:v 10 -y ',  'e:/temp/smips.avi')
#r above = frame rate
system2("C:/LocalProgs/FFmpeg/bin/ffmpeg.exe", args, stdout = T, stderr = T)


system2('e:/temp/smips.avi')

