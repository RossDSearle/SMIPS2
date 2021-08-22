library(raster)
library(imager)
library(png)
library(stringr)
library(rasterVis)
library(xts)
library(lubridate)


outDir <- 'e:/temp/movie'
SMIn <- paste0('O:/processed/delivery/SMIPS')

dts <- seq.Date(as.Date('2017-01-01'), as.Date('2021-06-30') , 1)
colfunc <- colorRampPalette(c("brown", 'lightyellow', "darkblue"))
colfunc2 <- colorRampPalette(c("red","green"))

# brk <- brick("E:/Projects/SMIPS/AWRA_ss_pct.nc")
# totAWC <- raster('Y:/Ross/TERN/AWC/SMIPS/SMIPS_Mean_AWC_Total_Resample.tif')



### Produce images  for all versions

for (i in 1:length(dts)) {
  
  print(paste0(i, ' of ', length(dts)))
  
  #### Start png file creation
  png(filename = paste0(outDir, '/image-', i, '.png'), width = 1250, height = 1000)
  
  #### Split up the graphics window into the various pllotting areas
  split.screen(matrix(c(0,1,  0,0.1,  0,1,  0.1,0.55,  0,1,  0.55,1), ncol=4, byrow = T)) # Makes Screen 1 and 2
  split.screen(c(1,3), screen=2) 
  split.screen(c(1,3), screen=3) 
 # split.screen(c(1,3), screen=4) 
  split.screen(matrix(c(0,0.9,  0,1,  0.9,1,  0,1), ncol=4, byrow = T), screen=1) 
  
  dt <- dts[i]  
  year(dt)
  bits <- str_split(dt, '-')
  yr <- bits[[1]][1]
  mn <- bits[[1]][2]
  dy <- bits[[1]][3]
  
  ##### Read in the SM rasters for the day
  
  # nid <- match(paste0('X', yr, '.',mn, '.', dy), names(brk))
  # ra<-brk[[nid]]
  

  awraR <- raster(paste0('e:/temp/AWRAComparisons/awra/awra_',yr, mn, dy, '.tif'))
  smipsIndexR  <- raster(paste0('e:/temp/AWRAComparisons/smipsindex/smips_totalbucketIndex_',yr, mn, dy, '.tif'))
  diffR <- raster(paste0('e:/temp/AWRAComparisons/compare/awraSmips_',yr, mn, dy, '.tif'))
  diffR <- clamp(diffR, lower=-1, upper=1)
  smipsIndexR <- clamp(smipsIndexR, lower=0, upper=1)
  
 # r0 <- raster(paste0(SMIn,'/v1.0.0/totalbucket/', yr, '/smips_totalbucket_mm_',yr, mn, dy, '.tif') )
   r1 <- raster(paste0(SMIn,'/v1.0.1/totalbucket/', yr, '/smips_totalbucket_mm_',yr, mn, dy, '.tif') )
  # r2 <- raster(paste0(SMIn,'/v1.0.2/totalbucket/', yr, '/smips_totalbucket_mm_',yr, mn, dy, '.tif') )
  # r3 <- raster(paste0(SMIn,'/v1.0.3/totalbucket/', yr, '/smips_totalbucket_mm_',yr, mn, dy, '.tif') )
  # r4 <- raster(paste0(SMIn,'/v1.0.4/totalbucket/', yr, '/smips_totalbucket_mm_',yr, mn, dy, '.tif') )
  # 
  # r5 <- raster(paste0(SMIn,'/v1.0.5/totalbucket/', yr, '/smips_totalbucket_mm_',yr, mn, dy, '.tif') )
  # r6 <- raster(paste0(SMIn,'/v1.0.6/totalbucket/', yr, '/smips_totalbucket_mm_',yr, mn, dy, '.tif') )
  r7 <- raster(paste0(SMIn,'/v1.0.7/totalbucket/', yr, '/smips_totalbucket_mm_',yr, mn, dy, '.tif') )
 #  r8 <- raster(paste0(SMIn,'/v1.0.8/totalbucket/', yr, '/smips_totalbucket_mm_',yr, mn, dy, '.tif') )
 #  
 #  r9 <- raster(paste0(SMIn,'/v1.0.9/totalbucket/', yr, '/smips_totalbucket_mm_',yr, mn, dy, '.tif') )
 # # r10 <- raster(paste0(SMIn,'/v1.1.0/totalbucket/', yr, '/smips_totalbucket_mm_',yr, mn, dy, '.tif') )
 # r11 <- raster(paste0(SMIn,'/v1.1.1/totalbucket/', yr, '/smips_totalbucket_mm_',yr, mn, dy, '.tif') )
 # r12 <- raster(paste0(SMIn,'/v1.1.2/totalbucket/', yr, '/smips_totalbucket_mm_',yr, mn, dy, '.tif') )
 # r13 <- raster(paste0(SMIn,'/v1.1.3/totalbucket/', yr, '/smips_totalbucket_mm_',yr, mn, dy, '.tif') )
  
  
  ##### Plot the rasters
  screen(4); par(mar = c(0.2, 0.2, 0.9, 0.2)); image(awraR, col=colfunc(20),zlim=c(0,1),useRaster=T, main='AWRA', cex.main=1,  axes=FALSE, xlab='', ylab='')
  screen(5); par(mar = c(0.2, 0.2, 0.9, 0.2)); image(smipsIndexR, col=colfunc(20),zlim=c(0,1),useRaster=T, main='SMIPS Index', cex.main=1, legend=FALSE, axes=FALSE, box=FALSE, xlab='', ylab='')
  screen(6); par(mar = c(0.2, 0.2, 0.9, 0.2)); image(diffR, col=colfunc2(20),zlim=c(-1,1),useRaster=T, main='AWRA minus SMIPS', cex.main=1, legend=FALSE, axes=FALSE, box=FALSE, xlab='', ylab='')
  #screen(8); par(mar = c(0.2, 0.2, 0.9, 0.2)); image(r3, col=colfunc(20),zlim=c(0,120),useRaster=T, main='SILO-Rain AWRA-Etp SMOS-normal', cex.main=1, legend=FALSE, axes=FALSE, box=FALSE, xlab='', ylab='')
  
  screen(7); par(mar = c(0.2, 0.2, 0.9, 0.2)); image(r1, col=colfunc(20),zlim=c(0,120),useRaster=T, main='ML-rain AWRA-Etp SMOS-smoothed', cex.main=1,  axes=FALSE, xlab='', ylab='')
  screen(8); par(mar = c(0.2, 0.2, 0.9, 0.2)); image(r7, col=colfunc(20),zlim=c(0,120),useRaster=T, main='AWRA-rain AWRA-Etp SMOS-smoothed', cex.main=1, legend=FALSE, axes=FALSE, box=FALSE, xlab='', ylab='')
 # screen(9); par(mar = c(0.2, 0.2, 0.9, 0.2)); image(r6, col=colfunc(20),zlim=c(0,120),useRaster=T, main='AWRA-rain SILO-FAO56 SMOS-normal', cex.main=1, legend=FALSE, axes=FALSE, box=FALSE, xlab='', ylab='')
  # screen(12); par(mar = c(0.2, 0.2, 0.9, 0.2)); image(r7, col=colfunc(20),zlim=c(0,120),useRaster=T, main='AWRA-rain AWRA-Etp SMOS-smoothed', cex.main=1, legend=FALSE, axes=FALSE, box=FALSE, xlab='', ylab='')
  # 
  # screen(13); par(mar = c(0.2, 0.2, 0.9, 0.2)); image(r8, col=colfunc(20),zlim=c(0,120),useRaster=T, main='AWRA-rain SILO-FAO56 SMOS-smoothed', cex.main=1,  axes=FALSE, xlab='', ylab='')
  # screen(14); par(mar = c(0.2, 0.2, 0.9, 0.2)); image(r9, col=colfunc(20),zlim=c(0,120),useRaster=T, main='SILO-Rain SILO-FAO56 SMOS-normal', cex.main=1, legend=FALSE, axes=FALSE, box=FALSE, xlab='', ylab='')
  # screen(15); par(mar = c(0.2, 0.2, 0.9, 0.2)); image(r12, col=colfunc(20),zlim=c(0,120),useRaster=T, main='MLPrecip_AWRA-ETp_SMOS-smoothed', cex.main=1, legend=FALSE, axes=FALSE, box=FALSE, xlab='', ylab='')
  # screen(16); par(mar = c(0.2, 0.2, 0.9, 0.2)); image(r13, col=colfunc(20),zlim=c(0,120),useRaster=T, main='AWRA-rain_AWRA-ETp_SMOS-smoothed', cex.main=1, legend=FALSE, axes=FALSE, box=FALSE, xlab='', ylab='')
  
  # par(mfrow=c(2,2))
  # image(r1, col=colfunc(20),zlim=c(0,150),useRaster=T, main='ML-rain AWRA-Etp SMOS-smoothed', legend=FALSE, axes=FALSE, box=FALSE, xlab='', ylab='')
  
  #####  Plot the timeline
  screen(10);par(mar = c(0, 0, 0, 0))
  
  x <- c(1, length(dts))
  y1 <- c(1.5, 1.5)
  plot.new(); plot.window(xlim=c(1, length(dts)),ylim=c(1,2) )
  lines (x, y1, col='gray', lwd=5)
  points(i, 1.5, pch=19, cex=2.2, col='blue')
  text(1, 1.2, '2017', cex = 1, font=2)
  text(365, 1.2, '2018', cex = 1, font=2)
  text(730, 1.2, '2019', cex = 1, font=2)
  text(1095, 1.2, '2020', cex = 1, font=2)
  text(1460, 1.2, '2021', cex = 1, font=2) 
  
  ####  Plot Legend
  screen(n = 9, new = T)
  par(mar = c(0.1, 0.1, 0.1, 0.1)) 
 # legend_image <- as.raster(matrix(rev(colfunc(20)), ncol=20))
  legend_image <- as.raster(matrix(colfunc(20), ncol=20))
  plot(c(0,4),c(0,2),type = 'n', axes = F,xlab = '', ylab = '')
  #text(x=1, y = seq(0,7,l=6), labels = seq(round(lowerBound),round(upperBound),l=6))
  #text(x=0.8, y = seq(0,6,l=5), labels = paste0(seq(round(0),round(120),l=5), ' mm'), cex=0.7, font=4)
  rasterImage(legend_image, xleft=0.1, ybottom=1.4, xright=4,ytop=1.6, main='Regionalised SM Map')
  text(x=2, y = 1.9,  'Soil Water (mm)', font=4)
  text(x=0.2, y = 1.7,  '0', font=4, cex=0.9)
  text(x=2, y = 1.7,  '60', font=4, cex=0.9)
  text(x=3.8, y = 1.7,  '120', font=4, cex=0.9)
  
  
  ####  Plot Difference Legend
  # screen(n = 9, new = T)
  # par(mar = c(0.1, 0.1, 0.1, 0.1)) 
  # legend_image <- as.raster(matrix(rev(colfunc(20)), ncol=20))
  legend_image <- as.raster(matrix(colfunc2(20), ncol=20))
 # plot(c(0,4),c(0,2),type = 'n', axes = F,xlab = '', ylab = '')
  rasterImage(legend_image, xleft=0.1, ybottom=0.1, xright=4,ytop=0.2, main='Regionalised SM Map')
  text(x=2, y = 0.6,  'AWRA minus SMIPS', font=4)
  text(x=0.5, y = 0.35,  'SMIPS Higher', font=4, cex=0.9)
  text(x=3.5, y = 0.35,  'AWRA Higher', font=4, cex=0.9)
 
  
  dev.off()
  close.screen(all = TRUE)  
  
}

#### Run ffmpeg to generte the movie

args <- paste0(' -r 10  -i ',  'e:/temp/movie/image-%d.png -q:v 10 -y ',  'e:/temp/smipsAll.avi')
#r above = frame rate
system2("C:/LocalProgs/FFmpeg/bin/ffmpeg.exe", args, stdout = T, stderr = T)

#system('C:/LocalProgs/VideoLAN/VLC/vlc file:///e:/temp/smipsAll.avi', wait=F)


outDir <- 'e:/temp/movieSingle'

for (i in 1:length(dts)) {
  
  print(paste0(i, ' of ', length(dts)))
  
  #### Start png file creation
  png(filename = paste0(outDir, '/image-', i, '.png'), width = 1000, height = 1000)
  
  dt <- dts[i]  
  year(dt)
  bits <- str_split(dt, '-')
  yr <- bits[[1]][1]
  mn <- bits[[1]][2]
  dy <- bits[[1]][3]
  
  ##### Read in the SM rasters for the day
  r7 <- raster(paste0(SMIn,'/v1.0.7/totalbucket/', yr, '/smips_totalbucket_mm_',yr, mn, dy, '.tif') )

  
  #### Split up the graphics window into the various pllotting areas
  split.screen(matrix(c(0,1,  0,0.2,  0,1,  0.2,1), ncol=4, byrow = T)) # Makes Screen 1 and 2
  split.screen(matrix(c(0,0.7,  0,1,  0.7,1,  0,1), ncol=4, byrow = T), screen=1) 
  
  
  
  ##### Plot the rasters
  screen(2); par(mar = c(0.2, 0.2, 0.9, 0.2)); image(r7, col=colfunc(20),zlim=c(0,120),useRaster=T, main='AWRA-rain AWRA-Etp SMOS-smoothed', cex.main=1, legend=FALSE, axes=FALSE, box=FALSE, xlab='', ylab='')
  
  #####  Plot the timeline
  screen(3);par(mar = c(0, 0, 0, 0))
  
  x <- c(1, length(dts))
  y1 <- c(1.5, 1.5)
  plot.new(); plot.window(xlim=c(1, length(dts)),ylim=c(1,2) )
  lines (x, y1, col='gray', lwd=5)
  points(i, 1.5, pch=19, cex=2.2, col='blue')
  text(1, 1.2, '2016', cex = 1, font=2)
  text(365, 1.2, '2017', cex = 1, font=2)
  text(730, 1.2, '2018', cex = 1, font=2)
  text(1095, 1.2, '2019', cex = 1, font=2)
  text(1460, 1.2, '2020', cex = 1, font=2) 
  text(1825, 1.2, '2021', cex = 1, font=2)
  
  ####  Plot Legend
  screen(n = 4, new = T)
  par(mar = c(0.1, 0.1, 0.1, 0.1)) 
  # legend_image <- as.raster(matrix(rev(colfunc(20)), ncol=20))
  legend_image <- as.raster(matrix(colfunc(20), ncol=20))
  plot(c(0,4),c(0,2),type = 'n', axes = F,xlab = '', ylab = '')
  #text(x=1, y = seq(0,7,l=6), labels = seq(round(lowerBound),round(upperBound),l=6))
  #text(x=0.8, y = seq(0,6,l=5), labels = paste0(seq(round(0),round(120),l=5), ' mm'), cex=0.7, font=4)
  rasterImage(legend_image, xleft=0.1, ybottom=0.8, xright=4,ytop=1, main='Regionalised SM Map')
  text(x=2, y = 1.9,  'Soil Water (mm)', font=4)
  text(x=0.2, y = 1.4,  '0', font=4, cex=0.9)
  text(x=2, y = 1.4,  '60', font=4, cex=0.9)
  text(x=3.8, y = 1.4,  '120', font=4, cex=0.9)
  
  dev.off()
  close.screen(all = TRUE)    
  
}

#### Run ffmpeg to generte the movie

args <- paste0(' -r 20 -i ',  'e:/temp/movieSingle/image-%d.png -q:v 10 -y ',  'e:/temp/smipsSingle.avi')
#r above = frame rate
system2("C:/LocalProgs/FFmpeg/bin/ffmpeg.exe", args, stdout = T, stderr = T)

#system('C:/LocalProgs/VideoLAN/VLC/vlc file:///e:/temp/smipsAll.avi', wait=F)
