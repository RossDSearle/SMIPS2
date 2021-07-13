library(jsonlite)
library(xts)
library(akima)
library(fields) 

locs <- fromJSON('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorLocations?sensorgroup=BoM&extendedSet=T')


df = data.frame(matrix(NA, ncol = 5, nrow = nrow(locs)))
colnames(df) <- c('Longitude', 'Latitude', 'maxTemp', 'minTemp', 'rain')

for (i in 1:nrow(locs)) {
  rec <- locs[i,]
  print(paste(i, ' of ', nrow(locs)))
  url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=', rec$SiteID, '&sensortype=Temperature')
  res <- fromJSON(url)
  if(is.null(res$error)){
    t <- res[1,]
    d <- t$DataStream[[1]]
    ts <-xts(x = d$v, order.by = as.POSIXct(d$t))
    maxTemps <- apply.daily(ts, max)
    minTemps <- apply.daily(ts, min)
    
    df[i,1] <- t$Longitude
    df[i,2] <- t$Latitude
    df[i,3] <- tail(maxTemps,1)
    df[i,4] <- tail(minTemps,1)
  }
  
  urlR <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=', rec$SiteID, '&sensortype=Rainfall')
  resR <- fromJSON(urlR)
  if(is.null(resR$error)){
    t <- resR[1,]
    d <- t$DataStream[[1]]
    if(ncol(d)>1){
      ts <-xts(x = d$v, order.by = as.POSIXct(d$t))
      rainfall <- apply.daily(ts, max)
      df[i,5] <- tail(rainfall,1)
    }
    
  }
  
}


df2 <- na.omit(df)

templateR <- raster('C:/Temp/clim/tmin_20210218.tif')
bomTemplate <- raster('c:/temp/bomT.tif')


#####  interpolation using fields package
tps <- Tps(data.frame(df2$Longitude, df2$Latitude), df2$rain)
p <- raster(bomTemplate)
p <- interpolate(p, tps)
r2 <- resample(p, templateR)
r3 <- mask(r2, templateR)
plot(r3)
writeRaster(r3, 'c:/temp/rain.tif')

tps <- Tps(data.frame(df2$Longitude, df2$Latitude), df2$maxTemp)
p <- raster(bomTemplate)
p <- interpolate(p, tps)
r2 <- resample(p, templateR)
r3 <- mask(r2, templateR)
plot(r3)
writeRaster(r3, 'c:/temp/maxTemp.tif')

tps <- Tps(data.frame(df2$Longitude, df2$Latitude), df2$minTemp)
p <- raster(bomTemplate)
p <- interpolate(p, tps)
r2 <- resample(p, templateR)
r3 <- mask(r2, templateR)
plot(r3)
writeRaster(r3, 'c:/temp/minTemp.tif')




#####  interpolation using Akima package
# r2 <- raster(xmn=min(df2$Longitude), xmx=max(df2$Longitude), ymn=min(df2$Latitude), ymx=max(df2$Latitude), res=0.02 )
# NAvalue( r2 ) <- -9999.0
# s = interp(x=df2$Longitude , y=df2$Latitude, z=df2$rain, nx=ncol(r2), ny=nrow(r2), duplicate = 'mean')
# r2[] <- t(s$z[])
# r3 <- flip(r2 , 2)
# r4 <- resample(r3, templateR)
# r5 <- mask(r4, templateR) 
# plot(r5)


