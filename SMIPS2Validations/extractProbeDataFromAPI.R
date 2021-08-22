library(RSQLite)
library(jsonlite)
library(xts)
library(stringr)
library(httr)
library(sf)
library(ggplot2)
library(httr)
library(utils)

#source('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/SensorFederator/Harvesting/TSUtils.R')

# //fs1-cbr.nexus.csiro.au/{lw-sm-forecast}/work/processed/delivery/SMIPS

options(timeout= 4000000)

#startDate <- '2019-01-01'
startDate <- '2016-01-01'
startDate <- '2021-06-01'
endDate <- '2021-06-07'

starts <- c('2015-01-01', '2016-01-01','2017-01-01','2018-01-01','2019-01-01','2020-01-01','2021-01-01')
finishes <- c('2015-12-31','2016-12-31','2017-12-31','2018-12-31','2019-12-31','2020-12-31','2021-07-12')



rootDir <- 'C:/Projects/SMIPS/SMIPS2_Validations/ProbeDumps'
#rootDir <- 'e:/Projects/EP/RegionalMoistureMaps'
dir.create(rootDir, recursive = T)


url <- "http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorLocations?sensortype=Soil-Moisture&usr=ross.searle@csiro.au&pwd=S4QQBMk74zhnBnEpTcd6iLwlUredn6kekLkjFL"

locs <- fromJSON(url)
head(locs)
locs$SiteID


write.csv(locs, paste0(rootDir,'/probeInfo.csv'), row.names = F)

bdy <- st_read(paste0('C:/Projects/GIS/National/AustOutlineGeneralisedshp.shp'))
plot(bdy)

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
pLocs <- st_as_sf(x = locs,                         
                  coords = c("Longitude", "Latitude"),
                  crs = projcrs)

st_write(pLocs,  paste0(rootDir,'/probeLocations.shp') , append = F)

p <- ggplot(bdy) + geom_sf() + geom_sf(data = pLocs, shape = 16, col = "red", size = 5)
p

#colrs <- c('red', 'seagreen4', 'mediumpurple2', 'tan4', 'yellow', 'dimgray', 'chocolate', 'peachpuff', 'turquoise3', 'plum1')


#######  Create sites and sensors tables

dbfile <-  paste0(rootDir, '/probeDumps_SMIPS.db')
con <- dbConnect(RSQLite::SQLite(),dbfile )

urlsitdf <- "http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorLocations?usr=ross.searle@csiro.au&pwd=S4QQBMk74zhnBnEpTcd6iLwlUredn6kekLkjFL"
sitdf <- fromJSON(urlsitdf)
dbWriteTable(con, 'Sites', sitdf)

urlsitdf <- "http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorInfo?usr=ross.searle@csiro.au&pwd=S4QQBMk74zhnBnEpTcd6iLwlUredn6kekLkjFL"
sitdf <- fromJSON(urlsitdf)
dbWriteTable(con, 'Sensors', sitdf)

RSQLite::dbDisconnect(con)


#att <- 'Soil-Temperature'
att <- 'Soil-Moisture'
#att <- 'Rainfall'
locs$SiteID
### download from API
for (i in 223:nrow(locs)) {
  sid<-locs[i,]$SiteID
  print(paste0(i, ' of ', nrow(locs), ' - ', sid))
  
  for (j in 1:length(starts)) {
    
    s <- starts[j]
    e <- finishes[j]
    f <- paste0(rootDir, '/', sid, '$', s, '$', att, '.rds')
    
    if(!file.exists(f)){
      #url <- paste0("http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=", sid ,"&sensortype=Soil-Moisture&startdate=", s, "T00:00:00&enddate=", e, "T00:00:00&aggperiod=days")
      url <- paste0("http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=", sid ,"&sensortype=", att, "&startdate=", s, "T00:00:00&enddate=", e, "T00:00:00&aggperiod=days&usr=ross.searle@csiro.au&pwd=S4QQBMk74zhnBnEpTcd6iLwlUredn6kekLkjFL")
      
      print(url)
      resp <- GET(URLencode(url),  timeout(1800))
      if(resp$status_code == 200){
        df <- fromJSON(content(resp, 'text'))
        saveRDS(df, paste0(rootDir, '/', sid, '$', s, '$', att, '.rds'))
      }
    }
  }
}


rd <- 'C:/Projects/SMIPS/SMIPS2_Validations/ProbeDumps'
fls <- list.files(rootDir, pattern = '.rds', full.names = F)
idxs <- which(grepl('hussat_terrasonde', fls))
rfs <- fls[idxs]

for (i in 1:length(rfs)) {
  f <- rfs[i]
  bits <- str_split(f, '_')
  file.rename(paste0(rd, '/', f ), paste0(rd, '/', bits[[1]][1], '_', bits[[1]][2], '_',bits[[1]][3], '_',bits[[1]][4], '$', bits[[1]][5], '$', bits[[1]][6]))
}

idxs <- which(!grepl('hussat_terrasonde', fls))
rfs <- fls[idxs]

for (i in 1:length(rfs)) {
  f <- rfs[i]
  bits <- str_split(f, '_')
  file.rename(paste0(rd, '/', f ), paste0(rd, '/', bits[[1]][1], '_', bits[[1]][2], '$', bits[[1]][3], '$', bits[[1]][4]))
}









### stitch the years together

dbfile <-  paste0(rootDir, '/probeDumps_SMIPS.db')
con <- dbConnect(RSQLite::SQLite(),dbfile )

sql <- 'DROP TABLE ProbeData'
RSQLite::dbSendStatement(con, sql)

ctsql <- 'CREATE TABLE ProbeData (
  sid      VARCHAR,
  dataType VARCHAR,
  depth    DOUBLE,
  date     DATETIME,
  value    DOUBLE,
  PRIMARY KEY (
    sid ASC,
    dataType ASC,
    depth ASC,
    date ASC
  )
);'
RSQLite::dbSendStatement(con, ctsql)
idx1 <- 'CREATE INDEX idx1 ON ProbeData (sid);'
RSQLite::dbSendStatement(con, idx1)
idx2 <- 'CREATE INDEX idx2 ON ProbeData (dataType);'
RSQLite::dbSendStatement(con, idx2)
idx3 <- 'CREATE INDEX idx3 ON ProbeData (depth);'
RSQLite::dbSendStatement(con, idx3)
idx4 <- 'CREATE INDEX idx4 ON ProbeData (date);'
RSQLite::dbSendStatement(con, idx4)


# sql <- 'delete from ProbeData'
# RSQLite::dbSendStatement(con, sql)

fls <- list.files(rootDir, pattern = '.rds', full.names = T)

for (i in 1:length(fls)) {
  
  print(paste0(i, ' of ', length(fls)))
  f <- fls[i]
  fn <- str_remove(basename(f), '.rds')
  
  bits <- str_split(fn, '[$]')
  sid<- paste0(bits[[1]][1])
  dt <- bits[[1]][2]
  
  resp <- readRDS(f)
  
  bits <- str_split(resp$DataStream[[1]]$t, ' ')
  dys <- sapply(bits, function (x) x[1])
  
  for (k in 1:length(resp$UpperDepth)) {
    vals<-resp$DataStream[[k]]$v
    idxs <- which(!is.na(vals))
    if(!is.null(vals)){
      
      if(resp$UpperDepth[k] < 0){
        resp$UpperDepth[k] <- resp$UpperDepth[k] * -10
      }
      
      odf <- data.frame(sid=sid, dataType=att, depth=resp$UpperDepth[k], date=dys, value=resp$DataStream[[k]]$v)
      odf <- odf[idxs,]
      idxs2 <- which(odf$value >= 0 & odf$value <= 100)
      odf <- odf[idxs2,] 
      try(dbAppendTable(conn = con, name = 'ProbeData', value = odf),silent=F)
      
    }
  }
}

RSQLite::dbDisconnect(con)


### do some tidying up - fixes implemented above

dbfile <-  paste0(rootDir, '/probeDumps_SMIPS.db')
con <- dbConnect(RSQLite::SQLite(),dbfile )
dbListTables(con)

df <- RSQLite::dbReadTable(con,'ProbeData')

unique(df$depth)
idxs <- which(df$depth < 0)
df$depth[idxs] <- df$depth[idxs] * -10

min(df$value)
summary(df$value)
hist(df$value)
tail(df$value[order(df$value)], 1000)

unique(df$sid)



