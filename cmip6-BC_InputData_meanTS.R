
# Objective: summarize the gridded time series into average time series for BC and each of the 9 ecoprovinces. 
#

library(scales)
library(raster)
library(rworldmap)
library(rworldxtra)
library(maps)
library(mapdata)
library(maptools)
library(sp)
library(colorRamps)
library(rgeos)
library(rgdal)


monthdays <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
seasons <- c("wt", "sp", "sm", "at")
seasonmonth.mat <- matrix(monthcodes[c(12, 1:11)],4, byrow=T)

dem.pts <- read.csv("outputs\\dem_cmip6eval.csv")
ecoprovs <- c("BC", sort(as.character(unique(dem.pts$id2))))
ecoprov.names <- c("British Columbia", "Boreal Plains", "Central Interior", "Coast and Mountains", "Georgia Depression", "Northern Boreal Mountains", "Sub-Boreal Interior", "Southern Interior Mountains", "Southern Interior", "Taiga Plains")
elements <- c("Tave", "Tmax", "Tmin", "PPT")



# ==========================================
#step 2d: create mean observational time series for province/ecoregion

obs.ts <- read.csv("outputs\\obs.ts.csv")

## station observations
ecoprov=ecoprovs[2]
for(ecoprov in ecoprovs){
  if(ecoprov=="BC") s <- 1:dim(obs.ts)[1] else {
    for(i in 1:length(unique(obs.ts$Year))){ if(i==1) s <- which(dem.pts$id2==ecoprov) else s <- c(s,which(dem.pts$id2==ecoprov)+length(dem.pts$id2)*(i-1))  }
  }
  ts <- aggregate(obs.ts[s,], by=list(obs.ts$Year[s]), FUN = mean, na.rm=T)[,-1]
  ts <- cbind(ts, (ts[,2:13]+ts[,14:25])/2)
  names(ts) <- c(names(ts)[1:37], paste("Tave", monthcodes, sep=""))
  
  element <- elements[1]
  for(element in elements){
    m <- seasons[1]
    for(m in seasons){
      seasonmonths <- seasonmonth.mat[which(seasons==m),]
      temp1 <- ts[,which(names(ts)==paste(element,seasonmonths[1], sep=""))]
      temp2 <- ts[,which(names(ts)==paste(element,seasonmonths[2], sep=""))]
      temp3 <- ts[,which(names(ts)==paste(element,seasonmonths[3], sep=""))]
      if(m=="wt") temp1[2:length(temp1)] <- temp1[1:(length(temp1)-1)] #advance december by one year (doesn't account for first year in series, but not a big deal)
      temp <- apply(cbind(temp1, temp2, temp3), 1, if(element=="PPT") "sum" else "mean")
      ts <- cbind(ts, temp)
    }
  }
  names(ts) <- c(names(ts)[1:49], paste(rep(elements, each=length(seasons)), rep(seasons, times=length(elements)), sep="_"))
  write.csv(ts,paste("C:\\Users\\CMAHONY\\OneDrive - Government of BC\\Projects\\2020_CMIP6Eval\\cmip6-BC\\data\\ts.obs.mean.", ecoprov, ".csv", sep=""), row.names=FALSE)
  print(ecoprov)
}


## ERA5 

era5.ts <- read.csv("outputs\\era5.ts.csv")

ecoprov=ecoprovs[2]
for(ecoprov in ecoprovs){
  if(ecoprov=="BC") s <- 1:dim(era5.ts)[1] else {
    for(i in 1:length(unique(era5.ts$Year))){ if(i==1) s <- which(dem.pts$id2==ecoprov) else s <- c(s,which(dem.pts$id2==ecoprov)+length(dem.pts$id2)*(i-1))  }
  }
  ts <- aggregate(era5.ts[s,], by=list(era5.ts$Year[s]), FUN = mean, na.rm=T)[,-1]
  ts <- cbind(ts, (ts[,2:13]+ts[,14:25])/2)
  names(ts) <- c(names(ts)[1:37], paste("Tave", monthcodes, sep=""))
  
  element <- elements[1]
  for(element in elements){
    m <- seasons[1]
    for(m in seasons){
      seasonmonths <- seasonmonth.mat[which(seasons==m),]
      temp1 <- ts[,which(names(ts)==paste(element,seasonmonths[1], sep=""))]
      temp2 <- ts[,which(names(ts)==paste(element,seasonmonths[2], sep=""))]
      temp3 <- ts[,which(names(ts)==paste(element,seasonmonths[3], sep=""))]
      if(m=="wt") temp1[2:length(temp1)] <- temp1[1:(length(temp1)-1)] #advance december by one year (doesn't account for first year in series, but not a big deal)
      temp <- apply(cbind(temp1, temp2, temp3), 1, if(element=="PPT") "sum" else "mean")
      ts <- cbind(ts, temp)
    }
  }
  names(ts) <- c(names(ts)[1:49], paste(rep(elements, each=length(seasons)), rep(seasons, times=length(elements)), sep="_"))
  write.csv(ts,paste("C:\\Users\\CMAHONY\\OneDrive - Government of BC\\Projects\\2020_CMIP6Eval\\cmip6-BC\\data\\ts.era5.mean.", ecoprov, ".csv", sep=""), row.names=FALSE)
  print(ecoprov)
}




# ==========================================
# step 3: GCM Files
# ==========================================

files <- list.files("outputs/", pattern=paste("^ts.*", sep="."))
gcms <- unique(sapply(strsplit(files, "[.]"), "[", 2))
gcms <- gcms[-grep("obs|era", gcms)]

i=1
for(i in 1:length(gcms)){
  gcm <- gcms[i]
  

  # ==========================================
  # step 3b: calculate average time series across BC/ecoprovince for each gcm and scenario
  
  files <- list.files("outputs/")
  files <- files[grep(paste("ts", gcm, sep="."), files)] #these ts (time series) files have one record for each grid cell for each year. 
  run.list <- sapply(strsplit(files, "[.]"), "[", 3)
  scenario.list <- sapply(strsplit(run.list, "_"), "[", 1)
  ripf.list <- sapply(strsplit(run.list, "_"), "[", 2)
  scenarios <- unique(scenario.list)
  
  scenario <- scenarios[1]
  for(scenario in scenarios){
    ripfs <- unique(ripf.list[which(scenario.list==scenario)])
    ripf <- ripfs[2]
    for(ripf in ripfs){
      data.full <- read.csv(paste("outputs\\ts.", gcm, ".", scenario, "_", ripf, ".csv", sep=""))
      ecoprov <- ecoprovs[1]
      for(ecoprov in ecoprovs){
        if(ecoprov=="BC") s <- 1:dim(data.full)[1] else {
          for(k in 1:length(unique(data.full$Year))){ if(k==1) s <- which(dem.pts$id2==ecoprov) else s <- c(s,which(dem.pts$id2==ecoprov)+length(dem.pts$id2)*(k-1))  }
        }
        data <- data.full[s,]
        Year <- data$Year
        x <- unique(data$Year)
        data <- cbind(data, (data[,2:13]+data[,14:25])/2)
        names(data) <- c(names(data)[1:37], paste("Tave", monthcodes, sep=""))
        
        ts <- aggregate(data, by=list(Year), FUN = mean, na.rm=T)[,-1]
        
        element <- elements[1]
        for(element in elements){
          m <- seasons[1]
          for(m in seasons){
            seasonmonths <- seasonmonth.mat[which(seasons==m),]
            temp1 <- ts[,which(names(ts)==paste(element,seasonmonths[1], sep=""))]
            temp2 <- ts[,which(names(ts)==paste(element,seasonmonths[2], sep=""))]
            temp3 <- ts[,which(names(ts)==paste(element,seasonmonths[3], sep=""))]
            if(m=="wt") temp1[2:length(temp1)] <- temp1[1:(length(temp1)-1)] #advance december by one year (doesn't account for first year in series, but not a big deal). required because winter is dec-jan-feb
            temp <- apply(cbind(temp1, temp2, temp3), 1, if(element=="PPT") "sum" else "mean")
            ts <- cbind(ts, temp)
          }
        }
        names(ts) <- c(names(ts)[1:49], paste(rep(elements, each=length(seasons)), rep(seasons, times=length(elements)), sep="_"))
        variables <- names(ts)[-1]
        
        assign(paste("ts.mean",ecoprov, ripf, sep="."), round(ts,1))
        # print(ecoprov)
      }
      print(ripf)
    }
    ecoprov=ecoprovs[1]
    for(ecoprov in ecoprovs){
      for(variable in variables){
        ensemble <- data.frame()
        for(ripf in ripfs){
          ts <- get(paste("ts.mean",ecoprov, ripf, sep="."))
          ensemble[1:length(x),which(ripfs==ripf)] <- ts[,which(names(ts)==variable)]
          # print(ripf)
        }
        names(ensemble) <- ripfs
        ensemble <- data.frame(Year=x, ensemble)
        write.csv(ensemble,paste("outputs\\ensemble", gcm, ecoprov, variable, scenario, "csv", sep="."), row.names=FALSE)
        # print(variable)
      }
      # print(ecoprov)
    }
    print(scenario)
  }
  
  print(gcm)
}

# ==========================================
# step 4: Ensemble Files. 
# min, max, and mean for each model and for whole ensemble
# ==========================================

temp <- read.csv(paste("outputs\\ts.obs.mean.BC.csv", sep=""))
variables <- names(temp)[-1]

files <- list.files("outputs//", pattern=paste("^ensemble.*", sep="."))
gcms.all <- unique(sapply(strsplit(files, "[.]"), "[", 2))
scenarios <- unique(sapply(strsplit(files, "[.]"), "[", 5))
funs <- c("min", "max", "mean")
fun <- funs[1]
for(fun in funs){
  scenario <- scenarios[1]
  for(scenario in scenarios){
    ecoprov <- ecoprovs[1]
    for(ecoprov in ecoprovs){
      variable <- variables[1]
      for(variable in variables){
        files <- list.files("outputs//", pattern=paste("^ensemble.*", ecoprov, variable, scenario,"*", sep="."))
        gcms <- unique(sapply(strsplit(files, "[.]"), "[", 2))
        gcm <- gcms[1]
        data <- read.csv(paste("outputs\\ensemble", gcm, ecoprov, variable, scenario, "csv", sep="."))
        temp <- data.frame(data[,1], matrix(NA, dim(data)[1],length(gcms.all)))
        names(temp) <- c("Year", gcms.all)
        for(gcm in gcms){
          data <- read.csv(paste("outputs\\ensemble", gcm, ecoprov, variable, scenario, "csv", sep="."))
          stat <- if(dim(data)[2]==2) data[,2] else round(apply(data[,-1], 1, fun),1)
          temp[match(data$Year, temp$Year),which(names(temp)==gcm)] <- stat
          # print(gcm)
        }
        temp <- cbind(temp, round(apply(temp[,-1], 1, fun, na.rm=T),1))
        names(temp) <- c("Year", gcms.all, "ensemble")
        write.csv(temp,paste(paste("outputs\\ens", fun, sep=""), ecoprov, variable, scenario, "csv", sep="."), row.names=FALSE)
        print(variable)
      }
      print(ecoprov)
    }
    print(scenario)
  }
  print(fun)
}

## rbind the scenarios together and write out. 
files <- list.files("outputs//", pattern=paste("^ensemble.*", sep="."))
scenarios <- unique(sapply(strsplit(files, "[.]"), "[", 5))
variables <- unique(sapply(strsplit(files, "[.]"), "[", 4))
ecoprovs <- unique(sapply(strsplit(files, "[.]"), "[", 3))
funs <- c("min", "max", "mean")
for(fun in funs){
  for(ecoprov in ecoprovs){
    for(variable in variables){
      for(scenario in scenarios){
        temp <- read.csv(paste(paste("outputs\\ens", fun, sep=""), ecoprov, variable, scenario, "csv", sep="."))
        data <- if(scenario==scenarios[1]) data.frame(scenario=rep(scenario, dim(temp)[1]), temp) else rbind(data, data.frame(scenario=rep(scenario, dim(temp)[1]), temp))
        # print(scenario)
      }
      write.csv(data,paste(paste("C:\\Users\\CMAHONY\\OneDrive - Government of BC\\Projects\\2020_CMIP6Eval\\cmip6-BC\\data\\ens", fun, sep=""), ecoprov, variable, "csv", sep="."), row.names=FALSE)
      # print(variable)
    }
    print(ecoprov)
  }
  print(fun)
}



