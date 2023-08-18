## intercomparison of various observational time series



## calculate regional average time series
elements <- c("Tmax", "Tmin", "PPT", "Tave")

dem.pts <- read.csv("C:\\Users\\CMAHONY\\OneDrive - Government of BC\\Projects\\2020_CMIP6Eval\\inputs\\dem_cmip6eval.csv")

ecoprovs <- c("BC", sort(as.character(unique(dem.pts$id2))))

for(ecoprov in ecoprovs){
  assign(paste("cbc", ecoprov, sep="."), read.csv(paste("data/ts.obs.mean.", ecoprov, ".csv", sep="")))
  assign(paste("era5", ecoprov, sep="."), read.csv(paste("data/ts.era5.mean.", ecoprov, ".csv", sep="")))
  assign(paste("pcic", ecoprov, sep="."), read.csv(paste("data/ts.pcic.mean.", ecoprov, ".csv", sep="")))
  assign(paste("cru", ecoprov, sep="."), read.csv(paste("data/ts.cru.mean.", ecoprov, ".csv", sep="")))
  print(ecoprov)
}

ecoprov <- ecoprovs[1]

cbc <- get(paste("cbc", ecoprov, sep="."))
era5 <- get(paste("era5", ecoprov, sep="."))
pcic <- get(paste("pcic", ecoprov, sep="."))
cru <- get(paste("cru", ecoprov, sep="."))

plot(cbc[which(cbc$Year%in%era5$Year),2], era5[which(era5$Year%in%cbc$Year),2])
lines(c(-9999,9999), c(-9999,9999), lty=2)
