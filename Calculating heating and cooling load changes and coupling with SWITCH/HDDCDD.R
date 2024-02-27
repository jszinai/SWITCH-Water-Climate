###########################
#This script takes the historical and climate scenario min and max daily temperatures from the downscaled 15 GCMs and calculates the CDD and HDD for each day, based on a 65 F threshhold
#The result outputs the HDD and CDD values for each day and each climate and historical scenario.

rm(list=ls())  #clear variables in R space

#replace file path here:
dat.dir <- ""

setwd(dat.dir)

#set time period for either historical or future study horizon
days <- seq(as.Date("2006/1/1"), as.Date("2099/12/31"), by = "day") #future

# Scenario <- "hist"; SkipMax=37;SkipMin=37; TN=23377
# Scenario <- "bcc-csm1-1_rcp85"; SkipMax=41; SkipMin=34;TN=34334
# Scenario <- "CESM1-BGC_rcp85"; SkipMax=41; SkipMin=34;TN=34334
# Scenario <- "CanESM2_rcp85"; SkipMax=41; SkipMin=34;TN=34334
# Scenario <- "ACCESS-1.0_rcp85"; SkipMax=41; SkipMin=34;TN=34334
# Scenario <- "CCSM_rcp85"; SkipMax=41; SkipMin=34;TN1=34335; TN2=34334
# Scenario <- "MPI-ESM-LR_rcp85"; SkipMax=41; SkipMin=34;TN1=34335; TN2=34334
# Scenario <- "HadGEM2-CC_rcp85"; SkipMax=41; SkipMin=34;TN1=34335; TN2=34334
# Scenario <- "HadGEM2-ES_rcp85"; SkipMax=41; SkipMin=34;TN1=34335; TN2=34334
# Scenario <- "MIROC5_rcp85"; SkipMax=41; SkipMin=34;TN1=34335; TN2=34334
##added Feb 21
# Scenario <- "CMCC-CM_rcp85"; SkipMax=41; SkipMin=34;TN1=34335; TN2=34334
# Scenario <- "CMCC-CMS_rcp85"; SkipMax=41; SkipMin=34;TN1=34335; TN2=34334
# Scenario <- "GFDL-CM3_rcp85"; SkipMax=41; SkipMin=34;TN1=34335; TN2=34334
# Scenario <- "GFDL-ESM2M_rcp85"; SkipMax=41; SkipMin=34;TN1=34335; TN2=34334
# Scenario <- "CNRM-CM5_rcp85"; SkipMax=41; SkipMin=34;TN1=34335; TN2=34334
Scenario <- "CESM1-CAM5_rcp85"; SkipMax=41; SkipMin=34;TN1=34335; TN2=34334

############################################

tmaxFiles     <- list.files(path=paste0("./cdd_hdd/",Scenario,"/tmintmax/"),pattern="tmax")
tmaxFilesLong <- list.files(path=paste0("./cdd_hdd/",Scenario,"/tmintmax/"),pattern="tmax",full.names=TRUE)

############################################
tminFiles     <- list.files(path=paste0("./cdd_hdd/",Scenario,"/tmintmax/"),pattern="tmin")
tminFilesLong <- list.files(path=paste0("./cdd_hdd/",Scenario,"/tmintmax/"),pattern="tmin",full.names=TRUE)

for (f in 1:length(tmaxFiles)) {
  tmax = read.csv(tmaxFilesLong[f], header=FALSE,skip=SkipMax,sep=",")
  tmax <- tmax[-c(dim(tmax)[1]),] #remove last row that has a nan. 
  tmax[c(dim(tmax)[1]),1] <-  as.numeric(tmax[c(dim(tmax)[1]-1),1])
  tmax$V2 <- NULL  # don't need 2nd column

  tmin = read.csv(tminFilesLong[f], header=FALSE,skip=SkipMin,sep=",")
  tmin <- tmin[-c(dim(tmin)[1]),] #remove last row that has a nan. 
  tmin[c(dim(tmin)[1]),1] <-  as.numeric(tmin[c(dim(tmin)[1]-1),1])
  tmin$V2 <- NULL

  dat <- cbind(days,tmin,tmax)
  names(dat) <- c("Date","Tmin","Tmax")
  dat$hdd <- round(((65-32)/1.8 - ((as.numeric(dat$Tmax)+as.numeric(dat$Tmin))/2)),2)
  dat$hdd[dat$hdd < 0] <- 0

  dat$cdd <- round(((as.numeric(dat$Tmax)+as.numeric(dat$Tmin))/2) - ((65-32)/1.8),2)
  dat$cdd[dat$cdd < 0] <- 0
  names(dat) <- c("Date","Tmin","Tmax","HDD","CDD")

  Name <- strsplit(tmaxFiles[f], "_t")
  write.table(c("Date,Tmin,Tmax,HDD,CDD"), file=paste0("./cdd_hdd/",Scenario,"/",unlist(Name)[1],"_CDD-HDD.csv"),
              row.names=FALSE,col.names=FALSE,quote=FALSE)
  write.table(dat, file=paste0("./cdd_hdd/",Scenario,"/",unlist(Name)[1],"_CDD-HDD.csv"),
              row.names=FALSE,col.names=FALSE,quote=FALSE,sep=",",append = TRUE)
  print(tmaxFiles[f])
  
}