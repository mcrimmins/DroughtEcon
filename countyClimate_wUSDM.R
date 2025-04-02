# get and process NCDC nClimDiv data
# ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/
# MAC 10/14/19

# load libraries, set dir ----
library(reshape2)
library(RCurl)
library(maps)
library(zoo)
library(SPEI)
library(tidyr)
library(raster)
library(xlsx)
library(dplyr)

# FIPS codes
data(county.fips)
countyDf <- map_data('county', 'arizona')
countyList<-unique(countyDf$subregion)

# get boundary
us<-getData('GADM', country='USA', level=2)

# capitalize county names
CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}

# Download data ----
# get directory listing and find most recent prcp file
url <- 'ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/'
filenames = getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
filelist<-unlist(strsplit(filenames,"\n"))
#precipFileName<-gsub("[\n]","",filelist[which((grepl("climdiv-pcpncy-v", filelist)) == TRUE)])
precipFileName<-filelist[which((grepl("climdiv-pcpncy-v", filelist)) == TRUE)]
# download precip file and format into data frame
url<-paste0("ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/",precipFileName)

precipData<-read.table(url, colClasses = c("character","numeric",
                                           "numeric","numeric","numeric","numeric","numeric",
                                           "numeric","numeric","numeric","numeric","numeric",
                                           "numeric"))
colnames(precipData)<-c("code",1:12)
precipData$var<-"precip"

# tmin
tminFileName<-filelist[which((grepl("climdiv-tmincy-v", filelist)) == TRUE)]
# download precip file and format into data frame
url<-paste0("ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/",tminFileName)

tminData<-read.table(url, colClasses = c("character","numeric",
                                           "numeric","numeric","numeric","numeric","numeric",
                                           "numeric","numeric","numeric","numeric","numeric",
                                           "numeric"))
colnames(tminData)<-c("code",1:12)
tminData$var<-"tmin"

# tmax
tmaxFileName<-filelist[which((grepl("climdiv-tmaxcy-v", filelist)) == TRUE)]
# download precip file and format into data frame
url<-paste0("ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/",tmaxFileName)

tmaxData<-read.table(url, colClasses = c("character","numeric",
                                         "numeric","numeric","numeric","numeric","numeric",
                                         "numeric","numeric","numeric","numeric","numeric",
                                         "numeric"))
colnames(tmaxData)<-c("code",1:12)
tmaxData$var<-"tmax"

# Wrangle data ----
allData<-rbind(precipData,tminData,tmaxData)
# parse into columns
allData$state<-(as.numeric(substr(allData$code, 1,2)))
allData$div<-(as.numeric(substr(allData$code, 3,5)))
allData$element<-(as.numeric(substr(allData$code, 6,7)))
allData$year<-(as.numeric(substr(allData$code, 8,11)))
#allData$fipsCode<-paste0(allData$state,allData$div)


# loop through counties in state - Arizona
datalist = list()

for(i in 1:length(countyList)){ 

    #county<-subset(us,NAME_2==countyName)
    countyName<-CapStr(countyList[i])
    county<-subset(us,NAME_2==countyName & NAME_1=="Arizona")
    ctyCentroid<-coordinates(county)
    
    # choose county, subset
    countyStName<-paste0("arizona,",countyList[i])
    fips<-as.numeric(substr(as.character(county.fips[which(county.fips$polyname==countyStName),1]),2,4))
    allDataSubset<-subset(allData, state==2 & div==fips)
    # melt data
    allDataSubset<-melt(allDataSubset, id.vars=c(14,18), measure.vars = 2:13)
    allDataSubset$date <- as.yearmon(paste(allDataSubset$year, as.numeric(allDataSubset$variable), sep = "-"))
    allDataSubset<-spread(allDataSubset, var, value)
    # sort, -999 to NA
    allDataSubset[allDataSubset == -9.99] <- NA
    allDataSubset[allDataSubset == -99.9] <- NA
    # trim to 2018
    allDataSubset<-allDataSubset[-(which(allDataSubset$year==2020)),]
    # calc tmean
    allDataSubset$tmean<-(allDataSubset$tmax+allDataSubset$tmin)/2
    
    
    # calculate drought indices
    spiData<-spi(ts(allDataSubset$precip,freq=12,start=c(1895,1)),3, na.rm = TRUE)
      allDataSubset$spi3<-spiData$fitted
    spiData<-spi(ts(allDataSubset$precip,freq=12,start=c(1895,1)),12, na.rm = TRUE)
      allDataSubset$spi12<-spiData$fitted
    # hargreaves
    allDataSubset$PET_Harg<-(hargreaves((5/9*(allDataSubset$tmin-32)),(5/9*(allDataSubset$tmax-32)), lat = ctyCentroid[1,2])/25.4)
    spiData<-spei(ts(allDataSubset$precip-allDataSubset$PET_Harg,freq=12,start=c(1895,1)),3, na.rm = TRUE)
      allDataSubset$spei3<-spiData$fitted
    spiData<-spei(ts(allDataSubset$precip-allDataSubset$PET_Harg,freq=12,start=c(1895,1)),12, na.rm = TRUE)
      allDataSubset$spei12<-spiData$fitted
    
    # # annual summaries
    # yearlySummary  <-allDataSubset %>% 
    #     group_by(year) %>% 
    #     summarize(tmean = mean(tmean),
    #               sumPrecip = sum(precip))
    # # diff from averages
    # yearlySummary$tmeanAnom<-yearlySummary$tmean-mean(yearlySummary$tmean)
    # yearlySummary$precipAnom<-yearlySummary$sumPrecip-mean(yearlySummary$sumPrecip)
    # # add in seasonal and annual drought indices - SPI
    # yearlySummary$JanDecSPI<-allDataSubset[which(allDataSubset$variable==12),9]
    # yearlySummary$JanMarSPI<-allDataSubset[which(allDataSubset$variable==3),8]
    # yearlySummary$AprJunSPI<-allDataSubset[which(allDataSubset$variable==6),8]
    # yearlySummary$JulSepSPI<-allDataSubset[which(allDataSubset$variable==9),8]
    # yearlySummary$OctDecSPI<-allDataSubset[which(allDataSubset$variable==12),8]
    # # add in seasonal and annual drought indices - SPEI
    # yearlySummary$JanDecSPEI<-allDataSubset[which(allDataSubset$variable==12),12]
    # yearlySummary$JanMarSPEI<-allDataSubset[which(allDataSubset$variable==3),11]
    # yearlySummary$AprJunSPEI<-allDataSubset[which(allDataSubset$variable==6),11]
    # yearlySummary$JulSepSPEI<-allDataSubset[which(allDataSubset$variable==9),11]
    # yearlySummary$OctDecSPEI<-allDataSubset[which(allDataSubset$variable==12),11]
    # 
    # # writing out county data
    # print(countyStName)
    # 
    # # Write the first data set in a new workbook
    # if(i==1){
    #   write.xlsx(yearlySummary, file = "AZCountyClimateData.xlsx",
    #             sheetName = countyName, append = FALSE)
    # }else{
    #   write.xlsx(yearlySummary, file = "AZCountyClimateData.xlsx",
    #              sheetName = countyName, append = TRUE)
    # }
    
    # add county name 
      allDataSubset$county<-countyName
    # put in list  
      datalist[[i]] <- allDataSubset
      
}

# all county data
allCounty = do.call(rbind, datalist)
allCounty$month<-as.numeric(format(allCounty$date, "%m"))

# load in USDM data
load("tempDFBind_AZ_USDM.RData")

# add in date fields
tempDFbind$year<-as.numeric(format(tempDFbind$week, "%Y"))
tempDFbind$month<-as.numeric(format(tempDFbind$week, "%m"))
tempDFbind$day<-as.numeric(format(tempDFbind$week, "%d"))
# thin to last in month
thinUSDM<- tempDFbind %>% group_by(region,year,month) %>% slice_max(day)

countyClim = merge(thinUSDM, allCounty, by.x=c("year","month","region"), by.y=c("year", "month","county"))

# summary plots
library(plotly)

p<-ggplot(countyClim, aes(spi3,Exceptional,color=as.factor(month),text=week))+
      geom_point()+
      geom_vline(xintercept = 0)+
      facet_wrap(~as.factor(region), ncol=3)+
      ggtitle("Exceptional % Coverage with monthly SPI3")
ggplotly(p,tooltip = c("spi3", "Exceptional", "week"))

p<-ggplot(countyClim, aes(spi12,Exceptional, color=as.factor(month),text=week))+
    geom_point()+
    geom_vline(xintercept = 0)+
    facet_wrap(~as.factor(region), ncol=3)+
    ggtitle("Exceptional % Coverage with monthly SPI12")
ggplotly(p,tooltip = c("spi12", "Exceptional", "week"))

p<-ggplot(countyClim, aes(spi3,Extreme, color=as.factor(month),text=week))+
  geom_point()+
  geom_vline(xintercept = 0)+
  facet_wrap(~as.factor(region), ncol=3)+
  ggtitle("Extreme % Coverage with monthly SPI3")
ggplotly(p,tooltip = c("spi3", "Extreme", "week"))

p<-ggplot(countyClim, aes(spi12,Extreme, color=as.factor(month),text=week))+
  geom_point()+
  geom_vline(xintercept = 0)+
  facet_wrap(~as.factor(region), ncol=3)+
  ggtitle("Extreme % Coverage with monthly SPI12")
ggplotly(p,tooltip = c("spi12", "Extreme", "week"))

p<-ggplot(countyClim, aes(spi3,Severe, color=as.factor(month),text=week))+
  geom_point()+
  geom_vline(xintercept = 0)+
  facet_wrap(~as.factor(region), ncol=3)+
  ggtitle("Severe % Coverage with monthly SPI3")
ggplotly(p,tooltip = c("spi3", "Extreme", "week"))

p<-ggplot(countyClim, aes(spi12,Severe, color=as.factor(month),text=week))+
  geom_point()+
  geom_vline(xintercept = 0)+
  facet_wrap(~as.factor(region), ncol=3)+
  ggtitle("Severe % Coverage with monthly SPI12")
ggplotly(p,tooltip = c("spi12", "Extreme", "week"))

##
# plot all cats together? Group by seasons, look at SPI-1, add in RPMS, NDVI estimates
##

