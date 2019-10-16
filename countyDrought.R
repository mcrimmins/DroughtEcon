# extract and calculate climate/drought metrics for economic impact analysis
# MAC 10/14/2019

library(prism)
library(raster)
library(sp)
library(rgdal)
# library(leaflet)
# library(prism)
# library(maptools)

# set rasteroptions
rasterOptions(progress = 'text')

# labels 
countyName<-"Mohave"
# get boundary
us<-getData('GADM', country='USA', level=2)
county<-subset(us,NAME_2==countyName)
#county<-subset(us,NAME_2==countyName & NAME_1=="Nebraska")

# get PRISM stacks
options(prism.path = "/scratch/crimmins/PRISM/monthly/precip") 
precip<- ls_prism_data(absPath=T)[,2]
pptStack<-stack(precip)
pptMean <- t(extract(pptStack, county, fun=mean, df=TRUE, weights=FALSE, normalizeWeights=FALSE))
# tmin temperature
options(prism.path = "/scratch/crimmins/PRISM/monthly/tmin") 
tmin<- ls_prism_data(absPath=T)[,2]
tminStack<-stack(tmin)
tminMean <- t(extract(tminStack, county, fun=mean, df=TRUE, weights=FALSE, normalizeWeights=FALSE))
# tmax temperature
options(prism.path = "/scratch/crimmins/PRISM/monthly/tmax") 
tmax<- ls_prism_data(absPath=T)[,2]
tmaxStack<-stack(tmax)
tmaxMean <- t(extract(tmaxStack, county, fun=mean, df=TRUE, weights=FALSE, normalizeWeights=FALSE))
