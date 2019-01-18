library(maptools)
library(mapproj)
library(RColorBrewer)
library(maps)
library(fields)
library(splancs)
source("mask.R")
source("kriging.R")
stateList = c("arkansas")

## read data, remove missing vals, and set color palette
color_pal = rev(tim.colors(64))
file = read.table("../data/ar_30days.txt",sep=",",header=1)
file=na.omit(file)
file = file[(file$numObs==30),]

## get the extent of the map
coords = map("state",region=stateList,plot=0)$range
lon=file$Longitude
lat=file$Latitude

zz_var = file$sumValue
#print(zz_var)
## restrict lonlat values to a box around arkansas
z=(lon <= coords[2]+1.0 &
	   lon >= coords[1]-1.0 &
	   lat <= coords[4]+1.0 &
	   lat >= coords[3]-1.0 &
	   !is.na(zz_var))
	
lonlat = matrix(c(lon,lat),ncol=2)
#print(lonlat)
lonlat = matrix(lonlat[z],ncol=2)
## restrict z values to a box around arkansas
zz_var=zz_var[z]

png("points.png")
plot(lonlat[,1],lonlat[,2],xlab='Longitude',
ylab='Latitude',pch=20,col='red')
map('state',region=stateList,add=1)
dev.off()

## material for class 2 on kriging starts here

#print(zz_var)
img = kriging(lonlat,zz_var)
names(img)=c('x','y','z')

proj_pts = mapproject(list(x=img$x,y=img$y),projection="lambert",par=c(25,35))
proj_img = list(x=proj_pts$x,y=proj_pts$y,z=img$z)
proj_img = mask(proj_img)

#postscript(file=paste("ar_30days.ps",sep=""),paper='us',width=800,height=600)
#bitmap(file="ar_30days.jpg",type="jpeg",width=9,height=6,res=288)
png("ar_30days.png")
map('state',region=stateList,proj="")
image.plot(proj_img,col=color_pal,xlab='Longitude',ylab='Latitude',add=1,projection="",horizontal=1)
contour(proj_img, add=TRUE,col='white')
map('state',region=stateList,add=1,proj="")
map('state',region=stateList,interior=0,add=1,lwd=3,proj="")
title("Rainfall Totals - April 2008")
#text(-90,26,"Data & Mapping: NOAA Southern Regional Climate Center",cex=0.8,font=3)
dev.off()
