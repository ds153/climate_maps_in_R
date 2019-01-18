# Provide support for shape files (sp library)
#
library(maptools)

# Support for projections (you'll need this later...)
#
library(mapproj)

# Support for color selection for plots and maps
#
library(RColorBrewer)

## read.shape reads a shape file and returns an object of class 'Map'
## A shape file has 3 files associated with it, having
##     the following suffixes - .shp, .dbf and .shx
## In this case, we are reading in a shape file that has the climate
##     divisions of the US.

usdiv <- readShapePoly("../clim_divisions/divisions.shp")

## usdiv_shp has the polygon information for each climate division
#
usdiv_shp <- Map2poly(usdiv)

## usdiv_df has attribute information from the .dbf file for the climate divisions
##    (state names, cilmate division names, areas, etc.)
#
usdiv_df <- usdiv$att.data

## In order to get a subset of climate divisions for the 6 states of 
##    AR, LA, MS, OK, MS, TN and TX
## We use the subset command to get a subset of polygons and dbf information from the usdiv_shp
##     and usdiv_df, respectively
#
srccdiv_shp<-subset(usdiv_shp,usdiv_df$ST=="LA" | usdiv_df$ST=="AR" | usdiv_df$ST=="MS" | usdiv_df$ST=="TX" | usdiv_df$ST=="TN" | usdiv_df$ST=="OK")
#
srccdiv_df<-subset(usdiv_df,usdiv_df$ST=="LA" | usdiv_df$ST=="AR" | usdiv_df$ST=="MS"
| usdiv_df$ST=="TX" | usdiv_df$ST=="TN" | usdiv_df$ST=="OK" )



## This subset of information can be written to a new shape file called 'srccdiv'.
#
## write.polylistShape takes in the polygon information, and the dbf information to 
##   generate a new shape file that is written to the external filesystem. This can
##   then be used in other GIS packages such as ARCGIS or Geomedia.
##
## It will have 3 componenents srccdiv.shp, srccdiv.dbf, and srccdiv.shx

write.polylistShape(srccdiv_shp,srccdiv_df,"srccdiv")


###
## Plotting Avg Temp values for each Division
###

## The average yearly divisional temperatures are stored in divTavg.txt
## The divisions in that file are in the same order as the ordering
##    of divisions in the dbf information stored in srccdiv_df

## d_file is a data frame
#
d_file=read.csv("../data/divTavg.txt",header=0)
names(d_file)=c("ST","DIV","Tavg")
tavg=d_file$Tavg

## Color schemes

## set the number of colors
nclr <- 9

## choose a color palette of 'nclr' colors
##    help(brewer.pal)
##   display.brewer.all()  displays palettes
##   display.brewer.all(type='div') gives diverging color palettes
##      'seq' or 'qual' can also be specified
#
colours <- brewer.pal(nclr,"YlOrRd")

## dividing the range of values into the same number of intervals as number of colors 
#
ratio<-(max(tavg)-min(tavg))/nclr

## getting a sequence 'brks' with equally spaced intervals, 
##   each interval of size 'ratio' was calculated earlier
#
brks<-round(seq(min(tavg),max(tavg),by=ratio),digits=0)

## findInterval helps find where each tavg value fits 
##   in the 'brks' interval chosen earlier,
##   facilitates mapping each 'tavg 'value to a particular color
##   'brks' has the same length as the number of colors ('nclr')
#
colorIndx <- findInterval(tavg, brks, all.inside=T)

length(colorIndx)   # 53 color indices and/or climate divisions


## plot the subset of polygons ('srccdiv_shp') using the 
##  color indices in 'colorIndx'
#
plot(srccdiv_shp, col=colours[colorIndx], xlab="Longitude", ylab="Latitude")
title("2005 Avg Temperature: SRCC Divisions")

## leglabs (short for legend labels) uses the seq 'brks' to set the labels
## leglabs is a part of maptools package, refer to help(plotpolys)
## Location of legend is givin in plotting coordinates
#
legend(-87.5, 30, legend=leglabs(brks), fill=colours, cex=1, bty="o")


####
## Plotting divisional normal temperatures
##   same order of steps as earlier, except we read
##   divisional normal values, instead of tavg values 
##   as we did earlier.
####

## note this is a tab-delim file
norm_file=read.delim("divNrmls.txt",header=0,sep=" ")
names(norm_file)=c("ST","DIV","normPrcp","normTA")
normTA=norm_file$normTA

## same order of color indexing as earlier
nclr <- 9
colours <- brewer.pal(nclr,"Reds")
ratio<-(max(normTA)-min(normTA))/nclr
brks<-round(seq(min(normTA),max(normTA),by=ratio),digits=0)
colorIndx <- findInterval(normTA, brks, all.inside=T)
                                                                                                                                                 
plot(srccdiv_shp,col=colours[colorIndx],xlab="Longitude",ylab="Latitude")
title("Normal Divisional Temperatures: SRCC Divisions")
legend(-87.5, 30, legend=leglabs(brks), fill=colours, cex=1, bty="o")


## For students of R:
##   1) calculate departure of temperature from normal values
#     2) calculate % of normal values
#     3) produce a 4-panel plot of observed, normal, departure, and percentage
##
#     4) repeat 1-3 for precipitation
#           normals:  divNrmls.txt   (column 3 is precipitation)
#           observed: divAvgPrcp.txt (same...)
#
#   Comment all code that you produce so that you and I can
#      understand what you did.
#
