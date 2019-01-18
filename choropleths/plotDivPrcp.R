# Provide support for shape files (sp library)
#
library(maps)

# Support for projections (you'll need this later...)
#
library(mapproj)

# Support for color selection for plots and maps
#
library(RColorBrewer)



getColorIndx = function(nclr,vals) {
        ratio<-(max(dfn)-min(dfn))/nclr
        brks<-round(seq(min(dfn),max(dfn),by=ratio),digits=0)
        colorIndx <- findInterval(dfn, brks, all.inside=T)
        return(colorIndx)
}


# read a shapefile
shp = readShapePoly('../clim_divisions/divisions.shp')

## get a subset of polygons
index=which(shp$ST=='LA' | shp$ST=='TX' | shp$ST=='MS' | shp$ST=='AR' | shp$ST=='OK' | shp$ST=='TN' )
srccdiv_shp = shp[index,]


###
## Plotting Avg Temp values for each Division
###

## The average yearly divisional temperatures are stored in divTavg.txt
## and divisional prcp totals are stored in divAvgPrcp.txt

## d_file is a data frame
#
d_file=read.csv("../data/divAvgPrcp.txt",header=0)
names(d_file)=c("ST","DIV","Prcp")
prcp=d_file$Prcp

## Color schemes

## set the number of colors
nclr <- 9

## choose a color palette of 'nclr' colors
##    help(brewer.pal)
##   display.brewer.all()  displays palettes
##   display.brewer.all(type='div') gives diverging color palettes
##      'seq' or 'qual' can also be specified
#
col_pal <- brewer.pal(nclr,"RdYlGn")

## dividing the range of values into the same number of intervals as number of colors 
#
ratio<-(max(prcp)-min(prcp))/nclr

## getting a sequence 'brks' with equally spaced intervals, 
##   each interval of size 'ratio' was calculated earlier
#
brks<-round(seq(min(prcp),max(prcp),by=ratio),digits=0)

## findInterval helps find where each prcp value fits 
##   in the 'brks' interval chosen earlier,
##   facilitates mapping each 'prcp 'value to a particular color
##   'brks' has the same length as the number of colors ('nclr')
#
colorIndx <- findInterval(prcp, brks, all.inside=T)

length(colorIndx)   # 53 color indices and/or climate divisions


## plot the subset of polygons ('srccdiv_shp') using the 
##  color indices in 'colorIndx'
#
plot(srccdiv_shp, col=col_pal[colorIndx], xlab="Longitude", ylab="Latitude")
title("2005 Prcp Total: SRCC Divisions")

## leglabs (short for legend labels) uses the seq 'brks' to set the labels
## leglabs is a part of maptools package, refer to help(plotpolys)
## Location of legend is givin in plotting coordinates
#
legend(-87.5, 30, legend=leglabs(brks), fill=col_pal, cex=1, bty="o")


## plots the centroid
poly_center=coordinates(srccdiv_shp)
points(poly_center,col='red',pch=20)

####
## Plotting divisional normal temperatures
##   same order of steps as earlier, except we read
##   divisional normal values, instead of prcp values 
##   as we did earlier.
####

## note this is a tab-delim file
norm_file=read.delim("../data/divNrmls.txt",header=0,sep=" ")
names(norm_file)=c("ST","DIV","normPrcp","normTA")
normPrcp=norm_file$normPrcp

## same order of color indexing as earlier
nclr <- 9
col_pal <- brewer.pal(nclr,"RdYlGn")
ratio<-(max(normPrcp)-min(normPrcp))/nclr
brks<-round(seq(min(normPrcp),max(normPrcp),by=ratio),digits=0)
colorIndx <- findInterval(normPrcp, brks, all.inside=T)
#colorIndx = getColorIndx(nclr,normPrcp)                                                                                                                                                 
plot(srccdiv_shp,col=col_pal[colorIndx],xlab="Longitude",ylab="Latitude")
title("Normal Divisional Prcp: SRCC Divisions")
legend(-87.5, 30, legend=leglabs(brks), fill=col_pal, cex=1, bty="o")


dfn = normPrcp - prcp
## same order of color indexing as earlier
nclr <- 9
col_pal <- brewer.pal(nclr,"RdYlGn")
ratio<-(max(dfn)-min(dfn))/nclr
brks<-round(seq(min(dfn),max(dfn),by=ratio),digits=0)
colorIndx <- findInterval(dfn, brks, all.inside=T)
                                                                                                                                
plot(srccdiv_shp,col=col_pal[colorIndx],xlab="Longitude",ylab="Latitude")
title("Deviation From Normal: SRCC Divisions")
legend(-87.5, 30, legend=leglabs(brks), fill=col_pal, cex=1, bty="o")


 
