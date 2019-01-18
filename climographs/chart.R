#postscript("btr_chart.ps",height=600,width=800)
#bitmap(file="btr_chart.jpg",type="jpeg",width=9,height=6,res=288)
# Read data from the ACIS Daily Lister product into a data frame
daily=read.table('../data/btr_dly.csv', sep=',', header=1)
daily$Date = as.Date(daily$Date,"%Y-%m-%d")


# Convenience feature to make variable naming easier
attach(daily)

stationTitle = 'Baton Rouge Ryan Airport'

### All data are now available as lists. 
#   This upper section can be replaced with Python data extraction commands. 
###

# Divide the graphics window into 2 plotting areas. The top plot is
#  allocated 65% of the plotting window. The 'mar' parameter defines
#  margins for (bottom, left, top, right) for the upper plot area
layout(matrix(c(1,2)),heights=c(.65,.35))
par(mar=c(1.8,4,1.5,2.5))

# Find the range of values for the y axis of the upper plot
yrange=range(MaxTemp,MinTemp,normalMax,normalMin)
yvals = seq(yrange[1],yrange[2])
xrange=range(Date)

# Plot an empty frame, define tick mark intervals, draw and label axes

plot(Date,MaxTemp,ylim=yrange,type='n',ylab='Temperature [F]',xaxt='n',yaxt='n',main=stationTitle)
dd <- format(Date,'%d') == '01' | format(Date,'%d') == '15'
dd1 <- format(Date,'%d') == '01' | format(Date,'%m/%d') == '12/31'
dd2 <- format(Date,'%d') == '15'

# plot the axes. The 'las' parameter rotates the y-axis labels
axis(1,at=Date[dd], labels=format(Date[dd],'%d'), tick=FALSE,line=-1.2,cex.axis=0.5)
axis(1,at=Date[dd2], labels=format(Date[dd2],'%b'), tick=FALSE,line=-0.4,cex.axis=0.8)
axis(2,at=yvals[!(yvals%%10)], labels=yvals[!(yvals%%10)],las=1)
axis(4,at=yvals[!(yvals%%10)], labels=yvals[!(yvals%%10)],las=1)

# Define polygons based on date and 'normal' maximum and minimum values
px=c(Date,rev(Date))
py=c(normalMax,rev(normalMin))
polygon(px,py,col='pink',border=NA)


# Draw grid lines and marker lines for significant temperatures
abline(h=seq(-50,140,10),col='lightgray')
abline(v=Date[dd1],col='lightgray')
abline(h=c(32,90),lty=2)

# Plot vertical lines between daily maximum and minimum temperatures
segments(Date,MaxTemp,Date,MinTemp,col='red')
lines(Date,smooth((MaxTemp+MinTemp)/2,'3R'),col='black',lwd=2)

par(mar=c(3,4,.5,2.5))

# Determine the plotting ranges
prange=range(cumsum(normalPrcp),cumsum(Prcp))
pvals = seq(prange[1],prange[2])

# The 'plot' statement moves to the second plotting area and produces
#  a plot with defined axis ranges and the cumulative normal precip
plot(Date,cumsum(normalPrcp),type='s',col='blue',ylab='Precipitation [in]',xaxt='n',yaxt='n')	# Normal prcp

# Plot the axes
axis(1,at=Date[dd], labels=format(Date[dd],'%d'), tick=FALSE,line=-1.2,cex.axis=0.5)
axis(1,at=Date[dd2], labels=format(Date[dd2],'%b'), tick=FALSE,line=-0.4,cex.axis=0.8)
axis(1,at=Date[182], labels=format(Date[182],'%Y'), tick=FALSE,line=0.7,cex.axis=1.2)
axis(2,at=pvals[!(pvals%%10)], labels=pvals[!(pvals%%10)],las=1)
axis(4,at=pvals[!(pvals%%10)], labels=pvals[!(pvals%%10)],las=1)


# Draw grid lines 
abline(h=seq(0,200,10),col='lightgray')
abline(v=Date[dd1],col='lightgray')



# Plot the observed cummulative precipitation
lines(Date,cumsum(Prcp),type='s',col='darkgreen',lwd=2)



