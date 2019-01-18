kriging = function(xy,z,color_pal,plot_title) {
	mx=250; nx=250
	# Smooting parameter, lambda - must be > 0.0  Larger values give more smoothing
	lmbd = 0.01
	#fit=krig.image(xy,z,cov.function=Exp.image.cov,m=mx,n=nx,lambda=lmbd,theta=1,kmax=1000)
	fit = Krig(lonlat,zz_var,cov.function="stationary.cov",lambda=lmbd,theta=1)
	print(str(fit))
	img=list(fit$surface$x,fit$surface$y,fit$surface$z)
	#bitmap(file='diagnostics1.jpg',type='jpeg',height=6,width=9,res=144)
	#set.panel(2,2)
	#plot(fit)
	#dev.off()
	
	# predict using 7.5 effective degrees of freedom:
	predict( fit, df=7.5)
	# predict on a grid ( grid chosen here by defaults)
	out<- predictSurface( fit)
	surface( out, type="C")
	print(str(surface))
	print(summary(fit))
	map('state','arkansas',add=1)
	res<- predict( fit, fit$xM) - fit$yM
	print(res)
	print(summary(res))
	return(img)
}
