#Atlatl target for Atlatl technology promoted sex equity in early forager societies
#Randy Haas
setwd("/home/whaas/Research/AtlatlAthelete/")
library(plotrix)
library(terra) #for halo text
diameter<-76
radius<-diameter/2
png("AtlatlAtheleteTarget.png",height=diameter,width=diameter,units="cm",bg="white",res=300)
par(mar=rep(0,4))
s<-radius/1.08 #scale plot window to take away default 4% * 2 inner margin
plot("n",xlim=c(-s,s),ylim=c(-s,s),xlab="cm", ylab="cm",bty="n",asp=1,main="")
draw.circle(x=0,y=0,radius=12.5,border=NA,col="gray")
for(i in seq(0,360,by=5)){
    draw.radial.line(start=5, end=radius, center=c(0, 0), deg=i, col="black", lwd=2)
}
draw.circle(x=0,y=0,radius=seq(0,radius,by=1),border="black",lty=1,lwd=2)
for(i in seq(0,315,by=45)){
    draw.radial.line(start=1, end=radius-2, center=c(0, 0), deg=i, col="black", lwd=4)
}
draw.circle(x=0,y=0,radius=seq(0,radius,by=5),border="black",lty=1,lwd=4)

#radial.plot.labels(lengths=rep(radius-2,8)+1,radial.pos=seq(0,315,by=45)/180*pi,units="degrees",radial.lim=0, start=90/180*pi,clockwise=TRUE,labels=seq(0,315,by=45),adj=NULL,pos=NULL,cex=4)

halo(x=c(0,26,37,26,0,-26,-36.5,-26),y=c(37,26,0,-26,-37,-26,0,26),labels=seq(0,315,by=45),col="white",hc="black",hw=.5,cex=4)
halo(x=rep(0,3),y=seq(-30,-10,by=10),labels=seq(30,10,by=-10),col="black",hc=c(rep("white",2),rep("gray",1)),hw=1,cex=4)
halo(x=rep(0,3),y=seq(10,30,by=10),labels=seq(10,30,by=10),col="black",hc=c(rep("gray",1),rep("white",2)),hw=1,cex=4)
halo(x=seq(10,30,by=10),y=rep(0,3),labels=seq(10,30,by=10),col="black",hc=c(rep("gray",1),rep("white",2)),hw=1,cex=4)
halo(x=seq(-30,-10,by=10),y=rep(0,3),labels=seq(30,10,by=-10),col="black",hc=c(rep("white",2),rep("gray",1)),hw=1,cex=4)

#legend
polygon(x=c(-radius+1,-radius+2,-radius+2,-radius+1),y=c(-radius+5,-radius+5,-radius+6,-radius+6),col="gray")
text(x=-radius+2.5,y=-radius+5.5,"large-mammal kill zone",cex=2,pos=4)

#scale bar
polygon(x=c(-radius+1,-radius+11,-radius+11,-radius+1),y=c(-radius+1,-radius+1,-radius+1.2,-radius+1.2),col="black")
text(x=-radius+5.5,y=-radius+1.5,"10 cm",cex=4,pos=3)
text(x=radius-.5,y=-radius+.5,labels="Forager Archaeology Lab 2024",adj=c(1,0),cex=3)

dev.off()
