#PhotoPointsPicker for selecting pixels in photos for use in OpenDroneMap by Randy Haas 2023
library(raster)
ppp<-function(d,f="JPG",utm,zone="12N"){
#d=image directory
#f=file type (e.g., JPG, jpg, tiff)
#utm=csv table of utm coordinates for targets with columns id,e,n,z
#zone=WGS84 UTM zone.
    utm<-read.csv(utm)
    prj<-paste("WGS84 UTM",zone)
    setwd(d)
    f<-"JPG" #file type
    id<-NULL
    e<-NULL
    n<-NULL
    z<-NULL
    x<-NULL
    y<-NULL
    img<-NULL
    print(paste("projection:",prj))
    l<-list.files(paste(d,"images/",sep=""),pattern=f)
    for(i in 1:length(l)){
        r<-brick(paste(paste(d,"images/",sep=""),l[i],sep=""))
        plotRGB(r)
        print(noquote(paste("Showing ",l[i],".",sep="")))
        no<-readline("How many targets will you digitize?\nIf you are done, enter 'exit'.\n")
        if(no=="exit"){break}
        if(no>0){
            for(j in 1:no){
                id[length(id)+1]<-readline(prompt=paste("Enter target identifier for target",j,"."))
                print(noquote(paste("Left click on target",id[length(id)],"in the image.")))
                p<-locator(1)
                img[length(img)+1]<-l[i]
                x[length(x)+1]<-round(p$x,0)
                y[length(y)+1]<-round(dim(r)[1]-p$y,0)
                e[length(e)+1]<-utm$e[utm$id==id[length(id)]]
                n[length(n)+1]<-utm$n[utm$id==id[length(id)]]
                z[length(z)+1]<-utm$z[utm$id==id[length(id)]]
            }
        }
    }
    result=data.frame(e,n,z,x,y,img,id)
    cat(paste(prj,"\n",sep=""),file="gcp_list.txt")
    write.table(result,'gcp_list.txt',append=TRUE,col.names=FALSE,row.names=F,quote=F)
    list(projection=prj,gcp_list=result)
}

