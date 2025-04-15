#Decollars USGS Quad maps from https://ngmdb.usgs.gov/topoview/
#by Randy Haas 14 April 2025
#requires the tif and xml files from the USGS download site, https://ngmdb.usgs.gov/topoview/

require("terra")
require("XML")

decollar<-function(file){
    #file=file name to be decollared. Should include "tif" extension.
    r<-rast(file)
    xml<-xmlToList(xmlParse(paste0(file,".xml")))
    w<-as.numeric(xml$idinfo$spdom$bounding$westbc)
    e<-as.numeric(xml$idinfo$spdom$bounding$eastbc)
    n<-as.numeric(xml$idinfo$spdom$bounding$northbc)
    s<-as.numeric(xml$idinfo$spdom$bounding$southbc)
    long<-(w+e)/2
    r2<-project(r,"EPSG:4269") #project to NAD83 geographic
    r3<-crop(r2,ext(w,e,s,n)) #crop to lat/long extents specified in xml file.
    r4<-project(r3,crs(r)) #project back to original crs.
    writeRaster(r4,paste0(gsub(".tif","",file),"_decollar.tif"))
    print("Complete. Find the decollared file in the working directory.") 
}

