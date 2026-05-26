#Decollars USGS Quad maps from https://ngmdb.usgs.gov/topoview/
#by Randy Haas 14 April 2025. revised 21 April 2025
#requires the tif and xml files from the USGS download site, https://ngmdb.usgs.gov/topoview/

require("terra")
require("XML")

decollar<-function(dir,file){
    #file=file name to be decollared. Should include "tif" extension.
    #dir=working directory where geotif and associated xml files are located
    r<-rast(paste0(dir,file))
    xml<-xmlToList(xmlParse(paste0(dir,file,".xml"))) #get metadata from xml file
    w<-as.numeric(xml$idinfo$spdom$bounding$westbc) #get bounding coorindates from xml
    e<-as.numeric(xml$idinfo$spdom$bounding$eastbc)
    n<-as.numeric(xml$idinfo$spdom$bounding$northbc)
    s<-as.numeric(xml$idinfo$spdom$bounding$southbc)
    ext1<-vect(ext(w,e,s,n),crs="EPSG:4269") #create a polygon from bounding coordinates
    ext2<-project(ext1,crs(r))
    r2<-crop(mask(r,ext2),ext2) #crop to lat/long extents specified in xml file.
    writeRaster(r2,paste0(dir,gsub(".tif","",file),"_decollar.tif"),overwrite=T)
    print("Complete. Find the decollared file in the working directory.") 
}

