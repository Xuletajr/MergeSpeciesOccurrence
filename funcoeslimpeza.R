library(taxize)
library(flora)
library(ggmap)
library(sp)
library(rgdal)
library(maptools)

library(dplyr) ## para manipular BD
library(rgbif)


##### functions  cleaning data with geographic information

download.shp=function(all.records,level=2){
  
  require(maps)
  require(maptools)
  require(raster)
  require(rgdal)
  require(rgeos)
  require(spatialEco)
  
  
  co=unique(all.records$country)
  data(wrld_simpl)
  codes=wrld_simpl@data[,1:5]
  codes_use=as.character(codes$ISO3[which(codes$NAME %in% co)])
  #cont.0=''#PHAM
  if (length(codes_use)>0){
    cont.0 <- getData("GADM", country = codes_use[1], level = level)
    #url="http://biogeo.ucdavis.edu/data/gadm2.8/rds/"
    for (i in 2:length(codes_use)){
      cont=codes_use[i]
      
      cont.1 <- getData("GADM", country = cont, level = level) 
      
      cont.0=rbind(cont.0,cont.1)} #join all the shps for contries in the dataset
      }
  return(cont.0)
}


# Criar variaveis sistema
info_geografica<-function(all.records){
  init=download.shp(all.records, level=2)
  HASC_0=HASC_1=HASC_2=rep(NA,nrow(init))
  for (i in 1:nrow(init)){
    print(i)
    code=init$HASC_2[i]
    HASC_S=unlist(strsplit(code,"[.]"))
    HASC_0[i]=HASC_S[1]
    HASC_1[i]=HASC_S[2]
    HASC_2[i]=HASC_S[3]
  }
  init@data=cbind(init@data,HASC_0,HASC_1,HASC_2)
  background.pol<<-download.shp(all.records, level=0)
  
  paises<<-estado<<-mpios<<-init
  
  #world cities
  data(world.cities)
  sedes<<-world.cities
  
  
  #produce centroids
  cents <- gCentroid(init,byid=T)
  
  cents0<-as.data.frame(coordinates(cents))
  cents1<<-SpatialPointsDataFrame(cents,data=cents0)
  b=over(init, cents1)
  centroid=init
  centroid@data$Longmuncen<-b$x
  centroid@data$Latmuncent<-b$y
}

###http://infocenter.ibi.com/wf8008/index.jsp?topic=%2Fpubdocs%2FRStat15%2Fsource%2Ftopic16.htm

### Revisar "overlay" de los registros con el "Shape" de departamento
corroboracion_pais=function(datos,mun){
  CRS.new=CRS.new=background.pol@proj4string
  #CRS.new=mun@proj4string
  #CRS.new=CRS("+init=epsg:4326") # WGS 84
  
  #projection(mun) <- projection("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  
  # proj4string(d) <- CRS("+init=epsg:4326") # WGS 84
  # CRS.new <- CRS("+proj=somerc +lat_0=46.9524056 +lon_0=7.43958333 +ellps=bessel +x_0=2600000 +y_0=1200000 +towgs84=674.374,15.056,405.346 +units=m +k_0=1 +no_defs")
  
  #CRS.new=CRS("+proj=longlat +datum=WGS84")

  
  coordinates(datos)=~longitude+latitude
  proj4string(datos) <- CRS.new
  #proj4string(datos) <- CRS("+init=epsg:4326") # WGS 84
  
  #ovm=overlay(datos,mun)
  ovm=over(datos,mun)
  #cntrm=as.character(mun@data$COUNTRY[ovm$PAIS]) #esta 
  cntrm=as.character(ovm$NAME_0)
  if(length(which(!is.na(datos@data$state)))>0){
    im=1:nrow(datos@data) # linea
    tb=as.character(datos@data$country)# fala
    
    
    CompareMun=cbind(im,cntrm,tb)
    
    
    ma1=NULL 
    for  ( i in 1:nrow(CompareMun)){
      tmp<-agrep(CompareMun[i,2],CompareMun[i,3], max=4,value=F,ignore.case=T)
      if (length(tmp)==0) tmp=0
      ma<-c(CompareMun[i,1],tmp)
      ma1=rbind(ma1,ma)
    }
    
    km=as.integer(ma1[which(is.na(ma1[,2])),1]) # pais diferente
    lm=as.integer(ma1[which(ma1[,2]==1),1]) # pais igual 
    
    
  }else{
    mm=rep(0,nrow(datos))
    op=rep(NA,nrow(datos))
  }
  
  X=list()
  X[[1]]=lm # pais correcto
  X[[2]]=km #pais errado
  X[[3]]=cntrm # sugerencia
  return(X)
}

corroboracion_dep=function(datos,estado){
  #CRS.new=estado@proj4string
  #CRS.new=CRS("+proj=longlat +datum=WGS84")
  #CRS.new=CRS("+init=epsg:4326") # WGS 84
  CRS.new=CRS.new=background.pol@proj4string
  
  coordinates(datos)=~longitude+latitude
  proj4string(datos) <- CRS.new
  
  ovm=over(datos,estado)
  #cntrm=as.character(estado@data$HASC_1[ovm])
  cntrm1=as.character(ovm$HASC_1)
  cntrm2=as.character(ovm$NAME_1)
  Encoding(cntrm2)="latin1"
  
  if(length(which(!is.na(datos@data$stateprovince)))>0){
    im=jm=NULL
    for (j in 1:length(cntrm1)){
      #print(j)
      name.c=cntrm1[j]
      name.c2=cntrm2[j]
      
      name.dat=as.character(datos@data$stateprovince)[j]
      Encoding(name.dat)="latin1"
      if (!is.na(nchar(name.dat)==2)){ comp=name.dat==name.c} else{
        comp=agrep(name.dat,name.c2,max.distance=3, ignore.case=T)
      }
      if (length(comp)==0){im=c(im,j) } else {## datos con diferente estado
        jm=c(jm,j)}## datos con igual estado 
    }
    
    diferente=as.data.frame(cbind(cntrm1,as.character(datos@data$stateprovince)))[im,]
    
    #CompareDpto=cbind(im,diferente)
    #MunCorrecto=cbind(cntrm,as.character(datos@data$state))[jm,]
    
    
  } else{
    im=rep(0,nrow(datos))
    jm=rep(NA,nrow(datos))
  }
  
  
  X=list()
  X[[1]]=jm # estado igual 
  X[[2]]=im# estado diferente
  X[[3]]=cntrm1 #nombre estado correcto
  return(X)
}

corroboracion=function(datos,mun){
  #CRS.new=mun@proj4string
  #CRS.new=CRS("+proj=longlat +datum=WGS84")
  #CRS.new=CRS("+init=epsg:4326") # WGS 84
  CRS.new=CRS.new=background.pol@proj4string
  
  coordinates(datos)=~longitude+latitude
  proj4string(datos) <- CRS.new
  #ovm=overlay(datos,mun)
  ovm=over(datos,mun)
  #cntrm=as.character(mun@data$NAME_2[ovm])
  cntrm=as.character(ovm$NAME_2)
  Encoding(cntrm)="latin1"
  
  if(length(which(!is.na(datos@data$county)))>0){
    mm=op=NULL
    for (j in 1:length(cntrm)){
      #print(j)
      name.c=cntrm[j]
      name.dat=as.character(datos@data$county)[j]
      Encoding(name.dat)="latin1"
      # if(name.dat==""){name.dat=NA}else{name.dat=name.dat}
      comp=''
      if (!is.na(name.dat)& name.dat!=""){comp=agrep(name.dat,name.c,max.distance=4, ignore.case=T)}
      if (length(comp)!=0){mm=c(mm,j) } else {##iqual municipality 
        op=c(op,j)} ## different municipality
    }
    
  }else{
    mm=rep(0,nrow(datos))
    op=rep(NA,nrow(datos))
  }
  
  X=list()
  X[[1]]=mm
  X[[2]]=op
  X[[3]]=cntrm
  return(X)
}




