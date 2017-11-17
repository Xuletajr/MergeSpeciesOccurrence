##### functions  cleaning data with geographic information

download.shp=function(all.records,level=2)
{
  
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
  
  if (length(codes_use)>0){
    cont.0 <- getData("GADM", country = codes_use[1], level = level)
    #url="http://biogeo.ucdavis.edu/data/gadm2.8/rds/"
    for (i in 2:length(codes_use)){
      cont=codes_use[i]
      cont.1=NA
      try(
        cont.1 <- getData("GADM", country = cont, level = level))# message=paste0("Searching level=",level-1))
      if (is.na(cont.1)){
        cont.1b <- getData("GADM", country = cont, level = level-1)
        
        cont.1<-cont.1b[,1:6]
        cont.1$ID_2=cont.1$ID_1
        cont.1$NAME_2=cont.1$NAME_1
        cont.1$HASC_2=NA
        cont.1$CCN_2=NA
        cont.1$CCA_2=NA
        cont.1$TYPE_2=NA
        cont.1$ENGTYPE_2=NA
        cont.1$NL_NAME_2=NA
        cont.1$VARNAME_2=NA
        
        
        
        
      }
      cont.0=rbind(cont.0,cont.1) #join all the shps for contries in the dataset
      
    }
  }
  
  
  return(cont.0)
}

info_geografica<-function(all.records)
{
  init=download.shp(all.records, level=2)
  HASC_0=HASC_1=HASC_2=rep(NA,nrow(init))
  for (i in 1:nrow(init)){
    #print(i)
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
  centroid<<-centroid
}

### Revisar "overlay" de los registros con el "Shape" de departamento
corroboracion_pais=function(datos,mun)
{
  CRS.new=mun@proj4string
  
  coordinates(datos)=~decimalLongitude+decimalLatitude
  proj4string(datos) <- CRS.new
  #ovm=overlay(datos,mun)
  ovm=over(datos,mun)
  #cntrm=as.character(mun@data$COUNTRY[ovm$PAIS]) #esta 
  cntrm=as.character(ovm$NAME_0)
  if(length(which(!is.na(datos@data$country)))>0){
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

corroboracion_dep=function(datos,estado)
{
  CRS.new=estado@proj4string
  
  coordinates(datos)=~decimalLongitude+decimalLatitude
  proj4string(datos) <- CRS.new
  
  ovm=over(datos,estado)
  #cntrm=as.character(estado@data$HASC_1[ovm])
  cntrm1=as.character(ovm$HASC_1)
  cntrm2=as.character(ovm$NAME_1)
  
  # aqui pablo "latin1" para "UTF-8"
  Encoding(cntrm2)="UTF-8"
  
  if(length(which(!is.na(datos@data$stateProvince)))>0){
    im=jm=NULL
    for (j in 1:length(cntrm1)){
      #print(j)
      name.c=cntrm1[j]
      name.c2=cntrm2[j]
      
      name.dat=as.character(datos@data$stateProvince)[j]

      if ( is.na(name.dat) ) {name.dat="No_data"}
      
      if(name.dat==""){name.dat="No_data"}
      

#AQUI
#      if ( is.na( (nchar(name.dat, allowNA = T)==2) ) | (name.dat=="No_data"))
      
      #name.dat = gsub("\\\\","",name.dat)
      
      teste = is.na( (nchar(name.dat, allowNA = T)==2)) 
      
      print(paste0(name.dat,teste))
      
      if (teste==F)
      {
        if ( (nchar(as.character(name.dat),allowNA = T)==2)  | (as.character(name.dat)=="No_data"))
          {comp=name.dat==name.c} 
        else
          {comp=agrep(name.dat,name.c2,max.distance=3, ignore.case=T)}
      }
      else
      {
        #comp=name.dat==name.c
        comp=name.dat=='errrrooooo'
      }  
      
      ## datos con diferente estado
      ## datos con igual estado 
      if (length(comp)==0)
        {im=c(im,j) } 
      else 
        {jm=c(jm,j)}
    }
    
    diferente=as.data.frame(cbind(cntrm1,as.character(datos@data$stateProvince)))[im,]
    

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

corroboracion=function(datos,mun)
{
  CRS.new=mun@proj4string
  
  coordinates(datos)=~decimalLongitude+decimalLatitude
  proj4string(datos) <- CRS.new
  ovm=over(datos,mun)
  cntrm=as.character(ovm$NAME_2)
  
  Encoding(cntrm)="UTF-8"
  
  if(length(which(!is.na(datos@data$county)))>0){
    mm=op=NULL
    for (j in 1:length(cntrm)){

      name.c=cntrm[j]
      name.dat=as.character(datos@data$county)[j]
      
      print(name.dat)
      
      # # aqui pablo "latin1" para "UTF-8"
      # Encoding(name.dat)="UTF-8"
      
      # aqui ultimo
      if ( is.na(name.dat) ) {name.dat=""}
      
      
      #testar aqui
       if(name.dat=="")
         {name.dat=NA}
       else
         {name.dat=name.dat}
      
      if (!is.na(name.dat) & is.na(name.dat!="")){comp=agrep(name.dat,name.c,max.distance=4, ignore.case=T)}else{comp=NULL}
      
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

limpeza<-function(occ.tmp=data.frame(na),
                  background.pol,
                  remove_latlongNA0=TRUE, 
                  remove_anoNA=TRUE,
                  remove_limiteespacial=TRUE,
                  check.country=TRUE,
                  check.state=TRUE,
                  check.municipality=TRUE,
                  check.citycoor=TRUE,
                  check.centroid=TRUE)
{
  
  ### atribuindo ID 
  
  ID=1:nrow(occ.tmp) 
  occ.tmp=cbind(ID,occ.tmp)
  occ.tmp.out <- occ.tmp.out.temp <- {}
  
  
  #remove_latlongNA0=TRUE # testar com lastInterpreted !!!
  if(remove_latlongNA0==TRUE){
    occ.tmp.out.temp <- occ.tmp[is.na(occ.tmp$decimalLatitude),]
    occ.tmp <- occ.tmp[!is.na(occ.tmp$decimalLatitude),]
    if (NROW(occ.tmp.out.temp)>0){
      remove.type = 'lat. NA'
      occ.tmp.out.temp <- cbind(occ.tmp.out.temp, reason=rep(remove.type,NROW(occ.tmp.out.temp)))
      occ.tmp.out <- rbind(occ.tmp.out,occ.tmp.out.temp)}
    
    occ.tmp.out.temp <- occ.tmp[is.na(occ.tmp$decimalLongitude),]
    occ.tmp <- occ.tmp[!is.na(occ.tmp$decimalLongitude),]
    if (NROW(occ.tmp.out.temp)>0){
      remove.type = 'long. NA'
      occ.tmp.out.temp <- cbind(occ.tmp.out.temp, reason=rep(remove.type,NROW(occ.tmp.out.temp)))
      occ.tmp.out <- rbind(occ.tmp.out,occ.tmp.out.temp)}
    
    occ.tmp.out.temp <- occ.tmp[occ.tmp$decimalLatitude %in% 0,]
    occ.tmp <- occ.tmp[!occ.tmp$decimalLatitude %in% 0,]
    if (NROW(occ.tmp.out.temp)>0){
      remove.type = 'lat. 0'
      occ.tmp.out.temp <- cbind(occ.tmp.out.temp, reason=rep(remove.type,NROW(occ.tmp.out.temp)))
      occ.tmp.out <- rbind(occ.tmp.out,occ.tmp.out.temp)}
    
    occ.tmp.out.temp <- occ.tmp[occ.tmp$decimalLongitude %in% 0,]
    occ.tmp <- occ.tmp[!occ.tmp$decimalLongitude == 0,]
    if (NROW(occ.tmp.out.temp)>0){
      remove.type = 'long. 0'
      occ.tmp.out.temp <- cbind(occ.tmp.out.temp,reason=rep(remove.type,NROW(occ.tmp.out.temp)))
      occ.tmp.out <- rbind(occ.tmp.out,occ.tmp.out.temp)}
  }
  
  #remove_anoNA=TRUE
  if(remove_anoNA==TRUE){
    occ.tmp.out.temp <- occ.tmp[is.na(occ.tmp$year),]
    occ.tmp <- occ.tmp[!is.na(occ.tmp$year),] # retira anos NA 
    if (NROW(occ.tmp.out.temp)>0)
    {remove.type = 'Ano NA'
    occ.tmp.out.temp <- cbind(occ.tmp.out.temp, reason=rep(remove.type,NROW(occ.tmp.out.temp)))
    occ.tmp.out <- rbind(occ.tmp.out,occ.tmp.out.temp)}
  }
  
  
  #remove_limiteespacial=TRUE # testar com lastInterpreted !!!
  if(remove_limiteespacial==TRUE & NROW(occ.tmp)>0)
  {
    occ.tmp.points=occ.tmp
    occ.tmp.points <- SpatialPointsDataFrame(cbind(occ.tmp$decimalLongitude,occ.tmp$decimalLatitude),occ.tmp)
    
    #coordinates(occ.tmp.points) <- SpatialPointsDataFrame(cbind(occ.tmp$decimalLongitude,occ.tmp$decimalLatitude),occ.tmp)
    CRS.new=background.pol@proj4string
    proj4string(occ.tmp.points) <- CRS.new 
    
    occ.tmp.points_in <- occ.tmp.points[!is.na(over(occ.tmp.points ,geometry(background.pol))),] # dentro do poligono
    occ.tmp.points_out <- occ.tmp.points[is.na(over(occ.tmp.points ,geometry(background.pol))),] # fora do poligono
    
    occ.tmp.out.temp <- occ.tmp.points_out@data
    if (NROW(occ.tmp.out.temp)>0)
    {
      remove.type = 'Out of boundary polygon'
 
      
      occ.tmp.out.temp <- cbind(occ.tmp.out.temp, reason=rep(remove.type,NROW(occ.tmp.out.temp))) #   Aqui
      occ.tmp.out <- rbind(occ.tmp.out,occ.tmp.out.temp)
      }
  }
  
  
  set4=occ.tmp.points_in@data
  set4$reason=NA
  #set4$ID=1:nrow(set4) # subir
  
  # ### 1. Check country
  #check.country=TRUE
  if(check.country==T){
    PAIS=corroboracion_pais(set4,paises)
    set4$ok_country=rep(NA,nrow(set4)) ;set4$ok_country[PAIS[[1]]]=1

    # aqui pablo incluir se der certo    
    # Encoding(A[[3]]) <- "UTF-8"
    # set4$suggest_mun=A[[3]]
    
    
    set4$suggest_country=PAIS[[3]]
    cat("Country concordance checked ")} # deixar msn como prints e não como janelas
  
  
  #### 2. check state # just working for Brasil
  #check.state=TRUE
  if(check.state==T){
    #set5<-set4[which(set4$suggest_country=="Brasil"),] #2055
    DEP=corroboracion_dep(set4,estado)
    ok_state=rep(NA,nrow(set4)) ;ok_state[DEP[[1]]]=1
    set4=cbind(set4,ok_state)
    set4$suggest_state=DEP[[3]]
    cat("State concordance checked ")
  }
  
  
  ### 3. check municipality
  #check.municipality=T
  if (check.municipality==T){
    A=corroboracion(set4,mpios)
    set4$ok_mun=NA ;set4$ok_mun[A[[1]]]=1
    
    # aqui Pablo  Encoding(A[[3]]) <- "UTF-8" implementado na fonte
    
    set4$suggest_mun=A[[3]]
    cat("Municipality concordance checked ","!")}
  
  ##### 4. check that the coordinate is equal to the coordinate of the city municipality 
  #check.citycoor=T
  if(check.citycoor==T){
    sedes_lat=which(set4$decimalLatitude %in% sedes$lat)
    set4$coord_mun=NA
    for ( i in sedes_lat){
      filas=which(sedes$lat %in% set4$decimalLatitude[i]) #elige decimalLatitudes iguales a las decimalLatitudes de municipalidades
      respuesta=NULL
      for(j in filas){# revisa  si para esas latitides iguales tambien las decimalLongitudes son igules
        resp=set4$decimalLongitude[i]==sedes$long[j]
        respuesta=c(respuesta,resp)
      }
      if (TRUE %in%respuesta){# si lat y longitud son igual a la de la municipalidad pone 1 de lo contrario 0
        set4$coord_mun[i]=1
      }else{set4$coord_mun[i]=0}
    }
    
    mun.ref=set4$ID[which(set4$coord_mun==1)] # identifica cuales  refID tienen la misma cordenada que la municipalidad
    # 
    if (length(mun.ref)!=0){
      cat(paste0("IDs ",mun.ref, " have the coordenate of the municipality, Be careful","!"))} else
      {cat(paste0("All the coordenate are different from the municipality","!"))}
  }
  
  ##### 7. check that the name coordinate is equal to the centroid of the  municipality 
  ##### 
  #check.centroid=T
  if(check.centroid==T){
    dat=set4
    CRS.new=centroid@proj4string
    
    coordinates(dat)=~decimalLongitude+decimalLatitude
    proj4string(dat) <- CRS.new
    
    ovm=over(dat,centroid)
    mun_cent=ovm[,c("Longmuncen", "Latmuncent")]
    
    set4=cbind(set4,mun_cent)
    
    
    #pablo

    # city = geocode(paste0(set4$suggest_mun,' ',set4$suggest_state  ))
    # 
    # dif.lon.city=abs(set4$decimalLongitude-city$lon)
    # dif.lat.city=abs(set4$decimalLatitude-city$lat)
    # dif.city=cbind(dif.lon.city,dif.lat.city)
    #   
    # iqual.city=which(dif.city[,1]<0.5 & dif.city[,2]<0.5)
    # set4$coord_mun_cent_city=NA
    # set4$long_city=city$lon
    # set4$lat_city=city$lat
    # set4$city=NA
    # if (length(iqual.city)!=0) { set4$coord_mun_cent_city[iqual.city]=1 } 
    # 
    # fim
    
    
    dif.lon=abs(set4$decimalLongitude-set4$Longmuncen)
    dif.lat=abs(set4$decimalLatitude-set4$Latmuncent)
    dif=cbind(dif.lon,dif.lat)
    
    # controle do buffer
    # sugestão 0.015 e 0.15
    
    # da carol 0.001 e 0.01
    
    iqual=which(dif[,1]< 0.015 & dif[,2]<0.15)
    set4$coord_mun_cent=NA
    if (length(iqual)!=0){
      set4$coord_mun_cent[iqual]=1
      cat("IDs", set4$ID[iqual], "have the coordinate of the centroid of the municipality")
    }else{
      cat("All the coordenate are different from the municipality")
    }
  }
  
  
  # salva dados e limpeza
  
  #   geo.out=set.fin[which(is.na(set.fin$ok_country)|is.na(set.fin$ok_state)|is.na(set.fin$ok_mun)|set.fin$coord_mun==1|set.fin$coord_mun_cent==1),]
  #     geo.ok=set.fin[which(set.fin$ok_country==1 & set.fin$ok_state==1 & set.fin$ok_mun==1 & is.na(set.fin$coord_mun==1) & is.na(set.fin$coord_mun_cent)),]
  
  #project="clean.results"
  if(NROW(occ.tmp.out)>0){
    occ.tmp.out=cbind(occ.tmp.out,ok_country=NA,suggest_country=NA,ok_state=NA, suggest_state=NA,ok_mun=NA,suggest_mun=NA,coord_mun=NA,Longmuncen=NA,Latmuncent=NA,coord_mun_cent=NA)
    # occ.tmp.out.all=rbind(occ.tmp.out, set4)
    # save_occurrence_records(occ.tmp.out.all,'limpeza-out',project,'clean_results',sep='\t')
    # return(occ.tmp.out.all)
  } 
  
  return(list(occ.in = set4, occ.out = occ.tmp.out))

}


# salva dados no diretorio de cada fonte de dados por projeto
save_occurrence_records <- function(dat, file.name, project='',data_source='', sep=' ',type.file='txt')
{
  ## Grava resultados totais da busca por registros
  wd.root <- getwd()
  if( ! dir.exists(paste0(getwd(),"/occurrencerecords/")))
  {dir.create("OccurrenceRecords")}
  setwd(paste0(getwd(),"/occurrencerecords"))
  if(! dir.exists(paste0(getwd(),"/",project,"/")))
  {dir.create(project)}
  setwd(paste0(getwd(),"/",project))
  if( ! dir.exists(paste0(getwd(),"/",data_source,"/")))
  {dir.create(data_source)}
  setwd(paste0(getwd(),"/",data_source))
  file_name_txt <- paste0(file.name,".",type.file)  
  write.table(dat,file_name_txt,append = FALSE, quote = TRUE, sep = sep, eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = "double", fileEncoding = "UTF-8")
  setwd(wd.root)
}