### funções auxiliares para as diferentes rotinas ###

# Pablo Hendrigo Alves de Melo - pablopains@yahoo.com.br

###  MergeSpeciesOccurrence	###

# https://developers.google.com/maps/documentation/javascript/
keygoogle1 = 'AIzaSyAiKBXoGdj0_hZWpvXfNbWpNpSGwD07DeE'
keygoogle2 = 'AIzaSyChCP0l84mu_-aP1w2L0CPX7zsacBOAkQY'
keygoogle3 = 'AIzaSyBTT_2ELu8sG-OHo2whHW4J3SCwllwrQcg'
keygoogle.ativa = keygoogle2

library(googleway)
###-----------------------------------------------------------------------------------------###

# 1. clear the memory and load the packages
# clear workspace and increase memory
rm(list = ls())
memory.limit(size = 1.75e13) 

###-----------------------------------------------------------------------------------------###

# diretório temporario de processamento do R 

tempdir <- function() "D:\\temps"
unlockBinding("tempdir", baseenv())
assignInNamespace("tempdir", tempdir, ns="base", envir=baseenv())
assign("tempdir", tempdir, baseenv())
lockBinding("tempdir", baseenv())
tempdir()

###---------------------------------------------------------------------###

# Informar parâmetros:

project = "plantascalcario"
dir.root= "C:/Dados/GitHub/MergeSpeciesOccurrence"; setwd(dir.root)
dir.gis = 'F:/Dados/GitHub/MergeSpeciesOccurrence/GisBase'
file.spp = "plantascalcario.txt"

###---------------------------------------------------------------------###
# carrega funções 
 
source("funcoesauxiliares.R")
#source("mergespeciesoccurrence.R")
source("MergeSpeciesOccurrenceFunction.R")
source("funcoesAlbertoVicentini.R")
source("par.ini.R")
# source("funcoesplantas.R")
# source("funcao-limpeza.R")
#source("funcoes.R")
source("Funcoes_limpeza.R")

library(ggmap)
# install.packages('RDSTK')
library(RDSTK)
library(httr)


###---------------------------------------------------------------------###

# carrega lista de espécies  

 spp.csv <- read.table(file.spp, header=FALSE, sep="\t",dec = ".") 
 spp <- as.matrix(spp.csv[ ,1])

# spp <- read.table(file.spp, header=F, sep="\t",dec = ".") 

###---------------------------------------------------------------------###

# junta ocorrências das diferentes fontes de dados 

alldata <- MergeSpeciesOccurrence(spp,project)
alldata.table<-data.frame(alldata$tabela, stringsAsFactors = FALSE)#; View(alldata.table)
View(alldata.table)

novoscoletores<-compilacoletores(alldata.table)
alldata.table <- atualizacoletores(alldata.table,novoscoletores)

cidade <- rep(NA,NROW(alldata.table))
k=!is.na(alldata.table$county)
for (m in 1:NROW(alldata.table))
{
  cidade[m] <- ifelse(!is.na(alldata.table$municipality[m]), alldata.table$municipality[m], 
                      ifelse(!is.na(alldata.table$county[m]),alldata.table$county[m],'não informado'))
}
alldata.table$county=cidade

alldata.table$decimalLatitude= as.numeric(alldata.table$decimalLatitude)
alldata.table$decimalLongitude= as.numeric(alldata.table$decimalLongitude)
alldata.table$year=(alldata.table$year)

#load geographic data
info_geografica(alldata.table)

clean_data=limpeza(alldata.table, background.pol=background.pol,
                   check.country=F,
                   check.state=F,
                   check.municipality=F,
                   check.citycoor=F,
                   check.centroid=F)
# dataf <- data.frame(clean_data$occ.in$decimalLatitudeMunicipality,clean_data$occ.in$decimalLongitudeMunicipality, clean_data$occ.in$coord_mun_cent)
# View(dataf)


###-----------------------------------------------------------------------------------------###
clean_data$occ.in_old <- clean_data$occ.in
clean_data$occ.in <- clean_data$occ.in_old

l = 1

ii=21
clean_data$occ.in$in_city=NA
clean_data$occ.in$reference_city=NA
clean_data$occ.in$lon_city=NA
clean_data$occ.in$lat_city=NA
clean_data$occ.in$pais=NA
clean_data$occ.in$pais.cod=NA
clean_data$occ.in$estado=NA
clean_data$occ.in$estado.cod=NA
clean_data$occ.in$municipio=NA

#clean_data$occ.in <- edit(clean_data$occ.in)


#require(R.utils)

diferenca.lon =  0.15 #0.015
diferenca.lat =  0.15 #0.015

keygoogle.ativa = keygoogle2

# totos os registros
for( ii in 1:NROW(clean_data$occ.in)) 
{
  
  if (!is.na(clean_data$occ.in$in_city[ii])) {print(paste0(ii,' - já verificado!')); next}
  
  res <- google_geocode(paste0(clean_data$occ.in$decimalLatitude[ii],' ',clean_data$occ.in$decimalLongitude[ii]),
                        key = keygoogle.ativa,
                        simplify=T)
  Sys.sleep(2)
  
  if (res$status == "OVER_QUERY_LIMIT"){ print("OVER_QUERY_LIMIT") ;break}
  
  enderecos = res$results$address_components[[1]]
  tipos.endereco = enderecos$types 
  
  pais <- pains.cod <- estado <- estado.cod <-  municipio <- ''
  
  for (l in 1:length(tipos.endereco)){
    print(l)
    if (tipos.endereco[[l]][1] == 'administrative_area_level_2')
    {municipio <- enderecos$short_name[[l]][1]}          
    
    if (tipos.endereco[[l]][1] == 'administrative_area_level_1')
    {estado <- enderecos$long_name[[l]][1]
    estado.cod <- enderecos$short_name[[l]][1]}          
    
    if (tipos.endereco[[l]][1] == 'country')
    {pais <- enderecos$long_name[[l]][1]
    pais.cod <- enderecos$long_name[[l]][1]}          
  }  
  
  #  busca <- ifelse( length(tipos.endereco)<=4,
  busca <- ifelse( municipio=='',
                   paste0(pais,', ', estado),
                   paste0(pais,', ', estado,', ', municipio))
  
  #busca <- paste0(pais,', ', estado,', ', municipio)
  
  cat(ii, ' - ', busca)
  
  # if (length(tipos.endereco)==4) {municipio <- 'Sem Adm2'}
  # if (length(tipos.endereco)==3) {municipio <- 'Sem Adm2'; pais <- 'Sem Adm1'} # será ?
  
  res2 <- google_geocode(busca,
                         key = keygoogle.ativa,
                         simplify=T)
  
  loc <- res2$results$geometry$location
  
  # res2$results$formatted_address
  
  dif.lon.city=abs(clean_data$occ.in$decimalLongitude[ii]-loc$lng)
  dif.lat.city=abs(clean_data$occ.in$decimalLatitude[ii]-loc$lat)
  dif.city=cbind(dif.lon.city,dif.lat.city)
  iqual.city=which(dif.city[,1] < diferenca.lon & dif.city[,2] < diferenca.lat)
  
  clean_data$occ.in$reference_city[ii] <- busca #paste0(pais,', ',estado,', ',municipio)
  clean_data$occ.in$lon_city[ii] <- loc$lng
  clean_data$occ.in$lat_city[ii] <- loc$lat
  clean_data$occ.in$pais <- pais
  clean_data$occ.in$pais.cod <- pais.cod
  clean_data$occ.in$estado <- estado
  clean_data$occ.in$estado.cod <- estado.cod
  clean_data$occ.in$municipio <- municipio
  
  
  if (length(iqual.city)!=0)
  { clean_data$occ.in$in_city[ii]=1 }
  else
  { clean_data$occ.in$in_city[ii]=0 }  
  
  cat(' >> ',busca,' ', clean_data$occ.in$in_city[ii])
}



#+proj=longlat +ellps=WGS84 +no_defs

print(CRSargs(CRS("+proj=longlat +ellps=WGS84 +no_defs +datum=WGS84")))

## pegando pais, estado, municipio dos pontos
if(!require(pacman)) install.packages("pacman")
pacman::p_load(taxize, divagis, ggmap, mapr, sp, rgdal, maptools, dplyr, rgbif)

adm.path <- 'E:/environmental_data/gadm/gadm28.shp/neo'
setwd(adm.path)
neo.adm=readShapePoly("neo_gadm28.shp")
#CRS.new=neo.adm@proj4string
CRS.new=CRSargs(CRS("+proj=longlat +ellps=WGS84 +no_defs +datum=WGS84"))



#ibge
# area edificada
path.ibge.loc <- 'E:/environmental_data/ibge/Localidade_v2015_2016-08-03'
setwd(path.ibge.loc)

ibge.area.edificada = readShapePoly("LOC_Area_Edificada_A.shp")

ibge.aglomerado_rural_isolado = readShapePoints("LOC_Aglomerado_Rural_Isolado_P.shp")
ibge.aglomerado_rural_isolado.df = data.frame(ibge.aglomerado_rural_isolado)

ibge.capital = readShapePoints("LOC_Capital_P.shp")
ibge.capital.df = data.frame(ibge.capital)

ibge.cidade = readShapePoints("LOC_Cidade_P.shp")
ibge.cidade.df = data.frame(ibge.cidade)

ibge.hab_indigena = readShapePoints("LOC_Hab_Indigena_P.shp")
ibge.hab_indigena.df = data.frame(ibge.hab_indigena)

ibge.vila = readShapePoints("LOC_Vila_P.shp")
ibge.vila.df = data.frame(ibge.vila)


# # cidades Brasil
# path.cidades <- 'E:/environmental_data/ibge'
# setwd(path.cidades)
# shp = 'E:/environmental_data/ibge/br_cidades_pto.shp'
# br.cidades.pto = readOGR(shp, layer = basename(strsplit(shp, "\\.")[[1]])[1])
# br.cidades.df = data.frame(br.cidades.pto)
# br.cidades.pto = SpatialPointsDataFrame(br.cidades.pto,br.cidades.df)
# 
# coordinates(br.cidades.pto)=~br.cidades.pto@coords[,1]+br.cidades.pto@coords[,2]


#world cities
data(world.cities)
sedes<<-world.cities

#produce centroids
cents <- gCentroid(neo.adm,byid=T)
cents0<-as.data.frame(coordinates(cents))
cents1<<-SpatialPointsDataFrame(cents,data=cents0)
b=over(neo.adm, cents1)
centroid=neo.adm
centroid@data$Longmuncen<-b$x
centroid@data$Latmuncent<-b$y
centroid<<-centroid

# carrega pontos
PointToPaleoClim <- data.frame(decimalLatitude=clean_data$occ.in$decimalLatitude,
                               decimalLongitude=clean_data$occ.in$decimalLongitude,
                               scientificNameKey=clean_data$occ.in$scientificNameKey,
                               stringsAsFactors = F)

coordinates(PointToPaleoClim)=~decimalLongitude+decimalLatitude
# proj4string(datos) <- CRS.new

  # pega dados adm
  ovm=over(PointToPaleoClim,centroid)
  
  # "latin1" para "UTF-8"
  pais.adm <- as.character(ovm$NAME_0); Encoding(pais.adm) = "UTF-8" 
  cod.pais.adm <- as.character(ovm$ISO); Encoding(cod.pais.adm) = "UTF-8" 
  
  estado.adm <- as.character(ovm$NAME_1); Encoding(estado.adm) = "UTF-8" 
  cod.estado.adm <- as.character(ovm$HASC_1); Encoding(cod.estado.adm) = "UTF-8" 
  tip.estado.adm <- as.character(ovm$ENGTYPE_1); Encoding(tip.estado.adm) = "UTF-8" 
  
  municipio.adm <- as.character(ovm$NAME_2); Encoding(municipio.adm) = "UTF-8" 
  tip.municipio.adm <- as.character(ovm$ENGTYPE_2); Encoding(tip.municipio.adm) = "UTF-8" 
  
  distrito.adm <- as.character(ovm$NAME_3); Encoding(municipio.adm) = "UTF-8" 
  tip.distrito.adm <- as.character(ovm$ENGTYPE_3); Encoding(tip.distrito.adm) = "UTF-8" 
  
  Longmuncen.adm <- ifelse(is.na(ovm$Longmuncen),0,as.numeric(ovm$Longmuncen))
  Latmuncent.adm <- ifelse(is.na(ovm$Latmuncen),0,as.numeric(ovm$Latmuncen))
  
  decimalLongitude <- as.numeric(clean_data$occ.in$decimalLongitude)
  decimalLatitude <- as.numeric(clean_data$occ.in$decimalLatitude)

coor_centroid <- rep(NA,length(decimalLatitude)) 

i=30
for(i in 1:length(coor_centroid))
{
  dif.lon=abs(decimalLongitude[i]-Longmuncen.adm[i])
  dif.lat=abs(decimalLatitude[i]-Latmuncent.adm[i])
  dif=cbind(dif.lon,dif.lat)
  
  iqual=which(dif[,1]< 0.015 & dif[,2]<0.15)
  #iqual=which(dif[,1]< 10 & dif[,2]< 10)
  coor_centroid[i]=ifelse(length(iqual)!=0,1,0)
  print(paste0(i, '=',coor_centroid[i]))
  
}


#teste
i=15692
cat(decimalLatitude[i],' ',decimalLongitude[i],' ', municipio.adm[i], estado.adm[i])


# pega sedes do mundo
#sedes_lat=which(decimalLatitude %in% sedes$lat)
coor_sede <- rep(NA,length(decimalLatitude)) 
for(i in 1:length(coor_sede))
{  
  filas=which(sedes$lat %in% decimalLatitude[i]) #elige decimalLatitudes iguales a las decimalLatitudes de municipalidades
  respuesta=NULL
  for(j in filas){# revisa  si para esas latitides iguales tambien las decimalLongitudes son igules
    resp=decimalLongitude[i]==sedes$long[j]
    respuesta=c(respuesta,resp)
  }
  
  # si lat y longitud son igual a la de la municipalidad pone 1 de lo contrario 0
  coor_sede[i]=ifelse(TRUE %in%respuesta,1,0)
  print(paste0(i, '=',coor_sede[i]))
}  

any(coor_sede=='1')


# pega sede dos municipios BR
coor_br.cidades <- rep(NA,length(decimalLatitude)) 
lat.br.cidades.pto <- br.cidades.pto@coords[,2]
log.br.cidades.pto <- br.cidades.pto@coords[,1]

for(i in 1:length(coor_br.cidades))
{  

  # teste exato
  filas=which(lat.br.cidades.pto %in% decimalLatitude[i]) #elige decimalLatitudes iguales a las decimalLatitudes de municipalidades
  respuesta=NULL
  for(j in filas){# revisa  si para esas latitides iguales tambien las decimalLongitudes son igules
    resp= ifelse(decimalLongitude[i]-log.br.cidades.pto[j]<0.015,1,0)
    respuesta=c(respuesta,resp)
  }

  # si lat y longitud son igual a la de la municipalidad pone 1 de lo contrario 0
  coor_br.cidades[i]=ifelse(TRUE %in% respuesta,1,0)
  print(paste0(i, '=',coor_br.cidades[i]))
}  
any(coor_br.cidades=='1')

i=1
cat(lat.br.cidades.pto[i], log.br.cidades.pto[i])


#ibge

# area edificada
check.ibge.area.edificada <- rep(NA,length(decimalLatitude)) 
ovm.tmp =over(PointToPaleoClim,ibge.area.edificada)
check.ibge.area.edificada = ifelse(!is.na(ovm.tmp$NOME),1,0)
any(adm.ed=='1')

# pontos com habitalçao o Brasil
check.ibge.aglomerado_rural_isolado <- check.ibge.capital <- check.ibge.cidade <- 
check.ibge.hab_indigena <- check.ibge.vila <- rep(NA,length(decimalLatitude))  

round.case = 2  # 2 é adequado
for(i in 1:length(check.ibge.aglomerado_rural_isolado))
{  
  #check.ibge.aglomerado_rural_isolado
  filas=which(round(ibge.aglomerado_rural_isolado.df$coords.x2,round.case) %in% round(decimalLatitude[i],round.case)) 
  respuesta=NULL
  for(j in filas)
  { resp=round(decimalLongitude[i],round.case)==round(ibge.aglomerado_rural_isolado.df$coords.x1[j],round.case)
    respuesta=c(respuesta,resp)}
  check.ibge.aglomerado_rural_isolado[i]=ifelse(TRUE %in%respuesta,1,0)
  
  # check.ibge.capital
  filas=which(round(ibge.capital.df$coords.x2,round.case) %in% round(decimalLatitude[i],round.case)) 
  respuesta=NULL
  for(j in filas)
  { resp=round(decimalLongitude[i],round.case)==round(ibge.capital.df$coords.x1[j],round.case)
  respuesta=c(respuesta,resp)}
  check.ibge.capital[i]=ifelse(TRUE %in%respuesta,1,0)

  # check.ibge.cidade
  filas=which(round(ibge.cidade.df$coords.x2,round.case) %in% round(decimalLatitude[i],round.case)) 
  respuesta=NULL
  for(j in filas)
  { resp=round(decimalLongitude[i],round.case)==round(ibge.cidade.df$coords.x1[j],round.case)
  respuesta=c(respuesta,resp)}
  check.ibge.cidade[i]=ifelse(TRUE %in%respuesta,1,0)
  
  # check.ibge.hab_indigena
  filas=which(round(ibge.hab_indigena.df$coords.x2,round.case) %in% round(decimalLatitude[i],round.case)) 
  respuesta=NULL
  for(j in filas)
  { resp=round(decimalLongitude[i],round.case)==round(ibge.hab_indigena.df$coords.x1[j],round.case)
  respuesta=c(respuesta,resp)}
  check.ibge.hab_indigena[i]=ifelse(TRUE %in%respuesta,1,0)
  
  # check.ibge.vila
  filas=which(round(ibge.vila.df$coords.x2,round.case) %in% round(decimalLatitude[i],round.case)) 
  respuesta=NULL
  for(j in filas)
  { resp=round(decimalLongitude[i],round.case)==round(ibge.vila.df$coords.x1[j],round.case)
  respuesta=c(respuesta,resp)}
  check.ibge.vila[i]=ifelse(TRUE %in%respuesta,1,0)

  print(paste0(i, '=',check.ibge.aglomerado_rural_isolado[i]))
}  

any(check.ibge.aglomerado_rural_isolado==1)
any(check.ibge.capital==1)
any(check.ibge.cidade==1)
any(check.ibge.hab_indigena==1)
any(check.ibge.vila==1)

check.ibge.aglomerado_rural_isolado==1
i=check.ibge.aglomerado_rural_isolado==1

cat(decimalLatitude[i],' ',decimalLongitude[i],' ', municipio.adm[i], estado.adm[i])

###-----------------------------------------------------------------------------------------###

# teste de carga e processamento 
# 8737

# View(alldata.table)
# View(alldata$juncao.lista$`Bomarea edulis`$gbif)
# View(alldata$tabela[alldata$tabela$REPOSITORIO=='gbif',])
# View(clean_data$occ.out)
# View(clean_data$occ.in)


# indata=clean_data$occ.in
# View(indata)
# xcol=data.frame(colnames(clean_data$occ.in), stringsAsFactors = F)
# colnames(xcol) <- c('colunas')
# xcol=xcol[order(xcol$colunas),]
# View(data.frame(xcol))

x=data.frame(clean_data$occ.in, stringsAsFactors = F)
View(x)
#identificação
notas <-paste0(ifelse(is.na(x$fieldNotes),'',x$fieldNotes),
               ifelse(is.na(x$eventRemarks),'',x$eventRemarks),
               ifelse(is.na(x$eventRemarks2),'',x$eventRemarks2),
               ifelse(is.na(x$eventRemarks3),'',x$eventRemarks3),
               ifelse(is.na(x$habitat),'',x$habitat))

clean_data.result <- data.frame(
  source = as.character(x$source),
  scientificName = as.character(x$scientificName),
  scientificNameKey = as.character(x$scientificNameKey),
  identificationQualifier = as.character(x$identificationQualifier),
  taxonRan = as.character(x$taxonRan),
  identifiedBy = as.character(x$identifiedBy),
  dayIdentifie = as.character(x$dayIdentifie),
  monthIdentifie = as.character(x$monthIdentifie),
  yearIdentifie = as.character(x$yearIdentifie),
  #
  eventDate = x$eventDate,
  day = x$day,
  month = x$month,
  year = x$year,             
  recordedBy = x$recordedBy,          
  recordNumber = x$recordNumber,
  recordedByStandardized = as.character(x$recordedByStandardized),
  recordNumberStandardized = as.character(x$recordNumberStandardized),
  fieldNotes = as.character(notas),
  #
  decimalLongitude = as.numeric(x$decimalLongitude),
  decimalLatitude = as.numeric(x$decimalLatitude),
  country = x$country,
  countryCode = x$countryCode,
  elevation = x$elevation,
  county = x$county,
  locality = x$locality,
  #
  suggest_country = x$suggest_country,
  suggest_state = x$suggest_state,
  suggest_mun = x$suggest_mun,
  ok_country = x$ok_country,
  ok_state = x$ok_state,
  ok_mun = x$ok_mun,
  coord_mun=x$coord_mun,
  coord_mun_cent=x$coord_mun_cent,
  Longmuncen = as.numeric(x$Longmuncen),
  Latmuncent = as.numeric(x$Latmuncent),
  decimalLongitudeMunicipality = x$decimalLongitudeMunicipality,
  decimalLatitudeMunicipality = x$decimalLatitudeMunicipality,
  stringsAsFactors  =  F)


View(clean_data.result[clean_data.result$coord_mun==1,c('suggest_mun','Latmuncent','Longmuncen','decimalLatitude','decimalLongitude')])      



fwrite(clean_data$occ.out,'registros_clean_out.txt')
fwrite(clean_data$occ.in,'registros_clean_in.txt')
fwrite(clean_data.result,'registros_clean_in_OK.txt')

row01 = colSums(as.numeric(ifelse(is.na(x$coord_mun_cent),0,x$coord_mun_cent)))


# scientificName          
# scientificNameKey       
# identificationQualifier 
# taxonRan                
# identifiedBy            
# dayIdentifie            
# monthIdentifie          
# yearIdentifie                      
# #evento
# eventDate               
# day                     
# month                   
# year                    
# recordedBy              
# recordNumber
# 
# recordedByStandardized
# recordNumberStandardized
# 
# 
# 
# notas <- paste0(ifelse(is.na(x$fieldNotes),'',x$fieldNotes),
#                 ifelse(is.na(x$eventRemarks),'',x$eventRemarks),
#                 ifelse(is.na(x$eventRemarks2),'',x$eventRemarks2),
#                 ifelse(is.na(x$eventRemarks3),'',x$eventRemarks3),
#                 ifelse(is.na(x$habitat),'',x$habitat))
# 
# # localizacao
# decimalLongitude
# decimalLatitude
# 
# country
# countryCode
# 
# elevation
# county
# locality
# 
# suggest_country
# suggest_state
# suggest_mun
# ok_country
# ok_state
# ok_mun
# decimalLongitudeMunicipality
# decimalLatitudeMunicipality
# coord_mun
# coord_mun_cent
# 
# 
# #coleção
# source
# catalogNumber
# collectionCode
# institutionCode


write.table(alldata.table,file='alldata.csv',sep=';', row.names=F,na='')

View(alldata.table)
###---------------------------------------------------------------------###





###---------------------------------------------------------------------###
xxx=clean_data$occ.in$coord_mun_cent_city==1
xxx=na.omit(xxx)
View(clean_data$occ.in)

#################deu certo
# cidade
# teste <- clean_data$occ.in[!is.na(clean_data$occ.in$decimalLatitudeMunicipality),]
ii = 1
ff = 1
max = nrow(clean_data$occ.in)

xy = geocode(paste0(clean_data$occ.in$suggest_mun[ii:ff],' ',clean_data$occ.in$suggest_state[ii:ff] ))


xy = street2coordinates(paste0(clean_data$occ.in$suggest_mun[ii:ff],' BA ',clean_data$occ.in$suggest_country[ii:ff] )
                   , session=getCurlHandle())

xyplo <- html2text(coordinates2politics(clean_data$occ.in$decimalLatitude[ii], 
                     clean_data$occ.in$decimalLongitude[ii], 
                     session=getCurlHandle()))
xyplo= coordinates2politics(37.769456, -122.429128)


library(ggmap)
res <- lapply(with(xys, paste(decimalLatitude, decimalLongitude, sep = ",")), geocode, output = "more")

res[[1]]$country                     # pais
res[[1]]$administrative_area_level_1 # uf
res[[1]]$administrative_area_level_2 # municipio
res[[1]]$lat
res[[1]]$lon

# daqui pra fente


l = 1
clean_data$occ.in$in_city=NA
clean_data$occ.in$reference_city=NA
clean_data$occ.in$lon_city=NA
clean_data$occ.in$lat_city=NA

library(googleway)
require(R.utils)

for( ii in 1:NROW(clean_data$occ.in)) 
{
  pais <- pains.cod <- estado <- estado.cod <-  municipio <- ''
  
  res2 <- google_geocode(paste0(clean_data$occ.in$decimalLatitude[ii],' ',clean_data$occ.in$decimalLongitude[ii]),
                  key = keygoogle3,
                  simplify=T)
  dataf <- res2[[1]][[1]] 
  
  transform(res2, city = sapply(res, "[[", "administrative_area_level_1"))
  
                   
  res2 <- lapply(with(clean_data$occ.in[ii,], paste(clean_data$occ.in$decimalLatitude[ii], clean_data$occ.in$decimalLongitude[ii], sep = ",")), 
                 geocode, 
                 output = "all",
                 source="google",
                 force=T)

  Sys.sleep(2)

  # res2 <- lapply(with(clean_data$occ.in[ii,], paste(decimalLatitude, decimalLongitude, sep = ",")), geocode, output = "all", source='dsk')
  pais <- res2[[1]]$results[[1]]$address_components[[4]]$long_name
  pains.cod <- res2[[l]]$results[[1]]$address_components[[4]]$short_name
  estado <- res2[[l]]$results[[1]]$address_components[[3]]$long_name
  estado.cod <- res2[[l]]$results[[1]]$address_components[[3]]$short_name
  municipio <- res2[[l]]$results[[1]]$address_components[[2]]$long_name
  
  if (municipio!='') {
  
  try({loc <-geocode(paste0(municipio,', ',estado.cod,', municipio' ),
                      output = "latlon",
                      source="google",
                      force=T)}, silent = TRUE) 
  
  Sys.sleep(2)
  
  # setSessionTimeLimit(cpu = 'Inf', elapsed = 10)
  
  # try(loc <- geocode(paste0(municipio,', ',estado.cod,', municipio' )))
  # lat.city <- loc$lat
  # lng.city <- loc$lng

  dif.lon.city=abs(clean_data$occ.in$decimalLongitude[ii]-loc$lon)
  dif.lat.city=abs(clean_data$occ.in$decimalLatitude[ii]-loc$lat)
  dif.city=cbind(dif.lon.city,dif.lat.city)
  iqual.city=which(dif.city[,1]<0.1 & dif.city[,2]<0.1)
  
  
  clean_data$occ.in$reference_city[ii] <- paste0(pais,', ',estado,', ',municipio)
  clean_data$occ.in$lon_city[ii] <- loc$lon
  clean_data$occ.in$lat_city[ii] <- loc$lat
  
  if (length(iqual.city)!=0) { clean_data$occ.in$in_city[ii]=1 }
  }
  else
  {
    clean_data$occ.in$in_city[ii]=0
  }  

}


# Use local instance of DSTK
options("RDSTK_api_base"="http://localhost:8080")

# Revert to original DSTK API
options("RDSTK_api_base"="http://www.datasciencetoolkit.org")



# municipio e pais
cidade_pais = geocode(paste0(clean_data$occ.in$suggest_mun,' ',clean_data$occ.in$suggest_country[ff:ii] ))



xy = geocode( paste0(clean_data$occ.in$suggest_mun[ii:ff],' ',clean_data$occ.in$suggest_country[ii:ff] )
  ,  source = "dsk")



dif.lon.city=abs(clean_data$occ.in$decimalLongitude[ii:ff]-xy$lon)
dif.lat.city=abs(clean_data$occ.in$decimalLatitude[ii:ff]-xy$lat)
dif.city=cbind(dif.lon.city,dif.lat.city)

iqual.city=which(dif.city[,1]<0.01 & dif.city[,2]<0.01)
xy$coord_mun_cent_city=NA
xy$city=NA
if (length(iqual.city)!=0) { xy$coord_mun_cent_city[iqual.city]=1 }
xy$coord_mun_cent_city


######
#fim


novoscoletores<-compilacoletores(alldata.table)
View(novoscoletores)
write.table(novoscoletores,file='novoscoletores.csv',append = FALSE, quote = TRUE, sep = ";", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = "double")
msg("Conferir arquivo:novoscoletores.csv")

###---------------------------------------------------------------------###

#novoscoletores = read.csv(file="novoscoletoresCorrigido.csv",sep='\t', dec='.',heade=T, as.is=T)

alldata.table <- atualizacoletores(alldata.table,novoscoletores)

###---------------------------------------------------------------------###

# Limpeza

all.records<-alldata.table
all.records$catalognumber=all.records$catalogNumber
all.records$collectioncode=all.records$collectionCode
all.records$collector=all.records$recordedBy
all.records$collectornumber=all.records$recordNumber
all.records$daycollected=all.records$day
all.records$locality=all.records$locality
all.records$monthcollected=all.records$month
all.records$notes=all.records$fieldNotes
all.records$scientificname=all.records$NOMECIENTIFICO_VALIDO
all.records$source=all.records$REPOSITORIO

all.records$latitude=as.numeric(all.records$decimalLatitude)
all.records$longitude=as.numeric(all.records$decimalLongitude)

all.records$country=all.records$country
all.records$stateprovince=all.records$stateProvince
all.records$yearcollected=(all.records$year)

cidade <- rep(NA,NROW(all.records))
k=!is.na(all.records$county)
for (m in 1:NROW(all.records))
{
cidade[m] <- ifelse(!is.na(all.records$municipality[m]), all.records$municipality[m], 
                    ifelse(!is.na(all.records$county[m]),all.records$county[m],'não informado'))
}
all.records$county=cidade

#load geographic data
info_geografica(all.records)

#plot(background.pol)
#Clean the data
clean_data=limpeza(all.records, background.pol=background.pol)

###---------------------------------------------------------------------###

# teste de carga e processamento 
# 8737


View(alldata.table)

View(alldata$juncao.lista$`Bomarea edulis`$gbif)
View(alldata$tabela[alldata$tabela$REPOSITORIO=='gbif',])
View(clean_data$occ.out)
View(clean_data$occ.in)

data.test <- data.frame(clean_data$occ.in$REPOSITORIO, 
                        clean_data$occ.in$county, 
                        clean_data$occ.in$municipality, 
                        clean_data$occ.in$ok_mun,
                        clean_data$occ.in$suggest_mun,
                        clean_data$occ.in$suggest_state,
                        clean_data$occ.in$suggest_country)
View(data.test)

lala <- 'TimbÃ³'
enc2native(lala)
enc2utf8(lala)

Encoding(lala) <- "latin1"
Encoding(lala) <- "UTF-8"

lala

intToUtf8(lala)

# grava
fwrite(clean_data$occ.out,'registros_clean_out.txt')
fwrite(clean_data$occ.in,'registros_clean_in.txt')

y<-data.frame(colnames(clean_data$occ.in))
colnames(y)<-c('nomecoluna')
fwrite(y, 'colnames.txt')



##############################################################################
####  Limpeza
all.records<-alldata.table
all.records<-alldata.table.novcol
all.records$catalognumber=all.records$INPA.TOMBAMENTO
all.records$collectioncode=all.records$HERBARIA
all.records$collector=all.records$COLETORNOME_CORRIGIDO
all.records$collectornumber=all.records$COLETANUMERO_SO
all.records$country=all.records$COUNTRY
all.records$daycollected=all.records$DIACOLETA
all.records$identifiedby=all.records$DETERMINADOR
all.records$latitude=as.numeric(all.records$LATITUDEDG)
all.records$locality=all.records$GAZETTEER
all.records$longitude=as.numeric(all.records$LONGITUDEDG)
all.records$monthcollected=all.records$MESCOLETA
all.records$notes=all.records$NOTASCOLETA
all.records$scientificname=all.records$NOMECIENTIFICO_VALIDO
all.records$source=all.records$REPOSITORIO
all.records$stateprovince=all.records$MAJORAREA
all.records$yearcollected=as.integer(all.records$ANOCOLETA)
all.records$county=all.records$MINORAREA

#unique(all.records$country)
# nao baixa os mapas para esses paises 
all.records = all.records[all.records$country!='Belize',]
all.records = all.records[all.records$country!='Grenada',]
all.records = all.records[all.records$country!='Jamaica',]
all.records = all.records[all.records$country!='Trinidad and Tobago',]
all.records = all.records[all.records$country!='Puerto Rico',]
all.records = all.records[all.records$longitude!='NA',]
all.records = all.records[all.records$longitude!='',]

##### 6.Clean data
#info_geografica(all.records) # spatialEco
setwd(dir.gis)
source('info_geografica.R')
setwd(dir.root)

# Clean the data
clean_data = limpeza(all.records, background.pol = background.pol,
                     remove_latlongNA0=TRUE, 
                     remove_anoNA=TRUE,
                     remove_limiteespacial=TRUE,
                     check.country=T,
                     check.state=T,
                     check.municipality=T,
                     check.citycoor=T,
                     check.centroid=T)

View(clean_data)
save_occurrence_records(clean_data,'all_records_clean.txt',project,'all')



##############################################################################



#h? v?rios registros em gbif que n?o tem nome de coletor nem numero de coleta 
#isso parece que n?o faz sentido (vamos ignorar e eliminar esses dados)
#nem um nem o outro a gente quer 

vl = is.na(alldata$SOBRENOME) | is.na(alldata$COLETANUMERO_SO)
sum(vl)
alldata = alldata[!vl,]

#agora os dados tem muito menos registros, mas pelo menos h? informa??o de coletor e n?mero para todos

vl = is.na(alldata2$SOBRENOME) | is.na(alldata2$COLETANUMERO_SO)
sum(vl)
dim(alldata2)

############################
#CRIA IDENTIFICADORES DAS COLETAS COM AS COLUNAS PADRONIZADAS

#faz um completo
idd = paste(alldata$SOBRENOME,alldata$COLETANUMERO_SO,alldata$DIACOLETA,alldata$MESCOLETA,alldata$ANOCOLETA,sep="_")
alldata$IDENTIFICADOR = idd

#quantos valores ?nicos est?o duplicados e representam registros da mesma coleta em diferentes bancos de dados e reposit?rios
sum(duplicated(idd))

#elimina dessas duplicacoes aquelas linhas que s?o id?nticas quando ignoramos a coluna REPOSITORIO
rownames(alldata) = paste("linha",1:nrow(alldata),sep='')
clss = colnames(alldata)
clss = clss[!clss=='REPOSITORIO']

ndados = unique(result.juncao.tmp[,clss])
rn = rownames(ndados)
ndados$REPOSITORIO = result.juncao.tmp[rn,'REPOSITORIO']

#quantos registros foram apagados
nrow(alldata)-nrow(alldata2)

#atualiza o objeto dados
result.juncao.tmp = ndados

#pega o identificador
idd = result.juncao.tmp$IDENTIFICADOR
sum(duplicated(idd))

#ainda h? muitas duplica??es (essas, j? n?o s?o identicas e precisam ser comparadas)

#separa os dados duplicados dos dados nao_duplicados (ou que tem 1 registro por IDENTIFICADOR)
idsdups = idd[duplicated(idd)]
idsunicos = idd[!idd%in%idsdups]

#quais sao os unicos
vl = idd%in%idsunicos
#salva os unicos num objeto a parte
novodados1 = result.juncao.tmp[vl,]

#salva isso temporariamente num arquivo na m?quina
write.table(novodados1,file=paste0(project,'-todas-id.csv'), sep='\t', na='',row.names=F, quote=TRUE)

write.table(novodados1,file="licariaherbariaSOUNICOS.csv", sep="\t", quote=T, na='', row.names=F)




unique(result.juncao2$NOMECIENTIFICO_VALIDO)


#salva os dados num arquivo
write.table(result.juncao2,file=paste0(project,'-todas.csv'), sep='\t', na='',row.names=F, quote=TRUE)


# na lista
nvdados.list=spp.dados
for (n.sp in 1:length(nvdados.list)) {
  nvdados=nvdados.list[[n.sp]]
  for(n in 1:length(dados)){
    dad = dados[[n]]
    base = names(dados)[n]	
    print(base)
    colet = dad$COLETORNOME
    ncolet = as.vector(lapply(colet, atualizacoletores, nvcol= novoscoletores), mode='character')
    dad$COLETORNOME_CORRIGIDO = ncolet
    dados[[n]] = dad}
  nvdados.list[[n.sp]]=nvdados
}

#d = dados[['gbif']]
#um dado estranho que tem data como nome de coletor e campo de data vazio (preenchendo)
#d[d$COLETORNOME=='23-11-1965',c("DIACOLETA","MESCOLETA","ANOCOLETA")] = c(23,11,1965)
#dados[['gbif']] = d
##############################################################################
nvdados.list=spp.dados

### dividir nome valido e sinonomo de uma lista maior 
### Juntar dados de nomes validos e sinonimos
#for (i in 1:NROW(spp)){
sp.valido <- spp[i]
spp.search=nomes_sinonimos_florabr(sp.valido, return_type ='names_synonyms')
occ <- {}
for (s in 1:NROW(spp.search)){
  sp <- spp.search[s,1]
  cat(' (',i,'-',sp)
  file.txt <- paste0(getwd(),'/OccurrenceRecords/',project,'/' ,data_source,'/',sp,'.tmp')
  if(file.exists(file.txt)){
    x<-read.table(file=file.txt,sep='\t', header=T,as.is=T)
    if (NROW(x)>0)
    {occ <- rbind(x,occ)}
    else{cat(' =','SEM REGISTROS! ')}
  }else{cat(' =','n?o encontrado!) (',file.txt,')')}
}
if (NROW(occ)>0){save_occurrence_records(x, sp.valido, project, data_source)}
}



## JABOT
# dicas de acessar registro jbrj pelo jabot$codtestemunho
# http://jabot.jbrj.gov.br/v2/ficha.php?codtestemunho=345157
# 
## Kew pelo barcode
# http://specimens.kew.org/herbarium/K000442332 
#  para baixar: http://apps.kew.org/herbcat/downloadData.do?downloadCode=2&barcode=K000442332
# 
# 
## NYBG 
# em ccurrenceDetails substituir specimen.php? por http://sweetgum.nybg.org/science/vh/specimen_details.php?irn=804656
# 
# 
# http://cerrado.rbge.org.uk/cerrado/database/search.php?genus=&code=&state=&locality=&cfg=cerrado%2Fqueryform.cfg
# 
# reflora: http://reflora.jbrj.gov.br/reflora/herbarioVirtual/ConsultaPublicoHVUC/ConsultaPublicoHVUC.do?idTestemunho=3065916
# 
# http://gbif.sibbr.gov.br/explorador/pt/busca?view=table&1_f=100&1_o=EQ&1_v_1=Bomarea+edulis&2_f=16&2_o=EQ&2_v_1=Bomarea+edulis+var.+grandis&3_f=16&3_o=EQ&3_v_1=Begonia+reniformis&4_f=16&4_o=EQ&4_v_1=Begonia+reniformis+grandis&5_f=16&5_o=EQ&5_v_1=Averrhoidium+paraguaiense&6_f=16&6_o=EQ&6_v_1=Aralia+warmingiana&7_f=16&7_o=EQ&7_v_1=Adiantum+lorentzii&8_f=16&8_o=EQ&8_v_1=Brasiliopuntia+brasiliensis&9_f=16&9_o=EQ&9_v_1=Cavanillesia+umbellata
# 
# http://ipt.jbrj.gov.br/jbrj/resource?r=lista_especies_flora_brasil#downloads
# 


#######################################################################################

# teste avulso
clean_data$occ.in[3818,]
# testes

diferenca.lon = 0.015 #0.01 #0.015
diferenca.lat = 0.015 #0.01 #0.015


res <- google_geocode(paste0('-25.48 -57.23000'),
                      key = keygoogle3,
                      simplify=T)
# res <- google_geocode(paste0(clean_data$occ.in$decimalLatitude[ii],' ',clean_data$occ.in$decimalLongitude[ii]),
#                       key = keygoogle3,
#                       simplify=T)
Sys.sleep(2)

if (res$status == "OVER_QUERY_LIMIT"){ print("OVER_QUERY_LIMIT") ;break}

enderecos = res$results$address_components[[1]]
tipos.endereco = enderecos$types 

pais <- pains.cod <- estado <- estado.cod <-  municipio <- ''

for (l in 1:length(tipos.endereco)){
  print(l)
  if (tipos.endereco[[l]][1] == 'administrative_area_level_2')
  {municipio <- enderecos$short_name[[l]][1]}          
  
  if (tipos.endereco[[l]][1] == 'administrative_area_level_1')
  {estado <- enderecos$long_name[[l]][1]
  estado.cod <- enderecos$short_name[[l]][1]}          
  
  if (tipos.endereco[[l]][1] == 'country')
  {pais <- enderecos$long_name[[l]][1]
  pais.cod <- enderecos$long_name[[l]][1]}          
}  

# busca <- ifelse( length(tipos.endereco)<=4,
#                  paste0(pais,', ', estado),
#                  paste0(pais,', ', estado,', ', municipio))  

#busca <- paste0(pais,', ', estado,', ', municipio)
busca <- ifelse( municipio=='',
                 paste0(pais,', ', estado),
                 paste0(pais,', ', estado,', ', municipio))

pais <- ifelse(is.na(clean_data$occ.in$suggest_country[1]), '', clean_data$occ.in$suggest_country[1])
estado <- ifelse(is.na(clean_data$occ.in$suggest_state[1]), '', clean_data$occ.in$suggest_state[1])
municipio <- ifelse(is.na(clean_data$occ.in$suggest_mun[1]), '', clean_data$occ.in$suggest_mun[1])

busca <- paste0('Paraguay, Pirayú')


cat(busca, print(ii))

# if (length(tipos.endereco)==4) {municipio <- 'Sem Adm2'}
# if (length(tipos.endereco)==3) {municipio <- 'Sem Adm2'; pais <- 'Sem Adm1'} # será ?

res2 <- google_geocode(busca,
                       key = keygoogle3,
                       simplify=T)

loc <- res2$results$geometry$location

# res2$results$formatted_address

dif.lon.city=abs(clean_data$occ.in$decimalLongitude[ii]-loc$lng)
dif.lat.city=abs(clean_data$occ.in$decimalLatitude[ii]-loc$lat)
dif.city=cbind(dif.lon.city,dif.lat.city)
iqual.city=which(dif.city[,1] < diferenca.lon & dif.city[,2] < diferenca.lat)

clean_data$occ.in$reference_city[ii] <- paste0(pais,', ',municipio)
clean_data$occ.in$lon_city[ii] <- loc$lng
clean_data$occ.in$lat_city[ii] <- loc$lat

clean_data$occ.in$in_city[ii] <- ifelse(length(iqual.city)!=0,1,0)  

cat(busca,' ', clean_data$occ.in$in_city[ii],' ',print(ii))
