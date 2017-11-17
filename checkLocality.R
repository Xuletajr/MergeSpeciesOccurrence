# fantastico 
# imagem
# https://cran.r-project.org/web/packages/magick/vignettes/intro.html


### funções auxiliares conferir localidades ###

# Pablo Hendrigo Alves de Melo - pablopains@yahoo.com.br

###  checkLocality	###

# https://www.labgis.uerj.br/fontes_dados_busca.php?g=0
# https://koordinates.com/my/downloads/
# https://koordinates.com/data/global/?q=cityworks

### -------------------------------------------------------------------------------- ###

# todas as cidades de mundo
# http://dev.maxmind.com/geoip/legacy/geolite/

# NGA GEOnet Names Server
# http://geonames.nga.mil/gns/html/namefiles.html

# http://www.diva-gis.org/gdata - principais baeses , bom prara material e métodos

# Detalha a relacao entre as areas adm:
# http://www.naturalearthdata.com/downloads/10m-cultural-vectors/10m-admin-0-details/

# contornos e grids
# Natural Earth Version 1.1 - http://www.naturalearthdata.com/forums/topic/download-urls-double-slash/

### -------------------------------------------------------------------------------- ###
#muitos mapas
# https://sosgisbr.com//?s=bacias&search=Ir

### -------------------------------------------------------------------------------- ###
# 1.Iniciar
### -------------------------------------------------------------------------------- ###

#filtro
#filter(occ.data, grepl(c("Lombardi"),recordedBy))

# Anelise nicho
# http://nichea.sourceforge.net/function_pca.html

# limpar wd
rm(list = ls())
memory.limit(size = 1.75e13) 

# 1.1 - redirecionar tempdit 
tempdir <- function() "D:\\temps"
unlockBinding("tempdir", baseenv())
assignInNamespace("tempdir", tempdir, ns="base", envir=baseenv())
assign("tempdir", tempdir, baseenv())
lockBinding("tempdir", baseenv())
tempdir()

if(!require(pacman)) install.packages("pacman")
pacman::p_load(taxize, divagis, ggmap, mapr, sp, rgdal, maptools, dplyr, rgbif, BIEN)


###---------------------------------------------------------------------###
# 2.Informar parâmetros
###---------------------------------------------------------------------###

# https://pt.wikipedia.org/wiki/Graus_decimais
# 1 região ou pais = ente 111.32 km	102.47 km
# 0.1 = grande cidade - entre 11.132 km	10.247 km	
# 0.01 = audeia ou vila - entre 1.1132 km	1.0247 km	
# 0.001 = bairro ou rua - entre 111.32 m	102.47 m
round.case = 2  # 2 é adequado

project = "plantascalcario-off"
dir.root= "C:/Dados/GitHub/MergeSpeciesOccurrence"; setwd(dir.root)

### -------------------------------------------------------------------------------- ###
# carregar funções
### -------------------------------------------------------------------------------- ###

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

### -------------------------------------------------------------------------------- ###
# carregar shapes

#CRS.new=CRSargs(CRS("+proj=longlat +ellps=WGS84 +no_defs +datum=WGS84"))
CRS.new=CRSargs(CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

### -------------------------------------------------------------------------------- ###
# neotropico
path.ev <- 'C:/Dados/GitHub/environmental_data'
mask.path <- paste0(path.ev,'/mask/'); setwd(mask.path)
neotropico <- readShapePoly(fn="Lowenberg_Neto_Neotropics.shp")
proj4string(neotropico) <- CRS.new 


# world cortado para o neotropico

data(world.cities)
sedes<<-world.cities

# adm mundo
adm.path <- 'E:/environmental_data/gadm/gadm28.shp/neo'; setwd(adm.path)
neo.adm=readShapePoly("neo_gadm28.shp"); proj4string(neo.adm) <- CRS.new 

# produce centroids
cents <- gCentroid(neo.adm,byid=T)
cents0<-as.data.frame(coordinates(cents))
cents1<<-SpatialPointsDataFrame(cents,data=cents0)
b=over(neo.adm, cents1)
centroid=neo.adm
centroid@data$Longmuncen<-b$x
centroid@data$Latmuncent<-b$y
proj4string(centroid) <- CRS.new 
centroid<<-centroid

#NGA GEOnet Names Server - http://geonames.nga.mil/gns/html/namefiles.html
city.path <- 'E:/environmental_data/NGA GEOnet Names Server/geonames_20171002/neo'; setwd(city.path)
city.GEOnet.neo <- readShapePoints('neo_Countries.shp') 
proj4string(city.GEOnet.neo) <- CRS.new 

#http://dev.maxmind.com/geoip/legacy/geolite/
city.path <- 'E:/environmental_data/GeoLite/GeoLiteCity-latest/neo'; setwd(city.path)
city.wld <- readShapePoints('neo_GeoLiteCity-Location.shp') 
proj4string(city.wld) <- CRS.new 
#plot(cyty.wld)

# lgm http://anthro.unige.ch/lgmvegetation/#top
veg.lgm.path = 'E:/environmental_data/VegetationMapWorldLGM/world_cut.shp/neo'; setwd(veg.lgm.path)
vegetationLGM = readShapePoly("neo_world_cut.shp"); 
proj4string(vegetationLGM) <- CRS.new 
vegetationLGM.df = data.frame(vegetationLGM)
vegetationLGM.df$LEG_VEG = NA

lgm_vegLGM = fread('lgm_veg.txt')
colnames(lgm_vegLGM) = c('VEG_ID','LEG_VEG')
for( index in 1:NROW(lgm_vegLGM))
  {vegetationLGM.df$LEG_VEG[index] = lgm_vegLGM$LEG_VEG[lgm_vegLGM$VEG_ID==vegetationLGM.df$VEG_ID[index]]}
vegetationLGM = SpatialPolygonsDataFrame(vegetationLGM,vegetationLGM.df)

### -------------------------------------------------------------------------------- ###

# #KoppenBrazil2013
# #path.koppen <- 'E:/environmental_data/KoppenBrazil2013/Raster/neo'; setwd(path.koppen)
# path.koppen <- 'E:/environmental_data/KoppenBrazil2013/Raster'; setwd(path.koppen)
# 
# tif <- list.files(patt = ".ovr")
# 
# koppen.neo <- stack(tif); koppen.neo <- na.omit(koppen.neo)
# koppen.neo - polygonizer(koppen.neo)
# #str(koppen.neo); plot(koppen.neo)
# 
# proj4string(koppen.neo) <- CRS.new 
# 
# require(raster)
# require(rgeos)
# 
# out <- gIntersection(koppen.neo[['koppen_paper']], neotropico, byid=TRUE)
# 
# plot(x)
# plot(out, add = T, col = "lightgreen")
# 



### -------------------------------------------------------------------------------- ###
# karst

path.ev <- 'C:/Dados/GitHub/environmental_data/mask'; setwd(path.ev)
karst_south_america2 <- readOGR(dsn=path.ev, layer="Karst_South_America"); proj4string(karst_south_america2) <- CRS.new 
karst_brazil <- readOGR(dsn=path.ev, layer="Karst_Brazil2"); proj4string(karst_brazil) <- CRS.new 
karst_carbo_CECAV <- readOGR(dsn=path.ev, layer="Karst_Carbo_CECAV"); proj4string(karst_carbo_CECAV) <- CRS.new 

path.ev <- 'E:/environmental_data/WMCROv3'; setwd(path.ev)
karst_south_america <- readOGR(dsn=path.ev, layer="South_American_Karst"); proj4string(karst_south_america) <- CRS.new 
karst_south_america.df = data.frame(karst_south_america)
karst_south_america = SpatialPolygonsDataFrame(karst_south_america,karst_south_america.df)
#plot(karst_south_america)

### -------------------------------------------------------------------------------- ###
# ibge 
# 

path.ibge.loc <- 'E:/environmental_data/ibge/Localidade_v2015_2016-08-03'
setwd(path.ibge.loc)

ibge.area.edificada = readShapePoly("LOC_Area_Edificada_A.shp")
proj4string(ibge.area.edificada) <- CRS.new 

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

path.ibge.loc <- 'E:/environmental_data/ibge/Geomorfologia_5000mil'
setwd(path.ibge.loc)
ibge.geomorfologia = readShapePoly("geom_area.shp"); proj4string(ibge.geomorfologia) <- CRS.new 

path.ibge.loc <- 'E:/environmental_data/ibge/Solos_5000mil'
setwd(path.ibge.loc)
ibge.solos = readShapePoly("Solos_5000.shp"); proj4string(ibge.solos) <- CRS.new 

# path.ibge.loc <- 'E:/environmental_data/ibge/Vegetacao_radambrasil'
# setwd(path.ibge.loc)
# ibge.vegetacao.radam = readShapePoly("Radam_Vegetacao_SIRGAS.shp")
# proj4string(ibge.vegetacao.radam) <- CRS.new 

path.ibge.loc <- 'E:/environmental_data/ibge/Vegetacao_5000mil'
setwd(path.ibge.loc)
ibge.vegetacao = readShapePoly("Vegetacao_5000.shp"); proj4string(ibge.vegetacao) <- CRS.new 

path.ibge <- 'E:/environmental_data/icmbio/biomas'
setwd(path.ibge)
icmbio.biomas = readShapePoly("biomas.shp"); proj4string(icmbio.biomas) <- CRS.new 

path.icmbio <- 'E:/environmental_data/icmbio/cavernas'
setwd(path.icmbio)

cavernas.carbo = readShapePoints("cavernas br carbonatos.shp"); proj4string(cavernas.carbo) <- CRS.new 
cavernas.carbo.df = data.frame(cavernas.carbo); # plot(cavernas.carbo)

# path.ana <- 'E:/environmental_data/ana'; setwd(path.ana)
# ana.bacias = readShapePoly("baciashidrobr.shp"); proj4string(icmbio.biomas) <- CRS.new 
# ana.bacias.df = data.frame(ana.bacias)
# ana.bacias = SpatialPolygonsDataFrame(ana.bacias, ana.bacias.df)
# proj4string(icmbio.biomas) <- CRS.new 
# # plot(ana.bacias)
path.ana <- 'E:/environmental_data/ana'; setwd(path.ana)
ana.bacias = readShapePoly("baciashidrobr2.shp") #; proj4string(ana.bacias) <- CRS.new
ana.bacias.df = data.frame(ana.bacias)
ana.bacias = SpatialPolygonsDataFrame(ana.bacias, ana.bacias.df)
proj4string(ana.bacias) <- CRS.new


### -------------------------------------------------------------------------------- ###
# carregar dados de ocorrência
### -------------------------------------------------------------------------------- ###

project = "plantascalcario-off"
dir.root= "C:/Dados/GitHub/MergeSpeciesOccurrence"; setwd(dir.root)
dir.gis = 'F:/Dados/GitHub/MergeSpeciesOccurrence/GisBase'
file.spp = "plantascalcario2.txt"

# carregar pontos
spp.csv <- read.table(file.spp, header=FALSE, sep="\t",dec = ".") 
spp <- as.matrix(spp.csv[ ,4])

# carregar e padronizar dado original
setwd(dir.root)
alldata <- MergeSpeciesOccurrence(spp, project)
occ.data <-data.frame(alldata$tabela, stringsAsFactors = F); View(occ.data)
#unique(occ.data$recordedByLastName)

# compilar coletores 
novoscoletores<-compilacoletores(occ.data); View(novoscoletores)

msg('Indexando coletores...')
occ.data$recordedByID = NA
for (c in 1:NROW(novoscoletores))
{
  nreg=(occ.data$recordedBy==novoscoletores$recordedBy[c])
  occ.data$recordedByID[nreg==T] <- novoscoletores$recordedByID[c]
  print(c)
}


write.csv(novoscoletores,paste0('C:/Dados/GitHub/MergeSpeciesOccurrence/OccurrenceRecords/',project,'/novoscoletores.txt'))
novoscoletores <- fread(paste0('C:/Dados/GitHub/MergeSpeciesOccurrence/OccurrenceRecords/',project,'/novoscoletores-corrigido.txt'))
View(novoscoletores)
# for( i in 1:NROW(novoscoletores)){
# enc = as.character(novoscoletores[i,1])
# print(repair_encoding(enc, from = "non-ASCII"))
# #print(utf8ToInt(enc))
# print(enc2native(enc))
# print(enc2utf8(enc))
# 
# 
# }

occ.data <- atualizacoletores(occ.data,novoscoletores); View(occ.data)
unique(occ.data$recordedByStandardized)

# carregar municipality para county 
cidade <- rep(NA,NROW(occ.data))
k=!is.na(occ.data$county)
for (m in 1:NROW(occ.data))
{
  cidade[m] <- ifelse(!is.na(occ.data$municipality[m]), occ.data$municipality[m], 
                      ifelse(!is.na(occ.data$county[m]),occ.data$county[m],NA))
  
  #Encoding(cidade[m]) <- 'latin1'#"UTF-8"
  msg(paste0(m,'-',cidade[m]))
}
occ.data$county=cidade

# lat, long e ano numericos
occ.data$decimalLatitude= as.numeric(occ.data$decimalLatitude)
occ.data$decimalLongitude= as.numeric(occ.data$decimalLongitude)
occ.data$year=as.numeric(occ.data$year)

### -------------------------------------------------------------------------------- ###

# limpeza 1, dados com NA em ano, lat, long  fora do neotropico

occ.tmp = occ.data
index = occ.tmp$decimalLatitude %in% c('-99133333','-19419444','-96.425')
nrow(occ.tmp); occ.tmp = occ.tmp[!index,]; nrow(occ.tmp)

ID=1:nrow(occ.tmp) 
occ.tmp=cbind(ID,occ.tmp)
occ.tmp.out <- occ.tmp.out.temp <- {}

remove_latlongNA0=TRUE
if(remove_latlongNA0==TRUE)
{
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

remove_anoNA=T
if(remove_anoNA==TRUE){
  occ.tmp.out.temp <- occ.tmp[is.na(occ.tmp$year),]
  occ.tmp <- occ.tmp[!is.na(occ.tmp$year),] # retira anos NA 
  if (NROW(occ.tmp.out.temp)>0)
  {remove.type = 'Ano NA'
  occ.tmp.out.temp <- cbind(occ.tmp.out.temp, reason=rep(remove.type,NROW(occ.tmp.out.temp)))
  occ.tmp.out <- rbind(occ.tmp.out,occ.tmp.out.temp)}
}

remove_limiteespacial=TRUE
if(remove_limiteespacial==TRUE & NROW(occ.tmp)>0)
{
  occ.tmp.points=occ.tmp
  occ.tmp.points <- SpatialPointsDataFrame(cbind(occ.tmp$decimalLongitude,occ.tmp$decimalLatitude),occ.tmp)
  # proj4string(occ.tmp.points) <- CRS.new 
  
  #coordinates(occ.tmp.points) <- SpatialPointsDataFrame(cbind(occ.tmp$decimalLongitude,occ.tmp$decimalLatitude),occ.tmp)
  #CRS.new=neotropico@proj4string
  proj4string(occ.tmp.points) <- CRS.new 
  
  occ.tmp.points_in <- occ.tmp.points[!is.na(over(occ.tmp.points ,geometry(neotropico))),] # dentro do poligono
  occ.tmp.points_out <- occ.tmp.points[is.na(over(occ.tmp.points ,geometry(neotropico))),] # fora do poligono
  
  occ.tmp.out.temp <- occ.tmp.points_out@data
  if (NROW(occ.tmp.out.temp)>0)
  {
    remove.type = 'Out of boundary polygon'
    occ.tmp.out.temp <- cbind(occ.tmp.out.temp, reason=rep(remove.type,NROW(occ.tmp.out.temp))) #   Aqui
    occ.tmp.out <- rbind(occ.tmp.out,occ.tmp.out.temp)
  }
}


### -------------------------------------------------------------------------------- ###

# carrega e geo dados in na limpeza 1 

occ.data.in <- occ.tmp.points_in
occ.data.pto <- SpatialPointsDataFrame(cbind.data.frame(occ.data.in$decimalLongitude,
                                                        occ.data.in$decimalLatitude),
                                                        data.frame(occ.data.in))
proj4string(occ.data.pto) <- CRS.new 

plot(neotropico,gridded=TRUE)
plot(occ.data.pto, add=T, col='black', cex=0.1, lwd=4)
#plot(icmbio.biomas, add=T, col='white')
plot(karst_south_america2, add=T, col='blue')
plot(karst_brazil, add=T, col='blue')


# plot(koppen.neo[['koppen_paper']], add=T)
# 
# koppen <- extract(koppen.neo[['koppen_paper']], occ.data.pto)
# str(koppen.neo[['koppen_paper']])

### -------------------------------------------------------------------------------- ###
# processamento
### -------------------------------------------------------------------------------- ###

# plot(neotropico)
# plot(vegetationLGM ,add=T)
# plot(centroid ,add=T)
# plot(ibge.geomorfologia ,add=T)
# plot(ibge.solos ,add=T)
# plot(ibge.vegetacao ,add=T)
# plot(ibge.area.edificada ,add=T)
# plot(city.GEOnet.neo ,add=T)
# plot(city.wld ,add=T)
# plot(sedes ,add=T)
# plot(ibge.aglomerado_rural_isolado,add=T)
# plot(ibge.capital ,add=T)
# plot(ibge.cidade ,add=T)
# plot(ibge.hab_indigena ,add=T)
# plot(ibge.vila ,add=T, col=1)
# plot(karst_south_america ,add=T)
# plot(karst_brazil ,add=T)
# plot(karst_carbo_CECAV ,add=T)
# plot(icmbio.biomas ,add=T)

### -------------------------------------------------------------------------------- ###

# pega dados adm e lat long dos pontos no neotropico

ovm=over(occ.data.pto,centroid)

# "latin1" ou "UTF-8"
pais.adm <- as.character(ovm$NAME_0) ; Encoding(pais.adm) = "UTF-8"
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
decimalLongitude <- as.numeric(occ.data.in$decimalLongitude)
decimalLatitude <- as.numeric(occ.data.in$decimalLatitude)

adm.neotripco = data.frame( pais.adm <- as.character(ovm$NAME_0), 
                            cod.pais.adm <- as.character(ovm$ISO),
                            estado.adm <- as.character(ovm$NAME_1),
                            cod.estado.adm <- as.character(ovm$HASC_1),
                            tip.estado.adm <- as.character(ovm$ENGTYPE_1),
                            municipio.adm <- as.character(ovm$NAME_2),
                            tip.municipio.adm <- as.character(ovm$ENGTYPE_2),
                            distrito.adm <- as.character(ovm$NAME_3),
                            tip.distrito.adm <- as.character(ovm$ENGTYPE_3),
                            Longmuncen.adm <- ifelse(is.na(ovm$Longmuncen),0,as.numeric(ovm$Longmuncen)),
                            Latmuncent.adm <- ifelse(is.na(ovm$Latmuncen),0,as.numeric(ovm$Latmuncen)),
                            decimalLatitude <- as.numeric(occ.data.in$decimalLatitude),
                            decimalLongitude <- as.numeric(occ.data.in$decimalLongitude),
                            stringsAsFactors = F)

### -------------------------------------------------------------------------------- ###
# geomorgoligia BR

ovm.geomorf=over(occ.data.pto, ibge.geomorfologia)
geomorfolgia = data.frame(geomor.unidade = ovm.geomorf$NOM_UNIDAD,
                          geomor.compartimento = ovm.geomorf$DSC_COMPAR,
                          geomor.regiao = ovm.geomorf$NOM_REGIAO,
                          geomor.dominio = ovm.geomorf$NOM_DOMINI,
                          geomor.unidade = ovm.geomorf$NOM_UNIDAD,
                          stringsAsFactors = F)

### -------------------------------------------------------------------------------- ###
# solos BR

ovm.solos=over(occ.data.pto, ibge.solos)
solos = data.frame(solos.simbolo = ovm.solos$COD_SIMBOL,
                          solos.descricao = ovm.solos$DSC_COMPON,
                          solos.textura = ovm.solos$DSC_TEXTUR,
                          solos.componente2 = ovm.solos$DSC_COMPO1,
                          solos.componente2 = ovm.solos$DSC_COMPO2,
                          stringsAsFactors = F)


### -------------------------------------------------------------------------------- ###
# vgetacao BR

ovm.vegetacao = over(occ.data.pto, ibge.vegetacao)
vegetacao = data.frame(vegetacao.cod_leg_ca = ovm.vegetacao$COD_LEG_CA,
                       vegetacao.cod_clas = ovm.vegetacao$COD_CLASS_,
                       vegetacao.dsc_clas = ovm.vegetacao$DSC_CLASS_,
                       vegetacao.dsc.veg.pr = ovm.vegetacao$DSC_VEG_PR,
                       stringsAsFactors = F)
### -------------------------------------------------------------------------------- ###
# Vegetation LGM
ovm.vegetacaoLGM=over(occ.data.pto, vegetationLGM)
# plot(vegetationLGM)
# plot(occ.data.pto, add=T)

vegetacaoLGM = data.frame(vegetacaoLGM.lgmcl = as.character(ovm.vegetacaoLGM$LGMCL_), 
                          vegetacaoLGM.veg.id =as.character(ovm.vegetacaoLGM$VEG_ID),
                          vegetacaoLGM.veg.leg =as.character(ovm.vegetacaoLGM$LEG_VEG),
                          stringsAsFactors = F)
### -------------------------------------------------------------------------------- ###
# bacias
# ovm.ana.bacias=over(occ.data.pto, ana.bacias)
# bacias = as.character(ovm.icmbio.biomas$NO_BIOMA1); Encoding(bioma) = "UTF-8"
# bacias = data.frame(bioma = bioma, stringsAsFactors = F)

ovm.ana.bacias=over(occ.data.pto, ana.bacias)
bacias = as.character(ovm.ana.bacias$FEAT_NAME )
Encoding(bacias) = "UTF-8"
bacias = data.frame(bacia = bacias, stringsAsFactors = F)

### -------------------------------------------------------------------------------- ###
# biomas
ovm.icmbio.biomas=over(occ.data.pto, icmbio.biomas)
bioma = as.character(ovm.icmbio.biomas$NO_BIOMA1); Encoding(bioma) = "UTF-8"
biomas = data.frame(bioma = bioma, stringsAsFactors = F)
### -------------------------------------------------------------------------------- ###
# karst

ovm.karst_south_america = over(occ.data.pto, karst_south_america2)
ovm.karst_brazil = over(occ.data.pto, karst_brazil)
ovm.karst_carbo_CECAV = over(occ.data.pto, karst_carbo_CECAV)

karst.south.america = ifelse(is.na(ovm.karst_south_america$Id),'',1)
karst.brazil = ovm.karst_brazil$Countries # karst.brazil = ifelse(is.na(ovm.karst_brazil$Countries),'',1)
karst.CECAV = ovm.karst_carbo_CECAV$NOME

### -------------------------------------------------------------------------------- ###

# vgetacao BR Radam está em sirgas 2000 nao abre
### -------------------------------------------------------------------------------- ###
# zonas biogeograficas do Neotropico

ovm.neo=over(occ.data.pto,neotropico)
biogeo.neo = data.frame(neo.regiao = ovm.neo$REGION,
                        neo.provincia = ovm.neo$Province_1,
                        neo.dominio = ovm.neo$Dominions,
                        neo.subregiao = ovm.neo$Subregio_1,
                        stringsAsFactors = F)
### -------------------------------------------------------------------------------- ###


# ibge - area edificada
check.ibge.area.edificada <- rep(NA,length(decimalLatitude)) 
ovm.tmp =over(occ.data.pto,ibge.area.edificada)
check.ibge.area.edificada = ifelse(!is.na(ovm.tmp$NOME),1,0)
any(check.ibge.area.edificada=='1')

# sedes do mundo
# centreiode neotropico
# pontos com habitalçao o Brasil
check.cavernas.carbo <- check.cavernas.carbo1 <- rep(NA,length(decimalLatitude))
check.sede.mundo <- check.centroide <- check.city.wld <- check.city.GEOnet.neo <- rep(NA,length(decimalLatitude))
check.ibge.aglomerado_rural_isolado <- check.ibge.capital <- check.ibge.cidade <- 
check.ibge.hab_indigena <- check.ibge.vila <- rep(NA,length(decimalLatitude))  

for(i in 1:length(check.ibge.aglomerado_rural_isolado))
{  

  #
  filas=which(round(cavernas.carbo$LAT_DD , round.case) %in% round(decimalLatitude[i], round.case)) 
  respuesta=NULL
  for(j in filas)
  { resp=round(decimalLongitude[i], round.case)==round(cavernas.carbo$LONG_DD[j], round.case)
  respuesta=c(respuesta,resp)}
  check.cavernas.carbo[i]=ifelse(TRUE %in%respuesta,1,0)
  
  filas=which(round(cavernas.carbo$LAT_DD , 1) %in% round(decimalLatitude[i], 1)) 
  respuesta=NULL
  for(j in filas)
  { resp=round(decimalLongitude[i], 1)==round(cavernas.carbo$LONG_DD[j], 1)
  respuesta=c(respuesta,resp)}
  check.cavernas.carbo1[i]=ifelse(TRUE %in%respuesta,1,0)
  
  #
  filas=which(round(city.GEOnet.neo$LAT , round.case) %in% round(decimalLatitude[i], round.case)) 
  respuesta=NULL
  for(j in filas)
  { resp=round(decimalLongitude[i], round.case)==round(city.GEOnet.neo$LONG[j], round.case)
  respuesta=c(respuesta,resp)}
  check.city.GEOnet.neo[i]=ifelse(TRUE %in%respuesta,1,0)

  #
  filas=which(round(city.wld$latitude , round.case) %in% round(decimalLatitude[i], round.case)) 
  respuesta=NULL
  for(j in filas)
  { resp=round(decimalLongitude[i], round.case)==round(city.wld$longitude[j], round.case)
  respuesta=c(respuesta,resp)}
  check.city.wld[i]=ifelse(TRUE %in%respuesta,1,0)
  
  # sedes do mundo
  filas=which(round(sedes$lat, round.case) %in% round(decimalLatitude[i], round.case)) 
  respuesta=NULL
  for(j in filas)
  { resp=round(decimalLongitude[i], round.case)==round(sedes$long[j], round.case)
  respuesta=c(respuesta,resp)}
  check.sede.mundo[i]=ifelse(TRUE %in%respuesta,1,0)

  #
  # centroides neotropico
  filas=which(round(Latmuncent.adm, round.case) %in% round(decimalLatitude[i], round.case)) 
  respuesta=NULL
  for(j in filas)
  { resp=round(decimalLongitude[i], round.case)==round(Longmuncen.adm[j], round.case)
  respuesta=c(respuesta,resp)}
  check.centroide[i]=ifelse(TRUE %in%respuesta,1,0)
  
  # ibge - localidades
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

  print(i)
}  

check = data.frame(ck.area.edi = check.ibge.area.edificada,  
                   ck.centroid = check.centroide,
                   ck.sede.mundo = check.sede.mundo,
                   ck.city.GEOnet.neo = check.city.GEOnet.neo,
                   ck.city.wld =check.city.wld,
                   ck.agl.rural.isolado = check.ibge.aglomerado_rural_isolado,
                   ck.capital = check.ibge.capital,
                   ck.cidade = check.ibge.cidade,
                   ck.hab.indigena = check.ibge.hab_indigena,
                   ck.vila = check.ibge.vila,
#                   ck.karst.sa = karst.south.america,
#                   ck.karst.br = karst.brazil, 
#                   ck.karst.CECAV = karst.CECAV,
#                   check.cavernas.carbo,
#                   check.cavernas.carbo1,
                   stringsAsFactors = F)

checkLocality = cbind.data.frame(check,
                                 adm.neotripco,
                                 occ.data.in,
                                 stringsAsFactors = F)


# checkLocality = cbind.data.frame(check,
#                            adm.neotripco,
#                            biomas,
#                            bacias,
#                            biogeo.neo,
#                            vegetacao,
#                            vegetacaoLGM,
#                            solos,
#                            geomorfolgia,
#                            occ.data.in,
#                            stringsAsFactors = F)
# View(checkLocality)

xxx = edit(checkLocality)

### -------------------------------------------------------------------------------- ###
#CRIA IDENTIFICADORES DAS COLETAS COM AS COLUNAS PADRONIZADAS


#x = cbind.data.frame(adm.neotripco, occ.data.in)

x = checkLocality

x$DupUniqueID = NA
  
# Year não é bom pq sisbbr não tem data
uniqueID.full = paste(x$scientificNameKey,
                      x$recordedByStandardized,
                      x$recordNumberStandardized,
                      x$year,
                      x$municipio.adm,
                      sep="_")

x$uniqueID = uniqueID.full

uniqueID = unique(uniqueID.full)

for (u in 1:NROW(uniqueID))
{

  index = paste(x$scientificNameKey,
                x$recordedByStandardized,
                x$recordNumberStandardized,
                x$year,
                x$municipio.adm,
                sep="_") == uniqueID[u]
  
  x %>% filter(index) -> data
  
  x$DupUniqueID[index]=u

  print(paste0(NROW(data),' = ',uniqueID[u]))

}  

View(x)


x$in_out = NA
x$in_out[x$ck.area.edi==1] ='out'
x$in_out[x$ck.centroid==1] ='out'
x$in_out[x$ck.sede.mundo==1] ='out'
x$in_out[x$ck.city.GEOnet.neo==1] ='out'
x$in_out[x$ck.city.wld==1] ='out'
x$in_out[x$ck.capital==1] ='out'
x$in_out[x$ck.cidade==1] ='out'

fwrite(x,paste0('C:/Dados/GitHub/MergeSpeciesOccurrence/OccurrenceRecords/',project,'/alldata_in_out_dup2.txt'))

index=is.na(x$in_out) # !='out'

x_in = x[index,]

fwrite(x_in,paste0('C:/Dados/GitHub/MergeSpeciesOccurrence/OccurrenceRecords/',project,'/alldata_clean_sem_unique.txt'))


# seleciona ocorrencias unicas
index_uniq = unique(x_in$uniqueID); str(index_uniq)

x_sel.m = {}
for ( u in 1:length(index_uniq))
{
  x_sel = data.frame(x_in[x_in$uniqueID == index_uniq[u],], stringsAsFactors = F)
  x_sel.m  = data.frame(rbind(x_sel.m,x_sel[1,]), stringsAsFactors = F)
  
  print(u)
  if (nrow(x_sel) ==1) {next}
  
  for (l in 2:nrow(x_sel) )
  { 
    for ( c in 1:(ncol(x_sel)) ){
      #print(paste0(l,'-',c))
      if (is.na(x_sel[l,c])) {next}
      x1 = as.character(x_sel.m[u,c]) == as.character(x_sel[l,c])
      if (is.na(x1)) {next}
      if (x1==F) { x_sel.m[u,c] = x_sel[l,c] } 
      # #aqui
      # if (colnames(x_sel)[u]=='source') 
      # {x_sel.m[u,c] = paste0(x_sel.m[u,c],'-',x_sel[l,c])}
      
    }  
  }  
}

fwrite(x_sel.m,paste0('C:/Dados/GitHub/MergeSpeciesOccurrence/OccurrenceRecords/',project,'/alldata_clean_unique_automatico.txt'))

x_sel.m = edit(x_sel.m)


index.ano = x_sel.m$year >= 1950
x_sel.m2 = x_sel.m[index.ano,]; nrow(x_sel.m); nrow(x_sel.m2)
fwrite(x_sel.m2,paste0('C:/Dados/GitHub/MergeSpeciesOccurrence/OccurrenceRecords/',project,'/alldata_clean_unique.txt'))

### -------------------------------------------------------------------------------- ###


# via google

# https://developers.google.com/maps/documentation/javascript/
keygoogle1 = 'AIzaSyAiKBXoGdj0_hZWpvXfNbWpNpSGwD07DeE'
keygoogle2 = 'AIzaSyChCP0l84mu_-aP1w2L0CPX7zsacBOAkQY'
keygoogle3 = 'AIzaSyBTT_2ELu8sG-OHo2whHW4J3SCwllwrQcg'
keygoogle.ativa = keygoogle2

### -------------------------------------------------------------------------------- ###

###  por grupo de coorrdenadas

keygoogle.ativa = keygoogle3
# controle do buffer # sugestão 0.015 e 0.15 # da carol 0.001 e 0.01

diferenca.lon =  0.015 #0.0015
diferenca.lat =  0.15 #0.015 

# lat 0.0830371 lon 0.0078919
keygoogle.ativa = keygoogle1

index <- unique(clean_data$occ.in[,c("decimalLatitude","decimalLongitude")])

index$urban_area=NA
index$reference_city=NA
index$lon_city=NA
index$lat_city=NA
index$pais=NA
index$pais.cod=NA
index$estado=NA
index$estado.cod=NA
index$municipio=NA
index$dif.lat.city=NA
index$dif.lon.city=NA

for( ii in 1:NROW(index)) 
{
  
  if (!is.na(index$urban_area[ii])) {print(paste0(ii,' - já verificado!')); next}
  
  res <- google_geocode(paste0(index$decimalLatitude[ii],' ',index$decimalLongitude[ii]),
                        key = keygoogle.ativa,
                        simplify=T)
  
  # res <- google_geocode(paste0('-18.57333',' ','-64.10444'),
  #                       key = keygoogle.ativa,
  #                       simplify=T)
  # 
  
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
  iqual.city=0
  dif.lon.city=abs(index$decimalLongitude[ii]-loc$lng)
  dif.lat.city=abs(index$decimalLatitude[ii]-loc$lat)
  dif.city=cbind(dif.lon.city,dif.lat.city)
  iqual.city=which(dif.city[,1] < diferenca.lon & dif.city[,2] < diferenca.lat)
  
  index$reference_city[ii] <- busca #paste0(pais,', ',estado,', ',municipio)
  index$lon_city[ii] <- loc$lng
  index$lat_city[ii] <- loc$lat
  index$pais[ii] <- pais
  index$pais.cod[ii] <- pais.cod
  index$estado[ii] <- estado
  index$estado.cod[ii] <- estado.cod
  index$municipio[ii] <- municipio
  index$dif.lat.city[ii] <- dif.lat.city
  index$dif.lon.city[ii] <- dif.lon.city
  
  
  
  index2 <- (clean_data$occ.in$decimalLatitude == index$decimalLatitude[ii] &
               clean_data$occ.in$decimalLongitude == index$decimalLongitude[ii])
  
  if (length(iqual.city)!=0) { index$urban_area[ii] = 0 } 
  else { index$urban_area[ii] = 1 }   
  
  cat(' >> ',busca,' ', index$urban_areay[ii])
}

x <- clean_data$occ.in
x$urban_area=NA
x$reference_city=NA
x$lon_city=NA
x$lat_city=NA
x$pais=NA
x$pais.cod=NA
x$estado=NA
x$estado.cod=NA
x$municipio=NA
x$dif.lat.city=NA
x$dif.lon.city=NA

# regarrega checagem 
n=NROW(x)
for (l in 1:NROW(x))
{
  index2 = (index$decimalLatitude ==x$decimalLatitude[l]  &
              index$decimalLongitude == x$decimalLongitude[l] )
  
  x$urban_area[l] <- index$urban_area[index2]
  x$reference_city[l] <- index$reference_city[index2]
  x$lon_city[l] <- index$lon_city[index2]
  x$lat_city[l] <- index$lat_city[index2]
  x$pais[l] <- index$pais[index2]
  x$pais.cod[l] <- index$pais.cod[index2]
  x$estado[l] <- index$estado[index2]
  x$estado.cod[l] <- index$estado.cod[index2]
  x$municipio[l] <- index$municipio[index2]
  x$dif.lat.city[l] <- index$dif.lat.city[index2]
  x$dif.lon.city[l] <- index$dif.lon.city[index2]
  
  print(paste0(l, ' de ', n, ' = (',x$urban_area[l],')'))
}  

View(x)

xxx=clean_data$occ.in[!is.na(clean_data$occ.in$urban_areay),]
colnames(xxx)

### -------------------------------------------------------------------------------- ###

