### prepara dados mobot ###

# Pablo Hendrigo Alves de Melo - pablopains@yahoo.com.br

###  datamobot	###

###---------------------------------------------------------------------###

# Prepara dados mobot - junta dados do KML com dados baixados do site,
# baixar dados em http://www.tropicos.org/SpecimenSearch.aspx - Urulizar Advanced Search
# Dados: Nome cientitico.csv, Format: Tab Delimited , Encoding Type: UTF-8
# Mapas:Nome cientitico.kml), Google Earth (KML)
# Nomes válidos e sinônimos podem ser baixados em aquivos separados que o sistema junta tudo

###---------------------------------------------------------------------###

# processa registros

datamobot<-function(species.list=NULL,project.name=NULL,data.source='mobot', base.synonyms='FLORABRASIL2020', include.synonyms=TRUE)
{

  spp<-species.list
  data_source<-data.source
  project<-project.name
  
# Junta coordenadas do KML de nomes validos e seus sinonimos
  for (i in 1:NROW(spp)){
  sp <- spp[i]
  
  if (include.synonyms == T){
    if (base.synonyms=='FLORABRASIL2020'){spp.search=nomes_sinonimos_florabr(sp, return_type ='names_synonyms')}
  }
  else{spp.search <- spp[i]}
  
  for (s in 1:NROW(spp.search)){
    sp <- spp.search[s,1]
    msg(paste0(' (',i,'-',sp))
    if(file.exists(paste0(getwd(),'/OccurrenceRecords/',project,'/' ,data_source,'/',sp,'.csv'))){  
      x<-coordgeomobot(sp,FALSE,paste0(getwd(),'/OccurrenceRecords/',project,'/' ,data_source))
      if (NROW(x)>0)
      {save_occurrence_records(x, sp, project, data_source, type.file = 'tmp')}
      else{msg(paste0(' =','SEM REGISTROS! '))}
    }else{msg(paste0(' =','não encontrado!) '))}
  }
  }

### Juntar dados de nomes validos e sinonimos
  for (i in 1:NROW(spp)){
  sp.valido <- spp[i]
  spp.search=nomes_sinonimos_florabr(sp.valido, return_type ='names_synonyms')
  occ <- {}
  for (s in 1:NROW(spp.search)){
    sp <- spp.search[s,1]
    msg(paste0(' (',i,'-',sp))
    file.txt <- paste0(getwd(),'/OccurrenceRecords/',project,'/' ,data_source,'/',sp,'.tmp')
    if(file.exists(file.txt)){
      x<-read.table(file=file.txt, header=T,as.is=T)
      if (NROW(x)>0)
      {occ <- rbind(x,occ)}
      else{msg(paste0(' =','SEM REGISTROS! '))}
    }else{msg(paste0(' =','não encontrado!) (',file.txt,')'))}
  }
  if (NROW(occ)>0)
  {
    #save_occurrence_records(x, sp.valido, project, data_source)
    return(x)
  } else {return(NULL)}
  }
}

###---------------------------------------------------------------------###

# pega longitude e latutude do KML
 
coordgeomobot <- function(sp, save=TRUE, path.file=getwd()) {

  mobot.file.csv <-paste0(path.file,'/',sp,'.csv')
  mobot.file.kml <-paste0(path.file,'/',sp,'.kml')
  mobot.file.txt <-paste0(path.file,'/',sp,'.txt')
  
  #le dados exportados de tropicos
  mo.dad <-read.csv(mobot.file.csv, dec='.',sep='\t', header = TRUE,encoding='UTF-8')
  
  #le mapa exportado de tropicos no formato GOOGLE EARTH
  mo.map <- readOGR(dsn= mobot.file.kml, layer='Specimen Search',encoding='UTF-8')
  
  #pega os identificadores dos pontos no mapa
  rn1 = as.vector(mo.map@data[,1]) 
  Encoding(rn1) <- "UTF-8"
  r=1
  
  #adiciona duas colunas em mo.dad para adicionar as coordenadas extraída do mapa
  mo.dad$longitudeMAP = NA
  mo.dad$latitudeMAP = NA
  #para cada ponto no mapa procura o dado e adiciona a latitude e longitude
  for(r in 1:length(rn1)) {
    #pega o nome da linha no mapa
    ru = rn1[r]
    #pega a latitude e longitude do ponto no mapa
    longg = as.vector(mo.map@coords[r,1],mode='numeric')
    latt = as.vector(mo.map@coords[r,2],mode='numeric')
    ru = strsplit(ru," ")[[1]]
    px = grep("-",ru)
    if (length(px)>1) {
      px = px[length(px)]
    }
    coletor = str_trim(paste(ru[1:(px-1)],collapse=" "))
    num = str_trim(paste(ru[(px+1):length(ru)],collapse=" "))
    #procura nos dados
    vl = mo.dad$Collector%in%coletor & mo.dad$Collection.Number==num
    #para todas as linhas encontradas adiciona a longitude e latitude
    if (sum(vl)>0) {
      if (is.na(mo.dad$longitudeMAP[r])) {
        mo.dad$longitudeMAP[r] = longg 
      } else {
        print(paste("Linha",r,"já tinha longitude"))
      }
      if (is.na(mo.dad$latitudeMAP[r])) {
        mo.dad$latitudeMAP[r] = latt 
      } else {
        print(paste("Linha",r,"já tinha latitude"))
      }
      
    } else {
      print(paste(rn1[r],' encontrado ',sum(vl),'registros em mo.dad'))
    }
  }
  if(save){
  mo.dad = as.data.frame(mo.dad, stringsAsFactors=F)
  write.table(mo.dad,mobot.file.txt, sep = "\t",eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = "double", fileEncoding = "UTF-8")}

  return(mo.dad)}

###---------------------------------------------------------------------###


