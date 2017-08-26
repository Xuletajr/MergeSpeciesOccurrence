### funções auxiliares para as diferentes rotinas ###

# Pablo Hendrigo Alves de Melo - pablopains@yahoo.com.br

###  MergeSpeciesOccurrence	###

###---------------------------------------------------------------------###

if(!require(pacman)) install.packages("pacman")
pacman::p_load(maptools, sp, maps, svDialogs, rgeos, rJava, data.table, lettercase, stringi,rgdal, stringr, rgbif, flora, RB, dplyr,BIEN)

###---------------------------------------------------------------------###

# controla exibição de mensagens de processamento
 
#showmessage=T

msg<-function(txt){if(showmessage){print(txt)}}

###---------------------------------------------------------------------###

# cria um diretorio pra cada fonte de dados do projeto

criarapastas<-function(data.source,project){
  dir.root <- getwd()
  if(!dir.exists(paste0(getwd(),"/occurrencerecords/")))
  {dir.create("OccurrenceRecords")}
  setwd(paste0(getwd(),"/occurrencerecords"))
  if(!dir.exists(paste0(getwd(),"/",project,"/")))
  {dir.create(project)}
  setwd(paste0(getwd(),"/",project))
  if(!dir.exists(paste0(getwd(),"/",data.source,"/")))
  {dir.create(data.source)}
  setwd(dir.root)  
}

###---------------------------------------------------------------------###

# nome aceito e sinonimos conforme pacote flora

# return_type  = 'names','names_synonyms','synonyms'

load('D:\\R\\flora\\flora-master\\R\\sysdata.rda')

nomes_sinonimos_florabr <- function(sp_name_search,return_type='names'){
  names_florabr = get.taxa(sp_name_search)
  names <- data.frame(name=names_florabr$search.str, stringsAsFactors = F)
  synanyms <- data.frame(name=all.taxa$search.str[all.taxa$id %in% relationships$related.id[relationships$id %in% names_florabr$id]], stringsAsFactors = F)
  
  names_synonyms <- as.vector(rbind(names,synanyms))
  if (return_type == 'names'){return(names)}
  if (return_type == 'synonyms'){return(synanyms)}
  if (return_type == 'names_synonyms'){return(names_synonyms)}
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
  write.table(dat,file_name_txt,append = FALSE, quote = TRUE, sep = sep, eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = "double")
  setwd(wd.root)
}


# ###---------------------------------------------------------------------###
# limpaNA<-function(x)
# {
#   if(is.na(x)){x<-''}
#   x<-gsub('NA','',x)
#   return(x)}

# ###---------------------------------------------------------------------###

# LongLatToUTM<-function(x,y,zone){
#   xy <- data.frame(ID = 1:length(x), X = x, Y = y)
#   coordinates(xy) <- c("X", "Y")
#   proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
#   res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
#   return(as.data.frame(res))
# }
# Example
# x<-c( -94.99729,-94.99726,-94.99457,-94.99458,-94.99729)
# y<-c( 29.17112, 29.17107, 29.17273, 29.17278, 29.17112)
# LongLatToUTM(x,y,15)
