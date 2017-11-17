### funções auxiliares para as diferentes rotinas ###

# Pablo Hendrigo Alves de Melo - pablopains@yahoo.com.br

###  MergeSpeciesOccurrence	###

###---------------------------------------------------------------------###

if(!require(pacman)) install.packages("pacman")
pacman::p_load(maptools, sp, maps, svDialogs, rgeos, data.table, lettercase, stringi,rgdal, stringr, rgbif, flora, dplyr,BIEN)
library(finch)
library(RB)

# if(!require(pacman)) install.packages("pacman")
# pacman::p_load(maptools, sp, maps, svDialogs, rgeos, rJava, data.table, lettercase, stringi,rgdal, stringr, rgbif, flora, dplyr,BIEN,finch,RB)

# Install RB:
# devtools::install_github("gustavobio/finch",force = TRUE)
# devtools::install_github("gustavobio/RB",force = TRUE)

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

separa<-function(x,sep=',')
{return(ifelse(x!='',paste0(sep,' ',x),''))}


limpaNA<-function(x)
{
  if(is.na(x)){x<-''}
  x<-gsub('NA','',x)
  return(x)}


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
