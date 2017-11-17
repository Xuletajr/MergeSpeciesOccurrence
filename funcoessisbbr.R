
# foi realizada busca em http://gbif.sibbr.gov.br/explorador/pt/busca?view=map&1_f=14&1_o=EQ&1_v_1=Plantae/
# reino Plantae

print('carregando base SisBBR...')
sisbbr.data.source <- fread("E:/sourceDada/sisbbr/occurrence.txt")

datasisbbr<-function(sp_search)
{
  
  data.full <- {}

  for (s in 1:NROW(sp_search))
  {
    
    data <- {}
    sp.name <- as.character(sp_search[s,1])
    msg(sp.name)
    
    # busca registros 
    sisbbr.data.source %>% filter(sisbbr.data.source$scientificName %in% sp.name) -> data

    if(NROW(data)==0){msg(': Sem Registros!')}
    else{
      data.full <- rbind(data.full,data)
      msg(paste0(': (',NROW(data),')'))
    }
  }
  
  return(data.full)
}



# 
# 
# 
# ###-----------------------------------------------------------------------------------------###
# 
# # 1. clear the memory and load the packages
# # clear workspace and increase memory
# rm(list = ls())
# memory.limit(size = 1.75e13) 
# 
# ###-----------------------------------------------------------------------------------------###
# 
# # diretório temporario de processamento do R 
# 
# tempdir <- function() "D:\\temps"
# unlockBinding("tempdir", baseenv())
# assignInNamespace("tempdir", tempdir, ns="base", envir=baseenv())
# assign("tempdir", tempdir, baseenv())
# lockBinding("tempdir", baseenv())
# tempdir()
# 
# # CRS.new=CRSargs(CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# 
# ###-----------------------------------------------------------------------------------------###
# 
# # Pacotes
# 
# if(!require(pacman)) install.packages("pacman")
# pacman::p_load(dplyr,data.table, magrittr)
# 
# # check loaded packets
# search()
# 
# ###-----------------------------------------------------------------------------------------###
# 
# project = "plantascalcario"
# dir.root= "C:/Dados/GitHub/MergeSpeciesOccurrence"; setwd(dir.root)
# #dir.gis = 'F:/Dados/GitHub/MergeSpeciesOccurrence/GisBase'
# file.spp = "plantascalcario.txt"
# 
# # carregar pontos
# spp <- fread(file.spp, header=F) 
# 
# # Diretorio com os registros de ocorrencia de especies do specieslink
# # foi realizada busca em http://inct.splink.org.br/ reino Plantae - retorna todos os registros de plantas na rede speciesLink
# setwd("E:/sourceDada/splink")
# spLink <- fread("speciesLink_all_10261_20171004142646.txt")
# 
# ##Diretorio onde salvar os arquivos
# setwd("E:/sourceDada/splink/plantascalcario")
# 
# for (i in 1:NROW(spp))
# {
#   spName <- as.character(paste0(spp[i,1],' ',spp[i,2]))
#   
#   spLink %>% filter(spLink$scientificname %in% spName) -> x
#   nome.arquivo <- print(spName)
#   write.table(x, file=paste0(nome.arquivo,".txt"), fileEncoding = "UTF-8", sep="\t")
# }
# 
# 
