if(!require(pacman)) install.packages("pacman")
pacman::p_load(taxize, divagis, ggmap, mapr, sp, rgdal, maptools, dplyr, rgbif)






##################################################################################
save_occurrence_records <- function(dat,file.name,base='gbif',dir='allrecords'){
  ## Grava resultados totais da busca por registros
  wd.root <- getwd()
  if( ! dir.exists(paste0(getwd(),"/OccurrenceRecords/")))
  {dir.create("OccurrenceRecords")}
  setwd(paste0(getwd(),"/OccurrenceRecords"))
  if(! dir.exists(paste0(getwd(),"/",base,"/")))
  {dir.create(base)}
  setwd(paste0(getwd(),"/",base))
  if( ! dir.exists(paste0(getwd(),"/",dir,"/")))
  {dir.create(dir)}
  setwd(paste0(getwd(),"/",dir))
  file_name_txt <- paste0(file.name,".txt")
  write.table(dat,file_name_txt,append = FALSE, quote = TRUE, sep = " ",eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = "double", fileEncoding = "UTF-8")
  setwd(wd.root)
}
##################################################################################
#base = gbif, splink, jabot, literatura
#sep = '\t', ';'
#encoding = "UTF-8", Latin-1
##################################################################################
##################################################################################
read_occurrence_records <- function(file.name,base='splink',dir='allrecords',sep='\t',encoding="UTF-8",name.fields=''){
  
  colunas.splink <- c("scientificname","longitude","latitude", "collectioncode","catalognumber","identifiedby","country","stateprovince","county","locality","daycollected","monthcollected","yearcollected","collector","collectornumber","notes")	
  colunas.splink_mun <- c("scientificname","longitude_mun","latitude_mun", "collectioncode","catalognumber","identifiedby","country","stateprovince","county","locality","daycollected","monthcollected","yearcollected","collector","collectornumber","notes")	
  colunas.jabot <- c("taxoncompleto","longitude","latitude", "siglacolecao","numtombo","determinador","pais","estado_prov","cidade","descrlocal","diacoleta","mescoleta","anocoleta","coletor","numcoleta","notas")	
  colunas.gbif <- c("scientificName","decimalLongitude","decimalLatitude", "collectionCode","catalogNumber","identifiedBy","country","stateProvince","county","locality","day","month","year","recordedBy","recordNumber","occurrenceRemarks")	
  
  file.csv <- paste0(getwd(),"/OccurrenceRecords/",base,'/',dir,'/',file.name,".txt")
  cat('lendo: ',file.csv)  
  
  if(file.exists(file.csv)){
    occ.csv.full = read.table(file.csv, dec='.',sep=sep, header = TRUE,encoding= encoding )
    if(NROW(occ.csv.full)>0){
      if(base=='gbif')  {name.fields=colunas.gbif}
      if(base=='splink'){name.fields=colunas.splink}
      if(base=='jabot') {name.fields=colunas.jabot} #& length(name.fields)==0
      if(base=='all'){
        name.fields=c(colunas.splink,'source')
        occ.csv <- {}
        occ.csv = occ.csv.full[,name.fields]
        cat(' (',base,': ',NROW(occ.csv),' carregados)')  
        colnames(occ.csv) <- name.fields
        return(as.data.frame(occ.csv, stringsAsFactors = FALSE))
      }
      occ.csv <- {}
      occ.csv = occ.csv.full[,name.fields]
      occ.csv <- cbind(occ.csv,c(rep(base,NROW(occ.csv))))
      colnames(occ.csv) = c(colunas.splink,'source')
      cat(' (',base,': ',NROW(occ.csv),' carregados)')  
      
      if(base=='splink'){
        occ.csv_mun = {}
        occ.csv_mun = occ.csv.full[,colunas.splink_mun]
        occ.csv_mun <- occ.csv_mun[!occ.csv_mun$latitude_mun %in% NA & !occ.csv_mun$longitude_mun %in% NA,]  
        occ.csv_mun <- cbind(occ.csv_mun,c(rep('splink.mun',NROW(occ.csv_mun))))
        colnames(occ.csv_mun) <- c(colunas.splink,'source')
        occ.csv <- rbind(occ.csv,occ.csv_mun)
        cat('( splinl.cm: ',NROW(occ.csv_mun),' carregados)')  
      }
    }
    return(as.data.frame(occ.csv,stringsAsFactors = FALSE))  
  }
  else{cat(file.csv,' não encontrado')}
}



