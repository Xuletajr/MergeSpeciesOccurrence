# pacman::p_load(rvest, data.table, stringr, dplyr, downloader)
# 
# dir.root <- "T:/GitHub/OccSpp"
# setwd(dir.root)
# source('funcoesplantas.R')
# project <- 'plantascalcario'
# data.source <- data.set[4]
# save.data=T
# save.image=F # Falta testar se o linque é valido para nao dar erro
# 
# x<-datakew('Bomarea edulis', project, data.source, save.data,save.image)

  
##################################################################################
# Concatena resultados de buscas por difirentes espécies no gbif conforme scientificName. Ideal para buscar nomes válidos e sinônimos de uma mesma espécie considerento difrentes fontes de sinônimos, p.ex. Flora do Brasil 2020, Tropicos para plantas.
# 
##################################################################################

# http://apps.kew.org/herbcat/gotoSearchPage.do (All names) Baixar pagina htm resultado da busca. Dica: Records per page = 100 depois alterar resultsPerPage=100000 na barra de nagegacao. Isso exibe totos dos resultados.


datakew <- function(sp_search=NULL, project='', data.source='kew', save.data=F,save.image=F )
{
  data.full<-data.frame({})
  criarapastas(project,data.source)
  
  dir.w <-  paste0(getwd(),'/OccurrenceRecords/',project,'/' ,data.source)
  dir.img <- paste0(getwd(),'/OccurrenceRecords/',project,'/' ,data.source,'/images/')
  file.html <- paste0(dir.w,"/",sp_search,".html")
  
  if(!file.exists(file.html)){return(data.full)}
  nomescolunas<-c("Barcode","Image","Family","Scientificname","Collector","Collectornumber","Location","Date","Item","Type")
  pg <- read_html(file.html)
  ta <- html_table( html_nodes(pg, "table"),fill = T)
  data<-ta[[2]]
  colnames(data) <- nomescolunas
  save_occurrence_records(data, file.html, project, data.source)
  
  for(l in 1:NROW(data)){
    id<-data$Barcode[l]
    if(id==''){next} #aqui
    msg(id)
    
    Sys.setenv(http_proxy="http://specimens.kew.org:80") # ? ou http://apps.kew.org
    url <- paste0("http://specimens.kew.org/herbarium/", id) 
    download.file(url, destfile = "spp.html", quiet=TRUE)
    pg <- read_html("spp.html")
    closeAllConnections()

    ta<-html_table( html_nodes(pg, "table"),fill = T)[[1]]
    co<-c(ta[1:7, 2], ta[8, 2], ta[1:7,  4])
    co<-gsub(":", "",co)
    co<-gsub(' ','.',co)
    co<-c(co,'Barcode','Typo')
    datain1 <- ta[1:7, 3]
    datain2 <- gsub('\r|\n|\t', "",  ta[8, 3])
    datain3 <- ta[1:7, 5]
    datain3 <- gsub('\r|\n|\t|,', "",  datain3)
    datain4 <- t(c(data$Barcode[l],data$Type[l]))
    datain <- data.frame(t(c(datain1,datain2,datain3,datain4)))
    colnames(datain) <- co
    data.full<-rbind.data.frame(data.full,datain)
    
    # image
    file.jpg<-paste0(dir.img,id,'.jpg')
    if(save.image==T&!file.exists(file.jpg)){
      #setwd(dir.img)
      url.img <- paste0("http://apps.kew.org/herbcat/getImage.do?imageBarcode=", id)
      #pg2 <- read_html(url.img)
      
      download.file(url.img, destfile = "image.html", quiet=TRUE)
      pg2 <- read_html("image.html")
      closeAllConnections()
      
      li <- html_attr(html_nodes(pg2, "a"), "href")
      im <- grep(".jpg", li, value = T)
      try(download.file(im, paste0(id, ".jpg"), mode = "wb"))
      #setwd(dir.root)
    }
  }
  
  # separa e transforma em graus decimais as coordeadas
  latlong <- as.vector(data.full$Lat.and.Long)
  latlong <- ifelse(latlong!='',latlong,',')
  latlong <-read.table(text = latlong,header = F, stringsAsFactors = FALSE,sep=',',dec='.')
  colnames(latlong) <- c('lat','long')
  
  #latitude sul (-)
  latlong$lat<-ifelse(str_count(latlong$lat,'S|s')>0,paste0('-',gsub('S|s','',latlong$lat)),latlong$lat)
  #latitude norte
  latlong$lat<-ifelse(str_count(latlong$lat,'N|n')>0,gsub('N|n','',latlong$lat),latlong$lat)
  
  #longitude oeste (-)
  latlong$long <- substr(latlong$long,2,nchar(latlong$long))
  latlong$long<-ifelse(str_count(latlong$long,'W|w')>0,paste0('-',gsub('W|w','',latlong$long)),latlong$long)
  #latitude norte
  latlong$long<-ifelse(str_count(latlong$long,'E|e')>0,gsub('E|e','',latlong$long),latlong$long)
  
  # convert from decimal minutes to decimal degrees
  latlong$latd <- as.vector(sapply(latlong$lat, measurements::conv_unit, from = 'deg_dec_min', to = 'dec_deg'), mode = 'character')
  latlong$longd <- as.vector(sapply(latlong$long, measurements::conv_unit, from = 'deg_dec_min', to = 'dec_deg'), mode = 'character')
  
  data.full$latidude <- latlong$latd
  data.full$longitude <- latlong$longd
  
  #salva
  if(save.data==T){
    save_occurrence_records(data.full, sp, project, data.source)
    cat(paste0(' - (',NROW(data.full),') baixadas )'))
  }
  
  #move imagens para pasta destinho
  if(save.image==T){
    jpg <- list.files(dir.root,pattern = '*.jpg', include.dirs = FALSE)
    if(length(jpg)>0){
      jpgfrom <- paste0(dir.root,'/',jpg)
      jpgto <- paste0(dir.img,'/',jpg)
      file.copy(jpgfrom, jpgto, overwrite = T)
      file.remove(jpgfrom)}}
  
  return(data.full)

}    


