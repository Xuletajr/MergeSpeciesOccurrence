pacman::p_load(rvest, data.table, stringr, dplyr, downloader,crul,htmltidy,httr)


project = 'plantascalcario'
dir.root= "D:/GitHub/OccSpp"
setwd(dir.root)

#--- Filtros ---# 
fam='Piperaceae'
gen='Peperomia'
sp='gardneriana'

#--- busca ocorrencia da especie ---#

baseURL <- 'http://reflora.jbrj.gov.br/reflora/herbarioVirtual/ConsultaPublicoHVUC/BemVindoConsultaPublicaHVConsultar.do?invalidatePageControlCounter=27&quantidadeResultado=&nomeCientifico='
suffixURLnomeCientifico <- paste0(fam,"+",gen,"+",sp)
url.full <- paste(baseURL,suffixURLnomeCientifico)
pg <- read_html(url.full)
text <- as.character(pg)
linhastmp = data.frame(strsplit(text,"idTestemunho=")[[1]], stringsAsFactors = F)
linhastmp = linhastmp[2:NROW(linhastmp),]
ll=data.frame(gsub('\\}|\\"|,',"a",linhastmp),stringsAsFactors = F)
colnames(ll) <-c('x')
idTestemunho<-{}
for(s in 1:NROW(ll)){
  x=ll$x[s]
  xp=regexpr('aaaa',x)[[1]]
  id=substr(x,1,xp-1)
  idTestemunho<-cbind(idTestemunho,id)
}  




baseURL<- 'http://reflora.jbrj.gov.br/reflora/herbarioVirtual/ConsultaPublicoHVUC/ConsultaPublicoHVUC.do?idTestemunho='
id =  '3065918'# idTestemunho[f]
url.sp <- paste0(baseURL,id)
#download.file(url.sp, destfile = "sp.html", quiet=TRUE)
pg = read_xml(url.sp,encoding = "Windows-1252")




$content = utf8_encode(file_get_contents(url.sp));
$xml = simplexml_load_string($content)


text <- as.character(pg)
linhastmp = data.frame(strsplit(text,"idTestemunho=")[[2]], stringsAsFactors = F)
linhastmp = linhastmp[603:NROW(linhastmp),]
lintastxt = as.character(linhastmp)

pg = read_html(lintastxt)


library(htmltidy) # devtools::install_github("hrbrmstr/htmltidy")
library(httr)

res <- GET(url.sp, user_agent("Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2490.76 Mobile Safari/537.36"))
cleaned <- tidy_html(content(res, as="text", encoding="UTF-8"),
                     list(TidyDocType="html5"))
pg <- read_html(cleaned)

ds = xmlToDataFrame(pg)


html <- url.sp %>% GET(user_agent('R')) %>% content('text')

html2 <- gsub('<spoiler:3tbt4d3m>.*</spoiler:3tbt4d3m>', '', html)

df <- html2 %>% read_html() %>% 
  html_node(xpath = '//table[@border="1"]') %>% 
  # obviously insufficient to parse double headers, but at least the data exists now
  html_table(fill = TRUE)




pg <- crul::url_parse(url.sp)
data <- pg$parameter



# 
# url <- paste0('http://reflora.jbrj.gov.br/reflora/herbarioVirtual/ConsultaPublicoHVUC/ConsultaPublicoHVUC.do?idTestemunho=',idTestemunho[f])
# 
# for(f in 1:length(idTestemunho)){
#   id =  '3065918' # idTestemunho[f]
#   #url.sp <- paste0("http://gbif.sibbr.gov.br/explorador/pt/ocorrencias/idTestemunho=",id,"?view=interpreted")
#   #url.sp <- paste0("http://gbif.sibbr.gov.br/explorador/pt/ocorrencias/",id)
#   url.sp <- paste0(baseURL,id)
#   
#   download.file(url.sp, destfile = "sp.html", quiet=TRUE)
#   pg <- read_html(url.sp)
#   text <- as.character(pg)
#   
#   pg <- pluck(url.sp)
#   #pg <- read_html(ur)
#   ta <- html_table( html_nodes(pg, "table"),fill = T)
#   datasp1<-t(ta[[1]])
#   datasp2<-t(ta[[2]])
#   datasp3<-t(ta[[3]])
#   datasp4<-t(ta[[4]])
#   datasp5<-t(ta[[5]]) # direitos
#   datasp =cbind(datasp1,datasp2,datasp3,datasp4,datasp5)
#  



#--- pega codigo de barras ---#
url <- paste0("http://gbif.sibbr.gov.br/explorador/pt/busca?view=table&1_f=16&1_o=EQ&1_v_1=",gen,"+",sp)
#download.file(url, destfile = "spp.html", quiet=TRUE); pg <- read_html("spp.html")
pg <- read_html(url)
ta <- html_table( html_nodes(pg, "table"),fill = T)
dataspp<-ta[[1]]
dataspp<-dataspp[,1:9]
colnames(dataspp) <- c("Nomecientifico",
                       "Familia",
                       "Pais",
                       "Estado_Provincia",
                       "Localidade",
                       "Habitat",
                       "Ano",
                       "Codigo_colecao",
                       "Numero_catalogo")



  
  
  
 
  
