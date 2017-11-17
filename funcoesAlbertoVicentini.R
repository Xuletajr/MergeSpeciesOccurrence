###############################################################################
# Acerta coordenadas data.set 
# carrega  coordenadas geograficas de dados data.set em KML
# Junta com dados exportados de tropicos
# Adaptado de: Ecologia e Evolu??o de Plantas Amaz?nicas - Alberto Vicentini - (http://www.botanicaamazonica.wiki.br/labotam/lib/exe/fetch.php?media=analises:baixandoespecimenes:dadostropicosdadmap.r)
###############################################################################
datadata.set <- function(sp, save=TRUE, path.file=getwd()) {
  
  data.set.file.csv <-paste0(path.file,'/',sp,'.csv')
  data.set.file.kml <-paste0(path.file,'/',sp,'.kml')
  data.set.file.txt <-paste0(path.file,'/',sp,'.txt')
  
  #le dados exportados de tropicos
  mo.dad <-read.csv(data.set.file.csv, dec='.',sep='\t', header = TRUE,encoding='UTF-8')
  
  #le mapa exportado de tropicos no formato GOOGLE EARTH
  mo.map <- readOGR(dsn= data.set.file.kml, layer='Specimen Search',encoding='UTF-8')
  
  #pega os identificadores dos pontos no mapa
  rn1 = as.vector(mo.map@data[,1]) 
  Encoding(rn1) <- "UTF-8"
  r=1
  
  #adiciona duas colunas em mo.dad para adicionar as coordenadas extra?da do mapa
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
        print(paste("Linha",r,"j? tinha longitude"))
      }
      if (is.na(mo.dad$latitudeMAP[r])) {
        mo.dad$latitudeMAP[r] = latt 
      } else {
        print(paste("Linha",r,"j? tinha latitude"))
      }
      
    } else {
      print(paste(rn1[r],' encontrado ',sum(vl),'registros em mo.dad'))
    }
  }
  if(save){
    mo.dad = as.data.frame(mo.dad, stringsAsFactors=F)
    write.table(mo.dad,data.set.file.txt, sep = "\t",eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = "double", fileEncoding = "UTF-8")}
  
  return(mo.dad)}
###################################################################################

###################################################################################
###################################################################################
# limpeza de s?mbolos indesej?veis (aspas, barras, espa?os duplos),

#library(stringr) #instalar este pacote se quiser usar a fun??o str_trim() na fun??o limpacolunas abaixo.
#cria uma fun??o que limpa textos
limpacolunas <- function(x) { 
  #onde x ? um texto (string)
  #remove espa?os duplos
  for(i in 1:10) {
    x = gsub("  "," ",x)
  }
  #remove aspas
  x = gsub("\"","",x)
  x = gsub('"',"",x)
  x = gsub("'","",x)
  #remove tabulacoes
  x = gsub("\t"," ",x)
  #remove quebras de linha
  x = gsub("\n"," ",x)
  #remove barras
  x = gsub("\\\\","",x)
  #remove virgulas duplas
  x = gsub(',,',",",x)
  x = gsub(',,',",",x)
  x = gsub(',,',",",x)
  #x = str_trim(x)  #para habilitar isso precisa do pacote stringr (remove espa?os em branco no inicio e final dos strings
  return(x)
}

################################################################################
# pega data 1
################################################################################
pegadata <- function(x) {
  x = strsplit(x," - ")[[1]]
  x = x[1]
  x = str_trim(x)
  return(x)
}


################################################################################
#pega dada 2
################################################################################
# corrige bagun?a do campo data no data.set

#funcao que pega valores de data para dados data.set
pegadata2 <- function(x, id=1) 
{
  #print(x)
  
  dia = NA
  mes = NA
  ano = NA

  if(is.na(x)){return(NA)}

  #--- pega dada iglesa com time---#
  if(nchar(x)==28){
    dt<-substr(x,1,10)
    dia=substr(dt,9,10)
    mes=substr(dt,6,7)
    ano=substr(dt,1,4)
    rr = c(dia,mes,ano)
    return(rr[id])
  }

  #--- pega dada com mes e ano---#
  if(nchar(x)==7){
    dia=NA
    mes=as.character(substr(x,1,2))
    ano=as.character(substr(x,4,7))
    rr = c(dia,mes,ano)
    return(rr[id])
  }
  
  #--- pega dada letras---#
  x.tmp = strsplit(x,"-")[[1]]
  x.tmp = str_trim(x.tmp)
  if (length(grep("[a-z]",x.tmp, ignore.case=T))>0) {
    mes = x.tmp[grep("[a-z]",x.tmp, ignore.case=T)]
    
    if(mes==x.tmp&length(x.tmp)==1){
      rr = c(dia,mes,ano)
      return(rr[id])
    }
    x.tmp = x.tmp[-grep("[a-z]",x.tmp,ignore.case=T)]
    d=1
    for(d in 1:length(x.tmp)){
      #print(x.tmp)
      if(nchar(x.tmp[d])==4){ano=x.tmp[d]}
      if(nchar(x.tmp[d])<=2){dia=x.tmp[d]}
    }
    rr = c(dia,mes,ano)
    return(rr[id])
  }
  
  #--- pega dada com dia mes e ano para dada ameriaca e brasileira---#
  #pablo h de == 9 para  >=8
  if((nchar(x)==10|nchar(x)==9)&length(strsplit(x,"-")[[1]])==3)
  {
    x = strsplit(x,"-")[[1]]
    
    if(nchar(x[1])==4){
      ano=x[1]
      mes=x[2]
      dia=x[3]}
    if(nchar(x[3])==4){
      ano=x[3]
      mes=x[2]
      dia=x[1]}
    rr = c(dia,mes,ano)
    return(rr[id])
  }
  
  if(nchar(x)==8&length(strsplit(x,"-")[[1]])==3){
    x = strsplit(x,"-")[[1]]
    dia=x[1]
    mes=as.character(x[2])
    ano=x[3]
    rr = c(dia,mes,ano)
    return(rr[id])
  }
  
  #--- pega dada com mes e ano---#

  ##quebra a data
  x = strsplit(x,"-")[[1]]
  ##tira trailing espacos
  x = str_trim(x)
  if (length(x)>0 && x!='') {
    #se o comprimento for tres
    #qual valor ? palavra
    mes = NA
    ano = NA
    dia = NA
    if (length(grep("[a-z]",x, ignore.case=T))>0) {
      mes = x[grep("[a-z]",x, ignore.case=T)]
      x = x[-grep("[a-z]",x,ignore.case=T)]
    }
    if (length(x)>0) {
      #qual valor tem 4 caracteres
      xt = as.character(as.numeric(x))
      nc = as.vector(lapply(xt,nchar),mode='numeric')
      if (sum(is.na(nc)==4)==1) {
        ano = xt[nc==4]
        xt = xt[!nc==4]
      } else {
        ano = xt[length(xt)]
        xt = xt[-length(xt)]
      }
      if (is.na(length(xt)>0)) {
        #pega o dia e o mes se ainda nao pegou
        if (is.na(length(xt))==1 & !is.na(mes) & !is.na(ano)) {
          dia = xt
        }
        else {
          if (is.na(length(xt)==1) & is.na(mes)) {
            mes = xt
          }
          #aqui
          if (is.na(length(xt))==2 & is.na(mes)) {
            vt = as.numeric(xt)>12
            if (sum(vt)>0) {
              dia = xt[vt]
              mes = xt[!vt]
              xt = NULL
            } else {
              mes = xt[2]
              dia = xt[1]
              xt = NULL
            }
          }
          if (is.na(length(xt)==2) & !is.na(mes)) {
            dia = mean(as.numeric(xt), na.rm=T)
          }
        }
      }
    }
    rr = c(dia,mes,ano)
    return(rr[id])
  } else {
    return(NA)
  }
}
################################################################################

################################################################################
# funcao para acertar data
acertadata <- function(data.set,field='eventDate')
{
  if(field=='eventDate'){dt = data.set$eventDate}
  if(field=='verbatimEventDate'){dt = data.set$verbatimEventDate}
  if(field=='dateIdentified'){dt = data.set$dateIdentified}
  
  #pega a coluna com datas
  #corrige quatro valores que as funcoes abaixo nao corrigem por problemas mais serios (nao tem l?gica)
  # dt = gsub("1998ense","1998",dt)
  # dt = gsub("194\\(2\\?\\)","1942",dt)
  # dt = gsub("1927-15","1927 - 15",dt)
  # dt = gsub("1841-42","1841",dt)
  dt = gsub(" de ","-",dt)
  
  #h? valores de data que tem duas datas e APENAS nestes casos ta separado por " - "  (note os espacos)
  
  # aqui pablo
  idx.dois = grep("-",dt)
  
  #idx.dois = grep(" ",dt) # aqui phamelo
  
  #pega apenas uma data aleatoriamente para esses registros
  vals.dois = dt[idx.dois]
  vals.dois
  ll = lapply(vals.dois,pegadata)
  ll = as.vector(ll, mode='character')
  dt[idx.dois] = ll
  
  #substritui virgula e barra, ponto, etc nos valores de data (padroniza simbolos)
  dt = gsub(","," ",dt)
  dt = gsub("\\\\","-",dt)
  dt = gsub("  "," ",dt)
  dt = gsub("  "," ",dt)
  dt = gsub("  "," ",dt)
  
  dt = gsub(" ","-",dt)
  dt = gsub("\\.","-",dt)
  dt = gsub("--","-",dt)
  
  #dt = gsub("-","/",dt) 
  dt = gsub("/","-",dt) 
  
  dt = gsub("NA","",dt)
  
  #pega o dia o mes e o ano
  #source("corrigedatafuncao.R") #funcao que faz isso est? neste script
  
  dia = as.vector(lapply(dt, pegadata2,id=1),mode='integer')
  mes = as.vector(lapply(dt, pegadata2,id=2),mode='character')
  ano = as.vector(lapply(dt, pegadata2,id=3),mode='integer')
  mes[mes=='NA'] = NA
  dia[dia=='NA'] = NA
  ano[ano=='NA'] = NA
  
  #a coluna meses esta por extenso e precisa corrigir
  #pega os meses por extenso e corrige por numeros
  idx.meses = grep("[a-z]", tolower(mes))
  vals.meses = mes[idx.meses]
  mm = sort(unique(vals.meses))
  names(mm) = mm
  mm[grep("abr|apr|avri",names(mm),ignore.case=T)] = 4
  mm[grep("sep|set",names(mm),ignore.case=T)] = 9
  mm[grep("oct|out",names(mm),ignore.case=T)] = 10
  mm[grep("nov",names(mm),ignore.case=T)]  = 11
  mm[grep("may|mei|mai",names(mm),ignore.case=T)]  = 5
  mm[grep("mar",names(mm),ignore.case=T)]  = 3
  mm[grep("jun|juin",names(mm),ignore.case=T)]  = 6
  mm[grep("jul|juil",names(mm),ignore.case=T)]  = 7
  mm[grep("jan",names(mm),ignore.case=T)]  = 1
  mm[grep("feb|fev|ene",names(mm),ignore.case=T)]  = 2
  mm[grep("dez|dic|dec",names(mm),ignore.case=T)]  = 12
  mm[grep("aug|ago|aou",names(mm),ignore.case=T)]  = 8
  mm[toupper(mm)=="XII"] = 12
  mm[toupper(mm)=="XI"] = 11
  mm[toupper(mm)=="X"] = 10
  mm[toupper(mm)=="IX"] = 9
  mm[toupper(mm)=="VIII"] = 8
  mm[toupper(mm)=="VII"] = 7
  mm[toupper(mm)=="VI"] = 6
  mm[toupper(mm)=="V"] = 5
  mm[toupper(mm)=="IV"] = 4
  mm[toupper(mm)=="III"] = 3
  mm[toupper(mm)=="II"] = 2
  mm[toupper(mm)=="I"] = 1
  
  mm[toupper(mm)=="Xll"] = 12
  mm[toupper(mm)=="Xl"] = 11
  #mm[toupper(mm)=="X"] = 10
  mm[toupper(mm)=="lX"] = 9
  mm[toupper(mm)=="Vlll"] = 8
  mm[toupper(mm)=="Vll"] = 7
  mm[toupper(mm)=="Vl"] = 6
  #mm[toupper(mm)=="V"] = 5
  mm[toupper(mm)=="lV"] = 4
  mm[toupper(mm)=="lll"] = 3
  mm[toupper(mm)=="ll"] = 2
  mm[toupper(mm)=="l"] = 1
  
  #corrige o vetor mes com esses valores
  for(m in 1:length(mm)) {
    mes[mes==names(mm)[m]] = mm[m]
  }
  
  #aqui para Brunão
  #adiciona as novas colunas ao objeto data.set
  if(field %in% c('eventDate','verbatimEventDate')){
    data.set$day = dia
    data.set$month = mes
    data.set$year = ano
  }  

  if(field=='dateIdentified'){
    data.set$dayIdentified = dia
    data.set$monthIdentified = mes
    data.set$yearIdentified = ano
  }
  
  return(data.set)
} 


################################################################################
# pega sobrenome do coletor
################################################################################
#cria uma coluna chamada sobrenome para cada base contendo apenas o sobrenome do coletor e sem acentos (isso ajuda)
#no caso do INPA e MOBOT o sobrenome do coletor consiste no primeiro valor antes da virgula de cada coletor
pegasobrenome <- function(x,sep=',',pos=1) 
{
  #quebra a frase pelo separador
  xx = strsplit(x,sep)[[1]]
  #tira espa?os em branco no in?cio e fim
  xx = str_trim(xx)
  
  # xx = gsub("\\[","\\]","\\(","\\)","",xx)
  # xx = gsub("\\.","",xx)
  
  #remove acentos
  # xx = gsub("[á|à|â|ã|ä]","a",xx)
  # xx = gsub("[Á|À|Â|Ã|Ä]","A",xx)
  # 
  # xx = gsub("[ó|ò|ô|õ|ö]","o",xx)
  # xx = gsub("[Ó|Ò|Ô|Õ|Ö]","O",xx)
  # 
  # xx = gsub("[í|ì|î|ï]","i",xx)
  # xx = gsub("[Í|Ì|Î|Ï]","I",xx)
  # 
  # xx = gsub("[ú|ù|û|ü]","u",xx)
  # xx = gsub("[Ú|Ù|Û|Ü]","U",xx)
  # 
  # xx = gsub("[é|è|ê|ë]","e",xx)
  # xx = gsub("[É|È|Ê|Ë]","E",xx)
  # 
  # xx = gsub("ñ","n",xx)
  # xx = gsub("Ñ","N",xx)
  
  # xx = iconv(xx, to="ASCII//TRANSLIT") 
  # xx = gsub("[~|^|~|\"|'|`]","",xx)
  #retorna o valor da posicao indicada
  return(xx[pos])
}
################################################################################

################################################################################
#pegasobrenomegbif
################################################################################
pegasobrenomegbif <- function(x) 
{
  
  if (length(grep("\\|",x))>0) {
    x = strsplit(x,"\\|")[[1]][1]
  }
  
  #pega o primeiro nome de uma lista de coletores separados por & se houver
  if (length(grep("&",x))>0) {
    x = strsplit(x,"&")[[1]][1]
  }
  #pega o primeiro nome de uma lista de coletores separados por ";" se houver
  if (length(grep(";",x))>0) {
    x = strsplit(x,";")[[1]][1]
  }
  #se houver v?rgula pode ser dois casos:
  #1. ou o valor antes da v?rgula ? o sobrenome (padr?o INPA)
  #2. ou a v?rgula esta separando diferentes coletores (e neste caso as palavras do primeiro elemento n?o s?o apenas abrevia??es)
  vl = grep(",|.",x) # PHAMELO
  #se tem v?rgula
  if(length(vl)>0) {
    #separa pela v?rgula e pega o primeiro elemento
    xx = strsplit(x,",")[[1]][1]
    
    #separa o primeiro elemento antes da v?rgula por espa?os
    xx = strsplit(xx," ")[[1]]
    #remove pontos
    
    # xx = gsub("\\[","\\]","\\(","\\)","",xx)
    # xx = gsub("\\.","",xx)

    # #remove acentos
    # 
    
    # xx = gsub('Ã£|Ã¡|ÃfÂ©|ÃfÂ¡|æ"¼ã¸±',"a",xx)
    # xx = gsub('A©|Ã???|Ã©|æ"¼ã¸¹',"e",xx)
    # #xx = gsub('æ"¼ã¹¤¸¹","i',xx)
    # xx = gsub('??????|Ã¼|Ã³',"o",xx)
    # xx = gsub('Ã¼|Aº',"u",xx)
    # 
    # xx = gsub("Ã§Ã£o","cao",xx)
    # xx = gsub("A§|Ã§","c",xx)

    xx = gsub("[á|à|â|ã|ä]","a",xx)
    xx = gsub("[Á|À|Â|Ã|Ä]","A",xx)

    xx = gsub("[ó|ò|ô|õ|ö]","o",xx)
    xx = gsub("[Ó|Ò|Ô|Õ|Ö]","O",xx)

    xx = gsub("[í|ì|î|ï]","i",xx)
    xx = gsub("[Í|Ì|Î|Ï]","I",xx)

    xx = gsub("[ú|ù|û|ü]","u",xx)
    xx = gsub("[Ú|Ù|Û|Ü]","U",xx)

    xx = gsub("[é|è|ê|ë]","e",xx)
    xx = gsub("[É|È|Ê|Ë]","E",xx)

    xx = gsub("ñ","n",xx)
    xx = gsub("Ñ","N",xx)
    
    #xx = gsub("æ~¼ã¸±","N",xx)
    

    xx = gsub("ç","c",xx)
    xx = gsub("Ç","C",xx)
    
    # xx=gsub('U+00C0|0xC0|À|Ã???|%C3 %80','a',xx)
    # xx=gsub('U+00C1|0xC1|Á|Ã|%C3 %81','a',xx)
    # xx=gsub('U+00C2|0xC2|Â|Ã,|%C3 %82','a',xx)
    # xx=gsub('U+00C3|0xC3|Ã|Ãf|%C3 %83','a',xx)
    # xx=gsub('U+00C4|0xC4|Ä|Ã"|%C3 %84','a',xx)
    # xx=gsub('U+00C5|0xC5|Å|Ã.|%C3 %85','a',xx)
    # xx=gsub('U+00C6|0xC6|Æ|Ã???|%C3 %86','a',xx)
    # xx=gsub('U+00C7|0xC7|Ç|Ã???|%C3 %87','c',xx)
    # xx=gsub('U+00C8|0xC8|È|Ã^|%C3 %88','e',xx)
    # xx=gsub('U+00C9|0xC9|É|Ã???|%C3 %89','e',xx)
    # xx=gsub('U+00CA|0xCA|Ê|ÃS|%C3 %8A','e',xx)
    # xx=gsub('U+00CB|0xCB|Ë|Ã<|%C3 %8B','e',xx)
    # xx=gsub('U+00CC|0xCC|Ì|ÃO|%C3 %8C','i',xx)
    # xx=gsub('U+00CD|0xCD|Í|Ã|%C3 %8D','i',xx)
    # xx=gsub('U+00CE|0xCE|Î|ÃZ|%C3 %8E','i',xx)
    # xx=gsub('U+00CF|0xCF|Ï|Ã|%C3 %8F','i',xx)
    # #xx=gsub('U+00D0|0xD0|Ð|Ã|%C3 %90','d',xx)
    # #xx=gsub('U+00D1|0xD1|Ñ|Ã'|%C3 %91','n',xx)
    # #xx=gsub('U+00D2|0xD2|Ò|Ã'|%C3 %92','o',xx)
    # xx=gsub('U+00D3|0xD3|Ó|Ã"|%C3 %93','o',xx)
    # xx=gsub('U+00D4|0xD4|Ô|Ã"|%C3 %94','o',xx)
    # xx=gsub('U+00D5|0xD5|Õ|Ã.|%C3 %95','o',xx)
    # xx=gsub('U+00D6|0xD6|Ö|Ã-|%C3 %96','o',xx)
    # xx=gsub('U+00D7|0xD7|×|Ã-|%C3 %97','!',xx)
    # xx=gsub('U+00D8|0xD8|Ø|Ã~|%C3 %98','o',xx)
    # xx=gsub('U+00D9|0xD9|Ù|ÃT|%C3 %99','u',xx)
    # xx=gsub('U+00DA|0xDA|Ú|Ãs|%C3 %9A','u',xx)
    # xx=gsub('U+00DB|0xDB|Û|Ã>|%C3 %9B','u',xx)
    # xx=gsub('U+00DC|0xDC|Ü|Ão|%C3 %9C','u',xx)
    # xx=gsub('U+00DD|0xDD|Ý|Ã|%C3 %9D','y',xx)
    # xx=gsub('U+00DE|0xDE|Þ|Ãz|%C3 %9E','!',xx)
    # xx=gsub('U+00DF|0xDF|ß|ÃY|%C3 %9F','!',xx)
    # xx=gsub('U+00E0|0xE0|à|Ã|%C3 %A0','a',xx)
    # xx=gsub('U+00E1|0xE1|á|Ã¡|%C3 %A1','a',xx)
    # xx=gsub('U+00E2|0xE2|â|Ã¢|%C3 %A2','a',xx)
    # xx=gsub('U+00E3|0xE3|ã|Ã£|%C3 %A3','a',xx)
    # xx=gsub('U+00E4|0xE4|ä|Ã¤|%C3 %A4','a',xx)
    # xx=gsub('U+00E5|0xE5|å|Ã¥|%C3 %A5','a',xx)
    # xx=gsub('U+00E6|0xE6|æ|Ã¦|%C3 %A6','a',xx)
    # xx=gsub('U+00E7|0xE7|ç|Ã§|%C3 %A7','c',xx)
    # xx=gsub('U+00E8|0xE8|è|Ã¨|%C3 %A8','e',xx)
    # xx=gsub('U+00E9|0xE9|é|Ã©|%C3 %A9','e',xx)
    # xx=gsub('U+00EA|0xEA|ê|Ãª|%C3 %AA','e',xx)
    # xx=gsub('U+00EB|0xEB|ë|Ã«|%C3 %AB','e',xx)
    # xx=gsub('U+00EC|0xEC|ì|Ã¬|%C3 %AC','i',xx)
    # #xx=gsub('U+00ED|0xED|í|Ã???|%C3 %AD','i',xx)
    # xx=gsub('U+00EE|0xEE|î|Ã®|%C3 %AE','i',xx)
    # xx=gsub('U+00EF|0xEF|ï|Ã¯|%C3 %AF','i',xx)
    # xx=gsub('U+00F0|0xF0|ð|Ã°|%C3 %B0','d',xx)
    # xx=gsub('U+00F1|0xF1|ñ|Ã±|%C3 %B1','n',xx)
    # xx=gsub('U+00F2|0xF2|ò|Ã²|%C3 %B2','o',xx)
    # xx=gsub('U+00F3|0xF3|ó|Ã³|%C3 %B3','o',xx)
    # xx=gsub('U+00F4|0xF4|ô|Ã´|%C3 %B4','o',xx)
    # xx=gsub('U+00F5|0xF5|õ|Ãµ|%C3 %B5','o',xx)
    # xx=gsub('U+00F6|0xF6|ö|Ã¶|%C3 %B6','o',xx)
    # xx=gsub('U+00F7|0xF7|÷|Ã·|%C3 %B7','!',xx)
    # xx=gsub('U+00F8|0xF8|ø|Ã¸|%C3 %B8','o',xx)
    # xx=gsub('U+00F9|0xF9|ù|Ã¹|%C3 %B9','u',xx)
    # xx=gsub('U+00FA|0xFA|ú|Ãº|%C3 %BA','u',xx)
    # xx=gsub('U+00FB|0xFB|û|Ã»|%C3 %BB','u',xx)
    # xx=gsub('U+00FC|0xFC|ü|Ã¼|%C3 %BC','u',xx)
    # xx=gsub('U+00FD|0xFD|ý|Ã½|%C3 %BD','y',xx)
    # xx=gsub('U+00FE|0xFE|þ|Ã¾|%C3 %BE','!',xx)
    # xx=gsub('U+00FF|0xFF|ÿ|Ã¿|%C3 %BF','!',xx)
    
    # <e1>??????
    # <f1> ??????

    # 
    # xx = gsub("[á|à|â|ã|ä]","a",xx)
    # xx = gsub("[Á|À|Â|Ã|Ä]","A",xx)
    # 
    # xx = gsub("[ó|ò|ô|õ|ö]","o",xx)
    # xx = gsub("[Ó|Ò|Ô|Õ|Ö]","O",xx)
    # 
    # xx = gsub("[í|ì|î|ï]","i",xx)
    # xx = gsub("[Í|Ì|Î|Ï]","I",xx)
    # 
    # xx = gsub("[ú|ù|û|ü]","u",xx)
    # xx = gsub("[Ú|Ù|Û|Ü]","U",xx)
    # 
    # xx = gsub("[é|è|ê|ë]","e",xx)
    # xx = gsub("[É|È|Ê|Ë]","E",xx)
    # 
    # xx = gsub("ñ","n",xx)
    # xx = gsub("Ñ","N",xx)
    # 
    # xx = gsub("ç","c",xx)
    # xx = gsub("Ç","C",xx)
    
    
    #apaga elementos vazios
    xx = xx[xx!=""]
    #se o numero de caracteres da maior palavra for maior do que 2, ent?o o primeiro elemento era todo o nome do coletor, pega apenas o sobrenome
    if (max(nchar(xx))>2) {
      #1 pegue esta palavra como sobrenome se houver apenas 1 palavra
      vll = which(nchar(xx)==max(nchar(xx)))
      #ou 2, se houver mais de uma palavra com o mesmo tamanho, pega a ?ltima delas
      if (length(vll)>1) {
        vll = vll[length(vll)]
      } 
      sobren = xx[vll]
    } else {
      #caso contrario h? apenas abrevia??es em xx, ent?o, virgula separa apenas sobrenome de abreviacoes ou prenome 
      sb = strsplit(x,",")[[1]]
      sb = str_trim(sb)
      nsb = nchar(sb)
      sbvl = which(nsb==max(nsb))
      if (length(sbvl)>1) {
        sbvl = sbvl[length(sbvl)]
      }
      sobren = sb[sbvl]
    }
  } else {
    #neste caso n?o h? virgula, ent?o o ultimo nome ? o sobrenome
    xx = strsplit(x," ")[[1]]
    sobren = xx[length(xx)]
  }
  sobren = str_trim(sobren)
  #pablo H
  # sobren = iconv(sobren, to="ASCII//TRANSLIT") 
  # sobren = gsub("[~|^|~|\"|'|`]","",sobren)
  # sobren = gsub("\\[|\\]|\\(|\\)","",sobren)
  #
  sobren = gsub("?","", sobren)
  sobren = paste(sobren,sep="-")
  if (length(sobren)>0){
    x = strsplit(sobren,"\\|")[[1]]
    sobren = x[1]
    #print(sobren)
    return(sobren)
  } else {
    return(NA)
  }
}
################################################################################

################################################################################
#cria um identificador
pegaprimeiro <- function(x) {
  xx = strsplit(x,",")[[1]][1]
  xx = str_trim(xx)
  #print(xx)
  if (sum(xx=="NA" | xx=="")>0 & !is.na(xx)) { xx = NA}
  return(xx)
}
################################################################################
################################################################################

###############################################################################
#colunas n?meros tem simbolos que podem causar a diferencas indesej?veis
#apaga simbolos, caracteres e espa?os desses n?meros

limpanum2 <- function(x) {
  #remove acentos
  x = iconv(x, to="ASCII//TRANSLIT") 
  x = gsub("[|(|~|^|~|\"|'|)|]","", x)
  x = gsub("[|~|^|~|\"|'|]","", x)

  x= gsub("[A-Z]","",x, ignore.case=T)
  x= gsub("[a-z]","",x, ignore.case=T)
  x= gsub("\\.","",x, ignore.case=T)
  x= gsub("_","",x, ignore.case=T)
  x= gsub("-","",x, ignore.case=T)
  x= gsub("\\(","",x, ignore.case=T)
  x= gsub("\\)","",x, ignore.case=T)
  x= gsub(" ","",x, ignore.case=T)
  x= gsub("/","",x, ignore.case=T)
  x= gsub("\\.","",x, ignore.case=T)
  #casos especias
  x= gsub("<U+FFFD>","",x, ignore.case=T) #PHAMELO
  x= gsub("\\]","",x, ignore.case=T) #PHAMELO
  x= gsub("\\[","",x, ignore.case=T) #PHAMELO
  x = str_trim(x)
  if (!is.na(length(x)>0)) {
    return(x)
  } else {
    return(NA)
  }
}
###############################################################################

##############################################################################
#Adicionar nova colunas COLETORNOME_CORRIGIDO e COLETANUMERO_SO
##############################################################################
atualizacoletores<-function(alldata.tmp,novoscoletores)
{
  msg('Atualizando coletores...')
  alldata.tmp$recordedByStandardized = NA
  for (c in 1:NROW(novoscoletores$recordedBy))
  {
    #nreg=(alldata.tmp$recordedBy==novoscoletores$recordedBy[c])
    nreg=(alldata.tmp$recordedByID==novoscoletores$recordedByID[c])
    alldata.tmp$recordedByStandardized[nreg==T] <- novoscoletores$recordedByStandardized[c]
    #msg(paste0(c,'-',alldata.tmp$COLETORNOME_CORRIGIDO[c]))
  }
  
  msg('Limpando número de coleta...')
  alldata.tmp$recordNumberStandardized = NA
  alldata.tmp$recordNumberStandardized = lapply(alldata.tmp$recordNumber,limpanum2)
  alldata.tmp$recordNumber[alldata.tmp$recordNumber==''] = NA
  msg('OK!')
  return(alldata.tmp)
}
###############################################################################

