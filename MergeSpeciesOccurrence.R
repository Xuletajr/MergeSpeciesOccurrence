### funções auxiliares para as diferentes rotinas ###

# Pablo Hendrigo Alves de Melo - pablopains@yahoo.com.br

###  MergeSpeciesOccurrence	###

###---------------------------------------------------------------------###

rm(list = ls()) #; gc()
memory.limit(size = 4e3) 
#encoding sessionInfo()

###---------------------------------------------------------------------###

# Informar parâmetros:

project = "plantascalcario"
dir.root= "D:/GitHub/MergeSpeciesOccurrence"; setwd(dir.root)
dir.gis = 'D:/GitHub/MergeSpeciesOccurrence/GisBase'
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

###---------------------------------------------------------------------###

# carrega lista de espécies  

spp.csv <- read.table(file.spp, header=FALSE, sep="\t",dec = ".") 
spp <- as.matrix(spp.csv[ ,1])

###---------------------------------------------------------------------###

# junta ocorrências das diferentes fontes de dados 

alldata <- MergeSpeciesOccurrence(spp,project)

alldata.table<-data.frame(alldata$tabela, stringsAsFactors = FALSE)#; View(alldata.table)

###---------------------------------------------------------------------###

novoscoletores<-compilacoletores(alldata.table)
#write.table(novoscoletores,file='novoscoletores.csv',sep='\t', row.names=F,na='')
msg("Conferir arquivo:novoscoletores.csv")

###---------------------------------------------------------------------###

#novoscoletores = read.csv(file="novoscoletoresCorrigido.csv",sep='\t', dec='.',heade=T, as.is=T)

alldata.table <- atualizacoletores(alldata.table,novoscoletores)

###---------------------------------------------------------------------###

View(alldata.table)


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
do.