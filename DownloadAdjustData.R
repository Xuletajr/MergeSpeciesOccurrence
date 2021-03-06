### Executa fun��es para baixar e preparar dados para jun��o de ocorr�ncias ###

# Pablo Hendrigo Alves de Melo - pablopains@yahoo.com.br

###  Download or adjust data	###

###---------------------------------------------------------------------###

# permite processar sin�nimos das esp�cies
# salva os resultados nas estrutura de diret�rios
# confere se aqruivo j� existe 

###---------------------------------------------------------------------###

# 1. clear the memory and load the packages
# clear workspace and increase memory
rm(list = ls())
memory.limit(size = 1.75e13)

###---------------------------------------------------------------------###

# diret�rio temporario de processamento do R

tempdir <- function() "D:\\temps"
unlockBinding("tempdir", baseenv())
assignInNamespace("tempdir", tempdir, ns="base", envir=baseenv())
assign("tempdir", tempdir, baseenv())
lockBinding("tempdir", baseenv())
tempdir()

###---------------------------------------------------------------------###

# Informar par�metros:

project = "plantascalcario-off"
dir.root= "c:/Dados/GitHub/MergeSpeciesOccurrence"; setwd(dir.root)
file.spp = "plantascalcario2.txt"

###---------------------------------------------------------------------###

# carrega fun��es 

source("funcoesauxiliares.R")
source("DownloadAdjustDataFunctoin.R")
setwd(dir.root)
source("par.ini.R")
setwd(dir.root)

###---------------------------------------------------------------------###

# carrega lista de esp�cies  

spp <- read.table(file.spp, header=F, sep="\t",dec = ".") 

###---------------------------------------------------------------------###

# baixa utilizando o pacote rgbif
 
DownloadData(species.list = spp, project.name = project, data.source = "gbif", base.synonyms= 'FLORABRASIL2020', include.synonyms = T,  override.result.file = F)  

###---------------------------------------------------------------------###

# selecionar splink

DownloadData(species.list = spp, project.name = project, data.source = "splink", base.synonyms= 'FLORABRASIL2020', include.synonyms = T,  override.result.file = T)  

###---------------------------------------------------------------------###

# baixa utilizando o pacote jabotRB

DownloadData(species.list=spp,project.name=project,data.source='jabotrb',include.synonyms=T, base.synonyms='FLORABRASIL2020', override.result.file=T)


###---------------------------------------------------------------------###

# baixa utilizando o pacote BIEN

DownloadData(species.list=spp,project.name=project,data.source='bien',include.synonyms=T, base.synonyms='FLORABRASIL2020', override.result.file=T)

###---------------------------------------------------------------------###

# baixa utilizando o pacote sisbbr

DownloadData(species.list=spp,project.name=project,data.source='sisbbr',include.synonyms=T, base.synonyms='FLORABRASIL2020', override.result.file=T)

###---------------------------------------------------------------------###

# baixa direto da pagina do Kew http://apps.kew.org/herbcat/gotoSearchPage.do
# para salvar imagens (save.image = T), criar pasta image em Kew
# sin�nimos (include.synonyms = T) ainda n�o implementado 
 
DownloadData(species.list=spp,project.name=project,data.source='kew',save.image = F,include.synonyms=F, override.result.file=F)  


###---------------------------------------------------------------------###

# Prepara dados mobot - junta dados do KML com dados baixados do site,
# baixar dados em http://www.tropicos.org/SpecimenSearch.aspx - Urulizar Advanced Search
# Dados: Nome cientitico.csv, Format:�Tab Delimited�, Encoding Type:�UTF-8
# Mapas:Nome cientitico.kml), Google Earth (KML)
# Nomes v�lidos e sin�nimos podem ser baixados em aquivos separados que o sistema junta tudo

AdjustData(species.list = spp, project.name = project, data.source = "mobot", base.synonyms= 'FLORABRASIL2020', include.synonyms = T,  override.result.file = F)
