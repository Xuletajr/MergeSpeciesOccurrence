### Executa fun��es para baixar e preparar dados para jun��o de ocorr�ncias ###

# Pablo Hendrigo Alves de Melo - pablopains@yahoo.com.br

###  Download or adjust data	###

###---------------------------------------------------------------------###

# permite processar sin�nimos das esp�cies
# salva os resultados nas estrutura de diret�rios
# confere se aqruivo j� existe 

###---------------------------------------------------------------------###

rm(list = ls()) #; gc()
memory.limit(size = 4e3) 
#encoding sessionInfo()

###---------------------------------------------------------------------###

# Informar par�metros:

project = "plantascalcario"
dir.root= "D:/GitHub/MergeSpeciesOccurrence"; setwd(dir.root)
file.spp = "plantascalcario.txt"

###---------------------------------------------------------------------###

# carrega fun��es 

source("funcoesauxiliares.R")
source("DownloadAdjustDataFunctoin.R")
source("par.ini.R")

###---------------------------------------------------------------------###

# carrega lista de esp�cies  

spp.csv <- read.table(file.spp, header=FALSE, sep="\t",dec = ".") 
spp <- as.matrix(spp.csv[ ,1])

###---------------------------------------------------------------------###

# baixa utilizando o pacote rgbif
 
DownloadData(species.list = spp, project.name = project, data.source = "gbif", base.synonyms= 'FLORABRASIL2020',include.synonyms = T,  override.result.file = F)  


###---------------------------------------------------------------------###

# baixa utilizando o pacote jabotRB

DownloadData(species.list=spp,project.name=project,data.source='jabotrb',include.synonyms=TRUE, base.synonyms='FLORABRASIL2020', override.result.file=FALSE)

###---------------------------------------------------------------------###

# baixa direto da pagina do Kew http://apps.kew.org/herbcat/gotoSearchPage.do
# para salvar imagens (save.image = T), criar pasta image em Kew
# sin�nimos (include.synonyms = T) ainda n�o implementado 
 
DownloadData(species.list=spp,project.name=project,data.source='kew',save.image = F)  


###---------------------------------------------------------------------###

# Prepara dados mobot - junta dados do KML com dados baixados do site,
# baixar dados em http://www.tropicos.org/SpecimenSearch.aspx - Urulizar Advanced Search
# Dados: Nome cientitico.csv, Format:�Tab Delimited�, Encoding Type:�UTF-8
# Mapas:Nome cientitico.kml), Google Earth (KML)
# Nomes v�lidos e sin�nimos podem ser baixados em aquivos separados que o sistema junta tudo

AdjustData(species.list = spp, project.name = project, data.source = "mobot", base.synonyms= 'FLORABRASIL2020', include.synonyms = T,  override.result.file = F)