### Executa funções para baixar e preparar dados para junção de ocorrências ###

# Pablo Hendrigo Alves de Melo - pablopains@yahoo.com.br

###  Download or adjust data	###

###---------------------------------------------------------------------###

# permite processar sinônimos das espécies
# salva os resultados nas estrutura de diretórios
# confere se aqruivo já existe 

###---------------------------------------------------------------------###

rm(list = ls()) #; gc()
memory.limit(size = 4e3) 
#encoding sessionInfo()

###---------------------------------------------------------------------###

# Informar parâmetros:

project = "plantascalcario"
dir.root= "D:/GitHub/MergeSpeciesOccurrence"; setwd(dir.root)
file.spp = "plantascalcario.txt"

###---------------------------------------------------------------------###

# carrega funções 

source("funcoesauxiliares.R")
source("DownloadAdjustDataFunctoin.R")
source("par.ini.R")

###---------------------------------------------------------------------###

# carrega lista de espécies  

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
# sinônimos (include.synonyms = T) ainda não implementado 
 
DownloadData(species.list=spp,project.name=project,data.source='kew',save.image = F)  


###---------------------------------------------------------------------###

# Prepara dados mobot - junta dados do KML com dados baixados do site,
# baixar dados em http://www.tropicos.org/SpecimenSearch.aspx - Urulizar Advanced Search
# Dados: Nome cientitico.csv, Format: Tab Delimited , Encoding Type: UTF-8
# Mapas:Nome cientitico.kml), Google Earth (KML)
# Nomes válidos e sinônimos podem ser baixados em aquivos separados que o sistema junta tudo

AdjustData(species.list = spp, project.name = project, data.source = "mobot", base.synonyms= 'FLORABRASIL2020', include.synonyms = T,  override.result.file = F)
