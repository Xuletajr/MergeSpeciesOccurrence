### Combina registros de ocorrências de diferentes fontes de dados ###

# Pablo Hendrigo Alves de Melo - pablopains@yahoo.com.br

###  MergeSpeciesOccurrence	###

###---------------------------------------------------------------------###

# Atualuza nome das colulas para o padrão Darwin Core http://rs.tdwg.org/dwc/terms/ 

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
source("MergeSpeciesOccurrenceFunction.R")
source("funcoesAlbertoVicentini.R")
source("par.ini.R")

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

