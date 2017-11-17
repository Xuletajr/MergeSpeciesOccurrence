###  funções para baixar e preparar dados para junção de ocorrências  ###

# Pablo Hendrigo Alves de Melo - pablopains@yahoo.com.br

### Download or adjust data	###

###---------------------------------------------------------------------###

# permite processar sinônimos das espécies
# salva os resultados nas estrutura de diretórios
# confere se aqruivo já existe 

###---------------------------------------------------------------------###


source("funcoesmobot.R")
source("funcoesgbif.R")
# source("funcoesgbif_local.R") # nao abre occurence.txt
source("funcoeskew.R")
source("funcoesbien.R")
source("funcoessplink.R")
source("funcoessisbbr.R")
source("funcoesjabotrb.R")

setwd("C:/Dados/GitHub/CheckNamesBrazilianFlora2020"); source("CheckNamesBrazilianFlora2020.R")


###---------------------------------------------------------------------###


DownloadData<-function(species.list=NULL,project.name=NULL,data.source=NULL,include.synonyms=TRUE, base.synonyms='FLORABRASIL2020', override.result.file=FALSE, save.image=FALSE)
{
  
  spp = data.frame(species.list, stringsAsFactors = F)
  project = project.name
  data_source = data.source
  
  for (j in 1:NROW(spp))
  {
    
    genus = limpaNA(spp[j,1])
    specificEpithet = limpaNA(spp[j,2])
    infraspecificEpithet = limpaNA(spp[j,3])
    sp = limpaNA(spp[j,4])
    
    msg(paste0(j,'-',sp))
    if ((!file.exists(paste0(getwd(),'/OccurrenceRecords/',project,'/' ,data_source,'/',sp,'.txt')))|(override.result.file==TRUE)){
      
      if (include.synonyms == T){
        
        # spp.search = nome.aceito.sinonimos.FloraBR2020('Begonia', 'reniformis' ,'')
          
        if (base.synonyms=='FLORABRASIL2020'){spp.search=nome.aceito.sinonimos.FloraBR2020(genus, specificEpithet ,infraspecificEpithet)}
      }
      else{spp.search <- data.frame(names=sp, stringsAsFactors = F)}
      
      if (NROW(spp.search)>0){
        x<-{}
        
        if(data_source=='splink'){x<-datasplink(spp.search)}
        
        if(data_source=='sisbbr'){x<-datasisbbr(spp.search)}

        if(data_source=='jabotrb'){x<-datajabotrb(spp.search)}
        
        if(data_source=='gbif'){x<-datagbif(spp.search$names)}

        #if(data_source=='gbif'){x<-datagbif_local(spp.search$names)}
        
        if(data_source=='bien'){x<-databien(spp.search)}

        if(data_source=='kew'){x<-datakew(sp, project.name, data.source,save.image=save.image)}
        
        
        # if(data_source=='spocc'){x<-dataspocc(as.character(spp.search))}
        # if(data_source=='spocc'){x<-dataspocc(as.character(spp.search))}
        
        if(data_source=='mobot'){x<-datamobot(spp.search,project.name,data.source, FALSE)}
        
        if (NROW(x)>0){
          save_occurrence_records(x, sp, project, data_source, sep="\t")
          msg(paste0('  : (',NROW(x),') baixadas '))}
        else{msg(': sem registros!')}} 
      else{msg(': informe uma espécie!')}}
    else{msg(': já baixada!')}
  }
}  

###---------------------------------------------------------------------###


AdjustData<-function(species.list=NULL,project.name=NULL,data.source=NULL, base.synonyms='FLORABRASIL2020',include.synonyms=TRUE,  override.result.file=FALSE)
{
  
  spp <- species.list
  project<-project.name
  data_source<-data.source
  
  for (i in 1:NROW(spp))
  {
    sp <- spp[i]
    msg(paste0(i,'-',sp))
    if ((!file.exists(paste0(getwd(),'/OccurrenceRecords/',project,'/' ,data_source,'/',sp,'.txt')))|(override.result.file==TRUE)){
      
  spp.search <- spp[i]
      
      if (NROW(spp.search)>0){
        x<-{}
        if(data_source=='mobot'){x<-datamobot(species.list=spp.search,project.name=project.name,data.source=data.source, base.synonyms=base.synonyms, include.synonyms=include.synonyms)}
        if (NROW(x)>0){
          save_occurrence_records(x, sp, project, data_source)
          msg(paste0(' - (',NROW(x),') baixadas '))}
        else{msg(': sem registros!')}} 
      else{msg(': informe uma espécie!')}}
    else{msg(': já baixada!')}
  }
}  
