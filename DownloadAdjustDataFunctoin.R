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
source("funcoeskew.R")

###---------------------------------------------------------------------###


DownloadData<-function(species.list=NULL,project.name=NULL,data.source=NULL,include.synonyms=TRUE, base.synonyms='FLORABRASIL2020', override.result.file=FALSE, save.image=FALSE)
{
  
  spp <- species.list
  project<-project.name
  data_source<-data.source
  
  for (i in 1:NROW(spp))
  {
    sp <- spp[i]
    msg(paste0(i,'-',sp))
    if ((!file.exists(paste0(getwd(),'/OccurrenceRecords/',project,'/' ,data_source,'/',sp,'.txt')))|(override.result.file==TRUE)){
      
      if (include.synonyms == T){
        if (base.synonyms=='FLORABRASIL2020'){spp.search=nomes_sinonimos_florabr(sp, return_type ='names_synonyms')}
      }
      else{spp.search <- spp[i]}
      
      if (NROW(spp.search)>0){
        x<-{}
        if(data_source=='jabotrb'){x<-search_rb(spp.search)}
        if(data_source=='gbif'){x<-datagbif(spp.search)}
        if(data_source=='kew'){x<-datakew(as.character(spp.search), project.name, data.source,save.image=save.image)}
        
        # if(data_source=='bien'){x<-databien(as.character(spp.search))}
        # if(data_source=='spocc'){x<-dataspocc(as.character(spp.search))}
        # if(data_source=='spocc'){x<-dataspocc(as.character(spp.search))}
        
        
        if(data_source=='mobot'){x<-datamobot(spp.search,project.name,data.source, FALSE)}
        if (NROW(x)>0){
          save_occurrence_records(x, sp, project, data_source)
          msg(paste0(' - (',NROW(x),') baixadas '))}
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
