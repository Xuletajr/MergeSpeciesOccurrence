print('carregando base gbif...')
gbif.data.source = fread('E:/sourceDada/gbif/occurrence.txt')
datagbif_local<-function(sp_search)
{

data.full <- {}

for (s in 1:NROW(sp_search))
{
  
  data <- {}
  
  sp.name <- as.character(sp_search[s,1])
  msg(sp.name)  

  # busca registros 
  # data <- BIEN_occurrence_species(sp.name,
  #                              all.taxonomy=T,
  #                              native.status=T,
  #                              observation.type=T,
  #                              political.boundaries=T)
  
  # pelo nome bien
  gbif.data.source %>% filter(gbif.data.source$scientificName %in% sp.name) -> data

  
  if(NROW(data)==0){msg(': Sem Registros!')}
  else{
    data.full <- rbind(data.full,data)
    msg(paste0(': (',NROW(data),')'))
  }
}

  return(data.full)
}


