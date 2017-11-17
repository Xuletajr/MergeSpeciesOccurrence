# Urerabaccifera<-  BIEN_ranges_load_species(species = "Urera baccifera")
# map('world',fill=T , col= "grey", bg="light blue",xlim = c(-180,-20),ylim = c(-60,80))
# plot(Urerabaccifera,col="green",add=T)


print('carregando base Bien...')
bien.data.source = fread('E:/sourceDada/bien/data_bien.txt')
colnames(bien.data.source); bien.data.source = bien.data.source[,-18]; colnames(bien.data.source)

databien<-function(sp_search)
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
  bien.data.source %>% filter(bien.data.source$scrubbed_species_binomial %in% sp.name) -> data
  
  
  if(NROW(data)==0){msg(': Sem Registros!')}
  else{
    data.full <- rbind(data.full,data)
    msg(paste0(': (',NROW(data),')'))
  }
}

  return(data.full)
}


download.neotopc.bien<-function()
{
  
  databien.full <- {}
  
  adm.path <- 'E:/environmental_data/gadm/gadm28.shp/neo'; setwd(adm.path)
  neo.adm=readShapePoly("neo_gadm28.shp"); proj4string(neo.adm) <- CRS.new 
  
  neo.country <- as.vector(unique(neo.adm$NAME_0))
  neo.country <- neo.country[-49] # EUA
  
  for (p in 1:length(neo.country))
  {  
    print(neo.country[p])
    databien <- BIEN_occurrence_country(country = neo.country[p], 
                                        collection.info = T,
                                        political.boundaries = T,
                                        observation.type = T)
    if (NROW(databien)>0){databien.full <- rbind(databien.full, databien)}
    print(NROW(databien))
  }  
  
  fwrite(databien.full, "E:/sourceDada/bien/data_bien.txt")
}

# databien_old<-function(sp_search){
#   # print(sp_search)
#   x<-{}
#   x <- BIEN_occurrence_species(sp_search,
#                              all.taxonomy=T,
#                              native.status=T,
#                              observation.type=T,
#                              political.boundaries=T)
#   return(x)
# }
