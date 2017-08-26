
databien<-function(sp_search){
  if(!require(pacman)) install.packages("pacman")
  pacman::p_load(BIEN)
  
  x<-{}
  x <- BIEN_occurrence_species(sp_search,
                             all.taxonomy=T,
                             native.status=T,
                             observation.type=T,
                             political.boundaries=T)
  return(x)
}

i=4
sp <- spp[i]
spp.search=nomes_sinonimos_florabr(sp, return_type ='names_synonyms')
x <- BIEN_occurrence_species(sp_search,
                             all.taxonomy=T,
                             native.status=T,
                             observation.type=T,
                             political.boundaries=T)


#BIEN_plot_sampling_protocol("Point-intercept")

#BIEN_plot_list_sampling_protocols() 

#x<- BIEN_occurrence_species(spp,
#                        all.taxonomy=T,
#                        native.status=T,
#                        observation.type=T,
#                        political.boundaries=T)
