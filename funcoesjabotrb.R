print('carregando base JabotRB...')

datajabotrb<-function(sp_search)
{
  
  data.full <- {}

  
  for (s in 1:NROW(sp_search))
  {
    
    data <- {}
    
    sp.name <- as.character(sp_search[s,1])
    msg(sp.name)  
    
    # busca registros 
    data <- search_rb(sp.name)

    if(NROW(data)==0){msg(': Sem Registros!')}
    else{
      data.full <- rbind(data.full,data)
      msg(paste0(': (',NROW(data),')'))
    }
  }
  
  return(data.full)
}


