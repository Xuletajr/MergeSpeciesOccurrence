
###---------------------------------------------------------------------###

MergeSpeciesOccurrence<-function(spp,project)
{
  
  spp.dados<-as.list({})
  juntar<-F
  dir.datain<-paste0(getwd(),'/OccurrenceRecords/',project,'/')
  for (i in 1:NROW(spp)){
    
    #--- Carrega dados ---#
    gbif <- inpa <- jabot <- kew <- mobot <- mydataset <- nybg <- splink <- jabotrb <- bien <- sisbbr <- NULL
    sp <- spp[i];msg(paste0(i,'-',sp))
    
    #--- gbif ---#
    file.txt = paste0(dir.datain ,data.set[1],'/',sp,'.txt')
    if(file.exists(file.txt)) {
      gbif= read.csv(file.txt,sep='\t', dec='.', header=T,as.is=T, encoding="UTF-8")  
      #gbif= fread(file.txt, encoding="UTF-8")  
      msg(paste(data.set[1], '-' ,NROW(gbif),',', length(colnames(gbif)) ))}
    
    #--- inpa ---#
    file.txt = paste0(dir.datain, data.set[2],'/',sp,'.txt')
    if(file.exists(file.txt)) {
      inpa=read.table(file.txt,sep='\t', header=T,as.is=T)
      #inpa=fread(file.txt, encoding="UTF-8")
      msg(paste(data.set[2], '-' ,NROW(inpa),',', length(colnames(inpa)) ))}
    
    #--- jabot ---#
    file.txt = paste0(dir.datain, data.set[3],'/',sp,'.txt')
    if(file.exists(file.txt)) {
      jabot=read.csv(file.txt,sep=';', dec='.',header=T,as.is=T, encoding="UTF-8")  
      #jabot= fread(file.txt, encoding="UTF-8")  
      msg(paste(data.set[3], '-' ,NROW(jabot),',', length(colnames(jabot)) ))}
    
    #--- kew ---#
    file.txt = paste0(dir.datain ,data.set[4],'/',sp,'.txt')
    if(file.exists(file.txt)) {
      kew=read.table(file.txt,sep='\t', dec='.',header=T,as.is=T, encoding="UTF-8")  
      #kew= fread(file.txt, encoding="UTF-8")  
      msg(paste(data.set[4], '-' ,NROW(kew),',', length(colnames(kew)) ))}
    
    #--- mobot ---#
    file.txt = paste0(dir.datain ,data.set[5],'/',sp,'.txt')
    if(file.exists(file.txt)) {
      mobot=read.table(file.txt, sep=' ', header=T,as.is=T, encoding="UTF-8")  
      #mobot= fread(file.txt, encoding="UTF-8")  
      msg(paste(data.set[5], '-' ,NROW(mobot),',', length(colnames(mobot)) ))}
    
    #--- mydataset ---#
    file.txt = paste0(dir.datain ,data.set[6],'/',sp,'.txt')
    if(file.exists(file.txt)) {
      mydataset=read.table(file.txt,sep='\t', header=T,as.is=T, encoding="UTF-8")  
      #mydataset = fread(file.txt, encoding="UTF-8")  
      msg(paste(data.set[6], '-' ,NROW(mydataset),',', length(colnames(mydataset)) ))}
    
    #--- nybg ---#
    file.txt = paste0(dir.datain ,data.set[7],'/',sp,'.csv')
    if(file.exists(file.txt)) {
      nybg=read.table(file.txt,sep=',', header=T,as.is=T, encoding="UTF-8")  
      #ntbg = fread(file.txt, encoding="UTF-8")  
      msg(paste(data.set[7], '-' ,NROW(nybg),',', length(colnames(nybg)) ))}
    
    #--- splink ---#
    file.txt = paste0(dir.datain ,data.set[8],'/',sp,'.txt')
    if(file.exists(file.txt)) {
      splink=read.csv(file.txt, dec='.',sep='\t', header = TRUE, encoding="UTF-8")  
      # #splink = fread(file=file.txt, nrows=-1, header = T); colnames(splink) 
      # # carregando todas as coordenadas
      # splink.ori <- splink[splink$latitude != 0 & splink$longitude != 0, ]
      # splink.mun <- splink[splink$latitude_mun != 0 & splink$longitude_mun != 0, ]
      # splink.mun$latitude = splink.mun$latitude_mun
      # splink.mun$longitude = splink.mun$longitude_mun
      # 
      # splink <- rbind(splink.mun,splink.ori)
      # 
      # msg(paste(data.set[8], ' orig. -' ,NROW(splink.ori),',', length(colnames(splink.ori)) ))
      # msg(paste(data.set[8], ' atrib. -' ,NROW(splink.mun),',', length(colnames(splink.mun)) ))
      msg(paste(data.set[8], ' total -' ,NROW(splink),',', length(colnames(splink)) ))

      }
    
    #--- jabotbr---#
    file.txt = paste0(dir.datain ,data.set[9],'/',sp,'.txt')
    if(file.exists(file.txt)) {
      jabotrb=read.csv(file.txt, dec='.',sep='\t', header = TRUE, encoding="UTF-8")  
      #jabotrb= fread(file.txt, encoding="UTF-8")  
      msg(paste(data.set[9], '-' ,NROW(jabotrb),',', length(colnames(jabotrb)) ))}
    
    #--- bien ---#
    file.txt = paste0(dir.datain ,data.set[10],'/',sp,'.txt')
    if(file.exists(file.txt)) {
      bien=read.csv(file.txt, dec='.',sep='\t', header = TRUE, encoding="UTF-8")  
      #bien = fread(file.txt, encoding="UTF-8")  
      msg(paste(data.set[10], '-' ,NROW(bien),',', length(colnames(bien)) ))}
    
    #--- sisbbr ---#
    file.txt = paste0(dir.datain ,data.set[11],'/',sp,'.txt')
    if(file.exists(file.txt)) {
      sisbbr=read.csv(file.txt, dec='.',sep='\t', header = TRUE, encoding="UTF-8")  
      #sisbbr = fread(file.txt, encoding="UTF-8")  
      msg(paste(data.set[11], '-' ,NROW(sisbbr),',', length(colnames(sisbbr)) ))}
    
    #--- spocc---#
    
    #--- Junta dados ---#
    dados<-as.list({})
    if(NROW(gbif)>0)      {dados=append(dados,list(gbif=gbif))}
    if(NROW(inpa)>0)      {dados=append(dados,list(inpa=inpa))}
    if(NROW(jabot)>0)     {dados=append(dados,list(jabot=jabot))}
    if(NROW(kew)>0)       {dados=append(dados,list(kew=kew))}
    if(NROW(mobot)>0)     {dados=append(dados,list(mobot=mobot))}
    if(NROW(mydataset)>0) {dados=append(dados,list(mydataset=mydataset))}
    if(NROW(nybg)>0)      {dados=append(dados,list(nybg=nybg))}
    if(NROW(splink)>0)    {dados=append(dados,list(splink=splink))}
    if(NROW(jabotrb)>0)  {dados=append(dados,list(jabotrb=jabotrb))}

    if(NROW(bien)>0)  {dados=append(dados,list(bien=bien))}
    if(NROW(sisbbr)>0)  {dados=append(dados,list(sisbbr=sisbbr))}
    
    if(length(dados)==0) {next}
    
    msg('Padronizando colunas...')
    #--- Padriniza c ---#
    nomescl <- read.table("NomesColunasTDWG.csv",sep=";",header=T,as.is=T)
    d=4
    for(d in 1:length(dados)) 
    {
      if(NROW(dados[[d]])>0){
        dad = dados[[d]]
        cl = colnames(dad)
        repositorio = names(dados)[d]
        vl = nomescl$COLUNANOME%in%cl & nomescl$source==repositorio & nomescl$INTERESSA==1
        #sum(vl)
        novonomecoluna = nomescl[vl,'NOVONOME']
        nomeoriginal = nomescl[vl,'COLUNANOME']
        if (length(unique(novonomecoluna))!=length(novonomecoluna)) {
          #se houve nomes novos de c que est?o duplicados, avisa e adiciona o numero 2 no final do nome duplicado
          vt = which(duplicated(novonomecoluna))
          print(paste("Os seguintes nomes NOVOS de c est?o duplicados na base", repositorio,paste(novonomecoluna[vt],collapse="\n")))
          novonomecoluna[vt] = paste(novonomecoluna[vt],2,sep="")
        }
        
        #atribui o novo nome de c aos dados
        cll = colnames(dad)
        names(cll) = cll
        cll[nomeoriginal] = novonomecoluna
        colnames(dad) = cll
        
        #pega os dados apenas para as c de interesse
        dad = dad[,novonomecoluna]
        #salva o dado na lista
        dados[[d]] = dad 
      } 
    }
    
    #- Limpa c ---#
    d=1
    for(d in 1:length(dados))
    {
      dad = dados[[d]]
      for(cl in 1:ncol(dad)){
        dad[,cl] = limpacolunas(dad[,cl])
      }
      dados[[d]] = dad
    }

    
    #--- Acertar datas de coleta e determinacao ---#
    
    #--- gbif ---#
    # data coleta já vem dividida em dia, mes e ano 
    # data determinação - dividir em dia, mes e ano
    if(NROW(dados[['gbif']])>0)
      {dat=dados[['gbif']]; dat$eventDate=substr(dat$eventDate,1,10); dados[['gbif']]=dat}
    
    if(NROW(dados[['gbif']])>0)
      {dados[['gbif']]= acertadata(dados[['gbif']],'dateIdentified')}
    
    #--- inpa ---#
    
    #--- jabot ---#
    # data coleta e determinação já vêm divididas em dia, mes e ano 
    
    #--- kew ---#
    # data coleta - dividir em dia, mes e ano
    # data determinação - não tem
    if(NROW(dados[['kew']])>0){dados[['kew']]= acertadata(dados[['kew']])}  
    
    #--- mobot ---#
    # data coleta - dividir em dia, mes e ano 
    # data determinação - não tem

    if(NROW(dados[['mobot']])>0){dados[['mobot']]= acertadata(dados[['mobot']],field='verbatimEventDate')}  
    
    #--- mydataset ---#
    #  deve vir tudo ok conforme modelo
    
    #--- nybg ---#
    # data coleta - dividir em dia, mes e ano
    # data determinação - dividir em dia, mes e ano (não compensa!)
    if(NROW(dados[['nybg']])>0){dados[['nybg']] = acertadata(dados[['nybg']])}  
    
    #--- splink ---#
    # data coleta e determinação já vêm divididas em dia, mes e ano 
    
    #--- jabotbr---#
    # data coleta e determinação já vêm divididas em dia, mes e ano 
    
    #--- bien ---#
    # data coleta - dividir em dia, mes e ano
    if(NROW(dados[['bien']])>0){dados[['bien']] = acertadata(dados[['bien']], field = 'eventDate')}  

    if(NROW(dados[['bien']])>0)
    {dados[['bien']]= acertadata(dados[['bien']],'dateIdentified')}
    
    #--- sisbbr ---#
    if(NROW(dados[['sisbbr']])>0){dados[['sisbbr']] = acertadata(dados[['sisbbr']], field = 'eventDate')}  
    
    #--- spocc---#
    
    
    ###############################################################################
    #aplica a fun??o sobrenome para inpa e mobot ...
    
    #aplica a fun??o para o dado INPA
    if(NROW(dados[['inpa']])>0){
      ll = toupper(as.vector(lapply(dados[['inpa']]$recordedBy,pegasobrenome),mode='character'))
      ll[ll=='NA'] =NA
      dados[['inpa']]$recordedByLastName = ll}
    
    
    #no caso do MOBOT tamb?m o primeiro valor ? o sobrenome
    if(NROW(dados[['mobot']])>0){
      ll = toupper(as.vector(lapply(dados[['mobot']]$recordedBy,pegasobrenome),mode='character'))
      ll[ll=='NA'] =NA
      dados[['mobot']]$recordedByLastName = ll}  
    
    ###############################################################################
    #aplica a fun??o pegasobrenomegbif para gbif, splink e jabot
    
    if(NROW(dados[['gbif']])>0){
      cols = dados[['gbif']]$recordedBy
      #remove pontos
      cols = gsub("\\.","\\. ",cols)
      #lapply(coletores, pegasobrenomegbif)
      
      ll = toupper(as.vector(lapply(cols, pegasobrenomegbif)))
      #ai pega so o sobrenome
      ll[ll=='NA'] = NA
      dados[['gbif']]$recordedByLastName = ll}
    
    #o do species link tem o mesmo padrao, entao usa a mesma funcao
    if(NROW(dados[['splink']])>0){
      cols = dados[['splink']]$recordedBy
      cols = gsub("\\.","\\. ",cols)
      ll = toupper(as.vector(lapply(cols, pegasobrenomegbif)))
      ll[ll=='NA'] = NA
      dados[['splink']]$recordedByLastName = ll}
    
    # jabot parce aplicar o mesmo caso
    if(NROW(dados[['jabot']])>0){
      cols = dados[['jabot']]$recordedBy
      #remove pontos
      cols = gsub("\\.","\\. ",cols)
      #ll = toupper(as.vector(lapply(cols, pegasobrenomegbif),mode='character'))
      ll = toupper(as.vector(lapply(cols, pegasobrenomegbif)))
      #ai pega so o sobrenome
      ll[ll=='NA'] = NA
      dados[['jabot']]$recordedByLastName = ll}
    
    ##aqui
    if(NROW(dados[['nybg']])>0){
      cols = dados[['nybg']]$recordedBy
      #remove pontos
      cols = gsub("\\.","\\. ",cols)
      #ll = toupper(as.vector(lapply(cols, pegasobrenomegbif),mode='character'))
      ll = toupper(as.vector(lapply(cols, pegasobrenomegbif)))
      #ai pega so o sobrenome
      ll[ll=='NA'] = NA
      dados[['nybg']]$recordedByLastName = ll}
    
    # RB jabot parce aplicar o mesmo caso
    if(NROW(dados[['jabotrb']])>0){
      cols = dados[['jabotrb']]$recordedBy
      #remove pontos
      cols = gsub("\\.","\\. ",cols)
      #ll = toupper(as.vector(lapply(cols, pegasobrenomegbif),mode='character'))
      ll = toupper(as.vector(lapply(cols, pegasobrenomegbif)))
      #ai pega so o sobrenome
      ll[ll=='NA'] = NA
      dados[['jabotrb']]$recordedByLastName = ll}
    
    # kew
    if(NROW(dados[['kew']])>0){
      cols = dados[['kew']]$recordedBy
      #remove pontos
      cols = gsub("\\.","\\. ",cols)
      #ll = toupper(as.vector(lapply(cols, pegasobrenomegbif),mode='character'))
      ll = toupper(as.vector(lapply(cols, pegasobrenomegbif)))
      #ai pega so o sobrenome
      ll[ll=='NA'] = NA
      dados[['kew']]$recordedByLastName = ll}

    if(NROW(dados[['bien']])>0){
      cols = dados[['bien']]$recordedBy
      #remove pontos
      cols = gsub("\\.","\\. ",cols)
      #ll = toupper(as.vector(lapply(cols, pegasobrenomegbif),mode='character'))
      ll = toupper(as.vector(lapply(cols, pegasobrenomegbif)))
      #ai pega so o sobrenome
      ll[ll=='NA'] = NA
      dados[['bien']]$recordedByLastName = ll}
    
    if(NROW(dados[['sisbbr']])>0){
      cols = dados[['sisbbr']]$recordedBy
      #remove pontos
      cols = gsub("\\.","\\. ",cols)
      #ll = toupper(as.vector(lapply(cols, pegasobrenomegbif),mode='character'))
      ll = toupper(as.vector(lapply(cols, pegasobrenomegbif)))
      #ai pega so o sobrenome
      ll[ll=='NA'] = NA
      dados[['sisbbr']]$recordedByLastName = ll}

    d=1
    for(d in 1:length(dados)) {
      dados[[d]]$scientificNameKey=sp
      dados[[d]]$source= names(dados)[d]
    }  
    
    spp.dados=append(spp.dados,list(ds=dados))
    names(spp.dados)[length(spp.dados)]=sp
    
  }  
  
  result.juncao.tmp <- juncao(spp.dados)
  
  return(list(tabela=result.juncao.tmp,juncao.lista=spp.dados))
}

###---------------------------------------------------------------------###

compilacoletores<-function(alldata)
{
  oscoletores = NULL
  if(NROW(alldata)>0){  
    is.na(alldata)=={}
    di = unique(alldata[,c("recordedByLastName","recordedBy")])
    oscoletores = rbind(oscoletores,di)}
  oscoletores = unique(oscoletores)
  oscoletores = oscoletores[order(oscoletores$recordedByLastName),]
  
  
  #oscoletores$NOVOCOLETORNOME = oscoletores[,c("SOBRENOME")]
  
  oscoletores$recordedByStandardized = rep(NA, NROW(oscoletores))
  oscoletores$recordedByID = (1:NROW(oscoletores))
  
  for (c in 1: NROW(oscoletores))
  {
    oscoletores$recordedByStandardized[c] = oscoletores$recordedByLastName[c]
  }
  
#  oscoletores$recordedByStandardized = oscoletores$recordedByLastName
 
  #oscoletores$CAIXAALTA = toupper(oscoletores[,"COLETORNOME"])
  return(oscoletores)
}

###---------------------------------------------------------------------###

juncao<-function(spp.list)
{
  cls = NULL
  rns = 0
  for (n.sp in 1:length(spp.list)) {
    nvdados = spp.list[[n.sp]]
    for (n in 1:length(nvdados)) {
      dad = nvdados[[n]]
      dad$source = rep(names(nvdados)[n],nrow(dad))
      cls = c(cls, colnames(dad))
      nvdados[[n]] = dad
      rns = nrow(dad)+rns
    }  
  }
  cls = unique(cls)
  #junta os dados
  todos = as.data.frame(matrix(NA,nrow=rns,ncol=length(cls),dimnames=list(1:rns,cls)),stringsAsFactors=F)
  idx = 0
  for (n.sp in 1:length(spp.list)) {
    nvdados = spp.list[[n.sp]]
    for (n in 1:length(nvdados)) {
      dad = nvdados[[n]]
      cl = colnames(dad)
      from = idx+1
      to = idx+nrow(dad)
      todos[from:to,cl] = dad[,cl]
      idx = to
    }  
  }
  return(todos)
}

###---------------------------------------------------------------------###
