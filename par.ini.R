### parâmetros do sistema ###

# Pablo Hendrigo Alves de Melo - pablopains@yahoo.com.br

###  par.ini.R	###

###---------------------------------------------------------------------###

# mostra mensagens de processamento

showmessage=T

###---------------------------------------------------------------------###

# lista de repositórios - nem todos estão implementados
 
data.set<-{}
data.set[1] = 'gbif' ; data.set[2] = 'inpa'   ; data.set[3] = 'jabot'    ;
data.set[4] = 'kew'  ; data.set[5] = 'mobot'  ; data.set[6] = 'mydataset';
data.set[7] = 'nybg' ; data.set[8] = 'splink' ; data.set[9] = 'jabotrb'  ;
data.set[10] = 'bien'; data.set[11] = 'sisbbr'; 

###---------------------------------------------------------------------###

# cria estrutura de diretórios em: OccurrenceRecords 

sapply(data.set,criarapastas,project=project)

###---------------------------------------------------------------------###

