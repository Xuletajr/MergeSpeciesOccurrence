### baixa dados gbif  ###

# Pablo Hendrigo Alves de Melo - pablopains@yahoo.com.br

###  datagbif	###

###---------------------------------------------------------------------###

# utiliza pacote rgbif, função occ_seach,  busca em scientificName
# concatena resultados de buscas nomes válidos e sinônimos de uma mesma espécie 
# permite ajustar pra utilizar difrentes fontes de sinônimos
# força retorno de todas as culunas gbif, com ou sem informação

###---------------------------------------------------------------------###

datagbif <- function(sp_search=NULL, remove.badissues=TRUE, limite=0, dgbif=data.frame(
  accessRights=NA,
  associatedReferences=NA,
  associatedSequences=NA,
  basisOfRecord=NA,
  behavior=NA,
  bibliographicCitation=NA,
  catalogNumber=NA,
  class=NA,
  classKey=NA,
  collectionCode=NA,
  collectionID=NA,
  continent=NA,
  coordinateAccuracy=NA,
  countryCode=NA,
  county=NA,
  datasetID=NA,
  datasetKey=NA,
  datasetName=NA,
  dateIdentified=NA,
  day=NA,
  decimalLatitude=NA,
  decimalLongitude=NA,
  depth=NA,
  depthAccuracy=NA,
  disposition=NA,
  dynamicProperties=NA,
  elevation=NA,
  elevationAccuracy=NA,
  endDayOfYear=NA,
  eventDate=NA,
  eventRemarks=NA,
  eventTime=NA,
  family=NA,
  familyKey=NA,
  fieldNotes=NA,
  fieldNumber=NA,
  gbifID=NA,
  genericName=NA,
  genus=NA,
  genusKey=NA,
  georeferencedBy=NA,
  georeferencedDate=NA,
  georeferenceProtocol=NA,
  georeferenceRemarks=NA,
  georeferenceVerificationStatus=NA,
  habitat=NA,
  hasCoordinate=NA,
  hasGeospatialIssues=NA,
  higherClassification=NA,
  higherGeography=NA,
  identificationQualifier=NA,
  identificationRemarks=NA,
  identificationVerificationStatus=NA,
  identifiedBy=NA,
  identifier=NA,
  individualID=NA,
  informationWithheld=NA,
  infraspecificEpithet=NA,
  institutionCode=NA,
  institutionID=NA,
  island=NA,
  islandGroup=NA,
  issue=NA,
  kingdom=NA,
  kingdomKey=NA,
  language=NA,
  lastCrawled=NA,
  lastInterpreted=NA,
  lastParsed=NA,
  locality=NA,
  locationAccordingTo=NA,
  locationRemarks=NA,
  materialSampleID=NA,
  mediaType=NA,
  modified=NA,
  month=NA,
  municipality=NA,
  nameAccordingTo=NA,
  nomenclaturalCode=NA,
  occurrenceID=NA,
  occurrenceRemarks=NA,
  occurrenceStatus=NA,
  order=NA,
  orderKey=NA,
  otherCatalogNumbers=NA,
  ownerInstitutionCode=NA,
  phylum=NA,
  phylumKey=NA,
  preparations=NA,
  previousIdentifications=NA,
  protocol=NA,
  publishingCountry=NA,
  recordedBy=NA,
  recordNumber=NA,
  references=NA,
  reproductiveCondition=NA,
  rights=NA,
  rightsHolder=NA,
  samplingProtocol=NA,
  scientificName=NA,
  source=NA,
  species=NA,
  speciesKey=NA,
  specificEpithet=NA,
  startDayOfYear=NA,
  stateProvince=NA,
  taxonKey=NA,
  taxonomicStatus=NA,
  taxonRank=NA,
  taxonRemarks=NA,
  type=NA,
  typeStatus=NA,
  typifiedName=NA,
  verbatimCoordinateSystem=NA,
  verbatimElevation=NA,
  verbatimEventDate=NA,
  verbatimLocality=NA,
  verbatimSRS=NA,
  verbatimTaxonRank=NA,
  vernacularName=NA,
  waterBody=NA,
  year=NA
)){
  for (s in 1:NROW(sp_search)){
    
    dat.full <- dat.full.in <- datain <- {}
    
    sp.name <- as.character(sp_search[s,1])
    #sp.name <- as.character(sp_search[s])
    speciesKey <- name_backbone(name=sp.name)$speciesKey
    
    if (limite==0){
      if (length(speciesKey)>0){n.occ <- occ_count(speciesKey,basisOfRecord = 'PRESERVED_SPECIMEN')}
      else{n.occ <- 10}}
    else{n.occ <-limite}
    
    cat(' -> ',sp.name,' [key gbif ',speciesKey,'(',n.occ,') ]')
    
    # busca registros 
    dat.full <- occ_search( scientificName = sp.name, limit = n.occ, basisOfRecord = 'PRESERVED_SPECIMEN')
    

    # dat.spocc <- occ(query = sp.name, from = source.data,limit = n.occ)
    #    if(NROW(dat.spocc$gbif$data[[1]])==0){cat(' (Sem Registros)')}
    #    else{
    #      dat.full <- as.data.frame(dat.spocc$gbif$data[[1]])
    
    #dat.full <- dat.full[!duplicated(dat.full),] 
    cat(' - (',NROW(dat.full$data),') encontrados ')
    
    # retira issues restritivos
    if (remove.badissues & NROW(dat.full$data) > 0){
      dat.full.in <- trata_issues(dat.full) 
      cat(' - (',NROW(dat.full.in$data),') sem issues ')}
    else{dat.full.in <-dat.full}
    
    if(NROW(dat.full.in$data)==0){cat(' (Sem Registros)')}
    else{
      datain <- as.data.frame(dat.full.in$data)
      name_fields <- colnames(datain)
      for (nlinha in 1:NROW(datain)){
        nlinhagbif=nrow(dgbif)+1
        for (ncoluna in 1:length(dgbif)){
          coluna = colnames(dgbif)[ncoluna]
          if (length(name_fields[name_fields %in% coluna])>0)
          {dgbif[nlinhagbif,ncoluna] = datain[nlinha,c(coluna)]}
        }
      }
    }
    cat(' - (',NROW(datain),') baixadas ')
  }
  return(dgbif[2:nrow(dgbif),])
}

###---------------------------------------------------------------------###

# Remove registros com bad issues

trata_issues <- function(dat.full){
  dat.full <- dat.full %>% occ_issues(mutate = "expand")
  
  # ver detalhes em http://gbif.github.io/gbif-api/apidocs/org/gbif/api/vocabulary/OccurrenceIssue.html
  dat.full.in <- dat.full %>% occ_issues(-ZERO_COORDINATE,
                                         -COORDINATE_OUT_OF_RANGE,
                                         -COORDINATE_INVALID,
                                         -GEODETIC_DATUM_INVALID,
                                         -COORDINATE_REPROJECTION_FAILED,
                                         -COORDINATE_ACCURACY_INVALID,
                                         -COORDINATE_PRECISION_INVALID,
                                         -COORDINATE_UNCERTAINTY_METERS_INVALID,
                                         -COUNTRY_INVALID,
                                         -CONTINENT_INVALID,
                                         -PRESUMED_SWAPPED_COORDINATE
                                         -RECORDED_DATE_INVALID,
                                         -PRESUMED_NEGATED_LONGITUDE,
                                         -PRESUMED_NEGATED_LATITUDE,
                                         -BASIS_OF_RECORD_INVALID)
  return(dat.full.in)}
