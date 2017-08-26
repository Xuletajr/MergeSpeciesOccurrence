init=download.shp(all.records, level=2)
HASC_0=HASC_1=HASC_2=rep(NA,nrow(init))
for (i in 1:nrow(init)){
  print(i)
  code=init$HASC_2[i]
  HASC_S=unlist(strsplit(code,"[.]"))
  HASC_0[i]=HASC_S[1]
  HASC_1[i]=HASC_S[2]
  HASC_2[i]=HASC_S[3]
}
init@data=cbind(init@data,HASC_0,HASC_1,HASC_2)

background.pol<<-download.shp(all.records, level=0)

paises<<-estado<<-mpios<<-init

#world cities
data(world.cities)
sedes<<-world.cities

#produce centroids
cents <- gCentroid(init,byid=T)
cents0<-as.data.frame(coordinates(cents))
cents1<<-SpatialPointsDataFrame(cents,data=cents0)
b=over(init, cents1)
centroid=init
centroid@data$Longmuncen<-b$x
centroid@data$Latmuncent<-b$y