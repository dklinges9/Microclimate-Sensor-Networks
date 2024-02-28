library(maps)
library(ggplot2)
library(raster)
library(colorRamps)
library(vegan)
library(FactoMineR)
library(devtools)
library(ggbiplot)
library(ggbiplot)
library(factoextra)
library(gridExtra)
library(viridis)

#data layer preparation####
#Rcode is exemplified for Belgium
Belgium<-getData('GADM', country='Belgium', level=0) 

#reading in a bunch of rasters with relevant environmental information - we worked with freely available global rasters; see the paper for details
TRI<-raster("Spatial data/tri_1KMmd_GMTEDmd.tif")#1km 
TPI<-raster("Spatial data/tpi_1KMmd_GMTEDmd.tif") #1km  
Elevation<-raster("Spatial data/elevation_1KMmd_GMTEDmd.tif") #1km 
Northness<-raster("Spatial data/northness_1KMmd_GMTEDmd.tif") #1km 
LandUse<-raster("Spatial data/ProbaV_LC100_epoch2015_global_v2.0.2_discrete-classification_EPSG-4326.tif") #250m
FAPAR<-raster("Spatial data/veg_fapar_proba.v.annnual_d_250m_s0..0cm_2014..2019_v1.0.tif") #250m
MacroT<-raster("Spatial data/CHELSA_bio1_1979-2013_V1_1.tif") #1km 
MacroP<-raster("Spatial data/CHELSA_bio12_1979-2013_V1_1.tif") #1km 

#crop all rasters to the country
BelgRAS <- rasterize(Belgium,TRI,1)
clip1 <- crop(TRI, extent(Belgium)) #crop to extent of polygon
BelgTRI <- rasterize(Belgium, clip1, mask=TRUE) #crops to polygon edge & converts to raster
clip1 <- crop(TPI,extent(Belgium))
BelgTPI <- rasterize(Belgium, clip1, mask=TRUE)
clip1 <- crop(LandUse,extent(Belgium))
BelgLaU <- rasterize(Belgium, clip1, mask=TRUE)
clip1 <- crop(FAPAR,extent(Belgium))
BelgFAP <- rasterize(Belgium, clip1, mask=TRUE)
clip1 <- crop(Elevation,extent(Belgium))
BelgEle <- rasterize(Belgium, clip1, mask=TRUE) 
clip1 <- crop(Northness,extent(Belgium))
BelgNOR <- rasterize(Belgium, clip1, mask=TRUE)
clip1 <- crop(MacroT,extent(Belgium))
BelgTEM <- rasterize(Belgium, clip1, mask=TRUE)
clip1 <- crop(MacroP,extent(Belgium))
BelgPRE <- rasterize(Belgium, clip1, mask=TRUE)

BelgCoarse<-aggregate(BelgFAP, fact=4) #1 km resolution instead of 250 m --> we decided we don't want stations closer than 1 kilometer from each other
#get coordinates for a 1-km² grid over the region in a big table, these are our potential sampling locations
BelgPoints<-rasterToPoints(BelgCoarse,spatial=T,progress="text")
XXX1<-extract(BelgTRI,BelgPoints) #extract values from the rasters to the grid points
XXX2<-extract(BelgTPI,BelgPoints)
XXX3<-extract(BelgLaU,BelgPoints)
XXX4<-extract(BelgFAP,BelgPoints)
XXX5<-extract(BelgEle,BelgPoints)
XXX6<-extract(BelgNOR,BelgPoints)
XXX7<-extract(BelgTEM,BelgPoints)
XXX8<-extract(BelgPRE,BelgPoints)
BelgPoints<-cbind(BelgPoints@coords,BelgPoints@data,XXX1,XXX2,XXX3,XXX4,XXX5,XXX6,XXX7,XXX8)
colnames(BelgPoints)<-c("x","y","FAP2","TRI","TPI","LaU","FAP","Ele","NOR","TEM","PRE")

BelgPoints<-BelgPoints[!is.na(BelgPoints$LaU),] #remove some NAs, they'd give trouble later
BelgPoints<-BelgPoints[!is.na(BelgPoints$TRI),]
BelgPoints<-BelgPoints[!is.na(BelgPoints$FAP),]
BelgPoints<-BelgPoints[!is.na(BelgPoints$TEM),]
BelgPoints$LaU<- as.factor(BelgPoints$LaU)

BelgPoints<-BelgPoints[complete.cases(BelgPoints),]

#square root correction of data that would heavily favour outliers (e.g. in mountains)####
#optional step, can be ignored (see paper Supplementary Figure 5).
BelgPointsMin<-BelgPoints[BelgPoints[,12]<=0,]
BelgPointsPlus<-BelgPoints[BelgPoints[,12]>0,]
BelgPointsPlus[,12]<-sqrt(BelgPointsPlus[,12])
BelgPointsMin[,12]<-(-sqrt(-BelgPointsMin[,12]+0.0001))
BelgPoints<-rbind(BelgPointsMin,BelgPointsPlus)

BelgPointsMin<-BelgPoints[BelgPoints[,13]<=0,]
BelgPointsPlus<-BelgPoints[BelgPoints[,13]>0,]
BelgPointsPlus[,13]<-sqrt(BelgPointsPlus[,13])
BelgPointsMin[,13]<-(-sqrt(-BelgPointsMin[,13]+0.0001))
BelgPoints<-rbind(BelgPointsMin,BelgPointsPlus)

BelgPointsMin<-BelgPoints[BelgPoints[,14]<=0,]
BelgPointsPlus<-BelgPoints[BelgPoints[,14]>0,]
BelgPointsPlus[,14]<-sqrt(BelgPointsPlus[,14])
BelgPointsMin[,14]<-(-sqrt(-BelgPointsMin[,14]+0.0001))
BelgPoints<-rbind(BelgPointsMin,BelgPointsPlus)

#PCA environmental variables####
Layers <- BelgPoints[,c(4:5,7:11)] #For simplicity, a PCA is used, so non-continuous data is excluded. 
#Alternatively, a FAMD can be used that allows inclusion of non-continuous data. 
#X and Y are excluded here as well, but can be included as well to maximize spatial variability

pcaBelg<-prcomp(na.omit(Layers), center = TRUE,scale. = TRUE)
summary(pcaBelg)

BelgPoints<-cbind(BelgPoints,get_pca_ind(pcaBelg)$coord[,1:3]) #add PCA coordinates to our big table

#hierarchical selection####
#Selection of sample locations by dividing 3D environmental space into little cubes, 
#and selecting same amount of locations in each cube
#Note that this is just one possible way in which the selection can be made, 
#alternatively, a more rigorous approach could be used that maximimizes the environmental distances between the 
#selected points (e.g., using Euclidian distances and the function dist() in the stats package)
#such an approach would however take very long to run, especially for large countries, making it less user-friendly.

Selection<-c()

Points<-100 #number of locations to select. 
#Note: algorithm does NOT cycle until all these points have been achieved, 
#although it has two optional clauses to select additional locations till this number is reached (see below)

Groups<-ceiling(Points^0.33) #0.33 is used, as we are using environmental space as a cube, which we want to cut in little cubes,
#with selection of points in each cube. 0.33 gives a reasonable selection, but number can be changed to have a coarser or more strict selection

#divide each PCA axis in the same number of bins with equal sizes along each axis
Steps_Dim.1<-(max(BelgPoints$Dim.1)-min(BelgPoints$Dim.1))/Groups
Steps_Dim.2<-(max(BelgPoints$Dim.2)-min(BelgPoints$Dim.2))/Groups
Steps_Dim.3<-(max(BelgPoints$Dim.3)-min(BelgPoints$Dim.3))/Groups

set.seed(1) #random selection in each bin is used; use set.seed to have the same selection in each run

for (i in 1:Groups) {
   #hierarchical selection procedure, first based on dimension 1, 
   #then on dim 2 and 3, each time within the groups of Dim. 1
  
  #select all points within the i'st bin of axis 1
  Selection1 <- BelgPoints[BelgPoints$Dim.1>(min(BelgPoints$Dim.1)+(i-1)*Steps_Dim.1) & 
                             BelgPoints$Dim.1<(min(BelgPoints$Dim.1)+i*Steps_Dim.1),]
  for (j in 1:Groups) {
    #select all points from the i'st bin that fall in the j'st bin of axis 2
    Selection2 <- Selection1[Selection1$Dim.2>(min(Selection1$Dim.2)+(j-1)*Steps_Dim.2) & 
                               Selection1$Dim.2<(min(Selection1$Dim.2)+j*Steps_Dim.2),]
    for (k in 1:Groups) {
      #select all points in that j'st bin that fall within the k'st bin of axis 3
      Selection3 <- Selection2[Selection2$Dim.3>(min(Selection2$Dim.3)+(k-1)*Steps_Dim.3) & 
                                 Selection2$Dim.3<(min(Selection2$Dim.3)+k*Steps_Dim.3),]
      #randomly select 3 points within that cube with dimensions i,j,k
      #depending on distribution of locations in the PCA space (and size of the bins suggested earlier), 
      #there will be more or less cubes with dimension i,j,k that have no points, where thus no points will be selected
      Selection <- rbind(Selection,Selection3[sample(1:nrow(Selection3), 1),])
    }
  }
  
  if(nrow(Selection[complete.cases(Selection[,1:5]),])<(i*Points/Groups)){ 
    #if not enough potential sampling points in the cubes, 
    #we can fill out with random selections from the main category, as many as are in there
    #this is optional and not done in the paper, as it only matters if a fixed amount of locations is required
    if((i*Points/Groups)-nrow(Selection[complete.cases(Selection[,1:5]),])< nrow(Selection1)){
      Selection4 <- Selection1[sample(1:nrow(Selection1), (i*Points/Groups)-nrow(Selection[complete.cases(Selection[,1:5]),])),]
      Selection <- rbind(Selection, Selection4)
    }
    else {
      Selection4<-Selection1
      Selection <- rbind(Selection, Selection4)
    }
    }
}
Selection<-Selection[complete.cases(Selection[,1:5]),]
if(nrow(Selection[complete.cases(Selection[,1:5]),])<Points){ 
  #if  still not enough points to go to the final requested number, just fill up with random points
    Selection4 <- BelgPoints_potential[sample(1:nrow(BelgPoints_potential), Points-nrow(Selection[complete.cases(Selection[,1:5]),])),]
    Selection <- rbind(Selection, Selection4)
}

#visualisations####
par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(0.2,0.2,0.2,0.2),mgp=c(0,0,0),bty="n")
plot(BelgEle,col=matlab.like(100),xaxt="n",yaxt="n", ann=FALSE)
plot(Belgium,add=TRUE)
points(BelgPoints_CNiDT[,1:2],pch=3,cex=1,col = "purple")
points(Selection[,1:2],pch=16,cex=0.8,col = "red")
points(Selection[,1:2],pch=3,cex=1.8,col = "black")


p1<-ggplot(data=BelgPoints,aes(x=Ele,y=TPI))+
  geom_point(size=0.2,pch=16,alpha=0.1)+
  geom_point(data=Selection,aes(x=Ele,y=TPI),size=1,pch=3) +
  geom_point(data=Selection,aes(x=Ele,y=TPI),size=0.5,col="red") +
  theme_classic()+xlab("Elevation (m a.s.l.)")+ylab("Topographic Position Index")

p2<-ggplot(data=BelgPoints,aes(x=Ele,y=TRI))+
  geom_point(size=0.2,pch=16,alpha=0.1)+
  geom_point(data=Selection,aes(x=Ele,y=TRI),size=1,pch=3) +
  geom_point(data=Selection,aes(x=Ele,y=TRI),size=0.5,col="red") +
  theme_classic()+xlab("Elevation (m a.s.l.)")+ylab("Topographic Roughness Index")

p3<-ggplot(data=BelgPoints,aes(x=Ele,y=FAP))+
  geom_point(size=0.2,pch=16,alpha=0.1)+
  geom_point(data=Selection,aes(x=Ele,y=FAP),size=1,pch=3) +
  geom_point(data=Selection,aes(x=Ele,y=FAP),size=0.5,col="red") +
  theme_classic()+xlab("Elevation (m a.s.l.)")+ylab("FAPAR")

grid.arrange(p1,p2,p3,nrow=1)