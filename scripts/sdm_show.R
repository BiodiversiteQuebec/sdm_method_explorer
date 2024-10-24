
library(terra)
library(FRutils)
library(data.table)
library(dismo)

cols<-c("#CCCCCC","#98abc5", "#8a89a6", "#7b6888", "#6b486b", "#a05d56", "#d0743c", "#ff8c00","darkorange")
cols<-colo.scale(1:200,cols)



lf<-list.files("outputs", full = TRUE, pattern = ".tif")




png("sdms.png",width=16,height=10,res=400,units="in")
par(mfrow=n2mfrow(length(lf),asp=3/2))
r<-lapply(lf,function(i){
  r<-rast(i)
  titre<-basename(i)
  titre<-gsub(".tif","",titre)
  titre<-sapply(strsplit(titre,"_"),function(i){paste(i[3:6],collapse="\n")})
  plot(r,mar=c(0,0,0,0),legend=FALSE,axes=FALSE)
  mtext(side=3,line=-3,text=titre,adj=0.05,cex=0.5)
})
par(mfrow=c(1,1))
dev.off()
file.show("sdms.png")


res<-fread("results.csv")
#res[order(species,algorithm,bias,usepredictors,spatial)]
res[rev(order(time)),]
res<-res[!is.na(auc),]
res<-res[!is.na(I),]
plot(I~auc,data=res)
betas<-lapply(split(res,res$species),function(i){
  m<-lm(I~auc,data=i)
  p<-predict(m,data.frame(auc=range(i$auc)))
  lines(range(i$auc),p)
  unname(coef(m)["auc"])
})
hist(unlist(betas),breaks=20)


res<-fread("results.csv")
#res[order(species,algorithm,bias,usepredictors,spatial)]
res[rev(order(time)),]
plot(auc~I,data=res[!is.na(auc),])
lapply(split(res[!is.na(auc),],))




lf<-list.files("outputs",full=TRUE,pattern="bonasa_umbellus_brt_Predictors_Bias_noSpatial")
r1<-rast(lf)

lf<-list.files("data",full=TRUE,pattern="Bonasa_umbellus")
r2<-rast(lf)

r3<-project(r2,r1,mask=TRUE)
r3<-mask(r3,r1)
#r1<-mask(r1,r3)

par(mfrow=1:2)
plot(r1)
plot(r3)

nicheOverlap(raster(r1),raster(r3),stat="I", checkNegatives=FALSE)



#sudo mkfs.xfs -L data -f /dev/vdb # check the volume name in the OpenStack UI (here, /dev/vdc)
#mkdir /home/frousseu/data
#sudo mount /dev/vdb /home/frousseu/data

#sudo chown -R frousseu /home






# scp frousseu@sdm:'/home/frousseu/data/sdms/*' C:/Users/rouf1703/Documents/BiodiversitéQuébec/sdm_explorer/sdms