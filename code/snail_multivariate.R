# script for schisto snail multivariate analyses -> structured as Wood et al. 2014
# RMF 5/30/19
library(vegan)

bulinus.raw <- read.csv("Bulinusbehavior_master.csv")
biomph.raw <- read.csv("Biomphalaria_masterdata.csv")

#splitting the datasets to run adonis and simper nmds analyses according to Wood et al 2014 previous data sets from PCA analsyses still apply 
cols.raw<- c(7:10) 
bul.behavs.raw<- bulinus.raw[, cols.raw]
biomph.behavs.raw <- biomph.raw[, cols.raw]
View(bul.behavs.raw)
View(biomph.behavs.raw)

cols.factors<- c(1:6)
bul.factors <- bulinus.raw[, cols.factors]
biomph.factors <- biomph.raw[, cols.factors]
View(bul.factors)
View(biomph.factors)

#adonis analyses 
adonis(bul.behavs.raw~bul.factors$Infection.status,permutations=1000,distance="bray")
adonis(biomph.behavs.raw~biomph.factors$infection.state,permutations=1000,distance="bray")

simper(bul.behavs.raw,bul.factors$Infection.status)
simper(biomph.behavs.raw,biomph.factors$infection.state)

#paper.ver
bul.behavsMDSresults<-metaMDS(bul.behavs.raw,distance="bray",k=3,autotransform=FALSE)
biomph.behavsMDSresults<-metaMDS(biomph.behavs.raw,distance="bray",k=3,autotransform=FALSE)

plot(bul.behavsMDSresults,xlab="",ylab="",type="none",main="bul_treatment")
points(bul.behavsMDSresults,col=c("steelblue4","red")[bul.factors$Infection.status],pch=c(19,21)[bul.factors$Infection.status],xlab="",ylab="",type="p",xaxt='n',yaxt='n')
legend(x=1.5,y=0.5, legend = c("infected", "uninfected"), pch=c(19,21), col=c("steelblue4","red"))
text(-0.95,0.8,labels="stress = 0.013",adj=c(0,0)) #look up stress 
text(-0.95,0.6,labels="k = 3",adj=c(0,0)) #leave k
text(-0.95,0.4,labels="p = 0.324",adj=c(0,0)) #p value from adonis results 

plot(biomph.behavsMDSresults,xlab="",ylab="",type="none",main="biomph_treatment")
points(biomph.behavsMDSresults,col=c("steelblue4","red")[biomph.factors$infection.state],pch=c(19,21)[biomph.factors$infection.state],xlab="",ylab="",type="p",xaxt='n',yaxt='n')
legend(x=1.5,y=0.5, legend = c("infected", "uninfected"), pch=c(19,21), col=c("steelblue4","red"))
text(-1.95,0.1,labels="stress = 0.020",adj=c(0,0)) #look up stress 
text(-1.95,-0.1,labels="k = 3",adj=c(0,0)) #leave k
text(-1.95,-0.3,labels="p = 0.034",adj=c(0,0))
