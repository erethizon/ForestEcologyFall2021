##Ordination and mapping environmental vars
library(vegan)
library(ggplot2)
library(dplyr)
library(ggvegan)

rm(list=ls())

#Importance values: load 3 fileslibrary(readr)
IV <- read.csv("~/FE2017/FinalDataAnalysis/importance_values.csv")
#that is the main IV data for both trees and seedlings combined
SeedIV <- read.csv("~/FE2017/FinalDataAnalysis/seedling_IV.csv")
#importance values for seedlings only
TreeIV <- read.csv("~/FE2017/FinalDataAnalysis/tree_IV.csv")
#importance values for trees only
#IV came in with for rows of excel junk at the bottom; delete
IV<-IV[-c(13,14,15,16),]
#SeedIV came in with a bunch of NAs that should be zeros in Beech column
Wrong<-is.na(SeedIV$Beech)
Wrong
#1:3 and 10:12 are NA
SeedIV$Beech[1:3]<-0
SeedIV$Beech[10:12]<-0

#nonmetric multi-dimensional scaling with main data set, IV
IV.nmds.bray<-metaMDS(IV[,2:22], distance = "bray", trace = FALSE, trymax = 100)
#meta mds is a "wrapper function" - takes raw species abundance data and
#standardizes is to that abundance isn't over important.  Then runs
#monoMDS a bunch of times until it times 2 similar configurations with
#minimized stress value and rotates answer.  trymax is number of tries

IV.nmds.bray #look at the result

plot(IV.nmds.bray, type = "t")

PlotData<-fortify(IV.nmds.bray)

ggplot(PlotData, aes(NMDS1,NMDS2,group = Score))+
   geom_text(aes(label=Label, color = Score))+
   scale_color_manual(values = c(sites = "brown", species = "green"))+
   geom_point(aes(color=Score, shape =Score, size =.1))+
   scale_color_manual(values = c(sites = "red", species = "black"))

PlotData$Label
Vars<-c("Degrasse 1", "Degrasse 2", "Degrasse 3",
        "Donnerville 1", "Donnerville 2", "Donnerville 3",
        "Peavine 1", "Peavine 2", "Peavine 3",
        "S. Hammond 1", "S. Hammond 2", "S. Hammond 3")
PlotData$Label<-as.character(PlotData$Label)
Vars<-as.factor(Vars)
i<-1
for (i in 1:12){
   PlotData$Label[i]<-Vars[i]
   i<- i +1
}
PlotData$Label<-as.factor(PlotData$Label)

#ok don't worry about the plot just keep going with the ordination
gof<-goodness(IV.nmds.bray) #goodness of fit
plot(IV.nmds.bray, type = "t", main = "goodness of fit")
points(IV.nmds.bray, display = "sites", cex=gof*100)

#now add environmental data
#read data
ENV <- read.csv("~/FE2017/FinalDataAnalysis/env_vars.csv")

fit<-envfit(IV.nmds.bray, ENV[,4:27], permu= 999)
fit

#now plot with the fit
plot(IV.nmds.bray, type = 't',display = "sites")
plot(fit, p.max = 0.06)

#Now seedling IV
SeedIV.nmds.bray<-metaMDS(SeedIV[,2:22], distance = "bray", trace = FALSE, trymax = 100)
SeedIV.nmds.bray #look at the result
plot(SeedIV.nmds.bray, type = "t")
gof<-goodness(SeedIV.nmds.bray) #goodness of fit
plot(SeedIV.nmds.bray, type = "t", main = "goodness of fit")
points(SeedIV.nmds.bray, display = "sites", cex=gof*100)

fit<-envfit(SeedIV.nmds.bray, ENV[,4:27], permu= 999)
fit
plot(SeedIV.nmds.bray, type = 't',display = "sites")
plot(fit, p.max = 0.06)

#now treeIV
TreeIV.nmds.bray<-metaMDS(TreeIV[,2:22], distance = "bray", trace = FALSE, trymax = 100)
TreeIV.nmds.bray #look at the result
plot(TreeIV.nmds.bray, type = "t")
gof<-goodness(TreeIV.nmds.bray) #goodness of fit
plot(TreeIV.nmds.bray, type = "t", main = "goodness of fit")
points(TreeIV.nmds.bray, display = "sites", cex=gof*100)

fit<-envfit(TreeIV.nmds.bray, ENV[,4:27], permu= 999)
fit
plot(TreeIV.nmds.bray, type = 't',display = "sites")
plot(fit, p.max = 0.06)

#now try it with species data rather than IV data
SeedSP <- read.csv("~/FE2017/FinalDataAnalysis/seedling_sp.csv")
TreeSP <- read.csv("~/FE2017/FinalDataAnalysis/tree_sp.csv")
TSSP <- read.csv("~/FE2017/FinalDataAnalysis/TS_speciesrichness.csv")

#all species
TSSP.nmds.bray<-metaMDS(TSSP[,2:22], distance = "bray", trace = FALSE, trymax = 100)
TSSP.nmds.bray #look at the result
plot(TSSP.nmds.bray, type = "t")
gof<-goodness(TSSP.nmds.bray) #goodness of fit
plot(TSSP.nmds.bray, type = "t", main = "goodness of fit")
points(TSSP.nmds.bray, display = "sites", cex=gof*100)

fit<-envfit(TSSP.nmds.bray, ENV[,4:27], permu= 999)
fit
plot(TSSP.nmds.bray, type = 't',display = "sites")
plot(fit, p.max = 0.06)

#Seedling Species
SeedSP.nmds.bray<-metaMDS(SeedSP[,2:22], distance = "bray", trace = FALSE, trymax = 200)
SeedSP.nmds.bray #look at the result
plot(SeedSP.nmds.bray, type = "t")
gof<-goodness(SeedSP.nmds.bray) #goodness of fit
plot(SeedSP.nmds.bray, type = "t", main = "goodness of fit")
points(SeedSP.nmds.bray, display = "sites", cex=gof*100)

fit<-envfit(SeedSP.nmds.bray, ENV[,4:27], permu= 999)
fit
plot(SeedSP.nmds.bray, type = 't',display = "sites")
plot(fit, p.max = 0.06)

#And now trees
TreeSP.nmds.bray<-metaMDS(TreeSP[,2:22], distance = "bray", trace = FALSE, trymax = 200)
TreeSP.nmds.bray #look at the result
plot(TreeSP.nmds.bray, type = "t")
gof<-goodness(TreeSP.nmds.bray) #goodness of fit
plot(TreeSP.nmds.bray, type = "t", main = "goodness of fit")
points(TreeSP.nmds.bray, display = "sites", cex=gof*100)

fit<-envfit(TreeSP.nmds.bray, ENV[,4:27], permu= 999)
fit
plot(TreeSP.nmds.bray, type = 't',display = "sites")
plot(fit, p.max = 0.06)
TreeSPout<-fortify(TreeSP.nmds.bray)
SeedSPout<-fortify(SeedSP.nmds.bray)
TSSPout<-fortify(TSSP.nmds.bray)

#try getting TSSPout into 2 DFs, one with sites, other with species
TSSPsites<-filter(TSSPout, Score =="sites")
TSSPsites$Forest<-c("Degrasse", "Degrasse", "Degrasse", "Donnerville", 
                    "Donnerville","Donnerville", "Peavine", "Peavine", "Peavine",
                    "S. Hammond", "S. Hammond", "S. Hammond")
TSSPspecies<-filter(TSSPout, Score =="species")
Forests<-c("#4c4cdb", "#800000", "#2f4f4f","#00cd00")
ggplot(TSSPspecies, aes(NMDS1, NMDS2))+
   geom_text(aes(label = Label), check_overlap = T)+
   geom_point(data = TSSPsites, aes(x = NMDS1, y = NMDS2, group = Forest, color = Forest))+
   geom_text(data = TSSPsites, 
             aes(label = Forest, group=Forest, color = Forest),
             check_overlap = T)+
   scale_x_continuous(limits=c(-3, 3), breaks = -3:3)
   
   
   