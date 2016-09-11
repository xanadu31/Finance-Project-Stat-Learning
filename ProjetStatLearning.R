# Importation des library
library(tseries)
library(FactoMineR)
# Inputs, période sur laquelle on va étudier les données
t0 = "2005-03-14"
t1 = "2009-03-04"
#liste 10 valeurs
cours=c("HSBA.L", "VOD.L", "BATS.L", "DGE.L", "IMT.L","BP.L","GSK.L","AZN.L","BG.L","TSCO.L")
# Initialisation des matrices
SERIESO<-matrix(nrow=1038,ncol=length(cours))
SERIESC<-matrix(nrow=1038,ncol=length(cours))
SERIESmean<-matrix(nrow=1038,ncol=length(cours))

# Importation et construction des données importees YAHOO FINANCE
for (i in 1:length(cours)) {
  SERIESO[,i]=get.hist.quote(instrument=cours[i],start=t0,end =t1,compression="d",quote="Open")
  SERIESC[,i]=get.hist.quote(instrument=cours[i],start=t0,end =t1,compression="d",quote="Close")
}
colnames(SERIESO)<-c("HSBA.L", "VOD.L", "BATS.L", "DGE.L", "IMT.L","BP.L","GSK.L","AZN.L","BG.L","TSCO.L")
colnames(SERIESC)<-c("HSBA.L", "VOD.L", "BATS.L", "DGE.L", "IMT.L","BP.L","GSK.L","AZN.L","BG.L","TSCO.L")
# Moyenne des 2 matrices: valeur haute et valeur basse
SERIESmean=(SERIESO+SERIESC)/2
#lectures des cours de bourses
summary(SERIESmean)

par(mfrow = c(2,5))
for (i in 1:10)
{
  plot(SERIESmean[1:1038,i],type="l",col="red",main=colnames(SERIESmean)[i],ylab="valeur cours")
  
}
# Construction des log rendements arithmetique Rt-Rt-1/Rt-1
rSERIES=(diff(log(SERIESmean)))
par(mfrow = c(2,5))
for (i in 1:10)
{
  plot(rSERIES[1:1037,i],type="l",col="blue",,main=colnames(SERIESmean)[i],ylab="rendements cours")
}

summary(rSERIES)
## Analyse en composantes principales des rendements quotidiens
par(mfrow = c(1,1))
D.PCA<-PCA(rSERIES)
summary(D.PCA)

D.PCA #donne fonction utile pour ACP (ex :D.PCA$eig,D.PCA$var)
D.PCA$var$coord


##  Reconstitution d'un cours de bourse théorique correspondant à la première composante principale
##  On reconstitue des rendements

K<-D.PCA$var$coord[,1]

##  La contribution de chaque cours à la première composante principale
K   
## La première composante principale
MDelta.P<-rSERIES %*% K
par(mfrow = c(1,2))
plot(MDelta.P,type="l",col="red")
Reconst<-(cumsum(MDelta.P))
plot(Reconst,type="l",col="blue")


##  Comparaison avec le FTSE
CoursFTSEOpen<-get.hist.quote(instrument=cours[i],start=t0,end =t1,compression="d",quote="Open")
CoursFTSEClose<-get.hist.quote(instrument=cours[i],start=t0,end =t1,compression="d",quote="Close")
CoursFTSEMean<-(CoursFTSEOpen+CoursFTSEClose)/2
rSERIESFTSE=(diff(log(CoursFTSEMean)))
par(mfrow = c(2,2))
plot(CoursFTSEMean,type="l",col="red")
plot(Reconst,type="l",col="blue")
plot(rSERIESFTSE,type="l",col="red")
plot(MDelta.P,type="l",col="blue")



