##### Variables à selectionner #####
Var0 <-"DR"
Var1 <-"PB recalcule"
Var2 <-"Entite legale"
Var3 <-"Code Titre"
Var4 <-"Libelle emission"
Var5 <-"Libelle Categorie Actifs Niveau 1"
Var6 <-"Libelle Categorie Actifs Niveau 2"
Var7 <-"Categorie Actifs Niveau 3"
Var8 <-"Valeur boursiere coupons exclus"
Var9 <-"Valeur nette comptable (hors PDD)"
Var10 <-"Valeur de remboursement"
Var11 <-"Plus ou Moins Values Latentes"
Var12 <-"Provision pour Depreciation Durable (PDD)"
Var13 <-"Duration"
Var14 <-"Date de Maturite"
Var15 <-"Rating emission deduit"
Var16 <-"Code secteur economique niveau 1"
Var17 <-"Valeur boursiere coupons inclus"
Var18 <-"Libelle UPC Tiers"
Var19 <-"Libelle Pays Tiers"
#Var17 <-"Libelle Pays UPC" Pas disponible dans la BDA mensuelle

##### Base de données #####
Data2 <- Data1 %>% select(Var0,Var1,Var2,Var3,Var4,Var5,Var6,Var7,
                          Var8,Var17,Var9,Var10,Var11,Var12,Var13,Var14,Var15,Var16,Var18,Var19)

NameC<- c("DR","Per","EL","CT","LE","LC1","LC2","LC3",
          "VBCexcl","VB","VC","VR","PMVL","PDD","Dur",
          "DM","Rat","CS","UPC","PAYS")

colnames(Data2)<-NameC

rm(Data1) # On supprime l'object Data1 car plus necessaire

##### Selection des dates de reporting ####

DatesRep <- unique(Data2$DR)
