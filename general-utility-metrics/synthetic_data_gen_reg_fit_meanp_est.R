## Author Niklas Reje (nreje@kth.se)
source("library_synthesizers_reg_fit_meanp_est.R")

## Polish ods

SD2011M <- read.csv("SD2011M2.csv")

vars <- c("sex", "age", "edu", "marital", "income",
          "ls", "trust", "nofriend", "smoke", "alcabuse", 
          "wkabint", "englang", "height", "weight")
ods <- SD2011M[, vars]

for(v in vars){
  if(is.factor(ods[,v])){
    ods[,v] <- as.character(ods[,v])
    ods[,v] <- factor(ods[,v],levels(SD2011[,v]))
  }
}

ods$wkabint <- as.character(ods$wkabint)
ods$wkabint[ods$wkabint == "YES, TO EU COUNTRY" | 
              ods$wkabint == "YES, TO NON-EU COUNTRY"] <- "YES"
ods$wkabint <- factor(ods$wkabint,c("YES","NO"))

ods$marital <- as.character(ods$marital)
ods$marital[ods$marital == "LEGALLY SEPARATED" | 
              ods$marital == "DE FACTO SEPARATED"] <- "DIVORCED"
ods$marital <- factor(ods$marital,c("SINGLE","MARRIED","WIDOWED","DIVORCED"))

varsTest <- c("sex", "age", "height", "weight", "edu", "englang",
              "marital", "wkabint", "income", "ls", "trust",
              "nofriend", "smoke", "alcabuse")

polishRelations <- matrix(0,nrow = 14, ncol = 14, dimnames = list(colnames(ods),colnames(ods)))
polishRelations[c(3,5,9:14),1] <- 1
polishRelations[c(3:12),2] <- 1
polishRelations[c(4,5,12),3] <- 1
polishRelations[c(5,6,8:12),4] <- 1
polishRelations[c(6,8:13),5] <- 1
polishRelations[c(8:11,13,14),6] <- 1
polishRelations[c(8,12),7] <- 1
polishRelations[c(11),8] <- 1
polishRelations[c(10),9] <- 1
#polishRelations[c(),10] <- 1
polishRelations[c(12),11] <- 1
#polishRelations[c(),12] <- 1
polishRelations[c(14),13] <- 1
#polishRelations[c(),14] <- 1
polishRelations <- polishRelations + t(polishRelations)


## Adult ods

adultODS <- read.obs("adultdata.csv")

adultODSM <- adultODS

for(i in c(1:ncol(adultODS))){
  remAD <- is.na(adultODSM[,i])
  adultODSM <- adultODSM[!remAD,]
}

adultODSM <- adultODSM[,-c(3,5)]
eduLev <- levels(adultODSM$education)[c(14,4,5,6,7,1,2,3,12,15,16,9,8,10,13,11)]
adultODSM$education <- factor(adultODSM$education,eduLev)
adultODSM$relationship <- factor(adultODSM$relationship,
                                 c("Not-in-family","Other-relative","Own-child","Unmarried","Wife","Husband"))

Europe <- c("England","France","Germany","Greece","Holand-Netherlands",
            "Hungary","Ireland","Italy","Poland","Portugal","Scotland",
            "Yugoslavia")
Asia <- c("Cambodia","China","Hong","India","Iran","Japan","Laos",
          "Philippines","South","Taiwan","Thailand","Vietnam")
OtherUS <- c("Outlying-US(Guam-USVI-etc)","Puerto-Rico")
SouthAmerica <- c("Columbia","Cuba","Dominican-Republic","Ecuador",
                  "El-Salvador","Guatemala","Haiti","Honduras",
                  "Jamaica","Nicaragua","Peru","Trinadad&Tobago")

adultODSM2 <- adultODSM
adultODSM2$native.country <- as.character(adultODSM2$native.country)
for(cs in Europe){
  adultODSM2$native.country[adultODSM2$native.country == cs] <- "Europe"
}
for(cs in Asia){
  adultODSM2$native.country[adultODSM2$native.country == cs] <- "Asia"
}
for(cs in OtherUS){
  adultODSM2$native.country[adultODSM2$native.country == cs] <- "OtherUS"
}
for(cs in SouthAmerica){
  adultODSM2$native.country[adultODSM2$native.country == cs] <- "South-America"
}

adultODSM2$native.country <- factor(adultODSM2$native.country,
                                    c("United-States","OtherUS","Canada","Mexico",
                                      "South-America","Europe","Asia"))

adultODS10K <- adultODSM2[1:10000,]

LH2Order <- c(3,5,2,4,12,6,7,8,13,1,9,10,11)

adultRelations <- matrix(0,nrow = 13, ncol = 13, dimnames = list(colnames(adultODSM2),colnames(adultODSM2)))
adultRelations[c(2:6,11,13),1] <- 1
adultRelations[c(3:5,7:13),2] <- 1
adultRelations[c(4,5,7:13),3] <- 1
adultRelations[c(6,7,9:13),4] <- 1
adultRelations[c(7:13),5] <- 1
adultRelations[c(8,11:13),6] <- 1
adultRelations[c(9:13),7] <- 1
adultRelations[c(11:13),8] <- 1
adultRelations[c(12,13),9] <- 1
adultRelations[c(12,13),10] <- 1
adultRelations[c(12,13),11] <- 1
adultRelations[c(13),12] <- 1
#adultRelations[c(),13] <- 1
adultRelations <- adultRelations + t(adultRelations)

## Avila ods

avilaODS <- read.csv("avila-tr.txt",header = FALSE)
colnames(avilaODS) <- c("Int_Dist","Up_Marg","Lo_Marg","Exploit","Row_Num","Mod_Ratio","Int_Spac","Weight","Peak_Num","MR/IS","Class")

CM <- c(1,2,3,4,5,11,6,7,8,9,10)

avilaRelations <- matrix(0,nrow = 11, ncol = 11, dimnames = list(colnames(avilaODS),colnames(avilaODS)))
corAv <- cor(avilaODS[,1:10],method = "spearman") > 0.05
diag(corAv) <- FALSE
corAv <- rbind(cbind(corAv,TRUE),TRUE)
corAv[[11,11]] <- FALSE
avilaRelations[corAv] <- 1

##TESTING AREA

## Polish

sdsPolishParM <- TestPar(ods,m = 1000)
sdsPolishDecM <- TestDec(ods,m = 1000)
sdsPolishParMT <- TestPar(ods,m = 1000,proper = TRUE)
sdsPolishDecMT <- TestDecPF(ods,m = 1000)
save(list=paste0("sds","Polish",c("Par","Par","Dec","Dec"),"M",c("","T")),file="sdsPolishM.RData")
rm(list=paste0("sds","Polish",c("Par","Par","Dec","Dec"),"M",c("","T")))

sdsPolishParMO <- TestPar(ods,c(14:1),m = 1000)
sdsPolishDecMO <- TestDec(ods,c(14:1),m = 1000)
sdsPolishParMOT <- TestPar(ods,c(14:1),m = 1000,proper = TRUE)
sdsPolishDecMOT <- TestDecPF(ods,c(14:1),m = 1000)
save(list=paste0("sds","Polish",c("Par","Par","Dec","Dec"),"MO",c("","T")),file="sdsPolishMO.RData")
rm(list=paste0("sds","Polish",c("Par","Par","Dec","Dec"),"MO",c("","T")))

sdsPolishParMV <- TestPar(ods,varsTest,m = 1000)
sdsPolishDecMV <- TestDec(ods,varsTest,m = 1000)
sdsPolishParMVT <- TestPar(ods,varsTest,m = 1000,proper = TRUE)
sdsPolishDecMVT <- TestDecPF(ods,varsTest,m = 1000)
save(list=paste0("sds","Polish",c("Par","Par","Dec","Dec"),"MV",c("","T")),file="sdsPolishMV.RData")
rm(list=paste0("sds","Polish",c("Par","Par","Dec","Dec"),"MV",c("","T")))

methods <- rep("catall",14)
methods[c(2,5,8,13,14)] <- "pmm"

#sdsPolishCatMP <- syn(ods,method = methods, visit.sequence = c(c(1,3,4,6,7,9:12),c(2,5,8,13,14)),m = 1000)
sdsPolishCatMP <- TestCAP(ods,methods,c(c(1,3,4,6,7,9:12),c(2,5,8,13,14)),m = 1000)
sdsPolishCatMPT <- TestCAP(ods,methods,c(c(1,3,4,6,7,9:12),c(2,5,8,13,14)),m = 1000,proper = TRUE)

methods <- rep("catall",14)
methods[c(2,5,8,13,14)] <- "cart"

sdsPolishCatMC <- TestCAC(ods,methods,c(c(1,3,4,6,7,9:12),c(2,5,8,13,14)),m = 1000)
sdsPolishCatMCT <- TestCAC(ods,methods,c(c(1,3,4,6,7,9:12),c(2,5,8,13,14)),m = 1000,proper = TRUE)

save(list=paste0("sds","PolishCatM",c("P","P","C","C"),"",c("","T")),file="sdsPolishMS.RData")
rm(list=paste0("sds","PolishCatM",c("P","P","C","C"),"",c("","T")))

sdsPolishSamp <- syn(ods,method = rep("sample",14),m = 1000)
save(list = "sdsPolishSamp",file="sdsPolishSamp.RData")


## TESTING

test <- c(sex ~ height + weight,sex ~ height,
          sex ~ age + edu + marital + income + ls + trust +
            nofriend + smoke + alcabuse + wkabint + englang + height + weight,
          wkabint ~ sex + age + edu + log(income + 1),
          wkabint ~ sex + edu + log(income + 1),
          wkabint ~ sex + edu + log(income + 1) + ls + englang,
          smoke ~ sex + age + edu + sex:edu,
          smoke ~ alcabuse + marital + log(income+1),
          alcabuse ~ age + sex + ls + edu,
          alcabuse ~ smoke + marital + log(income+1),
          alcabuse ~ smoke + income,
          marital ~ age + sex + height + weight + edu,
          log(income+1) ~ age + sex + edu,
          sqrt(income) ~ age + sex + edu,
          income ~ age + sex + edu,
          sqrt(nofriend) ~ age + sex + ls + trust,
          nofriend ~ sex + age + edu + marital + income + ls + trust +
            smoke + alcabuse + wkabint + englang + height + weight,
          height ~ weight + sex + age,
          weight ~ sex + age + edu + smoke + alcabuse + height,
          age ~ alcabuse + smoke + wkabint + englang + ls)

test2 <- c("binomial","binomial","binomial","binomial",
           "binomial","binomial","binomial","binomial",
           "binomial","binomial","binomial","binomial",
           "gaussian","gaussian","gaussian","gaussian",
           "gaussian","gaussian","gaussian","gaussian")


load("sdsPolishM.RData")

sds <- paste0("sds","Polish",c("Par","Par","Dec","Dec"),"M",c("","T"))
glm <- paste0("glm","Polish",c("Par","Par","Dec","Dec"),"M",c("","T"))
est <- paste0("est","Polish",c("Par","Par","Dec","Dec"),"M",c("","T"))
kld <- paste0("kld","Polish",c("Par","Par","Dec","Dec"),"M",c("","T"))

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <- TestGLM(act,ods,c(1,2,3,5,10,20,50,100),1000,test,test2)
  actE <- TestEst(act,ods,mean,var,c(2,5,8,13,14),c(1,2,3,5,10,20,50,100),1000)
  actK <- TestKLD(act,ods)
  assign(as.character(glm[[i]]),actG)
  assign(as.character(est[[i]]),actE)
  assign(as.character(kld[[i]]),actK)
}

rm(list = sds)

save(list = glm,file = "glmPolishM.RData")
save(list = est,file = "estPolishM.RData")
save(list = kld,file = "kldPolishM.RData")
rm(list = glm)
rm(list = est)
rm(list = kld)


load("sdsPolishMO.RData")

sds <- paste0("sds","Polish",c("Par","Par","Dec","Dec"),"MO",c("","T"))
glm <- paste0("glm","Polish",c("Par","Par","Dec","Dec"),"MO",c("","T"))
est <- paste0("est","Polish",c("Par","Par","Dec","Dec"),"MO",c("","T"))
kld <- paste0("kld","Polish",c("Par","Par","Dec","Dec"),"MO",c("","T"))

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <- TestGLM(act,ods,c(1,2,3,5,10,20,50,100),1000,test,test2)
  actE <- TestEst(act,ods,mean,var,c(2,5,8,13,14),c(1,2,3,5,10,20,50,100),1000)
  actK <- TestKLD(act,ods)
  assign(as.character(glm[[i]]),actG)
  assign(as.character(est[[i]]),actE)
  assign(as.character(kld[[i]]),actK)
}

rm(list = sds)

save(list = glm,file = "glmPolishMO.RData")
save(list = est,file = "estPolishMO.RData")
save(list = kld,file = "kldPolishMO.RData")
rm(list = glm)
rm(list = est)
rm(list = kld)


load("sdsPolishMV.RData")

sds <- paste0("sds","Polish",c("Par","Par","Dec","Dec"),"MV",c("","T"))
glm <- paste0("glm","Polish",c("Par","Par","Dec","Dec"),"MV",c("","T"))
est <- paste0("est","Polish",c("Par","Par","Dec","Dec"),"MV",c("","T"))
kld <- paste0("kld","Polish",c("Par","Par","Dec","Dec"),"MV",c("","T"))

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <- TestGLM(act,ods,c(1,2,3,5,10,20,50,100),1000,test,test2)
  actE <- TestEst(act,ods,mean,var,c(2,5,8,13,14),c(1,2,3,5,10,20,50,100),1000)
  actK <- TestKLD(act,ods)
  assign(as.character(glm[[i]]),actG)
  assign(as.character(est[[i]]),actE)
  assign(as.character(kld[[i]]),actK)
}

rm(list = sds)

save(list = glm,file = "glmPolishMV.RData")
save(list = est,file = "estPolishMV.RData")
save(list = kld,file = "kldPolishMV.RData")
rm(list = glm)
rm(list = est)
rm(list = kld)


load("sdsPolishMS.RData")

sds <- paste0("sds","PolishCatM",c("P","P","C","C"),"",c("","T"))
glm <- paste0("glm","PolishCatM",c("P","P","C","C"),"",c("","T"))
est <- paste0("est","PolishCatM",c("P","P","C","C"),"",c("","T"))
kld <- paste0("kld","PolishCatM",c("P","P","C","C"),"",c("","T"))

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <- TestGLM(act,ods,c(1,2,3,5,10,20,50,100),1000,test,test2)
  actE <- TestEst(act,ods,mean,var,c(2,5,8,13,14),c(1,2,3,5,10,20,50,100),1000)
  actK <- TestKLD(act,ods)
  assign(as.character(glm[[i]]),actG)
  assign(as.character(est[[i]]),actE)
  assign(as.character(kld[[i]]),actK)
}

rm(list = sds)

save(list = glm,file = "glmPolishMS.RData")
save(list = est,file = "estPolishMS.RData")
save(list = kld,file = "kldPolishMS.RData")
rm(list = glm)
rm(list = est)
rm(list = kld)


load("sdsPolishSamp.RData")

sds <- paste0("sds","PolishSamp",c(""),"",c(""))
glm <- paste0("glm","PolishSamp",c(""),"",c(""))
est <- paste0("est","PolishSamp",c(""),"",c(""))
kld <- paste0("kld","PolishSamp",c(""),"",c(""))

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <- TestGLM(act,ods,c(1,2,3,5,10,20,50,100),1000,test,test2)
  actE <- TestEst(act,ods,mean,var,c(2,5,8,13,14),c(1,2,3,5,10,20,50,100),1000)
  actK <- TestKLD(act,ods)
  assign(as.character(glm[[i]]),actG)
  assign(as.character(est[[i]]),actE)
  assign(as.character(kld[[i]]),actK)
}

rm(list = sds)

save(list = c(glm,est,kld),file = "testAvilaSamp.RData")
rm(list = c(glm,est,kld))




## Adult

sdsAdultParM <- TestPar(adultODS10K,m = 100)
sdsAdultDecM <- TestDec(adultODS10K,m = 1000)
sdsAdultParMT <- TestPar(adultODS10K,m = 100,proper = TRUE)
sdsAdultDecMT <- TestDecPF(adultODS10K,m = 1000)
save(list=paste0("sds","Adult",c("Par","Par","Dec","Dec"),"M",c("","T")),file="sdsAdultM.RData")
rm(list=paste0("sds","Adult",c("Par","Par","Dec","Dec"),"M",c("","T")))

sdsAdultParMH <- TestPar(adultODS10K,LH2Order,m = 100)
sdsAdultDecMH <- TestDecPF(adultODS10K,LH2Order,m = 1000,proper = FALSE)
sdsAdultParMHT <- TestPar(adultODS10K,LH2Order,m = 100,proper = TRUE)
sdsAdultDecMHT <- TestDecPF(adultODS10K,LH2Order,m = 1000)
save(list=paste0("sds","Adult",c("Par","Par","Dec","Dec"),"MH",c("","T")),file="sdsAdultMH.RData")
rm(list=paste0("sds","Adult",c("Par","Par","Dec","Dec"),"MH",c("","T")))

sdsAdultParML <- TestPar(adultODS10K,rev(LH2Order),m = 100)
sdsAdultDecML <- TestDec(adultODS10K,rev(LH2Order),m = 1000)
sdsAdultParMLT <- TestPar(adultODS10K,rev(LH2Order),m = 100,proper = TRUE)
sdsAdultDecMLT <- TestDecPF(adultODS10K,rev(LH2Order),m = 1000)
save(list=paste0("sds","Adult",c("Par","Par","Dec","Dec"),"ML",c("","T")),file="sdsAdultML.RData")
rm(list=paste0("sds","Adult",c("Par","Par","Dec","Dec"),"ML",c("","T")))


methods <- rep("catall",13)
methods[c(1,9:11)] <- "pmm"

sdsAdultCatMP <- TestCAP(adultODS10K,methods,c(c(2:8,12,13),c(1,9:11)),m = 1000)
sdsAdultCatMPT <- TestCAP(adultODS10K,methods,c(c(2:8,12,13),c(1,9:11)),m = 1000,proper = TRUE)

methods <- rep("catall",13)
methods[c(1,9:11)] <- "cart"

sdsAdultCatMC <- TestCAC(adultODS10K,methods,c(c(2:8,12,13),c(1,9:11)),m = 1000)
sdsAdultCatMCT <- TestCAC(adultODS10K,methods,c(c(2:8,12,13),c(1,9:11)),m = 1000,proper = TRUE)

save(list=paste0("sds","AdultCatM",c("P","P","C","C"),"",c("","T")),file="sdsAdultMS.RData")
rm(list=paste0("sds","AdultCatM",c("P","P","C","C"),"",c("","T")))

sdsAdultSamp <- syn(adultODS10K,method = rep("sample",13),m = 1000)
save(list="sdsAdultSamp",file="sdsAdultSamp.RData")


## TESTING

test <- c(income ~ native.country,
          income ~ occupation + hours.per.week,
          income ~ sex + age + workclass,
          income ~ education + marital.status + race,
          income ~ sex + race + relationship + age,
          income ~ age + workclass + education + marital.status +
            occupation + relationship + race + sex + capital.gain +
            capital.loss + hours.per.week + native.country,
          income ~ capital.gain + capital.loss + occupation,
          income ~ race*sex,
          sex ~ workclass + education + hours.per.week,
          sex ~ income,
          income ~ sex,
          relevel(race,"White") ~ workclass + marital.status + income,
          relevel(native.country,"United-States") ~ age + sex +
            education + race,
          cut(capital.gain,c(-1,0,10**9),c("ZERO","NON-ZERO")) ~
            workclass + education + occupation + income,
          cut(capital.loss,c(-1,0,10**9),c("ZERO","NON-ZERO")) ~
            workclass + education + occupation + income,
          cut(capital.gain,c(-1,0,10**9),c("ZERO","NON-ZERO")) ~ 
            age + sex + race + hours.per.week +
            workclass + education + occupation + income,
          cut(capital.loss,c(-1,0,10**9),c("ZERO","NON-ZERO")) ~
            age + sex + race + hours.per.week +
            workclass + education + occupation + income,
          capital.gain ~ workclass + education + occupation + income,
          capital.loss ~ workclass + education + occupation + income,
          capital.gain ~ age + sex + race + hours.per.week +
            workclass + education + occupation + income,
          capital.loss ~ age + sex + race + hours.per.week +
            workclass + education + occupation + income,
          hours.per.week ~ native.country + race + occupation,
          hours.per.week ~ native.country + race + occupation +
            age + workclass + sex + education + income,
          age ~ hours.per.week + education + marital.status)

test2 <- c("binomial","binomial","binomial","binomial",
           "binomial","binomial","binomial","binomial",
           "binomial","binomial","binomial","binomial",
           "binomial","binomial","binomial","binomial",
           "binomial","gaussian","gaussian","gaussian",
           "gaussian","gaussian","gaussian","gaussian")


load("sdsAdultM.RData")

sds <- paste0("sds","Adult",c("Par","Par","Dec","Dec"),"M",c("","T"))
glm <- paste0("glm","Adult",c("Par","Par","Dec","Dec"),"M",c("","T"))
est <- paste0("est","Adult",c("Par","Par","Dec","Dec"),"M",c("","T"))
kld <- paste0("kld","Adult",c("Par","Par","Dec","Dec"),"M",c("","T"))

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  cc <- c(1,2,3,5,10,20,50)
  m <- act$m
  if(m == 1000){
    cc <- c(cc,100)
  }
  actG <- TestGLM(act,adultODS10K,cc,m,test,test2)
  actE <- TestEst(act,adultODS10K,mean,var,c(1,9,10,11),cc,m)
  actK <- TestKLD(act,adultODS10K)
  assign(as.character(glm[[i]]),actG)
  assign(as.character(est[[i]]),actE)
  assign(as.character(kld[[i]]),actK)
}

rm(list = sds)

save(list = glm,file = "glmAdultM.RData")
save(list = est,file = "estAdultM.RData")
save(list = kld,file = "kldAdultM.RData")
rm(list = glm)
rm(list = est)
rm(list = kld)


load("sdsAdultMH.RData")

sds <- paste0("sds","Adult",c("Par","Par","Dec","Dec"),"MH",c("","T"))
glm <- paste0("glm","Adult",c("Par","Par","Dec","Dec"),"MH",c("","T"))
est <- paste0("est","Adult",c("Par","Par","Dec","Dec"),"MH",c("","T"))
kld <- paste0("kld","Adult",c("Par","Par","Dec","Dec"),"MH",c("","T"))

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  cc <- c(1,2,3,5,10,20,50)
  m <- act$m
  if(m == 1000){
    cc <- c(cc,100)
  }
  actG <- TestGLM(act,adultODS10K,cc,m,test,test2)
  actE <- TestEst(act,adultODS10K,mean,var,c(1,9,10,11),cc,m)
  actK <- TestKLD(act,adultODS10K)
  assign(as.character(glm[[i]]),actG)
  assign(as.character(est[[i]]),actE)
  assign(as.character(kld[[i]]),actK)
}

rm(list = sds)

save(list = glm,file = "glmAdultMH.RData")
save(list = est,file = "estAdultMH.RData")
save(list = kld,file = "kldAdultMH.RData")
rm(list = glm)
rm(list = est)
rm(list = kld)


load("sdsAdultML.RData")

sds <- paste0("sds","Adult",c("Par","Par","Dec","Dec"),"ML",c("","T"))
glm <- paste0("glm","Adult",c("Par","Par","Dec","Dec"),"ML",c("","T"))
est <- paste0("est","Adult",c("Par","Par","Dec","Dec"),"ML",c("","T"))
kld <- paste0("kld","Adult",c("Par","Par","Dec","Dec"),"ML",c("","T"))

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  cc <- c(1,2,3,5,10,20,50)
  m <- act$m
  if(m == 1000){
    cc <- c(cc,100)
  }
  actG <- TestGLM(act,adultODS10K,cc,m,test,test2)
  actE <- TestEst(act,adultODS10K,mean,var,c(1,9,10,11),cc,m)
  actK <- TestKLD(act,adultODS10K)
  assign(as.character(glm[[i]]),actG)
  assign(as.character(est[[i]]),actE)
  assign(as.character(kld[[i]]),actK)
}

rm(list = sds)

save(list = glm,file = "glmAdultML.RData")
save(list = est,file = "estAdultML.RData")
save(list = kld,file = "kldAdultML.RData")
rm(list = glm)
rm(list = est)
rm(list = kld)


load("sdsAdultMS.RData")

sds <- paste0("sds","AdultCatM",c("P","P","C","C"),"",c("","T"))
glm <- paste0("glm","AdultCatM",c("P","P","C","C"),"",c("","T"))
est <- paste0("est","AdultCatM",c("P","P","C","C"),"",c("","T"))
kld <- paste0("kld","AdultCatM",c("P","P","C","C"),"",c("","T"))

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  cc <- c(1,2,3,5,10,20,50)
  m <- act$m
  if(m == 1000){
    cc <- c(cc,100)
  }
  actG <- TestGLM(act,adultODS10K,cc,m,test,test2)
  actE <- TestEst(act,adultODS10K,mean,var,c(1,9,10,11),cc,m)
  actK <- TestKLD(act,adultODS10K)
  assign(as.character(glm[[i]]),actG)
  assign(as.character(est[[i]]),actE)
  assign(as.character(kld[[i]]),actK)
}

rm(list = sds)

save(list = glm,file = "glmAdultMS.RData")
save(list = est,file = "estAdultMS.RData")
save(list = kld,file = "kldAdultMS.RData")
rm(list = glm)
rm(list = est)
rm(list = kld)



load("sdsAdultSamp.RData")

sds <- paste0("sds","AdultSamp",c(""),"",c(""))
glm <- paste0("glm","AdultSamp",c(""),"",c(""))
est <- paste0("est","AdultSamp",c(""),"",c(""))
kld <- paste0("kld","AdultSamp",c(""),"",c(""))

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  cc <- c(1,2,3,5,10,20,50)
  m <- act$m
  if(m == 1000){
    cc <- c(cc,100)
  }
  actG <- TestGLM(act,adultODS10K,cc,m,test,test2)
  actE <- TestEst(act,adultODS10K,mean,var,c(1,9,10,11),cc,m)
  actK <- TestKLD(act,adultODS10K)
  assign(as.character(glm[[i]]),actG)
  assign(as.character(est[[i]]),actE)
  assign(as.character(kld[[i]]),actK)
}

rm(list = sds)

save(list = c(glm,est,kld),file = "testAdultSamp.RData")
rm(list = c(glm,est,kld))


## Avila

sdsAvilaParM <- TestPar(avilaODS,m = 1000)
sdsAvilaDecM <- TestDec(avilaODS,m = 1000)
sdsAvilaParMT <- TestPar(avilaODS,m = 1000,proper = TRUE)
sdsAvilaDecMT <- TestDec(avilaODS,m = 1000,proper = TRUE)
save(list=paste0("sds","Avila",c("Par","Par","Dec","Dec"),"M",c("","T")),file="sdsAvilaM.RData")
rm(list=paste0("sds","Avila",c("Par","Par","Dec","Dec"),"M",c("","T")))

sdsAvilaParMO <- TestPar(avilaODS,c(11:1),m = 1000)
sdsAvilaDecMO <- TestDec(avilaODS,c(11:1),m = 1000)
sdsAvilaParMOT <- TestPar(avilaODS,c(11:1),m = 1000,proper = TRUE)
sdsAvilaDecMOT <- TestDec(avilaODS,c(11:1),m = 1000,proper = TRUE)
save(list=paste0("sds","Avila",c("Par","Par","Dec","Dec"),"MO",c("","T")),file="sdsAvilaMO.RData")
rm(list=paste0("sds","Avila",c("Par","Par","Dec","Dec"),"MO",c("","T")))

sdsAvilaParMV <- TestPar(avilaODS,CM,m = 1000)
sdsAvilaDecMV <- TestDec(avilaODS,CM,m = 1000)
sdsAvilaParMVT <- TestPar(avilaODS,CM,m = 1000,proper = TRUE)
sdsAvilaDecMVT <- TestDec(avilaODS,CM,m = 1000,proper = TRUE)
save(list=paste0("sds","Avila",c("Par","Par","Dec","Dec"),"MV",c("","T")),file="sdsAvilaMV.RData")
rm(list=paste0("sds","Avila",c("Par","Par","Dec","Dec"),"MV",c("","T")))


methods <- rep("pmm",11)
methods[11] <- "catall"


sdsAvilaCatMP <- TestCAP(avilaODS,methods,c(11,1:10),m = 1000)
sdsAvilaCatMPT <- TestCAP(avilaODS,methods,c(11,1:10),m = 1000,proper = TRUE)

methods <- rep("cart",11)
methods[11] <- "catall"


sdsAvilaCatMC <- TestCAC(avilaODS,methods,c(11,1:10),m = 1000)
sdsAvilaCatMCT <- TestCAC(avilaODS,methods,c(11,1:10),m = 1000,proper = TRUE)

save(list=paste0("sds","AvilaCatM",c("P","P","C","C"),"",c("","T")),file="sdsAvilaMS.RData")
rm(list=paste0("sds","AvilaCatM",c("P","P","C","C"),"",c("","T")))


sdsAvilaSamp <- syn(avilaODS,method = rep("sample",11),m = 1000)
save(list="sdsAvilaSamp",file="sdsAvilaSamp.RData")

## TESTING

test <- c(relevel(Class,"A") ~ Int_Dist + Up_Marg + Lo_Marg + Exploit +
            Row_Num + Mod_Ratio + Int_Spac + Weight + Peak_Num + `MR/IS`,
          relevel(Class,"B") ~ Int_Dist + Up_Marg + Lo_Marg + Exploit +
            Row_Num + Mod_Ratio + Int_Spac + Weight + Peak_Num + `MR/IS`,
          relevel(Class,"C") ~ Int_Dist + Up_Marg + Lo_Marg + Exploit +
            Row_Num + Mod_Ratio + Int_Spac + Weight + Peak_Num + `MR/IS`,
          relevel(Class,"D") ~ Int_Dist + Up_Marg + Lo_Marg + Exploit +
            Row_Num + Mod_Ratio + Int_Spac + Weight + Peak_Num + `MR/IS`,
          relevel(Class,"E") ~ Int_Dist + Up_Marg + Lo_Marg + Exploit +
            Row_Num + Mod_Ratio + Int_Spac + Weight + Peak_Num + `MR/IS`,
          relevel(Class,"F") ~ Int_Dist + Up_Marg + Lo_Marg + Exploit +
            Row_Num + Mod_Ratio + Int_Spac + Weight + Peak_Num + `MR/IS`,
          relevel(Class,"G") ~ Int_Dist + Up_Marg + Lo_Marg + Exploit +
            Row_Num + Mod_Ratio + Int_Spac + Weight + Peak_Num + `MR/IS`,
          relevel(Class,"H") ~ Int_Dist + Up_Marg + Lo_Marg + Exploit +
            Row_Num + Mod_Ratio + Int_Spac + Weight + Peak_Num + `MR/IS`,
          relevel(Class,"I") ~ Int_Dist + Up_Marg + Lo_Marg + Exploit +
            Row_Num + Mod_Ratio + Int_Spac + Weight + Peak_Num + `MR/IS`,
          relevel(Class,"W") ~ Int_Dist + Up_Marg + Lo_Marg + Exploit +
            Row_Num + Mod_Ratio + Int_Spac + Weight + Peak_Num + `MR/IS`,
          relevel(Class,"X") ~ Int_Dist + Up_Marg + Lo_Marg + Exploit +
            Row_Num + Mod_Ratio + Int_Spac + Weight + Peak_Num + `MR/IS`,
          relevel(Class,"Y") ~ Int_Dist + Up_Marg + Lo_Marg + Exploit +
            Row_Num + Mod_Ratio + Int_Spac + Weight + Peak_Num + `MR/IS`,
          relevel(Class,"A") ~ Int_Dist + Up_Marg + Lo_Marg + Exploit,
          relevel(Class,"A") ~ Row_Num + Mod_Ratio + Int_Spac +
            Weight + Peak_Num + `MR/IS`,
          relevel(Class,"A") ~ Mod_Ratio + Int_Spac + Weight + `MR/IS`,
          relevel(Class,"A") ~ Exploit + Row_Num + Weight + Peak_Num,
          `MR/IS` ~ Mod_Ratio*Int_Spac,
          `MR/IS` ~ Int_Spac*Mod_Ratio,
          Row_Num ~ Class + Weight + Peak_Num,
          Mod_Ratio ~ Int_Spac + `MR/IS` + Int_Dist,
          Int_Spac ~ Mod_Ratio + `MR/IS` + Int_Dist,
          Int_Dist ~ Class + Lo_Marg + Up_Marg,
          Exploit ~ Weight + Lo_Marg + Mod_Ratio,
          Up_Marg ~ Lo_Marg + Int_Spac + `MR/IS` + Weight)

test2 <- c("binomial","binomial","binomial","binomial",
           "binomial","binomial","binomial","binomial",
           "binomial","binomial","binomial","binomial",
           "binomial","binomial","binomial","binomial",
           "gaussian","gaussian","gaussian","gaussian",
           "gaussian","gaussian","gaussian","gaussian")

load("sdsAvilaM.RData")

sds <- paste0("sds","Avila",c("Par","Par","Dec","Dec"),"M",c("","T"))
glm <- paste0("glm","Avila",c("Par","Par","Dec","Dec"),"M",c("","T"))
est <- paste0("est","Avila",c("Par","Par","Dec","Dec"),"M",c("","T"))
kld <- paste0("kld","Avila",c("Par","Par","Dec","Dec"),"M",c("","T"))

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <- TestGLM(act,avilaODS,c(1,2,3,5,10,20,50,100),1000,test,test2)
  actE <- TestEst(act,avilaODS,mean,var,c(1:10),c(1,2,3,5,10,20,50,100),1000)
  actK <- TestKLD(act,avilaODS)
  assign(as.character(glm[[i]]),actG)
  assign(as.character(est[[i]]),actE)
  assign(as.character(kld[[i]]),actK)
}

rm(list = sds)

save(list = glm,file = "glmAvilaM.RData")
save(list = est,file = "estAvilaM.RData")
save(list = kld,file = "kldAvilaM.RData")
rm(list = glm)
rm(list = est)
rm(list = kld)


load("sdsAvilaMO.RData")

sds <- paste0("sds","Avila",c("Par","Par","Dec","Dec"),"MO",c("","T"))
glm <- paste0("glm","Avila",c("Par","Par","Dec","Dec"),"MO",c("","T"))
est <- paste0("est","Avila",c("Par","Par","Dec","Dec"),"MO",c("","T"))
kld <- paste0("kld","Avila",c("Par","Par","Dec","Dec"),"MO",c("","T"))

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <- TestGLM(act,avilaODS,c(1,2,3,5,10,20,50,100),1000,test,test2)
  actE <- TestEst(act,avilaODS,mean,var,c(1:10),c(1,2,3,5,10,20,50,100),1000)
  actK <- TestKLD(act,avilaODS)
  assign(as.character(glm[[i]]),actG)
  assign(as.character(est[[i]]),actE)
  assign(as.character(kld[[i]]),actK)
}

rm(list = sds)

save(list = glm,file = "glmAvilaMO.RData")
save(list = est,file = "estAvilaMO.RData")
save(list = kld,file = "kldAvilaMO.RData")
rm(list = glm)
rm(list = est)
rm(list = kld)


load("sdsAvilaMV.RData")

sds <- paste0("sds","Avila",c("Par","Par","Dec","Dec"),"MV",c("","T"))
glm <- paste0("glm","Avila",c("Par","Par","Dec","Dec"),"MV",c("","T"))
est <- paste0("est","Avila",c("Par","Par","Dec","Dec"),"MV",c("","T"))
kld <- paste0("kld","Avila",c("Par","Par","Dec","Dec"),"MV",c("","T"))

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <- TestGLM(act,avilaODS,c(1,2,3,5,10,20,50,100),1000,test,test2)
  actE <- TestEst(act,avilaODS,mean,var,c(1:10),c(1,2,3,5,10,20,50,100),1000)
  actK <- TestKLD(act,avilaODS)
  assign(as.character(glm[[i]]),actG)
  assign(as.character(est[[i]]),actE)
  assign(as.character(kld[[i]]),actK)
}

rm(list = sds)

save(list = glm,file = "glmAvilaMV.RData")
save(list = est,file = "estAvilaMV.RData")
save(list = kld,file = "kldAvilaMV.RData")
rm(list = glm)
rm(list = est)
rm(list = kld)


load("sdsAvilaMS.RData")

sds <- paste0("sds","AvilaCatM",c("P","P","C","C"),"",c("","T"))
glm <- paste0("glm","AvilaCatM",c("P","P","C","C"),"",c("","T"))
est <- paste0("est","AvilaCatM",c("P","P","C","C"),"",c("","T"))
kld <- paste0("kld","AvilaCatM",c("P","P","C","C"),"",c("","T"))

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <- TestGLM(act,avilaODS,c(1,2,3,5,10,20,50,100),1000,test,test2)
  actE <- TestEst(act,avilaODS,mean,var,c(1:10),c(1,2,3,5,10,20,50,100),1000)
  actK <- TestKLD(act,avilaODS)
  assign(as.character(glm[[i]]),actG)
  assign(as.character(est[[i]]),actE)
  assign(as.character(kld[[i]]),actK)
}

rm(list = sds)

save(list = glm,file = "glmAvilaMS.RData")
save(list = est,file = "estAvilaMS.RData")
save(list = kld,file = "kldAvilaMS.RData")
rm(list = glm)
rm(list = est)
rm(list = kld)


load("sdsAvilaSamp.RData")

sds <- paste0("sds","AvilaSamp",c(""),"",c(""))
glm <- paste0("glm","AvilaSamp",c(""),"",c(""))
est <- paste0("est","AvilaSamp",c(""),"",c(""))
kld <- paste0("kld","AvilaSamp",c(""),"",c(""))

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <- TestGLM(act,avilaODS,c(1,2,3,5,10,20,50,100),1000,test,test2)
  actE <- TestEst(act,avilaODS,mean,var,c(1:10),c(1,2,3,5,10,20,50,100),1000)
  actK <- TestKLD(act,avilaODS)
  assign(as.character(glm[[i]]),actG)
  assign(as.character(est[[i]]),actE)
  assign(as.character(kld[[i]]),actK)
}

rm(list = sds)

save(list = c(glm,est,kld),file = "testAvilaSamp.RData")
rm(list = c(glm,est,kld))


## Selective

## Polish

sdsPolishParMSe <- TestPar(ods,m = 1000,predMat = polishRelations)
sdsPolishDecMSe <- TestDecPF(ods,m = 1000,proper = FALSE,predMat = polishRelations)
sdsPolishParMTSe <- TestPar(ods,m = 1000,proper = TRUE,predMat = polishRelations)
sdsPolishDecMTSe <- TestDecPF(ods,m = 1000,predMat = polishRelations)
save(list=paste0("sds","Polish",c("Par","Par","Dec","Dec"),"M",c("","T"),"Se"),file="sdsPolishMSe.RData")
rm(list=paste0("sds","Polish",c("Par","Par","Dec","Dec"),"M",c("","T"),"Se"))

sdsPolishParMOSe <- TestPar(ods,c(14:1),m = 1000,predMat = polishRelations)
sdsPolishDecMOSe <- TestDecPF(ods,c(14:1),proper = FALSE,m = 1000,predMat = polishRelations)
sdsPolishParMOTSe <- TestPar(ods,c(14:1),m = 1000,proper = TRUE,predMat = polishRelations)
sdsPolishDecMOTSe <- TestDecPF(ods,c(14:1),m = 1000,predMat = polishRelations)
save(list=paste0("sds","Polish",c("Par","Par","Dec","Dec"),"MO",c("","T"),"Se"),file="sdsPolishMOSe.RData")
rm(list=paste0("sds","Polish",c("Par","Par","Dec","Dec"),"MO",c("","T"),"Se"))

sdsPolishParMVSe <- TestPar(ods,varsTest,m = 1000,predMat = polishRelations)
sdsPolishDecMVSe <- TestDecPF(ods,varsTest,proper = FALSE,m = 1000,predMat = polishRelations)
sdsPolishParMVTSe <- TestPar(ods,varsTest,m = 1000,proper = TRUE,predMat = polishRelations)
sdsPolishDecMVTSe <- TestDecPF(ods,varsTest,m = 1000,predMat = polishRelations)
save(list=paste0("sds","Polish",c("Par","Par","Dec","Dec"),"MV",c("","T"),"Se"),file="sdsPolishMVSe.RData")
rm(list=paste0("sds","Polish",c("Par","Par","Dec","Dec"),"MV",c("","T"),"Se"))

methods <- rep("catall",14)
methods[c(2,5,8,13,14)] <- "pmm"

sdsPolishCatMPSe <- TestCAP(ods,methods,c(c(1,3,4,6,7,9:12),c(2,5,8,13,14)),m = 1000,predMat = polishRelations)
sdsPolishCatMPTSe <- TestCAP(ods,methods,c(c(1,3,4,6,7,9:12),c(2,5,8,13,14)),m = 1000,proper = TRUE,predMat = polishRelations)

methods <- rep("catall",14)
methods[c(2,5,8,13,14)] <- "cart"

sdsPolishCatMCSe <- TestCAC(ods,methods,c(c(1,3,4,6,7,9:12),c(2,5,8,13,14)),m = 1000,predMat = polishRelations)
sdsPolishCatMCTSe <- TestCAC(ods,methods,c(c(1,3,4,6,7,9:12),c(2,5,8,13,14)),m = 1000,proper = TRUE,predMat = polishRelations)

save(list=paste0("sds","PolishCatM",c("P","P","C","C"),"",c("","T"),"Se"),file="sdsPolishMSSe.RData")
rm(list=paste0("sds","PolishCatM",c("P","P","C","C"),"",c("","T"),"Se"))

## TESTING

test <- c(sex ~ height + weight,sex ~ height,
          sex ~ age + edu + marital + income + ls + trust +
            nofriend + smoke + alcabuse + wkabint + englang + height + weight,
          wkabint ~ sex + age + edu + log(income + 1),
          wkabint ~ sex + edu + log(income + 1),
          wkabint ~ sex + edu + log(income + 1) + ls + englang,
          smoke ~ sex + age + edu + sex:edu,
          smoke ~ alcabuse + marital + log(income+1),
          alcabuse ~ age + sex + ls + edu,
          alcabuse ~ smoke + marital + log(income+1),
          alcabuse ~ smoke + income,
          marital ~ age + sex + height + weight + edu,
          log(income+1) ~ age + sex + edu,
          sqrt(income) ~ age + sex + edu,
          income ~ age + sex + edu,
          sqrt(nofriend) ~ age + sex + ls + trust,
          nofriend ~ sex + age + edu + marital + income + ls + trust +
            smoke + alcabuse + wkabint + englang + height + weight,
          height ~ weight + sex + age,
          weight ~ sex + age + edu + smoke + alcabuse + height,
          age ~ alcabuse + smoke + wkabint + englang + ls)

test2 <- c("binomial","binomial","binomial","binomial",
           "binomial","binomial","binomial","binomial",
           "binomial","binomial","binomial","binomial",
           "gaussian","gaussian","gaussian","gaussian",
           "gaussian","gaussian","gaussian","gaussian")


load("sdsPolishMSe.RData")

sds <- paste0("sds","Polish",c("Par","Par","Dec","Dec"),"M",c("","T"),"Se")
glm <- paste0("glm","Polish",c("Par","Par","Dec","Dec"),"M",c("","T"),"Se")
est <- paste0("est","Polish",c("Par","Par","Dec","Dec"),"M",c("","T"),"Se")
kld <- paste0("kld","Polish",c("Par","Par","Dec","Dec"),"M",c("","T"),"Se")

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <- TestGLM(act,ods,c(1,2,3,5,10,20,50,100),1000,test,test2)
  actE <- TestEst(act,ods,mean,var,c(2,5,8,13,14),c(1,2,3,5,10,20,50,100),1000)
  actK <- TestKLD(act,ods)
  assign(as.character(glm[[i]]),actG)
  assign(as.character(est[[i]]),actE)
  assign(as.character(kld[[i]]),actK)
}

rm(list = sds)

save(list = glm,file = "glmPolishMSe.RData")
save(list = est,file = "estPolishMSe.RData")
save(list = kld,file = "kldPolishMSe.RData")
rm(list = glm)
rm(list = est)
rm(list = kld)


load("sdsPolishMOSe.RData")

sds <- paste0("sds","Polish",c("Par","Par","Dec","Dec"),"MO",c("","T"),"Se")
glm <- paste0("glm","Polish",c("Par","Par","Dec","Dec"),"MO",c("","T"),"Se")
est <- paste0("est","Polish",c("Par","Par","Dec","Dec"),"MO",c("","T"),"Se")
kld <- paste0("kld","Polish",c("Par","Par","Dec","Dec"),"MO",c("","T"),"Se")

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <- TestGLM(act,ods,c(1,2,3,5,10,20,50,100),1000,test,test2)
  actE <- TestEst(act,ods,mean,var,c(2,5,8,13,14),c(1,2,3,5,10,20,50,100),1000)
  actK <- TestKLD(act,ods)
  assign(as.character(glm[[i]]),actG)
  assign(as.character(est[[i]]),actE)
  assign(as.character(kld[[i]]),actK)
}

rm(list = sds)

save(list = glm,file = "glmPolishMOSe.RData")
save(list = est,file = "estPolishMOSe.RData")
save(list = kld,file = "kldPolishMOSe.RData")
rm(list = glm)
rm(list = est)
rm(list = kld)


load("sdsPolishMVSe.RData")

sds <- paste0("sds","Polish",c("Par","Par","Dec","Dec"),"MV",c("","T"),"Se")
glm <- paste0("glm","Polish",c("Par","Par","Dec","Dec"),"MV",c("","T"),"Se")
est <- paste0("est","Polish",c("Par","Par","Dec","Dec"),"MV",c("","T"),"Se")
kld <- paste0("kld","Polish",c("Par","Par","Dec","Dec"),"MV",c("","T"),"Se")

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <- TestGLM(act,ods,c(1,2,3,5,10,20,50,100),1000,test,test2)
  actE <- TestEst(act,ods,mean,var,c(2,5,8,13,14),c(1,2,3,5,10,20,50,100),1000)
  actK <- TestKLD(act,ods)
  assign(as.character(glm[[i]]),actG)
  assign(as.character(est[[i]]),actE)
  assign(as.character(kld[[i]]),actK)
}

rm(list = sds)

save(list = glm,file = "glmPolishMVSe.RData")
save(list = est,file = "estPolishMVSe.RData")
save(list = kld,file = "kldPolishMVSe.RData")
rm(list = glm)
rm(list = est)
rm(list = kld)


load("sdsPolishMSSe.RData")

sds <- paste0("sds","PolishCatM",c("P","P","C","C"),"",c("","T"),"Se")
glm <- paste0("glm","PolishCatM",c("P","P","C","C"),"",c("","T"),"Se")
est <- paste0("est","PolishCatM",c("P","P","C","C"),"",c("","T"),"Se")
kld <- paste0("kld","PolishCatM",c("P","P","C","C"),"",c("","T"),"Se")

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <- TestGLM(act,ods,c(1,2,3,5,10,20,50,100),1000,test,test2)
  actE <- TestEst(act,ods,mean,var,c(2,5,8,13,14),c(1,2,3,5,10,20,50,100),1000)
  actK <- TestKLD(act,ods)
  assign(as.character(glm[[i]]),actG)
  assign(as.character(est[[i]]),actE)
  assign(as.character(kld[[i]]),actK)
}

rm(list = sds)

save(list = glm,file = "glmPolishMSSe.RData")
save(list = est,file = "estPolishMSSe.RData")
save(list = kld,file = "kldPolishMSSe.RData")
rm(list = glm)
rm(list = est)
rm(list = kld)



## Adult

sdsAdultParMSe <- TestPar(adultODS10K,m = 100,predMat = adultRelations)
sdsAdultDecMSe <- TestDecPF(adultODS10K,m = 1000,proper = FALSE,predMat = adultRelations)
sdsAdultParMTSe <- TestPar(adultODS10K,m = 100,proper = TRUE,predMat = adultRelations)
sdsAdultDecMTSe <- TestDecPF(adultODS10K,m = 1000,predMat = adultRelations)
save(list=paste0("sds","Adult",c("Par","Par","Dec","Dec"),"M",c("","T"),"Se"),file="sdsAdultMSe.RData")
rm(list=paste0("sds","Adult",c("Par","Par","Dec","Dec"),"M",c("","T"),"Se"))

sdsAdultParMHSe <- TestPar(adultODS10K,LH2Order,m = 100,predMat = adultRelations)
sdsAdultDecMHSe <- TestDecPF(adultODS10K,LH2Order,m = 1000,proper = FALSE,predMat = adultRelations)
sdsAdultParMHTSe <- TestPar(adultODS10K,LH2Order,m = 100,proper = TRUE,predMat = adultRelations)
sdsAdultDecMHTSe <- TestDecPF(adultODS10K,LH2Order,m = 1000,predMat = adultRelations)
save(list=paste0("sds","Adult",c("Par","Par","Dec","Dec"),"MH",c("","T"),"Se"),file="sdsAdultMHSe.RData")
rm(list=paste0("sds","Adult",c("Par","Par","Dec","Dec"),"MH",c("","T"),"Se"))

sdsAdultParMLSe <- TestPar(adultODS10K,rev(LH2Order),m = 100,predMat = adultRelations)
sdsAdultDecMLSe <- TestDecPF(adultODS10K,rev(LH2Order),m = 1000,proper = FALSE,predMat = adultRelations)
sdsAdultParMLTSe <- TestPar(adultODS10K,rev(LH2Order),m = 100,proper = TRUE,predMat = adultRelations)
sdsAdultDecMLTSe <- TestDecPF(adultODS10K,rev(LH2Order),m = 1000,predMat = adultRelations)
save(list=paste0("sds","Adult",c("Par","Par","Dec","Dec"),"ML",c("","T"),"Se"),file="sdsAdultMLSe.RData")
rm(list=paste0("sds","Adult",c("Par","Par","Dec","Dec"),"ML",c("","T"),"Se"))

methods <- rep("catall",13)
methods[c(1,9:11)] <- "pmm"

sdsAdultCatMPSe <- TestCAP(adultODS10K,methods,c(c(2:8,12,13),c(1,9:11)),m = 1000,predMat = adultRelations)
sdsAdultCatMPTSe <- TestCAP(adultODS10K,methods,c(c(2:8,12,13),c(1,9:11)),m = 1000,proper = TRUE,predMat = adultRelations)

methods <- rep("catall",13)
methods[c(1,9:11)] <- "cart"

sdsAdultCatMCSe <- TestCAC(adultODS10K,methods,c(c(2:8,12,13),c(1,9:11)),m = 1000,predMat = adultRelations)
sdsAdultCatMCTSe <- TestCAC(adultODS10K,methods,c(c(2:8,12,13),c(1,9:11)),m = 1000,proper = TRUE,predMat = adultRelations)

save(list=paste0("sds","AdultCatM",c("P","P","C","C"),"",c("","T"),"Se"),file="sdsAdultMSSe.RData")
rm(list=paste0("sds","AdultCatM",c("P","P","C","C"),"",c("","T"),"Se"))

## TESTING

test <- c(income ~ native.country,
          income ~ occupation + hours.per.week,
          income ~ sex + age + workclass,
          income ~ education + marital.status + race,
          income ~ sex + race + relationship + age,
          income ~ age + workclass + education + marital.status +
            occupation + relationship + race + sex + capital.gain +
            capital.loss + hours.per.week + native.country,
          income ~ capital.gain + capital.loss + occupation,
          income ~ race*sex,
          sex ~ workclass + education + hours.per.week,
          sex ~ income,
          income ~ sex,
          relevel(race,"White") ~ workclass + marital.status + income,
          relevel(native.country,"United-States") ~ age + sex +
            education + race,
          cut(capital.gain,c(-1,0,10**9),c("ZERO","NON-ZERO")) ~
            workclass + education + occupation + income,
          cut(capital.loss,c(-1,0,10**9),c("ZERO","NON-ZERO")) ~
            workclass + education + occupation + income,
          cut(capital.gain,c(-1,0,10**9),c("ZERO","NON-ZERO")) ~ 
            age + sex + race + hours.per.week +
            workclass + education + occupation + income,
          cut(capital.loss,c(-1,0,10**9),c("ZERO","NON-ZERO")) ~
            age + sex + race + hours.per.week +
            workclass + education + occupation + income,
          capital.gain ~ workclass + education + occupation + income,
          capital.loss ~ workclass + education + occupation + income,
          capital.gain ~ age + sex + race + hours.per.week +
            workclass + education + occupation + income,
          capital.loss ~ age + sex + race + hours.per.week +
            workclass + education + occupation + income,
          hours.per.week ~ native.country + race + occupation,
          hours.per.week ~ native.country + race + occupation +
            age + workclass + sex + education + income,
          age ~ hours.per.week + education + marital.status)

test2 <- c("binomial","binomial","binomial","binomial",
           "binomial","binomial","binomial","binomial",
           "binomial","binomial","binomial","binomial",
           "binomial","binomial","binomial","binomial",
           "binomial","gaussian","gaussian","gaussian",
           "gaussian","gaussian","gaussian","gaussian")


load("sdsAdultMSe.RData")

sds <- paste0("sds","Adult",c("Par","Par","Dec","Dec"),"M",c("","T"),"Se")
glm <- paste0("glm","Adult",c("Par","Par","Dec","Dec"),"M",c("","T"),"Se")
est <- paste0("est","Adult",c("Par","Par","Dec","Dec"),"M",c("","T"),"Se")
kld <- paste0("kld","Adult",c("Par","Par","Dec","Dec"),"M",c("","T"),"Se")

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  cc <- c(1,2,3,5,10,20,50)
  m <- act$m
  if(m == 1000){
    cc <- c(cc,100)
  }
  actG <- TestGLM(act,adultODS10K,cc,m,test,test2)
  actE <- TestEst(act,adultODS10K,mean,var,c(1,9,10,11),cc,m)
  actK <- TestKLD(act,adultODS10K)
  assign(as.character(glm[[i]]),actG)
  assign(as.character(est[[i]]),actE)
  assign(as.character(kld[[i]]),actK)
}

rm(list = sds)

save(list = glm,file = "glmAdultMSe.RData")
save(list = est,file = "estAdultMSe.RData")
save(list = kld,file = "kldAdultMSe.RData")
rm(list = glm)
rm(list = est)
rm(list = kld)


load("sdsAdultMHSe.RData")

sds <- paste0("sds","Adult",c("Par","Par","Dec","Dec"),"MH",c("","T"),"Se")
glm <- paste0("glm","Adult",c("Par","Par","Dec","Dec"),"MH",c("","T"),"Se")
est <- paste0("est","Adult",c("Par","Par","Dec","Dec"),"MH",c("","T"),"Se")
kld <- paste0("kld","Adult",c("Par","Par","Dec","Dec"),"MH",c("","T"),"Se")

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  cc <- c(1,2,3,5,10,20,50)
  m <- act$m
  if(m == 1000){
    cc <- c(cc,100)
  }
  actG <- TestGLM(act,adultODS10K,cc,m,test,test2)
  actE <- TestEst(act,adultODS10K,mean,var,c(1,9,10,11),cc,m)
  actK <- TestKLD(act,adultODS10K)
  assign(as.character(glm[[i]]),actG)
  assign(as.character(est[[i]]),actE)
  assign(as.character(kld[[i]]),actK)
}

rm(list = sds)

save(list = glm,file = "glmAdultMHSe.RData")
save(list = est,file = "estAdultMHSe.RData")
save(list = kld,file = "kldAdultMHSe.RData")
rm(list = glm)
rm(list = est)
rm(list = kld)


load("sdsAdultMLSe.RData")

sds <- paste0("sds","Adult",c("Par","Par","Dec","Dec"),"ML",c("","T"),"Se")
glm <- paste0("glm","Adult",c("Par","Par","Dec","Dec"),"ML",c("","T"),"Se")
est <- paste0("est","Adult",c("Par","Par","Dec","Dec"),"ML",c("","T"),"Se")
kld <- paste0("kld","Adult",c("Par","Par","Dec","Dec"),"ML",c("","T"),"Se")

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  cc <- c(1,2,3,5,10,20,50)
  m <- act$m
  if(m == 1000){
    cc <- c(cc,100)
  }
  actG <- TestGLM(act,adultODS10K,cc,m,test,test2)
  actE <- TestEst(act,adultODS10K,mean,var,c(1,9,10,11),cc,m)
  actK <- TestKLD(act,adultODS10K)
  assign(as.character(glm[[i]]),actG)
  assign(as.character(est[[i]]),actE)
  assign(as.character(kld[[i]]),actK)
}

rm(list = sds)

save(list = glm,file = "glmAdultMLSe.RData")
save(list = est,file = "estAdultMLSe.RData")
save(list = kld,file = "kldAdultMLSe.RData")
rm(list = glm)
rm(list = est)
rm(list = kld)


load("sdsAdultMSSe.RData")

sds <- paste0("sds","AdultCatM",c("P","P","C","C"),"",c("","T"),"Se")
glm <- paste0("glm","AdultCatM",c("P","P","C","C"),"",c("","T"),"Se")
est <- paste0("est","AdultCatM",c("P","P","C","C"),"",c("","T"),"Se")
kld <- paste0("kld","AdultCatM",c("P","P","C","C"),"",c("","T"),"Se")

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  cc <- c(1,2,3,5,10,20,50)
  m <- act$m
  if(m == 1000){
    cc <- c(cc,100)
  }
  actG <- TestGLM(act,adultODS10K,cc,m,test,test2)
  actE <- TestEst(act,adultODS10K,mean,var,c(1,9,10,11),cc,m)
  actK <- TestKLD(act,adultODS10K)
  assign(as.character(glm[[i]]),actG)
  assign(as.character(est[[i]]),actE)
  assign(as.character(kld[[i]]),actK)
}

rm(list = sds)

save(list = glm,file = "glmAdultMSSe.RData")
save(list = est,file = "estAdultMSSe.RData")
save(list = kld,file = "kldAdultMSSe.RData")
rm(list = glm)
rm(list = est)
rm(list = kld)


## Avila

sdsAvilaParMSe <- TestPar(avilaODS,m = 1000,predMat = avilaRelations)
sdsAvilaDecMSe <- TestDecPF(avilaODS,m = 1000,proper = FALSE,predMat = avilaRelations)
sdsAvilaParMTSe <- TestPar(avilaODS,m = 1000,proper = TRUE,predMat = avilaRelations)
sdsAvilaDecMTSe <- TestDecPF(avilaODS,m = 1000,proper = TRUE,predMat = avilaRelations)
save(list=paste0("sds","Avila",c("Par","Par","Dec","Dec"),"M",c("","T"),"Se"),file="sdsAvilaMSe.RData")
rm(list=paste0("sds","Avila",c("Par","Par","Dec","Dec"),"M",c("","T"),"Se"))

sdsAvilaParMOSe <- TestPar(avilaODS,c(11:1),m = 1000,predMat = avilaRelations)
sdsAvilaDecMOSe <- TestDecPF(avilaODS,c(11:1),m = 1000,proper = FALSE,predMat = avilaRelations)
sdsAvilaParMOTSe <- TestPar(avilaODS,c(11:1),m = 1000,proper = TRUE,predMat = avilaRelations)
sdsAvilaDecMOTSe <- TestDecPF(avilaODS,c(11:1),m = 1000,proper = TRUE,predMat = avilaRelations)
save(list=paste0("sds","Avila",c("Par","Par","Dec","Dec"),"MO",c("","T"),"Se"),file="sdsAvilaMOSe.RData")
rm(list=paste0("sds","Avila",c("Par","Par","Dec","Dec"),"MO",c("","T"),"Se"))

sdsAvilaParMVSe <- TestPar(avilaODS,CM,m = 1000,predMat = avilaRelations)
sdsAvilaDecMVSe <- TestDecPF(avilaODS,CM,m = 1000,proper = FALSE,predMat = avilaRelations)
sdsAvilaParMVTSe <- TestPar(avilaODS,CM,m = 1000,proper = TRUE,predMat = avilaRelations)
sdsAvilaDecMVTSe <- TestDecPF(avilaODS,CM,m = 1000,proper = TRUE,predMat = avilaRelations)
save(list=paste0("sds","Avila",c("Par","Par","Dec","Dec"),"MV",c("","T"),"Se"),file="sdsAvilaMVSe.RData")
rm(list=paste0("sds","Avila",c("Par","Par","Dec","Dec"),"MV",c("","T"),"Se"))


methods <- rep("pmm",11)
methods[11] <- "catall"


sdsAvilaCatMPSe <- TestCAP(avilaODS,methods,c(11,1:10),m = 1000,predMat = avilaRelations)
sdsAvilaCatMPTSe <- TestCAP(avilaODS,methods,c(11,1:10),m = 1000,proper = TRUE,predMat = avilaRelations)

methods <- rep("cart",11)
methods[11] <- "catall"


sdsAvilaCatMCSe <- TestCAC(avilaODS,methods,c(11,1:10),m = 1000,predMat = avilaRelations)
sdsAvilaCatMCTSe <- TestCAC(avilaODS,methods,c(11,1:10),m = 1000,proper = TRUE,predMat = avilaRelations)

save(list=paste0("sds","AvilaCatM",c("P","P","C","C"),"",c("","T"),"Se"),file="sdsAvilaMSSe.RData")
rm(list=paste0("sds","AvilaCatM",c("P","P","C","C"),"",c("","T"),"Se"))

## TESTING

test <- c(relevel(Class,"A") ~ Int_Dist + Up_Marg + Lo_Marg + Exploit +
            Row_Num + Mod_Ratio + Int_Spac + Weight + Peak_Num + `MR/IS`,
          relevel(Class,"B") ~ Int_Dist + Up_Marg + Lo_Marg + Exploit +
            Row_Num + Mod_Ratio + Int_Spac + Weight + Peak_Num + `MR/IS`,
          relevel(Class,"C") ~ Int_Dist + Up_Marg + Lo_Marg + Exploit +
            Row_Num + Mod_Ratio + Int_Spac + Weight + Peak_Num + `MR/IS`,
          relevel(Class,"D") ~ Int_Dist + Up_Marg + Lo_Marg + Exploit +
            Row_Num + Mod_Ratio + Int_Spac + Weight + Peak_Num + `MR/IS`,
          relevel(Class,"E") ~ Int_Dist + Up_Marg + Lo_Marg + Exploit +
            Row_Num + Mod_Ratio + Int_Spac + Weight + Peak_Num + `MR/IS`,
          relevel(Class,"F") ~ Int_Dist + Up_Marg + Lo_Marg + Exploit +
            Row_Num + Mod_Ratio + Int_Spac + Weight + Peak_Num + `MR/IS`,
          relevel(Class,"G") ~ Int_Dist + Up_Marg + Lo_Marg + Exploit +
            Row_Num + Mod_Ratio + Int_Spac + Weight + Peak_Num + `MR/IS`,
          relevel(Class,"H") ~ Int_Dist + Up_Marg + Lo_Marg + Exploit +
            Row_Num + Mod_Ratio + Int_Spac + Weight + Peak_Num + `MR/IS`,
          relevel(Class,"I") ~ Int_Dist + Up_Marg + Lo_Marg + Exploit +
            Row_Num + Mod_Ratio + Int_Spac + Weight + Peak_Num + `MR/IS`,
          relevel(Class,"W") ~ Int_Dist + Up_Marg + Lo_Marg + Exploit +
            Row_Num + Mod_Ratio + Int_Spac + Weight + Peak_Num + `MR/IS`,
          relevel(Class,"X") ~ Int_Dist + Up_Marg + Lo_Marg + Exploit +
            Row_Num + Mod_Ratio + Int_Spac + Weight + Peak_Num + `MR/IS`,
          relevel(Class,"Y") ~ Int_Dist + Up_Marg + Lo_Marg + Exploit +
            Row_Num + Mod_Ratio + Int_Spac + Weight + Peak_Num + `MR/IS`,
          relevel(Class,"A") ~ Int_Dist + Up_Marg + Lo_Marg + Exploit,
          relevel(Class,"A") ~ Row_Num + Mod_Ratio + Int_Spac +
            Weight + Peak_Num + `MR/IS`,
          relevel(Class,"A") ~ Mod_Ratio + Int_Spac + Weight + `MR/IS`,
          relevel(Class,"A") ~ Exploit + Row_Num + Weight + Peak_Num,
          `MR/IS` ~ Mod_Ratio*Int_Spac,
          `MR/IS` ~ Int_Spac*Mod_Ratio,
          Row_Num ~ Class + Weight + Peak_Num,
          Mod_Ratio ~ Int_Spac + `MR/IS` + Int_Dist,
          Int_Spac ~ Mod_Ratio + `MR/IS` + Int_Dist,
          Int_Dist ~ Class + Lo_Marg + Up_Marg,
          Exploit ~ Weight + Lo_Marg + Mod_Ratio,
          Up_Marg ~ Lo_Marg + Int_Spac + `MR/IS` + Weight)

test2 <- c("binomial","binomial","binomial","binomial",
           "binomial","binomial","binomial","binomial",
           "binomial","binomial","binomial","binomial",
           "binomial","binomial","binomial","binomial",
           "gaussian","gaussian","gaussian","gaussian",
           "gaussian","gaussian","gaussian","gaussian")

load("sdsAvilaMSe.RData")

sds <- paste0("sds","Avila",c("Par","Par","Dec","Dec"),"M",c("","T"),"Se")
glm <- paste0("glm","Avila",c("Par","Par","Dec","Dec"),"M",c("","T"),"Se")
est <- paste0("est","Avila",c("Par","Par","Dec","Dec"),"M",c("","T"),"Se")
kld <- paste0("kld","Avila",c("Par","Par","Dec","Dec"),"M",c("","T"),"Se")

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <- TestGLM(act,avilaODS,c(1,2,3,5,10,20,50,100),1000,test,test2)
  actE <- TestEst(act,avilaODS,mean,var,c(1:10),c(1,2,3,5,10,20,50,100),1000)
  actK <- TestKLD(act,avilaODS)
  assign(as.character(glm[[i]]),actG)
  assign(as.character(est[[i]]),actE)
  assign(as.character(kld[[i]]),actK)
}

rm(list = sds)

save(list = glm,file = "glmAvilaMSe.RData")
save(list = est,file = "estAvilaMSe.RData")
save(list = kld,file = "kldAvilaMSe.RData")
rm(list = glm)
rm(list = est)
rm(list = kld)


load("sdsAvilaMOSe.RData")

sds <- paste0("sds","Avila",c("Par","Par","Dec","Dec"),"MO",c("","T"),"Se")
glm <- paste0("glm","Avila",c("Par","Par","Dec","Dec"),"MO",c("","T"),"Se")
est <- paste0("est","Avila",c("Par","Par","Dec","Dec"),"MO",c("","T"),"Se")
kld <- paste0("kld","Avila",c("Par","Par","Dec","Dec"),"MO",c("","T"),"Se")

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <- TestGLM(act,avilaODS,c(1,2,3,5,10,20,50,100),1000,test,test2)
  actE <- TestEst(act,avilaODS,mean,var,c(1:10),c(1,2,3,5,10,20,50,100),1000)
  actK <- TestKLD(act,avilaODS)
  assign(as.character(glm[[i]]),actG)
  assign(as.character(est[[i]]),actE)
  assign(as.character(kld[[i]]),actK)
}

rm(list = sds)

save(list = glm,file = "glmAvilaMOSe.RData")
save(list = est,file = "estAvilaMOSe.RData")
save(list = kld,file = "kldAvilaMOSe.RData")
rm(list = glm)
rm(list = est)
rm(list = kld)


load("sdsAvilaMVSe.RData")

sds <- paste0("sds","Avila",c("Par","Par","Dec","Dec"),"MV",c("","T"),"Se")
glm <- paste0("glm","Avila",c("Par","Par","Dec","Dec"),"MV",c("","T"),"Se")
est <- paste0("est","Avila",c("Par","Par","Dec","Dec"),"MV",c("","T"),"Se")
kld <- paste0("kld","Avila",c("Par","Par","Dec","Dec"),"MV",c("","T"),"Se")

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <- TestGLM(act,avilaODS,c(1,2,3,5,10,20,50,100),1000,test,test2)
  actE <- TestEst(act,avilaODS,mean,var,c(1:10),c(1,2,3,5,10,20,50,100),1000)
  actK <- TestKLD(act,avilaODS)
  assign(as.character(glm[[i]]),actG)
  assign(as.character(est[[i]]),actE)
  assign(as.character(kld[[i]]),actK)
}

rm(list = sds)

save(list = glm,file = "glmAvilaMVSe.RData")
save(list = est,file = "estAvilaMVSe.RData")
save(list = kld,file = "kldAvilaMVSe.RData")
rm(list = glm)
rm(list = est)
rm(list = kld)


load("sdsAvilaMSSe.RData")

sds <- paste0("sds","AvilaCatM",c("P","P","C","C"),"",c("","T"),"Se")
glm <- paste0("glm","AvilaCatM",c("P","P","C","C"),"",c("","T"),"Se")
est <- paste0("est","AvilaCatM",c("P","P","C","C"),"",c("","T"),"Se")
kld <- paste0("kld","AvilaCatM",c("P","P","C","C"),"",c("","T"),"Se")

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <- TestGLM(act,avilaODS,c(1,2,3,5,10,20,50,100),1000,test,test2)
  actE <- TestEst(act,avilaODS,mean,var,c(1:10),c(1,2,3,5,10,20,50,100),1000)
  actK <- TestKLD(act,avilaODS)
  assign(as.character(glm[[i]]),actG)
  assign(as.character(est[[i]]),actE)
  assign(as.character(kld[[i]]),actK)
}

rm(list = sds)

save(list = glm,file = "glmAvilaMSSe.RData")
save(list = est,file = "estAvilaMSSe.RData")
save(list = kld,file = "kldAvilaMSSe.RData")
rm(list = glm)
rm(list = est)
rm(list = kld)



## K-Anonymity

library(sdcMicro)

## Polish

odsF <- createFac(ods,ods,cbind(c(2,5,8,13,14),2),c(colnames(ods),paste0(colnames(ods)[c(2,5,8,13,14)],"G")))

odsK <- localSuppression(odsF,keyVars = c(1,3,4,6,7,9:12,15:19))$xAnon

polishSdc <- createSdcObj(odsF,keyVars = c(1,3,4,6,7,9:12),numVars = c(2,5,8,13,14))
odsK2p <- localSuppression(polishSdc)
odsK2 <- cbind(odsK2p@manipKeyVars,odsK2p@manipNumVars)[,c(1,10,2,3,11,4,5,12,6:9,13,14)]

glmPolishAno2 <- testGLMA(odsK2,odsF,test,test2)

testK <- c(sex ~ heightG + weightG,sex ~ heightG,
           sex ~ ageG + edu + marital + incomeG + ls + trust +
             nofriendG + smoke + alcabuse + wkabint + englang + heightG + weightG,
           wkabint ~ sex + ageG + edu + incomeG,
           wkabint ~ sex + edu + incomeG,
           wkabint ~ sex + edu + incomeG + ls + englang,
           smoke ~ sex + ageG + edu + sex:edu,
           smoke ~ alcabuse + marital + incomeG,
           alcabuse ~ ageG + sex + ls + edu,
           alcabuse ~ smoke + marital + incomeG,
           alcabuse ~ smoke + incomeG,
           marital ~ ageG + sex + heightG + weightG + edu,
           incomeG ~ ageG + sex + edu,
           incomeG ~ ageG + sex*edu,
           incomeG ~ ageG*sex + edu,
           nofriendG ~ ageG + sex + ls + trust,
           nofriendG ~ sex + ageG + edu + marital + incomeG + ls + trust +
             smoke + alcabuse + wkabint + englang + heightG + weightG,
           heightG ~ weightG + sex + ageG,
           weightG ~ sex + ageG + edu + smoke + alcabuse + heightG,
           ageG ~ alcabuse + smoke + wkabint + englang + ls)

testK2 <- rep("binomial",20)

glmPolishAno <- testGLMA(odsK,odsF,testK,testK2)

load("sdsPolishM.RData")

sds <- paste0("sds","Polish",c("Par","Par","Dec","Dec"),"M",c("","T"))
glmNK <- paste0("glm","Polish",c("Par","Par","Dec","Dec"),"M",c("","T"),"K")

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <- testGLMK(act,odsF,c(1,2,3,5,10,20,50,100),1000,testK,testK2,cbind(c(2,5,8,13,14),2))
  assign(as.character(glmNK[[i]]),actG)
}

rm(list = sds)

load("sdsPolishMO.RData")

sds <- paste0("sds","Polish",c("Par","Par","Dec","Dec"),"MO",c("","T"))
glmOK <- paste0("glm","Polish",c("Par","Par","Dec","Dec"),"MO",c("","T"),"K")

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <- testGLMK(act,odsF,c(1,2,3,5,10,20,50,100),1000,testK,testK2,cbind(c(2,5,8,13,14),2))
  assign(as.character(glmOK[[i]]),actG)
}

rm(list = sds)

load("sdsPolishMV.RData")

sds <- paste0("sds","Polish",c("Par","Par","Dec","Dec"),"MV",c("","T"))
glmVK <- paste0("glm","Polish",c("Par","Par","Dec","Dec"),"MV",c("","T"),"K")

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <- testGLMK(act,odsF,c(1,2,3,5,10,20,50,100),1000,testK,testK2,cbind(c(2,5,8,13,14),2))
  assign(as.character(glmVK[[i]]),actG)
}

rm(list = sds)

load("sdsPolishMS.RData")

sds <- paste0("sds","PolishCatM",c("P","P","C","C"),"",c("","T"))
glmSK <- paste0("glm","PolishCatM",c("P","P","C","C"),"",c("","T"),"K")

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <-testGLMK(act,odsF,c(1,2,3,5,10,20,50,100),1000,testK,testK2,cbind(c(2,5,8,13,14),2))
  assign(as.character(glmSK[[i]]),actG)
}

rm(list = sds)

load("sdsPolishSamp.RData")

glmPolishSampK <- testGLMK(sdsPolishSamp,odsF,c(1,2,3,5,10,20,50,100),1000,testK,testK2,cbind(c(2,5,8,13,14),2))

save(list = c(glmNK,glmOK,glmVK,glmSK,"glmPolishSampK"),file = "glmPolishK.RData")

## Adult

adultODSF <- createFac(adultODS10K,adultODS10K,cbind(c(1,9:11),2),c(colnames(adultODSM2),paste0(colnames(adultODSM2)[c(1,9:11)],"G")))

adultODSK <- localSuppression(adultODSF,keyVars = c(2:8,12,13,14:17))$xAnon


adultSdc <- createSdcObj(adultODSF,keyVars = c(2:8,12,13),numVars = c(1,9:11))
adultODSK2p <- localSuppression(adultSdc)
adultODSK2 <- cbind(adultODSK2p@manipKeyVars,adultODSK2p@manipNumVars)[,c(10,1:7,11:13,8,9)]

glmAdultAno2 <- testGLMA(adultODSK2,adultODSF,test,test2)

testK <- c(income ~ native.country,
           income ~ occupation + hours.per.weekG,
           income ~ sex + ageG + workclass,
           income ~ education + marital.status + race,
           income ~ sex + race + relationship + ageG,
           income ~ ageG + workclass + education + marital.status +
             occupation + relationship + race + sex + capital.gainG +
             capital.lossG + hours.per.weekG + native.country,
           income ~ capital.gainG + capital.lossG + occupation,
           income ~ race*sex,
           sex ~ workclass + education + hours.per.weekG,
           sex ~ income,
           income ~ sex,
           relevel(race,"White") ~ workclass + marital.status + income,
           relevel(native.country,"United-States") ~ ageG + sex +
             education + race,
           capital.gainG ~ workclass + education + occupation + income,
           capital.lossG ~ workclass + education + occupation + income,
           capital.gainG ~ ageG + sex + race + hours.per.weekG +
             workclass + education + occupation + income,
           capital.lossG ~ ageG + sex + race + hours.per.weekG +
             workclass + education + occupation + income,
           hours.per.weekG ~ native.country + race + occupation,
           hours.per.weekG ~ native.country + race + occupation +
             ageG + workclass + sex + education + income,
           ageG ~ hours.per.weekG + education + marital.status)

testK2 <- rep("binomial",20)


glmAdultAno <- testGLMA(adultODSK,adultODSF,testK,testK2)

load("sdsAdultM.RData")

sds <- paste0("sds","Adult",c("Par","Par","Dec","Dec"),"M",c("","T"))
glmNK <- paste0("glm","Adult",c("Par","Par","Dec","Dec"),"M",c("","T"),"K")

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  cc <- c(1,2,3,5,10,20,50)
  m <- act$m
  if(m == 1000){
    cc <- c(cc,100)
  }
  actG <- testGLMK(act,adultODSF,cc,m,testK,testK2,cbind(c(1,9:11),2))
  assign(as.character(glmNK[[i]]),actG)
}

rm(list = sds)

load("sdsAdultMH.RData")

sds <- paste0("sds","Adult",c("Par","Par","Dec","Dec"),"MH",c("","T"))
glmHK <- paste0("glm","Adult",c("Par","Par","Dec","Dec"),"MH",c("","T"),"K")

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  cc <- c(1,2,3,5,10,20,50)
  m <- act$m
  if(m == 1000){
    cc <- c(cc,100)
  }
  actG <- testGLMK(act,adultODSF,cc,m,testK,testK2,cbind(c(1,9:11),2))
  assign(as.character(glmHK[[i]]),actG)
}

rm(list = sds)

load("sdsAdultML.RData")

sds <- paste0("sds","Adult",c("Par","Par","Dec","Dec"),"ML",c("","T"))
glmLK <- paste0("glm","Adult",c("Par","Par","Dec","Dec"),"ML",c("","T"),"K")

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  cc <- c(1,2,3,5,10,20,50)
  m <- act$m
  if(m == 1000){
    cc <- c(cc,100)
  }
  actG <- testGLMK(act,adultODSF,cc,m,testK,testK2,cbind(c(1,9:11),2))
  assign(as.character(glmLK[[i]]),actG)
}

rm(list = sds)

load("sdsAdultMS.RData")

sds <- paste0("sds","AdultCatM",c("P","P","C","C"),"",c("","T"))
glmSK <- paste0("glm","AdultCatM",c("P","P","C","C"),"",c("","T"),"K")

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  cc <- c(1,2,3,5,10,20,50)
  m <- act$m
  if(m == 1000){
    cc <- c(cc,100)
  }
  actG <- testGLMK(act,adultODSF,cc,m,testK,testK2,cbind(c(1,9:11),2))
  assign(as.character(glmSK[[i]]),actG)
}

rm(list = sds)

load("sdsAdultSamp.RData")

glmAdultSampK <- testGLMK(sdsAdultSamp,adultODSF,c(1,2,3,5,10,20,50,100),1000,testK,testK2,cbind(c(1,9:11),2))

save(list = c(glmNK,glmHK,glmLK,glmSK,"glmAdultSampK"),file = "glmAdultK.RData")

## Avila

avilaODSF <- createFac(avilaODS,avilaODS,cbind(c(1:10),2),c(colnames(avilaODS),paste0(colnames(avilaODS)[c(1:10)],"G")))

colnames(avilaODSF)[[21]] <- "MR_ISG"

avilaODSK <- localSuppression(avilaODSF,keyVars = c(11,12:21))$xAnon

colnames(avilaODSF)[[21]] <- "MR/ISG"

colnames(avilaODSK)[[11]] <- "MR/ISG"


testK <- c(relevel(Class,"A") ~ Int_DistG + Up_MargG + Lo_MargG + ExploitG +
             Row_NumG + Mod_RatioG + Int_SpacG + WeightG + Peak_NumG + `MR/ISG`,
           relevel(Class,"B") ~ Int_DistG + Up_MargG + Lo_MargG + ExploitG +
             Row_NumG + Mod_RatioG + Int_SpacG + WeightG + Peak_NumG + `MR/ISG`,
           relevel(Class,"C") ~ Int_DistG + Up_MargG + Lo_MargG + ExploitG +
             Row_NumG + Mod_RatioG + Int_SpacG + WeightG + Peak_NumG + `MR/ISG`,
           relevel(Class,"D") ~ Int_DistG + Up_MargG + Lo_MargG + ExploitG +
             Row_NumG + Mod_RatioG + Int_SpacG + WeightG + Peak_NumG + `MR/ISG`,
           relevel(Class,"E") ~ Int_DistG + Up_MargG + Lo_MargG + ExploitG +
             Row_NumG + Mod_RatioG + Int_SpacG + WeightG + Peak_NumG + `MR/ISG`,
           relevel(Class,"F") ~ Int_DistG + Up_MargG + Lo_MargG + ExploitG +
             Row_NumG + Mod_RatioG + Int_SpacG + WeightG + Peak_NumG + `MR/ISG`,
           relevel(Class,"G") ~ Int_DistG + Up_MargG + Lo_MargG + ExploitG +
             Row_NumG + Mod_RatioG + Int_SpacG + WeightG + Peak_NumG + `MR/ISG`,
           relevel(Class,"H") ~ Int_DistG + Up_MargG + Lo_MargG + ExploitG +
             Row_NumG + Mod_RatioG + Int_SpacG + WeightG + Peak_NumG + `MR/ISG`,
           relevel(Class,"I") ~ Int_DistG + Up_MargG + Lo_MargG + ExploitG +
             Row_NumG + Mod_RatioG + Int_SpacG + WeightG + Peak_NumG + `MR/ISG`,
           relevel(Class,"W") ~ Int_DistG + Up_MargG + Lo_MargG + ExploitG +
             Row_NumG + Mod_RatioG + Int_SpacG + WeightG + Peak_NumG + `MR/ISG`,
           relevel(Class,"X") ~ Int_DistG + Up_MargG + Lo_MargG + ExploitG +
             Row_NumG + Mod_RatioG + Int_SpacG + WeightG + Peak_NumG + `MR/ISG`,
           relevel(Class,"Y") ~ Int_DistG + Up_MargG + Lo_MargG + ExploitG +
             Row_NumG + Mod_RatioG + Int_SpacG + WeightG + Peak_NumG + `MR/ISG`,
           relevel(Class,"A") ~ Int_DistG + Up_MargG + Lo_MargG + ExploitG,
           relevel(Class,"A") ~ Row_NumG + Mod_RatioG + Int_SpacG +
             WeightG + Peak_NumG + `MR/ISG`,
           relevel(Class,"A") ~ Mod_RatioG + Int_SpacG + WeightG + `MR/ISG`,
           relevel(Class,"A") ~ ExploitG + Row_NumG + WeightG + Peak_NumG,
           `MR/ISG` ~ Mod_RatioG*Int_SpacG,
           `MR/ISG` ~ Int_SpacG*Mod_RatioG,
           Row_NumG ~ Class + WeightG + Peak_NumG,
           Mod_RatioG ~ Int_SpacG + `MR/ISG` + Int_DistG,
           Int_SpacG ~ Mod_RatioG + `MR/ISG` + Int_DistG,
           Int_DistG ~ Class + Lo_MargG + Up_MargG,
           ExploitG ~ WeightG + Lo_MargG + Mod_RatioG,
           Up_MargG ~ Lo_MargG + Int_SpacG + `MR/ISG` + WeightG)

testK2 <- rep("binomial",24)


glmAvilaAno <- testGLMA(avilaODSK,avilaODSF,testK,testK2)

load("sdsAvilaM.RData")

sds <- paste0("sds","Avila",c("Par","Par","Dec","Dec"),"M",c("","T"))
glmNK <- paste0("glm","Avila",c("Par","Par","Dec","Dec"),"M",c("","T"),"K")

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <- testGLMK(act,avilaODSF,c(1,2,3,5,10,20,50,100),1000,testK,testK2,cbind(c(1:10),2))
  assign(as.character(glmNK[[i]]),actG)
}

rm(list = sds)

load("sdsAvilaMO.RData")

sds <- paste0("sds","Avila",c("Par","Par","Dec","Dec"),"MO",c("","T"))
glmOK <- paste0("glm","Avila",c("Par","Par","Dec","Dec"),"MO",c("","T"),"K")

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <- testGLMK(act,avilaODSF,c(1,2,3,5,10,20,50,100),1000,testK,testK2,cbind(c(1:10),2))
  assign(as.character(glmOK[[i]]),actG)
}

rm(list = sds)

load("sdsAvilaMV.RData")

sds <- paste0("sds","Avila",c("Par","Par","Dec","Dec"),"MV",c("","T"))
glmVK <- paste0("glm","Avila",c("Par","Par","Dec","Dec"),"MV",c("","T"),"K")

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <- testGLMK(act,avilaODSF,c(1,2,3,5,10,20,50,100),1000,testK,testK2,cbind(c(1:10),2))
  assign(as.character(glmVK[[i]]),actG)
}

rm(list = sds)

load("sdsAvilaMS.RData")

sds <- paste0("sds","AvilaCatM",c("P","P","C","C"),"",c("","T"))
glmSK <- paste0("glm","AvilaCatM",c("P","P","C","C"),"",c("","T"),"K")

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <- testGLMK(act,avilaODSF,c(1,2,3,5,10,20,50,100),1000,testK,testK2,cbind(c(1:10),2))
  assign(as.character(glmSK[[i]]),actG)
}

rm(list = sds)

load("sdsAvilaSamp.RData")

glmAvilaSampK <- testGLMK(sdsAvilaSamp,avilaODSF,c(1,2,3,5,10,20,50,100),1000,testK,testK2,cbind(c(1:10),2))

save(list = c(glmNK,glmOK,glmVK,glmSK,"glmAvilaSampK"),file = "glmAvilaK.RData")


## Avila Selective

avilaODSF <- createFac(avilaODS,avilaODS,cbind(c(1:10),2),c(colnames(avilaODS),paste0(colnames(avilaODS)[c(1:10)],"G")))

colnames(avilaODSF)[[21]] <- "MR_ISG"

avilaODSK <- localSuppression(avilaODSF,keyVars = c(11,12:21))$xAnon

colnames(avilaODSF)[[21]] <- "MR/ISG"

colnames(avilaODSK)[[11]] <- "MR/ISG"


testK <- c(relevel(Class,"A") ~ Int_DistG + Up_MargG + Lo_MargG + ExploitG +
             Row_NumG + Mod_RatioG + Int_SpacG + WeightG + Peak_NumG + `MR/ISG`,
           relevel(Class,"B") ~ Int_DistG + Up_MargG + Lo_MargG + ExploitG +
             Row_NumG + Mod_RatioG + Int_SpacG + WeightG + Peak_NumG + `MR/ISG`,
           relevel(Class,"C") ~ Int_DistG + Up_MargG + Lo_MargG + ExploitG +
             Row_NumG + Mod_RatioG + Int_SpacG + WeightG + Peak_NumG + `MR/ISG`,
           relevel(Class,"D") ~ Int_DistG + Up_MargG + Lo_MargG + ExploitG +
             Row_NumG + Mod_RatioG + Int_SpacG + WeightG + Peak_NumG + `MR/ISG`,
           relevel(Class,"E") ~ Int_DistG + Up_MargG + Lo_MargG + ExploitG +
             Row_NumG + Mod_RatioG + Int_SpacG + WeightG + Peak_NumG + `MR/ISG`,
           relevel(Class,"F") ~ Int_DistG + Up_MargG + Lo_MargG + ExploitG +
             Row_NumG + Mod_RatioG + Int_SpacG + WeightG + Peak_NumG + `MR/ISG`,
           relevel(Class,"G") ~ Int_DistG + Up_MargG + Lo_MargG + ExploitG +
             Row_NumG + Mod_RatioG + Int_SpacG + WeightG + Peak_NumG + `MR/ISG`,
           relevel(Class,"H") ~ Int_DistG + Up_MargG + Lo_MargG + ExploitG +
             Row_NumG + Mod_RatioG + Int_SpacG + WeightG + Peak_NumG + `MR/ISG`,
           relevel(Class,"I") ~ Int_DistG + Up_MargG + Lo_MargG + ExploitG +
             Row_NumG + Mod_RatioG + Int_SpacG + WeightG + Peak_NumG + `MR/ISG`,
           relevel(Class,"W") ~ Int_DistG + Up_MargG + Lo_MargG + ExploitG +
             Row_NumG + Mod_RatioG + Int_SpacG + WeightG + Peak_NumG + `MR/ISG`,
           relevel(Class,"X") ~ Int_DistG + Up_MargG + Lo_MargG + ExploitG +
             Row_NumG + Mod_RatioG + Int_SpacG + WeightG + Peak_NumG + `MR/ISG`,
           relevel(Class,"Y") ~ Int_DistG + Up_MargG + Lo_MargG + ExploitG +
             Row_NumG + Mod_RatioG + Int_SpacG + WeightG + Peak_NumG + `MR/ISG`,
           relevel(Class,"A") ~ Int_DistG + Up_MargG + Lo_MargG + ExploitG,
           relevel(Class,"A") ~ Row_NumG + Mod_RatioG + Int_SpacG +
             WeightG + Peak_NumG + `MR/ISG`,
           relevel(Class,"A") ~ Mod_RatioG + Int_SpacG + WeightG + `MR/ISG`,
           relevel(Class,"A") ~ ExploitG + Row_NumG + WeightG + Peak_NumG,
           `MR/ISG` ~ Mod_RatioG*Int_SpacG,
           `MR/ISG` ~ Int_SpacG*Mod_RatioG,
           Row_NumG ~ Class + WeightG + Peak_NumG,
           Mod_RatioG ~ Int_SpacG + `MR/ISG` + Int_DistG,
           Int_SpacG ~ Mod_RatioG + `MR/ISG` + Int_DistG,
           Int_DistG ~ Class + Lo_MargG + Up_MargG,
           ExploitG ~ WeightG + Lo_MargG + Mod_RatioG,
           Up_MargG ~ Lo_MargG + Int_SpacG + `MR/ISG` + WeightG)

testK2 <- rep("binomial",24)


glmAvilaAno <- testGLMA(avilaODSK,avilaODSF,testK,testK2)

load("sdsAvilaMSe.RData")

sds <- paste0("sds","Avila",c("Par","Par","Dec","Dec"),"M",c("","T"),"Se")
glmNK <- paste0("glm","Avila",c("Par","Par","Dec","Dec"),"M",c("","T"),"SeK")

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <- testGLMK(act,avilaODSF,c(1,2,3,5,10,20,50,100),1000,testK,testK2,cbind(c(1:10),2))
  assign(as.character(glmNK[[i]]),actG)
}

rm(list = sds)

load("sdsAvilaMOSe.RData")

sds <- paste0("sds","Avila",c("Par","Par","Dec","Dec"),"MO",c("","T"),"Se")
glmOK <- paste0("glm","Avila",c("Par","Par","Dec","Dec"),"MO",c("","T"),"SeK")

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <- testGLMK(act,avilaODSF,c(1,2,3,5,10,20,50,100),1000,testK,testK2,cbind(c(1:10),2))
  assign(as.character(glmOK[[i]]),actG)
}

rm(list = sds)

load("sdsAvilaMVSe.RData")

sds <- paste0("sds","Avila",c("Par","Par","Dec","Dec"),"MV",c("","T"),"Se")
glmVK <- paste0("glm","Avila",c("Par","Par","Dec","Dec"),"MV",c("","T"),"SeK")

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <- testGLMK(act,avilaODSF,c(1,2,3,5,10,20,50,100),1000,testK,testK2,cbind(c(1:10),2))
  assign(as.character(glmVK[[i]]),actG)
}

rm(list = sds)

load("sdsAvilaMSSe.RData")

sds <- paste0("sds","AvilaCatM",c("P","P","C","C"),"",c("","T"),"Se")
glmSK <- paste0("glm","AvilaCatM",c("P","P","C","C"),"",c("","T"),"SeK")

for(i in c(1:length(sds))){
  act <- eval(as.name(sds[[i]]))
  actG <- testGLMK(act,avilaODSF,c(1,2,3,5,10,20,50,100),1000,testK,testK2,cbind(c(1:10),2))
  assign(as.character(glmSK[[i]]),actG)
}

rm(list = sds)

save(list = c(glmNK,glmOK,glmVK,glmSK),file = "glmAvilaSeK.RData")