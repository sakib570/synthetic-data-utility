## Author Niklas Reje (nreje@kth.se)
source("lib_avg_cio_apo_kl_dv.R")

## POLISH

load("glmPolishM.RData")
load("glmPolishMO.RData")
load("glmPolishMV.RData")
load("glmPolishMS.RData")


load("estPolishM.RData")
load("estPolishMO.RData")
load("estPolishMV.RData")
load("estPolishMS.RData")


load("kldPolishM.RData")
load("kldPolishMO.RData")
load("kldPolishMV.RData")
load("kldPolishMS.RData")

load("testPolishSamp.RData")


teP2 <- c("S","P","D","PT","DT","PO","DO","POT","DOT","PV","DV","PVT","DVT","CP","CPT","CC","CCT")
glmP2 <- c("glmPolishSamp","glmPolishParM","glmPolishDecM","glmPolishParMT",
           "glmPolishDecMT","glmPolishParMO","glmPolishDecMO","glmPolishParMOT",
           "glmPolishDecMOT","glmPolishParMV","glmPolishDecMV","glmPolishParMVT",
           "glmPolishDecMVT","glmPolishCatMP","glmPolishCatMPT","glmPolishCatMC",
           "glmPolishCatMCT")
estP2 <- c("estPolishSamp","estPolishParM","estPolishDecM","estPolishParMT",
           "estPolishDecMT","estPolishParMO","estPolishDecMO","estPolishParMOT",
           "estPolishDecMOT","estPolishParMV","estPolishDecMV","estPolishParMVT",
           "estPolishDecMVT","estPolishCatMP","estPolishCatMPT","estPolishCatMC",
           "estPolishCatMCT")
kldP2 <- c("kldPolishSamp","kldPolishParM","kldPolishDecM","kldPolishParMT",
           "kldPolishDecMT","kldPolishParMO","kldPolishDecMO","kldPolishParMOT",
           "kldPolishDecMOT","kldPolishParMV","kldPolishDecMV","kldPolishParMVT",
           "kldPolishDecMVT","kldPolishCatMP","kldPolishCatMPT","kldPolishCatMC",
           "kldPolishCatMCT")



DiffavgPPS <- avgModelsDiffPerSamp(glm = glmP2,te = teP2,CI = NULL)

CIavg2PPS <- avgModelsCIPerSamp(2,glm = glmP2,te = teP2,CI = NULL)

CIavg3PPS <- avgModelsCIPerSamp(3,glm = glmP2,te = teP2,CI = NULL)


DiffavgPPS1 <- avgModelsDiffPerSamp(glm = glmP2,te = teP2,CI = 0.1)

CIavg2PPS9 <- avgModelsCIPerSamp(2,glm = glmP2,te = teP2,CI = 0.9)

CIavg3PPS9 <- avgModelsCIPerSamp(3,glm = glmP2,te = teP2,CI = 0.9)


DiffavgEstPPS1 <- avgModelsDiffPerSamp(glm = estP2,te = teP2,CI = 0.1,est = TRUE)

CIavgEst2PPS9 <- avgModelsCIPerSamp(2,glm = estP2,te = teP2,CI = 0.9,est = TRUE)

CIavgEst3PPS9 <- avgModelsCIPerSamp(3,glm = estP2,te = teP2,CI = 0.9,est = TRUE)




KLDavgP <- avgModelsKLD(kld = kldP2,te = teP2)

allRes <- c(paste0("DiffavgPPS",c("","1")),
            paste0("CIavg2PPS",c("","9")),paste0("CIavg3PPS",c("","9")),
            paste0("DiffavgEstPPS",c("1")),
            paste0("CIavgEst2PPS",c("9")),paste0("CIavgEst3PPS",c("9")),
            "KLDavgP")

save(list = allRes,file = "Polish3.RData")

rm(list = glmP2)
rm(list = estP2)
rm(list = kldP2)
rm(list = allRes)

## ADULT

load("glmAdultM.RData")
load("glmAdultMH.RData")
load("glmAdultML.RData")
load("glmAdultMS.RData")


load("estAdultM.RData")
load("estAdultMH.RData")
load("estAdultML.RData")
load("estAdultMS.RData")


load("kldAdultM.RData")
load("kldAdultMH.RData")
load("kldAdultML.RData")
load("kldAdultMS.RData")

load("testAdultSamp.RData")


teA2 <- c("S","P","D","PT","DT","PH","DH","PHT","DHT","PL","DL","PLT","DLT","CP","CPT","CC","CCT")
glmA2 <- c("glmAdultSamp","glmAdultParM","glmAdultDecM","glmAdultParMT",
           "glmAdultDecMT","glmAdultParMH","glmAdultDecMH","glmAdultParMHT",
           "glmAdultDecMHT","glmAdultParML","glmAdultDecML","glmAdultParMLT",
           "glmAdultDecMLT","glmAdultCatMP","glmAdultCatMPT","glmAdultCatMC",
           "glmAdultCatMCT")
estA2 <- c("estAdultSamp","estAdultParM","estAdultDecM","estAdultParMT",
           "estAdultDecMT","estAdultParMH","estAdultDecMH","estAdultParMHT",
           "estAdultDecMHT","estAdultParML","estAdultDecML","estAdultParMLT",
           "estAdultDecMLT","estAdultCatMP","estAdultCatMPT","estAdultCatMC",
           "estAdultCatMCT")
kldA2 <- c("kldAdultSamp","kldAdultParM","kldAdultDecM","kldAdultParMT",
           "kldAdultDecMT","kldAdultParMH","kldAdultDecMH","kldAdultParMHT",
           "kldAdultDecMHT","kldAdultParML","kldAdultDecML","kldAdultParMLT",
           "kldAdultDecMLT","kldAdultCatMP","kldAdultCatMPT","kldAdultCatMC",
           "kldAdultCatMCT")






DiffavgAPS <- avgModelsDiffPerSamp(glm = glmA2,te = teA2,CI = NULL)

CIavg2APS <- avgModelsCIPerSamp(2,glm = glmA2,te = teA2,CI = NULL)

CIavg3APS <- avgModelsCIPerSamp(3,glm = glmA2,te = teA2,CI = NULL)


DiffavgAPS1 <- avgModelsDiffPerSamp(glm = glmA2,te = teA2,CI = 0.1)

CIavg2APS9 <- avgModelsCIPerSamp(2,glm = glmA2,te = teA2,CI = 0.9)

CIavg3APS9 <- avgModelsCIPerSamp(3,glm = glmA2,te = teA2,CI = 0.9)


DiffavgEstAPS1 <- avgModelsDiffPerSamp(glm = estA2,te = teA2,CI = 0.1,est = TRUE)

CIavgEst2APS9 <- avgModelsCIPerSamp(2,glm = estA2,te = teA2,CI = 0.9,est = TRUE)

CIavgEst3APS9 <- avgModelsCIPerSamp(3,glm = estA2,te = teA2,CI = 0.9,est = TRUE)



KLDavgA <- avgModelsKLD(kld = kldA2,te = teA2)

allRes <- c(paste0("DiffavgAPS",c("","1")),
            paste0("CIavg2APS",c("","9")),paste0("CIavg3APS",c("","9")),
            paste0("DiffavgEstAPS",c("1")),
            paste0("CIavgEst2APS",c("9")),paste0("CIavgEst3APS",c("9")),
            "KLDavgA")

save(list = allRes,file = "Adult3.RData")

rm(list = glmA2)
rm(list = estA2)
rm(list = kldA2)
rm(list = allRes)

## Avila

load("glmAvilaM.RData")
load("glmAvilaMO.RData")
load("glmAvilaMV.RData")
load("glmAvilaMS.RData")


load("estAvilaM.RData")
load("estAvilaMO.RData")
load("estAvilaMV.RData")
load("estAvilaMS.RData")


load("kldAvilaM.RData")
load("kldAvilaMO.RData")
load("kldAvilaMV.RData")
load("kldAvilaMS.RData")

load("testAvilaSamp.RData")


teAv2 <- c("S","P","D","PT","DT","PO","DO","POT","DOT","PV","DV","PVT","DVT","CP","CPT","CC","CCT")
glmAv2 <- c("glmAvilaSamp","glmAvilaParM","glmAvilaDecM","glmAvilaParMT",
            "glmAvilaDecMT","glmAvilaParMO","glmAvilaDecMO","glmAvilaParMOT",
            "glmAvilaDecMOT","glmAvilaParMV","glmAvilaDecMV","glmAvilaParMVT",
            "glmAvilaDecMVT","glmAvilaCatMP","glmAvilaCatMPT","glmAvilaCatMC",
            "glmAvilaCatMCT")
estAv2 <- c("estAvilaSamp","estAvilaParM","estAvilaDecM","estAvilaParMT",
            "estAvilaDecMT","estAvilaParMO","estAvilaDecMO","estAvilaParMOT",
            "estAvilaDecMOT","estAvilaParMV","estAvilaDecMV","estAvilaParMVT",
            "estAvilaDecMVT","estAvilaCatMP","estAvilaCatMPT","estAvilaCatMC",
            "estAvilaCatMCT")
kldAv2 <- c("kldAvilaSamp","kldAvilaParM","kldAvilaDecM","kldAvilaParMT",
            "kldAvilaDecMT","kldAvilaParMO","kldAvilaDecMO","kldAvilaParMOT",
            "kldAvilaDecMOT","kldAvilaParMV","kldAvilaDecMV","kldAvilaParMVT",
            "kldAvilaDecMVT","kldAvilaCatMP","kldAvilaCatMPT","kldAvilaCatMC",
            "kldAvilaCatMCT")



DiffavgAvPS <- avgModelsDiffPerSamp(glm = glmAv2,te = teAv2,CI = NULL)

CIavg2AvPS <- avgModelsCIPerSamp(2,glm = glmAv2,te = teAv2,CI = NULL)

CIavg3AvPS <- avgModelsCIPerSamp(3,glm = glmAv2,te = teAv2,CI = NULL)


DiffavgAvPS1 <- avgModelsDiffPerSamp(glm = glmAv2,te = teAv2,CI = 0.1)

CIavg2AvPS9 <- avgModelsCIPerSamp(2,glm = glmAv2,te = teAv2,CI = 0.9)

CIavg3AvPS9 <- avgModelsCIPerSamp(3,glm = glmAv2,te = teAv2,CI = 0.9)


DiffavgEstAvPS1 <- avgModelsDiffPerSamp(glm = estAv2,te = teAv2,CI = 0.1,est = TRUE)

CIavgEst2AvPS9 <- avgModelsCIPerSamp(2,glm = estAv2,te = teAv2,CI = 0.9,est = TRUE)

CIavgEst3AvPS9 <- avgModelsCIPerSamp(3,glm = estAv2,te = teAv2,CI = 0.9,est = TRUE)



KLDavgAv <- avgModelsKLD(kld = kldAv2,te = teAv2)

allRes <- c(paste0("DiffavgAvPS",c("","1")),
            paste0("CIavg2AvPS",c("","9")),paste0("CIavg3AvPS",c("","9")),
            paste0("DiffavgEstAvPS",c("1")),
            paste0("CIavgEst2AvPS",c("9")),paste0("CIavgEst3AvPS",c("9")),
            "KLDavgAv")

save(list = allRes,file = "Avila3.RData")

rm(list = glmAv2)
rm(list = estAv2)
rm(list = kldAv2)
rm(list = allRes)


## Selective

load("glmPolishNSe.RData")
load("glmPolishM.RData")
load("estPolishNSe.RData")
load("estPolishM.RData")
load("kldPolishNSe.RData")
load("kldPolishM.RData")
tePN <- c("P","PSe","D","DSe","PT","PTSe","DT","DTSe")
glmPN <- c("glmPolishParM","glmPolishParMSe","glmPolishDecM",
           "glmPolishDecMSe","glmPolishParMT","glmPolishParMTSe",
           "glmPolishDecMT","glmPolishDecMTSe")
estPN <- c("estPolishParM","estPolishParMSe","estPolishDecM",
           "estPolishDecMSe","estPolishParMT","estPolishParMTSe",
           "estPolishDecMT","estPolishDecMTSe")
kldPN <- c("kldPolishParM","kldPolishParMSe","kldPolishDecM",
           "kldPolishDecMSe","kldPolishParMT","kldPolishParMTSe",
           "kldPolishDecMT","kldPolishDecMTSe")

DiffavgPNPS <- avgModelsDiffPerSamp(glm = glmPN,te = tePN,CI = NULL)

CIavg2PNPS <- avgModelsCIPerSamp(2,glm = glmPN,te = tePN,CI = NULL)
CIavg3PNPS <- avgModelsCIPerSamp(3,glm = glmPN,te = tePN,CI = NULL)


DiffavgPNPS1 <- avgModelsDiffPerSamp(glm = glmPN,te = tePN,CI = 0.1)

CIavg2PNPS9 <- avgModelsCIPerSamp(2,glm = glmPN,te = tePN,CI = 0.9)
CIavg3PNPS9 <- avgModelsCIPerSamp(3,glm = glmPN,te = tePN,CI = 0.9)



DiffavgEstPNPS1 <- avgModelsDiffPerSamp(glm = estPN,te = tePN,CI = 0.1,est = TRUE)

CIavgEst2PNPS9 <- avgModelsCIPerSamp(2,glm = estPN,te = tePN,CI = 0.9,est = TRUE)
CIavgEst3PNPS9 <- avgModelsCIPerSamp(3,glm = estPN,te = tePN,CI = 0.9,est = TRUE)


KLDavgPN <- avgModelsKLD(kld = kldPN,te = tePN)

rm(list = glmPN)
rm(list = estPN)
rm(list = kldPN)


load("glmPolishOSe.RData")
load("glmPolishMO.RData")
load("estPolishOSe.RData")
load("estPolishMO.RData")
load("kldPolishOSe.RData")
load("kldPolishMO.RData")
tePO <- c("PO","POSe","DO","DOSe","POT","POTSe","DOT","DOTSe")
glmPO <- c("glmPolishParMO","glmPolishParMOSe","glmPolishDecMO",
           "glmPolishDecMOSe","glmPolishParMOT","glmPolishParMOTSe",
           "glmPolishDecMOT","glmPolishDecMOTSe")
estPO <- c("estPolishParMO","estPolishParMOSe","estPolishDecMO",
           "estPolishDecMOSe","estPolishParMOT","estPolishParMOTSe",
           "estPolishDecMOT","estPolishDecMOTSe")
kldPO <- c("kldPolishParMO","kldPolishParMOSe","kldPolishDecMO",
           "kldPolishDecMOSe","kldPolishParMOT","kldPolishParMOTSe",
           "kldPolishDecMOT","kldPolishDecMOTSe")

DiffavgPOPS <- avgModelsDiffPerSamp(glm = glmPO,te = tePO,CI = NULL)

CIavg2POPS <- avgModelsCIPerSamp(2,glm = glmPO,te = tePO,CI = NULL)
CIavg3POPS <- avgModelsCIPerSamp(3,glm = glmPO,te = tePO,CI = NULL)


DiffavgPOPS1 <- avgModelsDiffPerSamp(glm = glmPO,te = tePO,CI = 0.1)

CIavg2POPS9 <- avgModelsCIPerSamp(2,glm = glmPO,te = tePO,CI = 0.9)
CIavg3POPS9 <- avgModelsCIPerSamp(3,glm = glmPO,te = tePO,CI = 0.9)


DiffavgEstPOPS1 <- avgModelsDiffPerSamp(glm = estPO,te = tePO,CI = 0.1,est = TRUE)

CIavgEst2POPS9 <- avgModelsCIPerSamp(2,glm = estPO,te = tePO,CI = 0.9,est = TRUE)
CIavgEst3POPS9 <- avgModelsCIPerSamp(3,glm = estPO,te = tePO,CI = 0.9,est = TRUE)



KLDavgPO <- avgModelsKLD(kld = kldPO,te = tePO)


rm(list = glmPO)
rm(list = estPO)
rm(list = kldPO)


load("glmPolishVSe.RData")
load("glmPolishMV.RData")
load("estPolishVSe.RData")
load("estPolishMV.RData")
load("kldPolishVSe.RData")
load("kldPolishMV.RData")
tePV <- c("PV","PVSe","DV","DVSe","PVT","PVTSe","DVT","DVTSe")
glmPV <- c("glmPolishParMV","glmPolishParMVSe","glmPolishDecMV",
           "glmPolishDecMVSe","glmPolishParMVT","glmPolishParMVTSe",
           "glmPolishDecMVT","glmPolishDecMVTSe")
estPV <- c("estPolishParMV","estPolishParMVSe","estPolishDecMV",
           "estPolishDecMVSe","estPolishParMVT","estPolishParMVTSe",
           "estPolishDecMVT","estPolishDecMVTSe")
kldPV <- c("kldPolishParMV","kldPolishParMVSe","kldPolishDecMV",
           "kldPolishDecMVSe","kldPolishParMVT","kldPolishParMVTSe",
           "kldPolishDecMVT","kldPolishDecMVTSe")

DiffavgPVPS <- avgModelsDiffPerSamp(glm = glmPV,te = tePV,CI = NULL)

CIavg2PVPS <- avgModelsCIPerSamp(2,glm = glmPV,te = tePV,CI = NULL)
CIavg3PVPS <- avgModelsCIPerSamp(3,glm = glmPV,te = tePV,CI = NULL)


DiffavgPVPS1 <- avgModelsDiffPerSamp(glm = glmPV,te = tePV,CI = 0.1)

CIavg2PVPS9 <- avgModelsCIPerSamp(2,glm = glmPV,te = tePV,CI = 0.9)
CIavg3PVPS9 <- avgModelsCIPerSamp(3,glm = glmPV,te = tePV,CI = 0.9)


DiffavgEstPVPS1 <- avgModelsDiffPerSamp(glm = estPV,te = tePV,CI = 0.1,est = TRUE)

CIavgEst2PVPS9 <- avgModelsCIPerSamp(2,glm = estPV,te = tePV,CI = 0.9,est = TRUE)
CIavgEst3PVPS9 <- avgModelsCIPerSamp(3,glm = estPV,te = tePV,CI = 0.9,est = TRUE)



KLDavgPV <- avgModelsKLD(kld = kldPV,te = tePV)

rm(list = glmPV)
rm(list = estPV)
rm(list = kldPV)


load("glmPolishSSe.RData")
load("glmPolishMS.RData")
load("estPolishSSe.RData")
load("estPolishMS.RData")
load("kldPolishSSe.RData")
load("kldPolishMS.RData")
tePS <- c("CP","CPSe","CC","CCSe","CPT","CPTSe","CCT","CCTSe")
glmPS <- paste0("glm","Polish","Cat","M",c("P","PSe","C","CSe","PT","PTSe","CT","CTSe"))
estPS <- paste0("est","Polish","Cat","M",c("P","PSe","C","CSe","PT","PTSe","CT","CTSe"))
kldPS <- paste0("kld","Polish","Cat","M",c("P","PSe","C","CSe","PT","PTSe","CT","CTSe"))

DiffavgPSPS <- avgModelsDiffPerSamp(glm = glmPS,te = tePS,CI = NULL)

CIavg2PSPS <- avgModelsCIPerSamp(2,glm = glmPS,te = tePS,CI = NULL)
CIavg3PSPS <- avgModelsCIPerSamp(3,glm = glmPS,te = tePS,CI = NULL)


DiffavgPSPS1 <- avgModelsDiffPerSamp(glm = glmPS,te = tePS,CI = 0.1)

CIavg2PSPS9 <- avgModelsCIPerSamp(2,glm = glmPS,te = tePS,CI = 0.9)
CIavg3PSPS9 <- avgModelsCIPerSamp(3,glm = glmPS,te = tePS,CI = 0.9)


DiffavgEstPSPS1 <- avgModelsDiffPerSamp(glm = estPS,te = tePS,CI = 0.1,est = TRUE)

CIavgEst2PSPS9 <- avgModelsCIPerSamp(2,glm = estPS,te = tePS,CI = 0.9,est = TRUE)
CIavgEst3PSPS9 <- avgModelsCIPerSamp(3,glm = estPS,te = tePS,CI = 0.9,est = TRUE)



KLDavgPS <- avgModelsKLD(kld = kldPS,te = tePS)

rm(list = glmPS)
rm(list = estPS)
rm(list = kldPS)

resPol <- c(paste0("DiffavgP",c("N","N","O","O","V","V","S","S"),"PS",c("",1)),
            paste0("CIavg2P",c("N","N","O","O","V","V","S","S"),"PS",c("",9)),
            paste0("CIavg3P",c("N","N","O","O","V","V","S","S"),"PS",c("",9)),
            paste0("DiffavgEstP",c("N","O","V","S"),"PS",1),
            paste0("CIavgEst2P",c("N","O","V","S"),"PS",9),
            paste0("CIavgEst3P",c("N","O","V","S"),"PS",9),
            paste0("KLDavgP",c("N","O","V","S")))

save(list = resPol, file = "PolishSe.RData")

rm(list = resPol)



load("glmAdultMSe.RData")
load("glmAdultM.RData")
load("estAdultMSe.RData")
load("estAdultM.RData")
load("kldAdultMSe.RData")
load("kldAdultM.RData")
tePN <- c("P","PSe","D","DSe","PT","PTSe","DT","DTSe")
glmPN <- c("glmAdultParM","glmAdultParMSe","glmAdultDecM",
           "glmAdultDecMSe","glmAdultParMT","glmAdultParMTSe",
           "glmAdultDecMT","glmAdultDecMTSe")
estPN <- c("estAdultParM","estAdultParMSe","estAdultDecM",
           "estAdultDecMSe","estAdultParMT","estAdultParMTSe",
           "estAdultDecMT","estAdultDecMTSe")
kldPN <- c("kldAdultParM","kldAdultParMSe","kldAdultDecM",
           "kldAdultDecMSe","kldAdultParMT","kldAdultParMTSe",
           "kldAdultDecMT","kldAdultDecMTSe")

DiffavgANPS <- avgModelsDiffPerSamp(glm = glmPN,te = tePN,CI = NULL)

CIavg2ANPS <- avgModelsCIPerSamp(2,glm = glmPN,te = tePN,CI = NULL)
CIavg3ANPS <- avgModelsCIPerSamp(3,glm = glmPN,te = tePN,CI = NULL)


DiffavgANPS1 <- avgModelsDiffPerSamp(glm = glmPN,te = tePN,CI = 0.1)

CIavg2ANPS9 <- avgModelsCIPerSamp(2,glm = glmPN,te = tePN,CI = 0.9)
CIavg3ANPS9 <- avgModelsCIPerSamp(3,glm = glmPN,te = tePN,CI = 0.9)


DiffavgEstANPS1 <- avgModelsDiffPerSamp(glm = estPN,te = tePN,CI = 0.1,est = TRUE)

CIavgEst2ANPS9 <- avgModelsCIPerSamp(2,glm = estPN,te = tePN,CI = 0.9,est = TRUE)
CIavgEst3ANPS9 <- avgModelsCIPerSamp(3,glm = estPN,te = tePN,CI = 0.9,est = TRUE)



KLDavgAN <- avgModelsKLD(kld = kldPN,te = tePN)

rm(list = glmPN)
rm(list = estPN)
rm(list = kldPN)


load("glmAdultMHSe.RData")
load("glmAdultMH.RData")
load("estAdultMHSe.RData")
load("estAdultMH.RData")
load("kldAdultMHSe.RData")
load("kldAdultMH.RData")
tePO <- c("PH","PHSe","DH","DHSe","PHT","PHTSe","DHT","DHTSe")
glmPO <- c("glmAdultParMH","glmAdultParMHSe","glmAdultDecMH",
           "glmAdultDecMHSe","glmAdultParMHT","glmAdultParMHTSe",
           "glmAdultDecMHT","glmAdultDecMHTSe")
estPO <- c("estAdultParMH","estAdultParMHSe","estAdultDecMH",
           "estAdultDecMHSe","estAdultParMHT","estAdultParMHTSe",
           "estAdultDecMHT","estAdultDecMHTSe")
kldPO <- c("kldAdultParMH","kldAdultParMHSe","kldAdultDecMH",
           "kldAdultDecMHSe","kldAdultParMHT","kldAdultParMHTSe",
           "kldAdultDecMHT","kldAdultDecMHTSe")

DiffavgAOPS <- avgModelsDiffPerSamp(glm = glmPO,te = tePO,CI = NULL)

CIavg2AOPS <- avgModelsCIPerSamp(2,glm = glmPO,te = tePO,CI = NULL)
CIavg3AOPS <- avgModelsCIPerSamp(3,glm = glmPO,te = tePO,CI = NULL)


DiffavgAOPS1 <- avgModelsDiffPerSamp(glm = glmPO,te = tePO,CI = 0.1)

CIavg2AOPS9 <- avgModelsCIPerSamp(2,glm = glmPO,te = tePO,CI = 0.9)
CIavg3AOPS9 <- avgModelsCIPerSamp(3,glm = glmPO,te = tePO,CI = 0.9)


DiffavgEstAOPS1 <- avgModelsDiffPerSamp(glm = estPO,te = tePO,CI = 0.1,est = TRUE)

CIavgEst2AOPS9 <- avgModelsCIPerSamp(2,glm = estPO,te = tePO,CI = 0.9,est = TRUE)
CIavgEst3AOPS9 <- avgModelsCIPerSamp(3,glm = estPO,te = tePO,CI = 0.9,est = TRUE)



KLDavgAO <- avgModelsKLD(kld = kldPO,te = tePO)

rm(list = glmPO)
rm(list = estPO)
rm(list = kldPO)


load("glmAdultMLSe.RData")
load("glmAdultML.RData")
load("estAdultMLSe.RData")
load("estAdultML.RData")
load("kldAdultMLSe.RData")
load("kldAdultML.RData")
tePV <- c("PL","PLSe","DL","DLSe","PLT","PLTSe","DLT","DLTSe")
glmPV <- c("glmAdultParML","glmAdultParMLSe","glmAdultDecML",
           "glmAdultDecMLSe","glmAdultParMLT","glmAdultParMLTSe",
           "glmAdultDecMLT","glmAdultDecMLTSe")
estPV <- c("estAdultParML","estAdultParMLSe","estAdultDecML",
           "estAdultDecMLSe","estAdultParMLT","estAdultParMLTSe",
           "estAdultDecMLT","estAdultDecMLTSe")
kldPV <- c("kldAdultParML","kldAdultParMLSe","kldAdultDecML",
           "kldAdultDecMLSe","kldAdultParMLT","kldAdultParMLTSe",
           "kldAdultDecMLT","kldAdultDecMLTSe")

DiffavgAVPS <- avgModelsDiffPerSamp(glm = glmPV,te = tePV,CI = NULL)

CIavg2AVPS <- avgModelsCIPerSamp(2,glm = glmPV,te = tePV,CI = NULL)
CIavg3AVPS <- avgModelsCIPerSamp(3,glm = glmPV,te = tePV,CI = NULL)


DiffavgAVPS1 <- avgModelsDiffPerSamp(glm = glmPV,te = tePV,CI = 0.1)

CIavg2AVPS9 <- avgModelsCIPerSamp(2,glm = glmPV,te = tePV,CI = 0.9)
CIavg3AVPS9 <- avgModelsCIPerSamp(3,glm = glmPV,te = tePV,CI = 0.9)


DiffavgEstAVPS1 <- avgModelsDiffPerSamp(glm = estPV,te = tePV,CI = 0.1,est = TRUE)

CIavgEst2AVPS9 <- avgModelsCIPerSamp(2,glm = estPV,te = tePV,CI = 0.9,est = TRUE)
CIavgEst3AVPS9 <- avgModelsCIPerSamp(3,glm = estPV,te = tePV,CI = 0.9,est = TRUE)



KLDavgAV <- avgModelsKLD(kld = kldPV,te = tePV)

rm(list = glmPV)
rm(list = estPV)
rm(list = kldPV)


load("glmAdultMSSe.RData")
load("glmAdultMS.RData")
load("estAdultMSSe.RData")
load("estAdultMS.RData")
load("kldAdultMSSe.RData")
load("kldAdultMS.RData")
tePS <- c("CP","CPSe","CC","CCSe","CPT","CPTSe","CCT","CCTSe")
glmPS <- paste0("glm","Adult","Cat","M",c("P","PSe","C","CSe","PT","PTSe","CT","CTSe"))
estPS <- paste0("est","Adult","Cat","M",c("P","PSe","C","CSe","PT","PTSe","CT","CTSe"))
kldPS <- paste0("kld","Adult","Cat","M",c("P","PSe","C","CSe","PT","PTSe","CT","CTSe"))

DiffavgASPS <- avgModelsDiffPerSamp(glm = glmPS,te = tePS,CI = NULL)

CIavg2ASPS <- avgModelsCIPerSamp(2,glm = glmPS,te = tePS,CI = NULL)
CIavg3ASPS <- avgModelsCIPerSamp(3,glm = glmPS,te = tePS,CI = NULL)


DiffavgASPS1 <- avgModelsDiffPerSamp(glm = glmPS,te = tePS,CI = 0.1)

CIavg2ASPS9 <- avgModelsCIPerSamp(2,glm = glmPS,te = tePS,CI = 0.9)
CIavg3ASPS9 <- avgModelsCIPerSamp(3,glm = glmPS,te = tePS,CI = 0.9)


DiffavgEstASPS1 <- avgModelsDiffPerSamp(glm = estPS,te = tePS,CI = 0.1,est = TRUE)

CIavgEst2ASPS9 <- avgModelsCIPerSamp(2,glm = estPS,te = tePS,CI = 0.9,est = TRUE)
CIavgEst3ASPS9 <- avgModelsCIPerSamp(3,glm = estPS,te = tePS,CI = 0.9,est = TRUE)



KLDavgAS <- avgModelsKLD(kld = kldPS,te = tePS)

rm(list = glmPS)
rm(list = estPS)
rm(list = kldPS)

resAdu <- c(paste0("DiffavgA",c("N","N","O","O","V","V","S","S"),"PS",c("",1)),
            paste0("CIavg2A",c("N","N","O","O","V","V","S","S"),"PS",c("",9)),
            paste0("CIavg3A",c("N","N","O","O","V","V","S","S"),"PS",c("",9)),
            paste0("DiffavgEstA",c("N","O","V","S"),"PS",1),
            paste0("CIavgEst2A",c("N","O","V","S"),"PS",9),
            paste0("CIavgEst3A",c("N","O","V","S"),"PS",9),
            paste0("KLDavgA",c("N","O","V","S")))

save(list = resAdu, file = "AdultSe.RData")

rm(list = resAdu)



load("glmAvilaNSe.RData")
load("glmAvilaM.RData")
load("estAvilaNSe.RData")
load("estAvilaM.RData")
load("kldAvilaNSe.RData")
load("kldAvilaM.RData")
tePN <- c("P","PSe","D","DSe","PT","PTSe","DT","DTSe")
glmPN <- c("glmAvilaParM","glmAvilaParMSe","glmAvilaDecM",
           "glmAvilaDecMSe","glmAvilaParMT","glmAvilaParMTSe",
           "glmAvilaDecMT","glmAvilaDecMTSe")
estPN <- c("estAvilaParM","estAvilaParMSe","estAvilaDecM",
           "estAvilaDecMSe","estAvilaParMT","estAvilaParMTSe",
           "estAvilaDecMT","estAvilaDecMTSe")
kldPN <- c("kldAvilaParM","kldAvilaParMSe","kldAvilaDecM",
           "kldAvilaDecMSe","kldAvilaParMT","kldAvilaParMTSe",
           "kldAvilaDecMT","kldAvilaDecMTSe")

DiffavgAvNPS <- avgModelsDiffPerSamp(glm = glmPN,te = tePN,CI = NULL)

CIavg2AvNPS <- avgModelsCIPerSamp(2,glm = glmPN,te = tePN,CI = NULL)
CIavg3AvNPS <- avgModelsCIPerSamp(3,glm = glmPN,te = tePN,CI = NULL)


DiffavgAvNPS1 <- avgModelsDiffPerSamp(glm = glmPN,te = tePN,CI = 0.1)

CIavg2AvNPS9 <- avgModelsCIPerSamp(2,glm = glmPN,te = tePN,CI = 0.9)
CIavg3AvNPS9 <- avgModelsCIPerSamp(3,glm = glmPN,te = tePN,CI = 0.9)


DiffavgEstAvNPS1 <- avgModelsDiffPerSamp(glm = estPN,te = tePN,CI = 0.1,est = TRUE)

CIavgEst2AvNPS9 <- avgModelsCIPerSamp(2,glm = estPN,te = tePN,CI = 0.9,est = TRUE)
CIavgEst3AvNPS9 <- avgModelsCIPerSamp(3,glm = estPN,te = tePN,CI = 0.9,est = TRUE)



KLDavgAvN <- avgModelsKLD(kld = kldPN,te = tePN)
rm(list = glmPN)
rm(list = estPN)
rm(list = kldPN)


load("glmAvilaOSe.RData")
load("glmAvilaMO.RData")
load("estAvilaOSe.RData")
load("estAvilaMO.RData")
load("kldAvilaOSe.RData")
load("kldAvilaMO.RData")
tePO <- c("PO","POSe","DO","DOSe","POT","POTSe","DOT","DOTSe")
glmPO <- c("glmAvilaParMO","glmAvilaParMOSe","glmAvilaDecMO",
           "glmAvilaDecMOSe","glmAvilaParMOT","glmAvilaParMOTSe",
           "glmAvilaDecMOT","glmAvilaDecMOTSe")
estPO <- c("estAvilaParMO","estAvilaParMOSe","estAvilaDecMO",
           "estAvilaDecMOSe","estAvilaParMOT","estAvilaParMOTSe",
           "estAvilaDecMOT","estAvilaDecMOTSe")
kldPO <- c("kldAvilaParMO","kldAvilaParMOSe","kldAvilaDecMO",
           "kldAvilaDecMOSe","kldAvilaParMOT","kldAvilaParMOTSe",
           "kldAvilaDecMOT","kldAvilaDecMOTSe")

DiffavgAvOPS <- avgModelsDiffPerSamp(glm = glmPO,te = tePO,CI = NULL)

CIavg2AvOPS <- avgModelsCIPerSamp(2,glm = glmPO,te = tePO,CI = NULL)
CIavg3AvOPS <- avgModelsCIPerSamp(3,glm = glmPO,te = tePO,CI = NULL)


DiffavgAvOPS1 <- avgModelsDiffPerSamp(glm = glmPO,te = tePO,CI = 0.1)

CIavg2AvOPS9 <- avgModelsCIPerSamp(2,glm = glmPO,te = tePO,CI = 0.9)
CIavg3AvOPS9 <- avgModelsCIPerSamp(3,glm = glmPO,te = tePO,CI = 0.9)


DiffavgEstAvOPS1 <- avgModelsDiffPerSamp(glm = estPO,te = tePO,CI = 0.1,est = TRUE)

CIavgEst2AvOPS9 <- avgModelsCIPerSamp(2,glm = estPO,te = tePO,CI = 0.9,est = TRUE)
CIavgEst3AvOPS9 <- avgModelsCIPerSamp(3,glm = estPO,te = tePO,CI = 0.9,est = TRUE)



KLDavgAvO <- avgModelsKLD(kld = kldPO,te = tePO)

rm(list = glmPO)
rm(list = estPO)
rm(list = kldPO)


load("glmAvilaVSe.RData")
load("glmAvilaMV.RData")
load("estAvilaVSe.RData")
load("estAvilaMV.RData")
load("kldAvilaVSe.RData")
load("kldAvilaMV.RData")
tePV <- c("PV","PVSe","DV","DVSe","PVT","PVTSe","DVT","DVTSe")
glmPV <- c("glmAvilaParMV","glmAvilaParMVSe","glmAvilaDecMV",
           "glmAvilaDecMVSe","glmAvilaParMVT","glmAvilaParMVTSe",
           "glmAvilaDecMVT","glmAvilaDecMVTSe")
estPV <- c("estAvilaParMV","estAvilaParMVSe","estAvilaDecMV",
           "estAvilaDecMVSe","estAvilaParMVT","estAvilaParMVTSe",
           "estAvilaDecMVT","estAvilaDecMVTSe")
kldPV <- c("kldAvilaParMV","kldAvilaParMVSe","kldAvilaDecMV",
           "kldAvilaDecMVSe","kldAvilaParMVT","kldAvilaParMVTSe",
           "kldAvilaDecMVT","kldAvilaDecMVTSe")

DiffavgAvVPS <- avgModelsDiffPerSamp(glm = glmPV,te = tePV,CI = NULL)

CIavg2AvVPS <- avgModelsCIPerSamp(2,glm = glmPV,te = tePV,CI = NULL)
CIavg3AvVPS <- avgModelsCIPerSamp(3,glm = glmPV,te = tePV,CI = NULL)


DiffavgAvVPS1 <- avgModelsDiffPerSamp(glm = glmPV,te = tePV,CI = 0.1)

CIavg2AvVPS9 <- avgModelsCIPerSamp(2,glm = glmPV,te = tePV,CI = 0.9)
CIavg3AvVPS9 <- avgModelsCIPerSamp(3,glm = glmPV,te = tePV,CI = 0.9)


DiffavgEstAvVPS1 <- avgModelsDiffPerSamp(glm = estPV,te = tePV,CI = 0.1,est = TRUE)

CIavgEst2AvVPS9 <- avgModelsCIPerSamp(2,glm = estPV,te = tePV,CI = 0.9,est = TRUE)
CIavgEst3AvVPS9 <- avgModelsCIPerSamp(3,glm = estPV,te = tePV,CI = 0.9,est = TRUE)



KLDavgAvV <- avgModelsKLD(kld = kldPV,te = tePV)

rm(list = glmPV)
rm(list = estPV)
rm(list = kldPV)


load("glmAvilaSSe.RData")
load("glmAvilaMS.RData")
load("estAvilaSSe.RData")
load("estAvilaMS.RData")
load("kldAvilaSSe.RData")
load("kldAvilaMS.RData")
tePS <- c("CP","CPSe","CC","CCSe","CPT","CPTSe","CCT","CCTSe")
glmPS <- paste0("glm","Avila","Cat","M",c("P","PSe","C","CSe","PT","PTSe","CT","CTSe"))
estPS <- paste0("est","Avila","Cat","M",c("P","PSe","C","CSe","PT","PTSe","CT","CTSe"))
kldPS <- paste0("kld","Avila","Cat","M",c("P","PSe","C","CSe","PT","PTSe","CT","CTSe"))

DiffavgAvSPS <- avgModelsDiffPerSamp(glm = glmPS,te = tePS,CI = NULL)

CIavg2AvSPS <- avgModelsCIPerSamp(2,glm = glmPS,te = tePS,CI = NULL)
CIavg3AvSPS <- avgModelsCIPerSamp(3,glm = glmPS,te = tePS,CI = NULL)


DiffavgAvSPS1 <- avgModelsDiffPerSamp(glm = glmPS,te = tePS,CI = 0.1)

CIavg2AvSPS9 <- avgModelsCIPerSamp(2,glm = glmPS,te = tePS,CI = 0.9)
CIavg3AvSPS9 <- avgModelsCIPerSamp(3,glm = glmPS,te = tePS,CI = 0.9)


DiffavgEstAvSPS1 <- avgModelsDiffPerSamp(glm = estPS,te = tePS,CI = 0.1,est = TRUE)

CIavgEst2AvSPS9 <- avgModelsCIPerSamp(2,glm = estPS,te = tePS,CI = 0.9,est = TRUE)
CIavgEst3AvSPS9 <- avgModelsCIPerSamp(3,glm = estPS,te = tePS,CI = 0.9,est = TRUE)



KLDavgAvS <- avgModelsKLD(kld = kldPS,te = tePS)

rm(list = glmPS)
rm(list = estPS)
rm(list = kldPS)

resAvi <- c(paste0("DiffavgAv",c("N","N","O","O","V","V","S","S"),"PS",c("",1)),
            paste0("CIavg2Av",c("N","N","O","O","V","V","S","S"),"PS",c("",9)),
            paste0("CIavg3Av",c("N","N","O","O","V","V","S","S"),"PS",c("",9)),
            paste0("DiffavgEstAv",c("N","O","V","S"),"PS",1),
            paste0("CIavgEst2Av",c("N","O","V","S"),"PS",9),
            paste0("CIavgEst3Av",c("N","O","V","S"),"PS",9),
            paste0("KLDavgAv",c("N","O","V","S")))

save(list = resAvi, file = "AvilaSe.RData")

rm(list = resAvi)

## K-Anonymity

load("glmPolishK.RData")

tePK <- c("S","P","D","PT","DT","PO","DO","POT","DOT","PV","DV","PVT","DVT","CP","CPT","CC","CCT")
glmPK <- c("glmPolishSampK","glmPolishParMK","glmPolishDecMK","glmPolishParMTK",
           "glmPolishDecMTK","glmPolishParMOK","glmPolishDecMOK","glmPolishParMOTK",
           "glmPolishDecMOTK","glmPolishParMVK","glmPolishDecMVK","glmPolishParMVTK",
           "glmPolishDecMVTK","glmPolishCatMPK","glmPolishCatMPTK","glmPolishCatMCK",
           "glmPolishCatMCTK")


DiffavgPPSK <- avgModelsDiffPerSamp(glm = glmPK,te = tePK,CI = NULL)

CIavg2PPSK <- avgModelsCIPerSamp(2,glm = glmPK,te = tePK,CI = NULL)

CIavg3PPSK <- avgModelsCIPerSamp(3,glm = glmPK,te = tePK,CI = NULL)


DiffavgPPS1K <- avgModelsDiffPerSamp(glm = glmPK,te = tePK,CI = 0.1)

CIavg2PPS9K <- avgModelsCIPerSamp(2,glm = glmPK,te = tePK,CI = 0.9)

CIavg3PPS9K <- avgModelsCIPerSamp(3,glm = glmPK,te = tePK,CI = 0.9)

allRes <- c(paste0("DiffavgPPS",c("K","1K")),
            paste0("CIavg2PPS",c("K","9K")),paste0("CIavg3PPS",c("K","9K")))

save(list = allRes,file = "PolishK.RData")

rm(list = glmPK)
rm(list = allRes)


load("glmAdultK.RData")

teAK <- c("S","P","D","PT","DT","PH","DH","PHT","DHT","PL","DL","PLT","DLT","CP","CPT","CC","CCT")
glmAK <- c("glmAdultSampK","glmAdultParMK","glmAdultDecMK","glmAdultParMTK",
           "glmAdultDecMTK","glmAdultParMHK","glmAdultDecMHK","glmAdultParMHTK",
           "glmAdultDecMHTK","glmAdultParMLK","glmAdultDecMLK","glmAdultParMLTK",
           "glmAdultDecMLTK","glmAdultCatMPK","glmAdultCatMPTK","glmAdultCatMCK",
           "glmAdultCatMCTK")


DiffavgAPSK <- avgModelsDiffPerSamp(glm = glmAK,te = teAK,CI = NULL)

CIavg2APSK <- avgModelsCIPerSamp(2,glm = glmAK,te = teAK,CI = NULL)

CIavg3APSK <- avgModelsCIPerSamp(3,glm = glmAK,te = teAK,CI = NULL)


DiffavgAPS1K <- avgModelsDiffPerSamp(glm = glmAK,te = teAK,CI = 0.1)

CIavg2APS9K <- avgModelsCIPerSamp(2,glm = glmAK,te = teAK,CI = 0.9)

CIavg3APS9K <- avgModelsCIPerSamp(3,glm = glmAK,te = teAK,CI = 0.9)

allRes <- c(paste0("DiffavgAPS",c("K","1K")),
            paste0("CIavg2APS",c("K","9K")),paste0("CIavg3APS",c("K","9K")))

save(list = allRes,file = "AdultK.RData")

rm(list = glmAK)
rm(list = allRes)


load("glmAvilaK.RData")

teAvK <- c("S","P","D","PT","DT","PO","DO","POT","DOT","PV","DV","PVT","DVT","CP","CPT","CC","CCT")
glmAvK <- c("glmAvilaSampK","glmAvilaParMK","glmAvilaDecMK","glmAvilaParMTK",
            "glmAvilaDecMTK","glmAvilaParMOK","glmAvilaDecMOK","glmAvilaParMOTK",
            "glmAvilaDecMOTK","glmAvilaParMVK","glmAvilaDecMVK","glmAvilaParMVTK",
            "glmAvilaDecMVTK","glmAvilaCatMPK","glmAvilaCatMPTK","glmAvilaCatMCK",
            "glmAvilaCatMCTK")


DiffavgAvPSK <- avgModelsDiffPerSamp(glm = glmAvK,te = teAvK,CI = NULL)

CIavg2AvPSK <- avgModelsCIPerSamp(2,glm = glmAvK,te = teAvK,CI = NULL)

CIavg3AvPSK <- avgModelsCIPerSamp(3,glm = glmAvK,te = teAvK,CI = NULL)


DiffavgAvPS1K <- avgModelsDiffPerSamp(glm = glmAvK,te = teAvK,CI = 0.1)

CIavg2AvPS9K <- avgModelsCIPerSamp(2,glm = glmAvK,te = teAvK,CI = 0.9)

CIavg3AvPS9K <- avgModelsCIPerSamp(3,glm = glmAvK,te = teAvK,CI = 0.9)

allRes <- c(paste0("DiffavgAvPS",c("K","1K")),
            paste0("CIavg2AvPS",c("K","9K")),paste0("CIavg3AvPS",c("K","9K")))

save(list = allRes,file = "AvilaK.RData")

rm(list = glmAvK)
rm(list = allRes)



load("glmAvilaSeK.RData")

teAvSeK <- c("P","D","PT","DT","PO","DO","POT","DOT","PV","DV","PVT","DVT","CP","CPT","CC","CCT")
glmAvSeK <- c("glmAvilaParMSeK","glmAvilaDecMSeK","glmAvilaParMTSeK",
              "glmAvilaDecMTSeK","glmAvilaParMOSeK","glmAvilaDecMOSeK","glmAvilaParMOTSeK",
              "glmAvilaDecMOTSeK","glmAvilaParMVSeK","glmAvilaDecMVSeK","glmAvilaParMVTSeK",
              "glmAvilaDecMVTSeK","glmAvilaCatMPSeK","glmAvilaCatMPTSeK","glmAvilaCatMCSeK",
              "glmAvilaCatMCTSeK")


DiffavgAvPSSeK <- avgModelsDiffPerSamp(glm = glmAvSeK,te = teAvSeK,CI = NULL)

CIavg2AvPSSeK <- avgModelsCIPerSamp(2,glm = glmAvSeK,te = teAvSeK,CI = NULL)

CIavg3AvPSSeK <- avgModelsCIPerSamp(3,glm = glmAvSeK,te = teAvSeK,CI = NULL)


DiffavgAvPS1SeK <- avgModelsDiffPerSamp(glm = glmAvSeK,te = teAvSeK,CI = 0.1)

CIavg2AvPS9SeK <- avgModelsCIPerSamp(2,glm = glmAvSeK,te = teAvSeK,CI = 0.9)

CIavg3AvPS9SeK <- avgModelsCIPerSamp(3,glm = glmAvSeK,te = teAvSeK,CI = 0.9)

allRes <- c(paste0("DiffavgAvPS",c("SeK","1SeK")),
            paste0("CIavg2AvPS",c("SeK","9SeK")),paste0("CIavg3AvPS",c("SeK","9SeK")))

save(list = allRes,file = "AvilaSeK.RData")

rm(list = glmAvK)
rm(list = allRes)



## Plots

lst <- list(c("S","darkorange","1","19"),
            c("P","darkred","1","19"),c("D","navy","1","19"),
            c("PT","darkred","2","19"),c("DT","navy","2","19"),
            c("PO","red","1","19"),c("DO","blue","1","19"),
            c("POT","red","2","19"),c("DOT","blue","2","19"),
            c("PV","orangered","1","19"),c("DV","steelblue","1","19"),
            c("PVT","orangered","2","19"),c("DVT","steelblue","2","19"),
            c("CP","forestgreen","1","19"),c("CPT","forestgreen","2","19"),
            c("CC","limegreen","1","19"),c("CCT","limegreen","2","19"))

lst = data.frame(Syn = factor(c("S","P","D","PT","DT","PO","DO","POT","DOT","PV","DV","PVT","DVT","CP","CPT","CC","CCT"),
                              levels = c("S","P","D","PT","DT","PO","DO","POT","DOT","PV","DV","PVT","DVT","CP","CPT","CC","CCT")) ,
                 Clr = c("darkorange","darkred","navy","darkred","navy","red","blue","red","blue",
                         "tomato","steelblue","tomato","steelblue","forestgreen","forestgreen",
                         "limegreen","limegreen"),
                 Lntp = c(1,1,1,2,2,1,1,2,2,1,1,2,2,1,2,1,2),
                 Shp = replicate(17,19))

scale_y_continuous(breaks =  seq(from = 0.2,to = 0.9, by = 0.1))

## Polish

load("Polish3.RData")

C2P <- createPlot(CIavg2PPS[[5]][[3]],c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "CI overlap",title = "CI overlap with variance type 1 for Polish over m") +
  scale_y_continuous(breaks =  seq(from = 0.2,to = 0.9, by = 0.1))
C3P <- createPlot(CIavg3PPS[[5]][[3]],c(2,3,5,10,20,50,100),lst) +
  labs(y = "CI overlap",title = "CI overlap with variance type 2 for Polish over m") +
  scale_y_continuous(breaks =  seq(from = 0.2,to = 0.9, by = 0.1))
C2P95 <- createPlot(NDCI(CIavg2PPS[[5]],5),c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "ND CI overlap",title = "ND CI overlap for 95 percentile with variance type 1 for Polish over m") +
  scale_y_continuous(limits = c(0,0.1),breaks =  seq(from = 0.0,to = 0.1, by = 0.0125))
C3P95 <- createPlot(NDCI(CIavg3PPS[[5]],5),c(2,3,5,10,20,50,100),lst) +
  labs(y = "ND CI overlap",title = "ND CI overlap for 95 percentile with variance type 2 for Polish over m") +
  scale_y_continuous(limits = c(0,0.1),breaks =  seq(from = 0.0,to = 0.1, by = 0.0125))

C2P9 <- createPlot(CIavg2PPS9[[5]][[3]],c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "APO above 90 %",title = "APO above 90 % with variance type 1 for Polish over m") +
  scale_y_continuous(breaks =  seq(from = 0.0,to = 0.6, by = 0.1))
C3P9 <- createPlot(CIavg3PPS9[[5]][[3]],c(2,3,5,10,20,50,100),lst) +
  labs(y = "APO above 90 %",title = "APO above 90 % with variance type 2 for Polish over m") +
  scale_y_continuous(breaks =  seq(from = 0.0,to = 0.6, by = 0.1))
C2P995 <- createPlot(NDCI(CIavg2PPS9[[5]],5),c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "ND APO above 90 %",title = "ND APO above 90 % for 95 percentile with variance type 1 for Polish over m") +
  scale_y_continuous(limits = c(0,1),breaks =  seq(from = 0.0,to = 1.0, by = 0.125))
C3P995 <- createPlot(NDCI(CIavg3PPS9[[5]],5),c(2,3,5,10,20,50,100),lst) +
  labs(y = "ND APO above 90 %",title = "ND APO above 90 % for 95 percentile with variance type 2 for Polish over m") +
  scale_y_continuous(limits = c(0,1),breaks =  seq(from = 0.0,to = 1.0, by = 0.125))


DP <- createPlot(DiffavgPPS[[4]][[3]],c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "Average NDC",title = "Average NDC for Polish over m") +
  scale_y_continuous(breaks =  seq(from = 0.0,to = 1.5, by = 0.25))
DP95 <- createPlot(NDCI(DiffavgPPS[[4]],5),c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "ND Average NDC",title = "ND Average NDC for 95 percentile for Polish over m") +
  scale_y_continuous(limits = c(0,0.35),breaks =  seq(from = 0.0,to = 0.35, by = 0.05))

DP1 <- createPlot(DiffavgPPS1[[4]][[3]],c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "PND below 10 %",title = "PND below 10 % for Polish over m") +
  scale_y_continuous(breaks =  seq(from = 0.0,to = 0.7, by = 0.1))
DP195 <- createPlot(NDCI(DiffavgPPS1[[4]],5),c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "ND PND below 10 %",title = "ND PND below 10 % for 95 percentile for Polish over m") +
  scale_y_continuous(limits = c(0,0.45),breaks =  seq(from = 0.0,to = 0.45, by = 0.05))

plots <- c(paste0("C2P",c("","95","9","995")),paste0("C3P",c("","95","9","995")),paste0("DP",c("","95","1","195")))
for(c in plots){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots/",c,".pdf"),t.plot,device = "pdf")
}

timePolish <- getTime(c(paste0("timePolishDec",c("","T","O","OT","V","VT"),".RData"),paste0("timePolishPar",c("","T","O","OT","V","VT","S","ST"),".RData"),
                        paste0("timePolishCat",c("","T"),".RData")),
                      c(paste0("timePolishDec",c("","T","O","OT","V","VT")),paste0("timePolishPar",c("","T","O","OT","V","VT","S","ST")),
                        paste0("timePolishCat",c("","T"))),
                      c(paste0("D",c("","T","O","OT","V","VT")),paste0("P",c("","T","O","OT","V","VT")),c("CP","CPT","CC","CCT")),
                      "time2/")
pol <- remT2(paste0("CIavg2P",c("N","O","V","S"),"PS"),c("P","D","PT","DT","PO","DO","POT","DOT","PV","DV","PVT","DVT","CP","CC","CPT","CCT"),1)[,c(1:12,13,15,14,16)]
timeD <- data.frame(Syn = factor(colnames(pol),levels = colnames(pol)),time = timePolish[1,c(as.vector(rbind(c(7:12),c(1:6))),13:16)]/10,CI = pol[8,],Shape = 
                      c(19,19,1,1,19,19,1,1,19,19,1,1,19,1,19,1))
polTime <- ggplot(data = timeD, aes(x = time,y = CI,color = Syn,shape = Syn)) + geom_point(size = 7) +
  scale_x_continuous(limits = c(0,2500),breaks = c(0,250,500,1000,1500,2000,2500)) + scale_color_manual("Syn",values =
                                                                                                          c("darkred","navy","darkred","navy","red","blue","red","blue",
                                                                                                            "tomato","steelblue","tomato","steelblue","forestgreen","forestgreen","limegreen","limegreen")) +
  scale_shape_manual("Syn",values = timeD$Shape) + scale_y_continuous(limits = c(0.6,0.95),breaks =  seq(from = 0.6,to = 0.95, by = 0.05)) +
  labs(x = "Seconds",y = "CI overlap",title = "Average CI overlap per generation time of 100 datasets in seconds for simple SC")

pol9 <- remT2(paste0("CIavg2P",c("N","O","V","S"),"PS9"),c("P","D","PT","DT","PO","DO","POT","DOT","PV","DV","PVT","DVT","CP","CC","CPT","CCT"),1)[,c(1:12,13,15,14,16)]
time9D <- data.frame(Syn = factor(colnames(pol9),levels = colnames(pol9)),time = timePolish[1,c(as.vector(rbind(c(7:12),c(1:6))),13:16)]/10,CI = pol9[8,],Shape = 
                       c(19,19,1,1,19,19,1,1,19,19,1,1,19,1,19,1))
pol9Time <- ggplot(data = time9D, aes(x = time,y = CI,color = Syn,shape = Syn)) + geom_point(size = 7) +
  scale_x_continuous(limits = c(0,2500),breaks = c(0,250,500,1000,1500,2000,2500)) + scale_color_manual("Syn",values =
                                                                                                          c("darkred","navy","darkred","navy","red","blue","red","blue",
                                                                                                            "tomato","steelblue","tomato","steelblue","forestgreen","forestgreen","limegreen","limegreen")) +
  scale_shape_manual("Syn",values = time9D$Shape) + scale_y_continuous(limits = c(0.2,0.7),breaks =  seq(from = 0.2,to = 0.7, by = 0.05)) +
  labs(x = "Seconds",y = "APO above 90 %",title = "APO above 90 % per generation time of 100 datasets in seconds for simple SC")

timePolishSe <- getTime(c(paste0("timePolishDec",c("","T","O","OT","V","VT"),"Se.RData"),paste0("timePolishPar",c("","T","O","OT","V","VT","S","ST"),"Se.RData"),
                          paste0("timePolishCat",c("","T"),"Se.RData")),
                        c(paste0("timePolishDec",c("","T","O","OT","V","VT"),"Se"),paste0("timePolishPar",c("","T","O","OT","V","VT","S","ST"),"Se"),
                          paste0("timePolishCat",c("","T"),"Se")),
                        c(paste0("D",c("","T","O","OT","V","VT"),"Se"),paste0("P",c("","T","O","OT","V","VT"),"Se"),paste0("C",c("P","PT","C","CT"))),
                        "time2/")
polSe <- remT2(paste0("CIavg2P",c("N","O","V","S"),"PS"),paste0(c("P","D","PT","DT","PO","DO","POT","DOT","PV","DV","PVT","DVT","CP","CC","CPT","CCT"),"Se"),2)[,c(1:12,13,15,14,16)]
timeSeD <- data.frame(Syn = factor(colnames(polSe),levels = colnames(polSe)),time = timePolishSe[1,c(as.vector(rbind(c(7:12),c(1:6))),13:16)]/10,CI = polSe[8,],Shape = 
                        c(19,19,1,1,19,19,1,1,19,19,1,1,19,1,19,1))
polSeTime <- ggplot(data = timeSeD, aes(x = time,y = CI,color = Syn,shape = Syn)) + geom_point(size = 7) +
  scale_x_continuous(limits = c(0,3000),breaks = c(0,250,500,1000,1500,2000,2500)) + scale_color_manual("Syn",values =
                                                                                                          c("darkred","navy","darkred","navy","red","blue","red","blue",
                                                                                                            "tomato","steelblue","tomato","steelblue","forestgreen","forestgreen","limegreen","limegreen")) +
  scale_shape_manual("Syn",values = timeSeD$Shape) + scale_y_continuous(limits = c(0.5,0.8),breaks =  seq(from = 0.5,to = 0.8, by = 0.05)) +
  labs(x = "Seconds",y = "CI overlap",title = "Average CI overlap per generation time of 100 datasets in seconds for selective SC")

pol9Se <- remT2(paste0("CIavg2P",c("N","O","V","S"),"PS9"),paste0(c("P","D","PT","DT","PO","DO","POT","DOT","PV","DV","PVT","DVT","CP","CC","CPT","CCT"),"Se"),2)[,c(1:12,13,15,14,16)]
time9SeD <- data.frame(Syn = factor(colnames(pol9Se),levels = colnames(pol9Se)),time = timePolishSe[1,c(as.vector(rbind(c(7:12),c(1:6))),13:16)]/10,CI = pol9Se[8,],Shape = 
                         c(19,19,1,1,19,19,1,1,19,19,1,1,19,1,19,1))
pol9SeTime <- ggplot(data = time9SeD, aes(x = time,y = CI,color = Syn,shape = Syn)) + geom_point(size = 7) +
  scale_x_continuous(limits = c(0,3000),breaks = c(0,250,500,1000,1500,2000,2500)) + scale_color_manual("Syn",values =
                                                                                                          c("darkred","navy","darkred","navy","red","blue","red","blue",
                                                                                                            "tomato","steelblue","tomato","steelblue","forestgreen","forestgreen","limegreen","limegreen")) +
  scale_shape_manual("Syn",values = time9SeD$Shape) + scale_y_continuous(limits = c(0.1,0.6),breaks =  seq(from = 0.1,to = 0.6, by = 0.05)) +
  labs(x = "Seconds",y = "APO above 90 %",title = "APO above 90 % per generation time of 100 datasets in seconds for selective SC")

plots <- paste0("pol",c("","9","Se","9Se"),"Time")
for(c in plots){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots/",c,".pdf"),t.plot,device = "pdf")
}



## Adult

lst = data.frame(Syn = factor(c("S","P","D","PT","DT","PH","DH","PHT","DHT","PL","DL","PLT","DLT","CP","CPT","CC","CCT"),
                              levels = c("S","P","D","PT","DT","PH","DH","PHT","DHT","PL","DL","PLT","DLT","CP","CPT","CC","CCT")) ,
                 Clr = c("darkorange","darkred","navy","darkred","navy","red","blue","red","blue",
                         "tomato","steelblue","tomato","steelblue","forestgreen","forestgreen",
                         "limegreen","limegreen"),
                 Lntp = c(1,1,1,2,2,1,1,2,2,1,1,2,2,1,2,1,2),
                 Shp = replicate(17,19))

load("Adult3.RData")

C2A <- createPlot(CIavg2APS[[4]][[3]],c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "CI overlap",title = "CI overlap with variance type 1 for Adult over m") +
  scale_y_continuous(breaks =  seq(from = 0.5,to = 0.9, by = 0.1))
C3A <- createPlot(CIavg3APS[[4]][[3]],c(2,3,5,10,20,50,100),lst) +
  labs(y = "CI overlap",title = "CI overlap with variance type 2 for Adult over m") +
  scale_y_continuous(breaks =  seq(from = 0.5,to = 0.9, by = 0.1))
C2A95 <- createPlot(NDCI(CIavg2APS[[4]],5),c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "ND CI overlap",title = "ND CI overlap for 95 percentile with variance type 1 for Adult over m") +
  scale_y_continuous(limits = c(0,0.1),breaks =  seq(from = 0.0,to = 0.8, by = 0.01))
C3A95 <- createPlot(NDCI(CIavg3APS[[4]],5),c(2,3,5,10,20,50,100),lst) +
  labs(y = "ND CI overlap",title = "ND CI overlap for 95 percentile with variance type 2 for Adult over m") +
  scale_y_continuous(limits = c(0,0.1),breaks =  seq(from = 0.0,to = 0.8, by = 0.01))

C2A9 <- createPlot(CIavg2APS9[[4]][[3]],c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "APO above 90 %",title = "APO above 90 % with variance type 1 for Adult over m") +
  scale_y_continuous(limits = c(0,0.7),breaks =  seq(from = 0.0,to = 0.7, by = 0.1))
C3A9 <- createPlot(CIavg3APS9[[4]][[3]],c(2,3,5,10,20,50,100),lst) +
  labs(y = "APO above 90 %",title = "APO above 90 % with variance type 2 for Adult over m") +
  scale_y_continuous(limits = c(0,0.7),breaks =  seq(from = 0.0,to = 0.7, by = 0.1))
C2A995 <- createPlot(NDCI(CIavg2APS9[[4]],5),c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "ND APO above 90 %",title = "ND APO above 90 % for 95 percentile with variance type 1 for Adult over m") +
  scale_y_continuous(limits = c(0,1),breaks =  seq(from = 0.0,to = 1.0, by = 0.125))
C3A995 <- createPlot(NDCI(CIavg3APS9[[4]],5),c(2,3,5,10,20,50,100),lst) +
  labs(y = "ND APO above 90 %",title = "ND APO above 90 % for 95 percentile with variance type 2 for Adult over m") +
  scale_y_continuous(limits = c(0,1),breaks =  seq(from = 0.0,to = 1.0, by = 0.125))


DA <- createPlot(DiffavgAPS[[4]][[3]],c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "Average NDC",title = "Average NDC for Adult over m") +
  scale_y_continuous(breaks =  seq(from = 0,to = 250, by = 25))
DA95 <- createPlot(NDCI(DiffavgAPS[[4]],5),c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "ND Average NDC",title = "ND Average NDC for 95 percentile for Adult over m") +
  scale_y_continuous(limits = c(0,4),breaks =  seq(from = 0,to = 4, by = 0.5))

DA1 <- createPlot(DiffavgAPS1[[4]][[3]],c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "PND below 10 %",title = "PND below 10 % for Adult over m") +
  scale_y_continuous(breaks =  seq(from = 0.0,to = 0.5, by = 0.1))
DA195 <- createPlot(NDCI(DiffavgAPS1[[4]],5),c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "ND PND below 10 %",title = "ND PND below 10 % for 95 percentile for Adult over m") +
  scale_y_continuous(limits = c(0,1),breaks =  seq(from = 0.0,to = 1, by = 0.1))

plots <- c(paste0("C2A",c("","95","9","995")),paste0("C3A",c("","95","9","995")),paste0("DA",c("","95","1","195")))
for(c in plots){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots/",c,".pdf"),t.plot,device = "pdf")
}

timeAdult <- getTime(c(paste0("timeAdultDec",c("","T","H","HT","L","LT"),".RData"),paste0("timeAdultPar",c("","T","H","HT","L","LT","S","ST"),".RData"),
                       paste0("timeAdultCat",c("","T"),".RData")),
                     c(paste0("timeAdultDec",c("","T","H","HT","L","LT")),paste0("timeAdultPar",c("","T","H","HT","L","LT","S","ST")),
                       paste0("timeAdultCat",c("","T"))),
                     c(paste0("D",c("","T","H","HT","L","LT")),paste0("P",c("","T","H","HT","L","LT")),c("CP","CPT","CC","CCT")),
                     "time2/")
timeAdult[1,c(1:6,13:16)] <- timeAdult[1,c(1:6,13:16)]/10
adu <- remT2(paste0("CIavg2A",c("N","O","V","S"),"PS"),c("P","D","PT","DT","PH","DH","PHT","DHT","PL","DL","PLT","DLT","CP","CC","CPT","CCT"),1)[,c(1:12,13,15,14,16)]
timeD <- data.frame(Syn = factor(colnames(adu),levels = colnames(adu)),time = timeAdult[1,c(as.vector(rbind(c(7:12),c(1:6))),13:16)],CI = adu[7,],Shape = 
                      c(19,19,1,1,19,19,1,1,19,19,1,1,19,1,19,1))
aduTime <- ggplot(data = timeD, aes(x = time,y = CI,color = Syn,shape = Syn)) + geom_point(size = 7) +
  scale_x_log10() +
  scale_color_manual("Syn",values = c("darkred","navy","darkred","navy","red","blue","red","blue",
                                      "tomato","steelblue","tomato","steelblue","forestgreen","forestgreen","limegreen","limegreen")) +
  scale_shape_manual("Syn",values = timeD$Shape) + scale_y_continuous(limits = c(0.6,0.9),breaks =  seq(from = 0.6,to = 0.9, by = 0.05)) +
  labs(x = "Seconds",y = "CI overlap",title = "Average CI overlap per generation time of 100 datasets in seconds for simple SC")

adu9 <- remT2(paste0("CIavg2A",c("N","O","V","S"),"PS9"),c("P","D","PT","DT","PH","DH","PHT","DHT","PL","DL","PLT","DLT","CP","CC","CPT","CCT"),1)[,c(1:12,13,15,14,16)]
time9D <- data.frame(Syn = factor(colnames(adu9),levels = colnames(adu9)),time = timeAdult[1,c(as.vector(rbind(c(7:12),c(1:6))),13:16)],CI = adu9[7,],Shape = 
                       c(19,19,1,1,19,19,1,1,19,19,1,1,19,1,19,1))
adu9Time <- ggplot(data = time9D, aes(x = time,y = CI,color = Syn,shape = Syn)) + geom_point(size = 7) +
  scale_x_log10() +
  scale_color_manual("Syn",values = c("darkred","navy","darkred","navy","red","blue","red","blue",
                                      "tomato","steelblue","tomato","steelblue","forestgreen","forestgreen","limegreen","limegreen")) +
  scale_shape_manual("Syn",values = timeD$Shape) + scale_y_continuous(limits = c(0.15,0.6),breaks =  seq(from = 0.15,to = 0.6, by = 0.05)) +
  labs(x = "Seconds",y = "APO above 90 %",title = "APO above 90 % per generation time of 100 datasets in seconds for simple SC")

timeAdultSe <- getTime(c(paste0("timeAdultDec",c("","T","H","HT","L","LT"),"Se.RData"),paste0("timeAdultPar",c("","T","H","HT","L","LT","S","ST"),"Se.RData"),
                         paste0("timeAdultCat",c("","T"),"Se.RData")),
                       c(paste0("timeAdultDec",c("","T","H","HT","L","LT"),"Se"),paste0("timeAdultPar",c("","T","H","HT","L","LT","S","ST"),"Se"),
                         paste0("timeAdultCat",c("","T"),"Se")),
                       c(paste0("D",c("","T","H","HT","L","LT"),"Se"),paste0("P",c("","T","H","HT","L","LT"),"Se"),paste0("C",c("P","PT","C","CT"),"Se")),
                       "time2/")
timeAdultSe[1,c(1:6,13:16)] <- timeAdultSe[1,c(1:6,13:16)]/10
aduSe <- remT2(paste0("CIavg2A",c("N","O","V","S"),"PS"),paste0(c("P","D","PT","DT","PO","DO","POT","DOT","PV","DV","PVT","DVT","CP","CC","CPT","CCT"),"Se"),2)[,c(1:12,13,15,14,16)]
timeSeD <- data.frame(Syn = factor(colnames(aduSe),levels = colnames(aduSe)),time = timeAdultSe[1,c(as.vector(rbind(c(7:12),c(1:6))),13:16)],CI = aduSe[7,],Shape = 
                        c(19,19,1,1,19,19,1,1,19,19,1,1,19,1,19,1))
aduSeTime <- ggplot(data = timeSeD, aes(x = time,y = CI,color = Syn,shape = Syn)) + geom_point(size = 7) +
  scale_x_log10() +
  scale_color_manual("Syn",values = c("darkred","navy","darkred","navy","red","blue","red","blue",
                                      "tomato","steelblue","tomato","steelblue","forestgreen","forestgreen","limegreen","limegreen")) +
  scale_shape_manual("Syn",values = timeSeD$Shape) + scale_y_continuous(limits = c(0.55,0.9),breaks =  seq(from = 0.55,to = 0.9, by = 0.05)) +
  labs(x = "Seconds",y = "CI overlap",title = "Average CI overlap per generation time of 100 datasets in seconds for selective SC")

adu9Se <- remT2(paste0("CIavg2A",c("N","O","V","S"),"PS9"),paste0(c("P","D","PT","DT","PO","DO","POT","DOT","PV","DV","PVT","DVT","CP","CC","CPT","CCT"),"Se"),2)[,c(1:12,13,15,14,16)]
time9SeD <- data.frame(Syn = factor(colnames(adu9Se),levels = colnames(adu9Se)),time = timeAdultSe[1,c(as.vector(rbind(c(7:12),c(1:6))),13:16)],CI = adu9Se[7,],Shape = 
                         c(19,19,1,1,19,19,1,1,19,19,1,1,19,1,19,1))
adu9SeTime <- ggplot(data = time9SeD, aes(x = time,y = CI,color = Syn,shape = Syn)) + geom_point(size = 7) +
  scale_x_log10() +
  scale_color_manual("Syn",values = c("darkred","navy","darkred","navy","red","blue","red","blue",
                                      "tomato","steelblue","tomato","steelblue","forestgreen","forestgreen","limegreen","limegreen")) +
  scale_shape_manual("Syn",values = timeSeD$Shape) + scale_y_continuous(limits = c(0.15,0.65),breaks =  seq(from = 0.15,to = 0.65, by = 0.05)) +
  labs(x = "Seconds",y = "APO above 90 %",title = "APO above 90 % per generation time of 100 datasets in seconds for selective SC")


plots <- paste0("adu",c("","9","Se","9Se"),"Time")
for(c in plots){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots/",c,".pdf"),t.plot,device = "pdf")
}

## Avila

lst = data.frame(Syn = factor(c("S","P","D","PT","DT","PO","DO","POT","DOT","PV","DV","PVT","DVT","CP","CPT","CC","CCT"),
                              levels = c("S","P","D","PT","DT","PO","DO","POT","DOT","PV","DV","PVT","DVT","CP","CPT","CC","CCT")) ,
                 Clr = c("darkorange","darkred","navy","darkred","navy","red","blue","red","blue",
                         "tomato","steelblue","tomato","steelblue","forestgreen","forestgreen",
                         "limegreen","limegreen"),
                 Lntp = c(1,1,1,2,2,1,1,2,2,1,1,2,2,1,2,1,2),
                 Shp = replicate(17,19))

load("Avila3.RData")

C2Av <- createPlot(CIavg2AvPS[[5]][[3]],c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "CI overlap",title = "CI overlap with variance type 1 for Avila over m") +
  scale_y_continuous(breaks =  seq(from = 0.2,to = 0.7, by = 0.1))
C3Av <- createPlot(CIavg3AvPS[[5]][[3]],c(2,3,5,10,20,50,100),lst) +
  labs(y = "CI overlap",title = "CI overlap with variance type 2 for Avila over m") +
  scale_y_continuous(breaks =  seq(from = 0.2,to = 0.7, by = 0.1))
C2Av95 <- createPlot(NDCI(CIavg2AvPS[[5]],5),c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "ND CI overlap",title = "ND CI overlap for 95 percentile with variance type 1 for Avila over m") +
  scale_y_continuous(limits = c(0,0.18),breaks =  seq(from = 0.0,to = 0.18, by = 0.02))
C3Av95 <- createPlot(NDCI(CIavg3AvPS[[5]],5),c(2,3,5,10,20,50,100),lst) +
  labs(y = "ND CI overlap",title = "ND CI overlap for 95 percentile with variance type 2 for Avila over m") +
  scale_y_continuous(limits = c(0,0.18),breaks =  seq(from = 0.0,to = 0.18, by = 0.02))

C2Av9 <- createPlot(CIavg2AvPS9[[5]][[3]],c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "APO above 90 %",title = "APO above 90 % with variance type 1 for Avila over m") +
  scale_y_continuous(limits = c(0,0.3),breaks =  seq(from = 0.0,to = 0.3, by = 0.05))
C3Av9 <- createPlot(CIavg3AvPS9[[5]][[3]],c(2,3,5,10,20,50,100),lst) +
  labs(y = "APO above 90 %",title = "APO above 90 % with variance type 2 for Avila over m") +
  scale_y_continuous(limits = c(0,0.3),breaks =  seq(from = 0.0,to = 0.3, by = 0.05))
C2Av995 <- createPlot(NDCI(CIavg2AvPS9[[5]],5),c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "ND APO above 90 %",title = "ND APO above 90 % for 95 percentile with variance type 1 for Avila over m") +
  scale_y_continuous(limits = c(0,1),breaks =  seq(from = 0.0,to = 1.0, by = 0.125))
C3Av995 <- createPlot(NDCI(CIavg3AvPS9[[5]],5),c(2,3,5,10,20,50,100),lst) +
  labs(y = "ND APO above 90 %",title = "ND APO above 90 % for 95 percentile with variance type 2 for Avila over m") +
  scale_y_continuous(limits = c(0,1),breaks =  seq(from = 0.0,to = 1.0, by = 0.125))

tsD <- AHFix(DiffavgAvPS[[1]])

DAv <- createPlot(tsD[[3]],c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "Average NDC",title = "Average NDC for Avila over m") +
  scale_y_continuous(breaks = seq(from = 0.0,to = 8, by = 1))
DAv95 <- createPlot(NDCI(tsD,2),c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "ND Average NDC",title = "ND Average NDC for 95 percentile for Avila over m") +
  scale_y_continuous(limits = c(0,8),breaks = seq(from = 0,to = 8, by = 1))

DAv1 <- createPlot(DiffavgAvPS1[[4]][[3]],c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "PND below 10 %",title = "PND below 10 % for Avila over m") +
  scale_y_continuous(breaks =  seq(from = 0.0,to = 0.4, by = 0.05))
DAv195 <- createPlot(NDCI(DiffavgAvPS1[[4]],5),c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "ND PND below 10 %",title = "ND PND below 10 % for 95 percentile for Avila over m") +
  scale_y_continuous(limits = c(0,0.45),breaks =  seq(from = 0.0,to = 0.45, by = 0.05))

plots <- c(paste0("C2Av",c("","95","9","995")),paste0("C3Av",c("","95","9","995")),paste0("DAv",c("","95","1","195")))
for(c in plots){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots/",c,".pdf"),t.plot,device = "pdf")
}

C2AvK <- createPlot(CIavg2AvPSK[[5]][[3]],c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "CI overlap",title = "CI overlap with variance type 1 for Avila over m with modified regression fits and simple SC") +
  scale_y_continuous(breaks =  seq(from = 0.1,to = 0.9, by = 0.1))
C3AvK <- createPlot(CIavg3AvPSK[[5]][[3]],c(2,3,5,10,20,50,100),lst) +
  labs(y = "CI overlap",title = "CI overlap with variance type 2 for Avila over m with modified regression fits and simple SC") +
  scale_y_continuous(breaks =  seq(from = 0.1,to = 0.9, by = 0.1))
C2AvK95 <- createPlot(NDCI(CIavg2AvPSK[[5]],5),c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "ND CI overlap",title = "ND CI overlap for 95 percentile with variance type 1 for Avila over m with modified regression fits and simple SC") +
  scale_y_continuous(limits = c(0,0.12),breaks =  seq(from = 0.0,to = 0.12, by = 0.01))
C3AvK95 <- createPlot(NDCI(CIavg3AvPSK[[5]],5),c(2,3,5,10,20,50,100),lst) +
  labs(y = "ND CI overlap",title = "ND CI overlap for 95 percentile with variance type 2 for Avila over m with modified regression fits and simple SC") +
  scale_y_continuous(limits = c(0,0.08),breaks =  seq(from = 0.0,to = 0.08, by = 0.01))

C2Av9K <- createPlot(CIavg2AvPS9K[[5]][[3]],c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "APO above 90 %",title = "APO above 90 % with variance type 1 for Avila over m with modified regression fits and simple SC") +
  scale_y_continuous(limits = c(0,0.7),breaks =  seq(from = 0.0,to = 0.7, by = 0.1))
C3Av9K <- createPlot(CIavg3AvPS9K[[5]][[3]],c(2,3,5,10,20,50,100),lst) +
  labs(y = "APO above 90 %",title = "APO above 90 % with variance type 2 for Avila over m with modified regression fits and simple SC") +
  scale_y_continuous(limits = c(0,0.7),breaks =  seq(from = 0.0,to = 0.7, by = 0.1))
C2Av9K95 <- createPlot(NDCI(CIavg2AvPS9K[[5]],5),c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "ND APO above 90 %",title = "ND APO above 90 % for 95 percentile with variance type 1 for Avila over m with modified regression fits and simple SC") +
  scale_y_continuous(limits = c(0,1),breaks =  seq(from = 0.0,to = 1.0, by = 0.125))
C3Av9K95 <- createPlot(NDCI(CIavg3AvPS9K[[5]],5),c(2,3,5,10,20,50,100),lst) +
  labs(y = "ND APO above 90 %",title = "ND APO above 90 % for 95 percentile with variance type 2 for Avila over m with modified regression fits and simple SC") +
  scale_y_continuous(limits = c(0,1),breaks =  seq(from = 0.0,to = 1.0, by = 0.125))


lst = data.frame(Syn = factor(c("P","D","PT","DT","PO","DO","POT","DOT","PV","DV","PVT","DVT","CP","CPT","CC","CCT"),
                              levels = c("P","D","PT","DT","PO","DO","POT","DOT","PV","DV","PVT","DVT","CP","CPT","CC","CCT")) ,
                 Clr = c("darkred","navy","darkred","navy","red","blue","red","blue",
                         "tomato","steelblue","tomato","steelblue","forestgreen","forestgreen",
                         "limegreen","limegreen"),
                 Lntp = c(1,1,2,2,1,1,2,2,1,1,2,2,1,2,1,2),
                 Shp = replicate(16,19))

C2AvSeK <- createPlot(CIavg2AvPSSeK[[5]][[3]],c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "CI overlap",title = "CI overlap with variance type 1 for Avila over m with modified regression fits and selective SC") +
  scale_y_continuous(breaks =  seq(from = 0.2,to = 0.7, by = 0.05))
C3AvSeK <- createPlot(CIavg3AvPSSeK[[5]][[3]],c(2,3,5,10,20,50,100),lst) +
  labs(y = "CI overlap",title = "CI overlap with variance type 2 for Avila over m with modified regression fits and selective SC") +
  scale_y_continuous(breaks =  seq(from = 0.2,to = 0.7, by = 0.05))
C2AvSeK95 <- createPlot(NDCI(CIavg2AvPSSeK[[5]],5),c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "ND CI overlap",title = "ND CI overlap for 95 percentile with variance type 1 for Avila over m with modified regression fits and selective SC") +
  scale_y_continuous(limits = c(0,0.125),breaks =  seq(from = 0.0,to = 0.125, by = 0.0125))
C3AvSeK95 <- createPlot(NDCI(CIavg3AvPSSeK[[5]],5),c(2,3,5,10,20,50,100),lst) +
  labs(y = "ND CI overlap",title = "ND CI overlap for 95 percentile with variance type 2 for Avila over m with modified regression fits and selective SC") +
  scale_y_continuous(limits = c(0,0.1),breaks =  seq(from = 0.0,to = 0.1, by = 0.0125))

C2Av9SeK <- createPlot(CIavg2AvPS9SeK[[5]][[3]],c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "APO above 90 %",title = "APO above 90 % with variance type 1 for Avila over m with modified regression fits and selective SC") +
  scale_y_continuous(limits = c(0,0.25),breaks =  seq(from = 0.0,to = 0.25, by = 0.05))
C3Av9SeK <- createPlot(CIavg3AvPS9SeK[[5]][[3]],c(2,3,5,10,20,50,100),lst) +
  labs(y = "APO above 90 %",title = "APO above 90 % with variance type 2 for Avila over m with modified regression fits and selective SC") +
  scale_y_continuous(limits = c(0,0.25),breaks =  seq(from = 0.0,to = 0.25, by = 0.05))
C2Av9SeK95 <- createPlot(NDCI(CIavg2AvPS9SeK[[5]],5),c(1,2,3,5,10,20,50,100),lst) +
  labs(y = "ND APO above 90 %",title = "ND APO above 90 % for 95 percentile with variance type 1 for Avila over m with modified regression fits and selective SC") +
  scale_y_continuous(limits = c(0,1),breaks =  seq(from = 0.0,to = 1.0, by = 0.125))
C3Av9SeK95 <- createPlot(NDCI(CIavg3AvPS9SeK[[5]],5),c(2,3,5,10,20,50,100),lst) +
  labs(y = "ND APO above 90 %",title = "ND APO above 90 % for 95 percentile with variance type 2 for Avila over m with modified regression fits and selective SC") +
  scale_y_continuous(limits = c(0,1),breaks =  seq(from = 0.0,to = 1.0, by = 0.125))

plots <- c(paste0("C2Av",c("K","K95","9K","9K95","SeK","SeK95","9SeK","9SeK95")),paste0("C3Av",c("K","K95","9K","9K95","SeK","SeK95","9SeK","9SeK95")))
for(c in plots){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots/",c,".pdf"),t.plot,device = "pdf")
}



timeAvila <- getTime(c(paste0("timeAvilaDec",c("","T","O","OT","V","VT"),".RData"),paste0("timeAvilaPar",c("","T","O","OT","V","VT","S","ST"),".RData"),
                       paste0("timeAvilaCat",c("","T"),".RData")),
                     c(paste0("timeAvilaDec",c("","T","O","OT","V","VT")),paste0("timeAvilaPar",c("","T","O","OT","V","VT","S","ST")),
                       paste0("timeAvilaCat",c("","T"))),
                     c(paste0("D",c("","T","O","OT","V","VT")),paste0("P",c("","T","O","OT","V","VT")),c("CP","CPT","CC","CCT")),
                     "time2/")
avi <- CIavg2AvPSK[[5]][[3]][,c(2:17)]
timeD <- data.frame(Syn = factor(colnames(avi),levels = colnames(avi)),time = timeAvila[1,c(as.vector(rbind(c(7:12),c(1:6))),13:16)]/10,CI = avi[8,],Shape = 
                      c(19,19,1,1,19,19,1,1,19,19,1,1,19,1,19,1))
aviTime <- ggplot(data = timeD, aes(x = time,y = CI,color = Syn,shape = Syn)) + geom_point(size = 7) +
  scale_x_log10() +
  scale_color_manual("Syn",values =
                       c("darkred","navy","darkred","navy","red","blue","red","blue",
                         "tomato","steelblue","tomato","steelblue","forestgreen","forestgreen","limegreen","limegreen")) +
  scale_shape_manual("Syn",values = timeD$Shape) + scale_y_continuous(limits = c(0.2,0.9),breaks =  seq(from = 0.2,to = 0.9, by = 0.1)) +
  labs(x = "Seconds",y = "CI overlap",title = "Average CI overlap per generation time of 100 datasets in seconds for simple SC with modified regression fits")

avi9 <- CIavg2AvPS9K[[5]][[3]][,c(2:17)]
time9D <- data.frame(Syn = factor(colnames(avi9),levels = colnames(avi9)),time = timeAvila[1,c(as.vector(rbind(c(7:12),c(1:6))),13:16)]/10,CI = avi9[8,],Shape = 
                       c(19,19,1,1,19,19,1,1,19,19,1,1,19,1,19,1))
avi9Time <- ggplot(data = time9D, aes(x = time,y = CI,color = Syn,shape = Syn)) + geom_point(size = 7) +
  scale_x_log10() +
  scale_color_manual("Syn",values =
                       c("darkred","navy","darkred","navy","red","blue","red","blue",
                         "tomato","steelblue","tomato","steelblue","forestgreen","forestgreen","limegreen","limegreen")) +
  scale_shape_manual("Syn",values = time9D$Shape) + scale_y_continuous(limits = c(0.0,0.70),breaks =  seq(from = 0.0,to = 0.70, by = 0.1)) +
  labs(x = "Seconds",y = "APO above 90 %",title = "APO above 90 % per generation time of 100 datasets in seconds for simple SC with modified regression fits")

timeAvilaSe <- getTime(c(paste0("timeAvilaDec",c("","T","O","OT","V","VT"),"Se.RData"),paste0("timeAvilaPar",c("","T","O","OT","V","VT","S","ST"),"Se.RData"),
                         paste0("timeAvilaCat",c("","T"),"Se.RData")),
                       c(paste0("timeAvilaDec",c("","T","O","OT","V","VT"),"Se"),paste0("timeAvilaPar",c("","T","O","OT","V","VT","S","ST"),"Se"),
                         paste0("timeAvilaCat",c("","T"),"Se")),
                       c(paste0("D",c("","T","O","OT","V","VT"),"Se"),paste0("P",c("","T","O","OT","V","VT"),"Se"),paste0("C",c("P","PT","C","CT"),"Se")),
                       "time2/")
aviSe <- CIavg2AvPSSeK[[5]][[3]]
timeSeD <- data.frame(Syn = factor(colnames(aviSe),levels = colnames(aviSe)),time = timeAvilaSe[1,c(as.vector(rbind(c(7:12),c(1:6))),13:16)]/10,CI = aviSe[8,],Shape = 
                        c(19,19,1,1,19,19,1,1,19,19,1,1,19,1,19,1))
aviSeTime <- ggplot(data = timeSeD, aes(x = time,y = CI,color = Syn,shape = Syn)) + geom_point(size = 7) +
  scale_x_log10() +
  scale_color_manual("Syn",values =
                       c("darkred","navy","darkred","navy","red","blue","red","blue",
                         "tomato","steelblue","tomato","steelblue","forestgreen","forestgreen","limegreen","limegreen")) +
  scale_shape_manual("Syn",values = timeSeD$Shape) + scale_y_continuous(limits = c(0.2,0.7),breaks =  seq(from = 0.2,to = 0.7, by = 0.05)) +
  labs(x = "Seconds",y = "CI overlap",title = "Average CI overlap per generation time of 100 datasets in seconds for selective SC with modified regression fits")

avi9Se <- CIavg2AvPS9SeK[[5]][[3]]
time9SeD <- data.frame(Syn = factor(colnames(avi9Se),levels = colnames(avi9Se)),time = timeAvilaSe[1,c(as.vector(rbind(c(7:12),c(1:6))),13:16)]/10,CI = avi9Se[8,],Shape = 
                         c(19,19,1,1,19,19,1,1,19,19,1,1,19,1,19,1))
avi9SeTime <- ggplot(data = time9SeD, aes(x = time,y = CI,color = Syn,shape = Syn)) + geom_point(size = 7) +
  scale_x_log10() +
  scale_color_manual("Syn",values =
                       c("darkred","navy","darkred","navy","red","blue","red","blue",
                         "tomato","steelblue","tomato","steelblue","forestgreen","forestgreen","limegreen","limegreen")) +
  scale_shape_manual("Syn",values = time9SeD$Shape) + scale_y_continuous(limits = c(0.0,0.2),breaks =  seq(from = 0.0,to = 0.2, by = 0.025)) +
  labs(x = "Seconds",y = "APO above 90 %",title = "APO above 90 % per generation time of 100 datasets in seconds for selective SC with modified regression fits")

plots <- paste0("avi",c("","9","Se","9Se"),"Time")
for(c in plots){
  t.plot <- eval(as.name(c))
  ggsave(paste0("plots/",c,".pdf"),t.plot,device = "pdf")
}
