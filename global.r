library(shiny)
library(readxl)
library(data.table)
library(tidyr)
library(DT)
library(shinythemes)
library(dplyr)
library(hablar)
library(treemap)
library(d3treeR)
library(shinycssloaders)



riik_palk_2018 <- as.data.frame(readRDS("./andmed/riik_palk_2018.RData"))
riik_palk_2019 <- as.data.frame(readRDS("./andmed/riik_palk_2019.RData"))
riik_kogu_palk_2018 <- as.data.frame(readRDS("./andmed/riik_kogu_palk_2018.RData"))

kov_palk_2018 <- as.data.frame(readRDS("./andmed/kov_palk_2018.RData"))
kov_palk_2019 <- as.data.frame(readRDS("./andmed/kov_palk_2019.RData"))
kov_kogu_palk_2018 <- as.data.frame(readRDS("./andmed/kov_kogu_palk_2018.RData"))


cols <- c("Asutus","Struktuuriüksus","Ametikoht", "Ametniku koormus ametkohal")
cols2 <- c("Asutus","Struktuuriüksus","Ametikoht", "Ametniku koormus ametikohal")
riik_palk_2018[cols] <- lapply(riik_palk_2018[cols], factor)
riik_palk_2019[cols] <- lapply(riik_palk_2019[cols], factor)
kov_palk_2018[cols] <- lapply(kov_palk_2018[cols], factor)
kov_palk_2019[cols] <- lapply(kov_palk_2019[cols], factor)
kov_kogu_palk_2018[cols] <- lapply(kov_kogu_palk_2018[cols], factor)
riik_kogu_palk_2018[cols2] <- lapply(riik_kogu_palk_2018[cols2], factor)



riik_palk_2018$nimi <- paste(riik_palk_2018$Eesnimi, riik_palk_2018$Perekonnanimi)
riik_palk_2018 <- riik_palk_2018 %>% select(-c(Eesnimi, Perekonnanimi))
riik_palk_2018$Põhipalk <- gsub(",","", riik_palk_2018$Põhipalk)
riik_palk_2018$Põhipalk <- as.numeric(riik_palk_2018$Põhipalk)

riik_palk_2019$nimi <- paste(riik_palk_2019$Eesnimi, riik_palk_2019$Perekonnanimi)
riik_palk_2019 <- riik_palk_2019 %>% select(-c(Eesnimi, Perekonnanimi))
riik_palk_2019$Põhipalk <- gsub(",","", riik_palk_2019$Põhipalk)
riik_palk_2019$Põhipalk <- as.numeric(riik_palk_2019$Põhipalk)


kov_palk_2018$nimi <- paste(kov_palk_2018$Eesnimi, kov_palk_2018$Perekonnanimi)
kov_palk_2018 <- kov_palk_2018 %>% select(-c(Eesnimi, Perekonnanimi))
kov_palk_2018$Põhipalk <- gsub(",","", kov_palk_2018$Põhipalk)
kov_palk_2018$Põhipalk <- as.numeric(kov_palk_2018$Põhipalk)


kov_palk_2019$nimi <- paste(kov_palk_2019$Eesnimi, kov_palk_2019$Perekonnanimi)
kov_palk_2019 <- kov_palk_2019 %>% select(-c(Eesnimi, Perekonnanimi))
kov_palk_2019$Põhipalk <- gsub(",","", kov_palk_2019$Põhipalk)
kov_palk_2019$Põhipalk <- as.numeric(kov_palk_2019$Põhipalk)


kov_kogu_palk_2018$nimi <- paste(kov_kogu_palk_2018$Eesnimi, kov_kogu_palk_2018$Perekonnanimi)
kov_kogu_palk_2018 <- kov_kogu_palk_2018 %>% select(-c(Eesnimi, Perekonnanimi, V13))
kov_kogu_palk_2018$`Põhipalk, sh puhkusetasu` <- as.numeric(gsub(",","", kov_kogu_palk_2018$`Põhipalk, sh puhkusetasu`))
kov_kogu_palk_2018$Muutuvpalk <- as.numeric(gsub(",","", kov_kogu_palk_2018$Muutuvpalk))
kov_kogu_palk_2018$`Muu tulu` <- as.numeric(gsub(",","", kov_kogu_palk_2018$`Muu tulu`))
kov_kogu_palk_2018$`Kokku     (7+8+9)` <- as.numeric(gsub(",","", kov_kogu_palk_2018$`Kokku     (7+8+9)`))
colnames(kov_kogu_palk_2018)[8] <- "Kokku"

riik_kogu_palk_2018$nimi <- paste(riik_kogu_palk_2018$Eesnimi, riik_kogu_palk_2018$Perekonnanimi)
riik_kogu_palk_2018 <- riik_kogu_palk_2018 %>% select(-c(Eesnimi, Perekonnanimi))
riik_kogu_palk_2018$`Põhipalk, sh puhkusetasu` <- as.numeric(gsub(",","", riik_kogu_palk_2018$`Põhipalk, sh puhkusetasu`))
riik_kogu_palk_2018$Muutuvpalk <- as.numeric(gsub(",","", riik_kogu_palk_2018$Muutuvpalk))
riik_kogu_palk_2018$`Muu tulu` <- as.numeric(gsub(",","", riik_kogu_palk_2018$`Muu tulu`))
riik_kogu_palk_2018$`Kokku     (7+8+9)` <- as.numeric(gsub(",","", riik_kogu_palk_2018$`Kokku     (7+8+9)`))
colnames(riik_kogu_palk_2018)[8] <- "Kokku"



datasetList <-  c("Ametnike põhipalk KOV üksuste ametiasutustes 2018" = "kov_palk_2018" ,
                                                           "Ametnike põhipalk KOV üksuste ametiasutustes 2019" = "kov_palk_2019",
                                                           "Ametnike põhipalk riigi ametiasutustes 2018" = "riik_palk_2018",
                                                           "Ametnike põhipalk riigi ametiasutustes 2019" = "riik_palk_2019",
                  "Ametnike põhipalk ja muutuvpalk ning teenistusülesannete täitmisest tulenev muu tulu riigi ametiasutustes 2018" = "riik_kogu_palk_2018",
                  "Ametnike põhipalk ja muutuvpalk ning teenistusülesannete täitmisest tulenev muu tulu KOV üksuste ametiasutustes 2018" = "kov_kogu_palk_2018")
