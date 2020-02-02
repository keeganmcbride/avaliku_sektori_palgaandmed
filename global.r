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


#setwd("~/dev/palgaandmed")
# 
# riik_palk_2015 <- fread("./andmed/riik_palk_2015.csv") %>% select(-c(9,10,11,12))
# riik_palk_2016 <- fread("./ignore/riik_palk_2016.csv") 
# riik_palk_2017 <- fread("./ignore/riik_palk_2017.csv") 
# 
# riik_kogu_palk_2014 <- fread("./andmed/riik_kogu_palk_2014.csv")
# riik_kogu_palk_2015 <- fread("./andmed/riik_kogu_palk_2015.csv")
# riik_kogu_palk_2016 <- fread("./ignore/riik_kogu_palk_2016.csv")
# riik_kogu_palk_2017 <- fread("./andmed/riik_kogu_palk_2017.csv")
# 
# kov_palk_2015 <- fread("./andmed/kov_palk_2015.csv")
# kov_palk_2016 <- fread("./andmed/kov_palk_2016.csv")
# kov_palk_2017 <- fread("./andmed/kov_palk_2017.csv")
# 
# kov_kogu_palk_2014 <- fread("./andmed/kov_kogu_palk_2014.csv")
# kov_kogu_palk_2015 <- fread("./andmed/kov_kogu_palk_2015.csv")
# kov_kogu_palk_2016 <- fread("./ignore/kov_kogu_palk_2016.csv")
# kov_kogu_palk_2017 <- fread("./andmed/kov_kogu_palk_2017.csv")
# 
# saveRDS(riik_palk_2015, "./andmed/riik_palk_2015.RData")
# saveRDS(riik_palk_2016, "./andmed/riik_palk_2016.RData")
# saveRDS(riik_palk_2017, "./andmed/riik_palk_2017.RData")
# saveRDS(riik_kogu_palk_2014, "./andmed/riik_kogu_palk_2014.RData")
# saveRDS(riik_kogu_palk_2015, "./andmed/riik_kogu_palk_2015.RData")
# saveRDS(riik_kogu_palk_2016, "./andmed/riik_kogu_palk_2016.RData")
# saveRDS(riik_kogu_palk_2017, "./andmed/riik_kogu_palk_2017.RData")
# saveRDS(kov_palk_2015, "./andmed/kov_palk_2015.RData")
# saveRDS(kov_palk_2016, "./andmed/kov_palk_2016.RData")
# saveRDS(kov_palk_2017, "./andmed/kov_palk_2017.RData")
# saveRDS(kov_kogu_palk_2014, "./andmed/kov_kogu_palk_2014.RData")
# saveRDS(kov_kogu_palk_2015, "./andmed/kov_kogu_palk_2015.RData")
# saveRDS(kov_kogu_palk_2016, "./andmed/kov_kogu_palk_2016.RData")
## saveRDS(kov_kogu_palk_2017, "./andmed/kov_kogu_palk_2017.RData")


riik_palk_2015 <- as.data.frame(readRDS("./andmed/riik_palk_2015.RData"))
riik_palk_2016 <- as.data.frame(readRDS("./andmed/riik_palk_2016.RData"))
riik_palk_2017 <- as.data.frame(readRDS("./andmed/riik_palk_2017.RData"))
riik_palk_2018 <- as.data.frame(readRDS("./andmed/riik_palk_2018.RData"))
riik_palk_2019 <- as.data.frame(readRDS("./andmed/riik_palk_2019.RData"))

riik_kogu_palk_2014 <- as.data.frame(readRDS("./andmed/riik_kogu_palk_2014.RData"))
riik_kogu_palk_2015 <- as.data.frame(readRDS("./andmed/riik_kogu_palk_2015.RData"))
riik_kogu_palk_2016 <- as.data.frame(readRDS("./andmed/riik_kogu_palk_2016.RData"))
riik_kogu_palk_2017 <- as.data.frame(readRDS("./andmed/riik_kogu_palk_2017.RData"))
riik_kogu_palk_2018 <- as.data.frame(readRDS("./andmed/riik_kogu_palk_2018.RData"))



kov_palk_2015 <- as.data.frame(readRDS("./andmed/kov_palk_2015.RData"))
kov_palk_2016 <- as.data.frame(readRDS("./andmed/kov_palk_2016.RData"))
kov_palk_2017 <- as.data.frame(readRDS("./andmed/kov_palk_2017.RData"))
kov_palk_2018 <- as.data.frame(readRDS("./andmed/kov_palk_2018.RData"))
kov_palk_2019 <- as.data.frame(readRDS("./andmed/kov_palk_2019.RData"))
kov_kogu_palk_2014 <- as.data.frame(readRDS("./andmed/kov_kogu_palk_2014.RData"))
kov_kogu_palk_2015 <- as.data.frame(readRDS("./andmed/kov_kogu_palk_2015.RData"))
kov_kogu_palk_2016 <- as.data.frame(readRDS("./andmed/kov_kogu_palk_2016.RData"))
kov_kogu_palk_2017 <- as.data.frame(readRDS("./andmed/kov_kogu_palk_2017.RData"))
kov_kogu_palk_2018 <- as.data.frame(readRDS("./andmed/kov_kogu_palk_2018.RData"))


cols <- c("Asutus","Struktuuriüksus","Ametikoht", "Ametniku koormus ametkohal")
cols2 <- c("Asutus","Struktuuriüksus","Ametikoht", "Ametniku koormus ametikohal")
riik_palk_2015[cols] <- lapply(riik_palk_2015[cols], factor)
riik_palk_2016[cols] <- lapply(riik_palk_2016[cols], factor)
riik_palk_2017[cols] <- lapply(riik_palk_2017[cols], factor)
riik_palk_2018[cols] <- lapply(riik_palk_2018[cols], factor)
riik_palk_2019[cols] <- lapply(riik_palk_2019[cols], factor)
kov_palk_2015[cols] <- lapply(kov_palk_2015[cols], factor)
kov_palk_2016[cols] <- lapply(kov_palk_2016[cols], factor)
kov_palk_2017[cols] <- lapply(kov_palk_2017[cols], factor)
kov_palk_2018[cols] <- lapply(kov_palk_2018[cols], factor)
kov_palk_2019[cols] <- lapply(kov_palk_2019[cols], factor)
kov_kogu_palk_2014[cols] <- lapply(kov_kogu_palk_2014[cols], factor)
kov_kogu_palk_2015[cols] <- lapply(kov_kogu_palk_2015[cols], factor)
kov_kogu_palk_2016[cols] <- lapply(kov_kogu_palk_2016[cols], factor)
kov_kogu_palk_2017[cols] <- lapply(kov_kogu_palk_2017[cols], factor)
kov_kogu_palk_2018[cols] <- lapply(kov_kogu_palk_2018[cols], factor)
riik_kogu_palk_2014[cols2] <- lapply(riik_kogu_palk_2014[cols2], factor)
riik_kogu_palk_2015[cols2] <- lapply(riik_kogu_palk_2015[cols2], factor)
riik_kogu_palk_2016[cols2] <- lapply(riik_kogu_palk_2016[cols2], factor)
riik_kogu_palk_2017[cols2] <- lapply(riik_kogu_palk_2017[cols2], factor)
riik_kogu_palk_2018[cols2] <- lapply(riik_kogu_palk_2018[cols2], factor)








riik_palk_2015$nimi <- paste(riik_palk_2015$Eesnimi, riik_palk_2015$Perekonnanimi)
riik_palk_2015 <- riik_palk_2015 %>% select(-c(Eesnimi, Perekonnanimi))
riik_palk_2015$Põhipalk <- gsub(",","", riik_palk_2015$Põhipalk)
riik_palk_2015$Põhipalk <- as.integer(riik_palk_2015$Põhipalk)

riik_palk_2016$nimi <- paste(riik_palk_2016$Eesnimi, riik_palk_2016$Perekonnanimi)
riik_palk_2016 <- riik_palk_2016 %>% select(-c(Eesnimi, Perekonnanimi))
riik_palk_2016$Põhipalk <- gsub(",","", riik_palk_2016$Põhipalk)
riik_palk_2016$Põhipalk <- as.integer(riik_palk_2016$Põhipalk)

riik_palk_2017$nimi <- paste(riik_palk_2017$Eesnimi, riik_palk_2017$Perekonnanimi)
riik_palk_2017 <- riik_palk_2017 %>% select(-c(Eesnimi, Perekonnanimi))
riik_palk_2017$Põhipalk <- gsub(",","", riik_palk_2017$Põhipalk)
riik_palk_2017$Põhipalk <- as.integer(riik_palk_2017$Põhipalk)

riik_palk_2018$nimi <- paste(riik_palk_2018$Eesnimi, riik_palk_2018$Perekonnanimi)
riik_palk_2018 <- riik_palk_2018 %>% select(-c(Eesnimi, Perekonnanimi))
riik_palk_2018$Põhipalk <- gsub(",","", riik_palk_2018$Põhipalk)
riik_palk_2018$Põhipalk <- as.integer(riik_palk_2018$Põhipalk)

riik_palk_2019$nimi <- paste(riik_palk_2019$Eesnimi, riik_palk_2019$Perekonnanimi)
riik_palk_2019 <- riik_palk_2019 %>% select(-c(Eesnimi, Perekonnanimi))
riik_palk_2019$Põhipalk <- gsub(",","", riik_palk_2019$Põhipalk)
riik_palk_2019$Põhipalk <- as.integer(riik_palk_2019$Põhipalk)


kov_palk_2015$nimi <- paste(kov_palk_2015$Eesnimi, kov_palk_2015$Perekonnanimi)
kov_palk_2015 <- kov_palk_2015 %>% select(-c(Eesnimi, Perekonnanimi))
kov_palk_2015$Põhipalk <- gsub(",","", kov_palk_2015$Põhipalk)
kov_palk_2015$Põhipalk <- as.integer(kov_palk_2015$Põhipalk)


kov_palk_2016$nimi <- paste(kov_palk_2016$Eesnimi, kov_palk_2016$Perekonnanimi)
kov_palk_2016 <- kov_palk_2016 %>% select(-c(Eesnimi, Perekonnanimi))
kov_palk_2016$Põhipalk <- gsub(",","", kov_palk_2016$Põhipalk)
kov_palk_2016$Põhipalk <- as.integer(kov_palk_2016$Põhipalk)


kov_palk_2017$nimi <- paste(kov_palk_2017$Eesnimi, kov_palk_2017$Perekonnanimi)
kov_palk_2017 <- kov_palk_2017 %>% select(-c(Eesnimi, Perekonnanimi))
kov_palk_2017$Põhipalk <- gsub(",","", kov_palk_2017$Põhipalk)
kov_palk_2017$Põhipalk <- as.integer(kov_palk_2017$Põhipalk)


kov_palk_2018$nimi <- paste(kov_palk_2018$Eesnimi, kov_palk_2018$Perekonnanimi)
kov_palk_2018 <- kov_palk_2018 %>% select(-c(Eesnimi, Perekonnanimi))
kov_palk_2018$Põhipalk <- gsub(",","", kov_palk_2018$Põhipalk)
kov_palk_2018$Põhipalk <- as.integer(kov_palk_2018$Põhipalk)


kov_palk_2019$nimi <- paste(kov_palk_2019$Eesnimi, kov_palk_2019$Perekonnanimi)
kov_palk_2019 <- kov_palk_2019 %>% select(-c(Eesnimi, Perekonnanimi))
kov_palk_2019$Põhipalk <- gsub(",","", kov_palk_2019$Põhipalk)
kov_palk_2019$Põhipalk <- as.integer(kov_palk_2019$Põhipalk)

kov_kogu_palk_2014$nimi <- paste(kov_kogu_palk_2014$Eesnimi, kov_kogu_palk_2014$Perekonnanimi)
kov_kogu_palk_2014 <- kov_kogu_palk_2014 %>% select(-c(Eesnimi, Perekonnanimi, V14))
# kov_kogu_palk_2014$`Põhipalk, sh puhkusetasu` <- as.numeric(gsub(",","", kov_kogu_palk_2014$`Põhipalk, sh puhkusetasu`))
# kov_kogu_palk_2014$Muutuvpalk <- as.numeric(gsub(",","", kov_kogu_palk_2014$Muutuvpalk))
# kov_kogu_palk_2014$`Muu tulu` <- as.numeric(gsub(",","", kov_kogu_palk_2014$`Muu tulu`))
# kov_kogu_palk_2014$`Kokku     (7+8+9)` <- as.integer(gsub(",","", kov_kogu_palk_2014$`Kokku     (7+8+9)`))
colnames(kov_kogu_palk_2014)[9] <- "Kokku"

kov_kogu_palk_2015$nimi <- paste(kov_kogu_palk_2015$Eesnimi, kov_kogu_palk_2015$Perekonnanimi)
kov_kogu_palk_2015 <- kov_kogu_palk_2015 %>% select(-c(Eesnimi, Perekonnanimi))
# kov_kogu_palk_2015$`Põhipalk, sh puhkusetasu` <- as.numeric(gsub(",","", kov_kogu_palk_2015$`Põhipalk, sh puhkusetasu`))
# kov_kogu_palk_2015$Muutuvpalk <- as.numeric(gsub(",","", kov_kogu_palk_2015$Muutuvpalk))
# kov_kogu_palk_2015$`Muu tulu` <- as.numeric(gsub(",","", kov_kogu_palk_2015$`Muu tulu`))
# kov_kogu_palk_2015$`Kokku     (7+8+9)` <- as.integer(gsub(",","", kov_kogu_palk_2015$`Kokku     (7+8+9)`))
colnames(kov_kogu_palk_2015)[8] <- "Kokku"

kov_kogu_palk_2016$nimi <- paste(kov_kogu_palk_2016$Eesnimi, kov_kogu_palk_2016$Perekonnanimi)
kov_kogu_palk_2016 <- kov_kogu_palk_2016 %>% select(-c(Eesnimi, Perekonnanimi))
#kov_kogu_palk_2016$`Põhipalk, sh puhkusetasu` <- as.numeric(gsub(",","", kov_kogu_palk_2016$`Põhipalk, sh puhkusetasu`))
# kov_kogu_palk_2016$Muutuvpalk <- as.numeric(gsub(",","", kov_kogu_palk_2016$Muutuvpalk))
# kov_kogu_palk_2016$`Muu tulu` <- as.numeric(gsub(",","", kov_kogu_palk_2016$`Muu tulu`))
# kov_kogu_palk_2016$`Kokku     (7+8+9)` <- as.integer(gsub(",","", kov_kogu_palk_2016$`Kokku     (7+8+9)`))
colnames(kov_kogu_palk_2016)[8] <- "Kokku"

kov_kogu_palk_2017$nimi <- paste(kov_kogu_palk_2017$Eesnimi, kov_kogu_palk_2017$Perekonnanimi)
kov_kogu_palk_2017 <- kov_kogu_palk_2017 %>% select(-c(Eesnimi, Perekonnanimi))
#kov_kogu_palk_2017$`Põhipalk, sh puhkusetasu` <- as.numeric(gsub(",","", kov_kogu_palk_2017$`Põhipalk, sh puhkusetasu`))
kov_kogu_palk_2017$Muutuvpalk <- as.numeric(gsub(",","", kov_kogu_palk_2017$Muutuvpalk))
kov_kogu_palk_2017$`Muu tulu` <- as.numeric(gsub(",","", kov_kogu_palk_2017$`Muu tulu`))
#kov_kogu_palk_2017$`Kokku     (7+8+9)` <- as.integer(gsub(",","", kov_kogu_palk_2017$`Kokku     (7+8+9)`))
colnames(kov_kogu_palk_2017)[8] <- "Kokku"


kov_kogu_palk_2018$nimi <- paste(kov_kogu_palk_2018$Eesnimi, kov_kogu_palk_2018$Perekonnanimi)
kov_kogu_palk_2018 <- kov_kogu_palk_2018 %>% select(-c(Eesnimi, Perekonnanimi, V13))
kov_kogu_palk_2018$`Põhipalk, sh puhkusetasu` <- as.numeric(gsub(",","", kov_kogu_palk_2018$`Põhipalk, sh puhkusetasu`))
kov_kogu_palk_2018$Muutuvpalk <- as.numeric(gsub(",","", kov_kogu_palk_2018$Muutuvpalk))
kov_kogu_palk_2018$`Muu tulu` <- as.numeric(gsub(",","", kov_kogu_palk_2018$`Muu tulu`))
kov_kogu_palk_2018$`Kokku     (7+8+9)` <- as.integer(gsub(",","", kov_kogu_palk_2018$`Kokku     (7+8+9)`))
colnames(kov_kogu_palk_2018)[8] <- "Kokku"

riik_kogu_palk_2014$nimi <- paste(riik_kogu_palk_2014$Eesnimi, riik_kogu_palk_2014$Perekonnanimi)
riik_kogu_palk_2014 <- riik_kogu_palk_2014 %>% select(-c(Eesnimi, Perekonnanimi))
riik_kogu_palk_2014$`Põhipalk, sh puhkusetasu` <- as.numeric(gsub(",","", riik_kogu_palk_2014$`Põhipalk, sh puhkusetasu`))
riik_kogu_palk_2014$Muutuvpalk <- as.numeric(gsub(",","", riik_kogu_palk_2014$Muutuvpalk))
riik_kogu_palk_2014$`Muu tulu` <- as.numeric(gsub(",","", riik_kogu_palk_2014$`Muu tulu`))
riik_kogu_palk_2014$`Kokku     (8+9+10)` <- as.integer(gsub(",","", riik_kogu_palk_2014$`Kokku     (8+9+10)`))
colnames(riik_kogu_palk_2014)[9] <- "Kokku"

riik_kogu_palk_2015$nimi <- paste(riik_kogu_palk_2015$Eesnimi, riik_kogu_palk_2015$Perekonnanimi)
riik_kogu_palk_2015 <- riik_kogu_palk_2015 %>% select(-c(Eesnimi, Perekonnanimi))
# riik_kogu_palk_2015$`Põhipalk, sh puhkusetasu` <- as.numeric(gsub(",","", riik_kogu_palk_2015$`Põhipalk, sh puhkusetasu`))
# riik_kogu_palk_2015$Muutuvpalk <- as.numeric(gsub(",","", riik_kogu_palk_2015$Muutuvpalk))
# riik_kogu_palk_2015$`Muu tulu` <- as.numeric(gsub(",","", riik_kogu_palk_2015$`Muu tulu`))
# riik_kogu_palk_2015$`Kokku     (7+8+9)` <- as.integer(gsub(",","", riik_kogu_palk_2015$`Kokku     (7+8+9)`))
colnames(riik_kogu_palk_2015)[9] <- "Kokku"


riik_kogu_palk_2016$nimi <- paste(riik_kogu_palk_2016$Eesnimi, riik_kogu_palk_2016$Perekonnanimi)
riik_kogu_palk_2016 <- riik_kogu_palk_2016 %>% select(-c(Eesnimi, Perekonnanimi))
# riik_kogu_palk_2016$`Põhipalk, sh puhkusetasu` <- as.numeric(gsub(",","", riik_kogu_palk_2016$`Põhipalk, sh puhkusetasu`))
# riik_kogu_palk_2016$Muutuvpalk <- as.numeric(gsub(",","", riik_kogu_palk_2016$Muutuvpalk))
# riik_kogu_palk_2016$`Muu tulu` <- as.numeric(gsub(",","", riik_kogu_palk_2016$`Muu tulu`))
# riik_kogu_palk_2016$`Kokku     (7+8+9)` <- as.integer(gsub(",","", riik_kogu_palk_2016$`Kokku     (7+8+9)`))
colnames(riik_kogu_palk_2016)[8] <- "Kokku"


riik_kogu_palk_2017$nimi <- paste(riik_kogu_palk_2017$Eesnimi, riik_kogu_palk_2018$Perekonnanimi)
riik_kogu_palk_2017 <- riik_kogu_palk_2017 %>% select(-c(Eesnimi, Perekonnanimi))
riik_kogu_palk_2017$`Põhipalk, sh puhkusetasu` <- as.numeric(gsub(",","", riik_kogu_palk_2017$`Põhipalk, sh puhkusetasu`))
riik_kogu_palk_2017$Muutuvpalk <- as.numeric(gsub(",","", riik_kogu_palk_2017$Muutuvpalk))
riik_kogu_palk_2017$`Muu tulu` <- as.numeric(gsub(",","", riik_kogu_palk_2017$`Muu tulu`))
riik_kogu_palk_2017$`Kokku     (7+8+9)` <- as.integer(gsub(",","", riik_kogu_palk_2017$`Kokku     (7+8+9)`))
colnames(riik_kogu_palk_2017)[8] <- "Kokku"


riik_kogu_palk_2018$nimi <- paste(riik_kogu_palk_2018$Eesnimi, riik_kogu_palk_2018$Perekonnanimi)
riik_kogu_palk_2018 <- riik_kogu_palk_2018 %>% select(-c(Eesnimi, Perekonnanimi))
riik_kogu_palk_2018$`Põhipalk, sh puhkusetasu` <- as.numeric(gsub(",","", riik_kogu_palk_2018$`Põhipalk, sh puhkusetasu`))
riik_kogu_palk_2018$Muutuvpalk <- as.numeric(gsub(",","", riik_kogu_palk_2018$Muutuvpalk))
riik_kogu_palk_2018$`Muu tulu` <- as.numeric(gsub(",","", riik_kogu_palk_2018$`Muu tulu`))
riik_kogu_palk_2018$`Kokku     (7+8+9)` <- as.integer(gsub(",","", riik_kogu_palk_2018$`Kokku     (7+8+9)`))
colnames(riik_kogu_palk_2018)[8] <- "Kokku"





datasetList <-  c(
  "Ametnike põhipalk ja muutuvpalk ning teenistusülesannete täitmisest tulenev muu tulu riigi ametiasutustes 2018" = "riik_kogu_palk_2018",
  "Ametnike põhipalk ja muutuvpalk ning teenistusülesannete täitmisest tulenev muu tulu riigi ametiasutustes 2017" = "riik_kogu_palk_2017",
  "Ametnike põhipalk ja muutuvpalk ning teenistusülesannete täitmisest tulenev muu tulu riigi ametiasutustes 2016" = "riik_kogu_palk_2016",
  "Ametnike põhipalk ja muutuvpalk ning teenistusülesannete täitmisest tulenev muu tulu riigi ametiasutustes 2015" = "riik_kogu_palk_2015",
  "Ametnike põhipalk ja muutuvpalk ning teenistusülesannete täitmisest tulenev muu tulu riigi ametiasutustes 2014" = "riik_kogu_palk_2014",
  "Ametnike põhipalk ja muutuvpalk ning teenistusülesannete täitmisest tulenev muu tulu KOV üksuste ametiasutustes 2018" = "kov_kogu_palk_2018",
  "Ametnike põhipalk ja muutuvpalk ning teenistusülesannete täitmisest tulenev muu tulu KOV üksuste ametiasutustes 2017" = "kov_kogu_palk_2017",
  "Ametnike põhipalk ja muutuvpalk ning teenistusülesannete täitmisest tulenev muu tulu KOV üksuste ametiasutustes 2016" = "kov_kogu_palk_2016",
  "Ametnike põhipalk ja muutuvpalk ning teenistusülesannete täitmisest tulenev muu tulu KOV üksuste ametiasutustes 2015" = "kov_kogu_palk_2015",
    "Ametnike põhipalk ja muutuvpalk ning teenistusülesannete täitmisest tulenev muu tulu KOV üksuste ametiasutustes 2014" = "kov_kogu_palk_2014",
"Ametnike põhipalk KOV üksuste ametiasutustes 2019" = "kov_palk_2019" ,
"Ametnike põhipalk KOV üksuste ametiasutustes 2018" = "kov_palk_2018" ,
"Ametnike põhipalk KOV üksuste ametiasutustes 2017" = "kov_palk_2017" ,
"Ametnike põhipalk KOV üksuste ametiasutustes 2016" = "kov_palk_2016" ,
 "Ametnike põhipalk KOV üksuste ametiasutustes 2015" = "kov_palk_2015",
"Ametnike põhipalk riigi ametiasutustes 2019" = "riik_palk_2019",
"Ametnike põhipalk riigi ametiasutustes 2018" = "riik_palk_2018",
"Ametnike põhipalk riigi ametiasutustes 2017" = "riik_palk_2017",
"Ametnike põhipalk riigi ametiasutustes 2016" = "riik_palk_2016",
 "Ametnike põhipalk riigi ametiasutustes 2015" = "riik_palk_2015")
