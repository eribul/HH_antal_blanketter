


is.inca <- function(){
  unname(!(Sys.info()["user"] == "erikbulow"))
}

if (!is.inca()){
  setwd("~/Documents/huvud_hals/atal_blanketter")
  rm(list=ls())
  is.inca <- function(){
    unname(!(Sys.info()["user"] == "erikbulow"))
  }
  if (!file.exists("ov.rda")) {
    df <- read.csv2("cx2.txt")
    df_rec <- read.csv2("cx4.txt")
    save(df, file = "ov.rda")
    save(df_rec, file = "ov4.rda")
  } else {
    load("ov.rda")
    load("ov4.rda")
  }
  param <- list(
      Länindelning = "Sydöstra",
      Från = 2008,
      Till = 2014,
      Diagnos = "Cervix"
  )
}



################################################################################
#                                                                              #
#           Gemensam bearbetningar för indikatorer (Blankett 1,2,3,5           #
#                                                                              #
################################################################################
library(Epi)
library(gdata)
library(reshape2)
library(jsonlite)
library(plyr)
library(dplyr)
data <- df
names(data)<-tolower(colnames(data))  # Gemener till alla variabelnamn
data$region<-trim(as.character(data$region))
data$a_diadat_ar <- substring(data$a_diadat,1,4)

Länindelning<- param[["Länindelning"]]
if(Länindelning == "Sydöstra"){
  data$a_lkf[nchar(data$a_lkf)== 5] <- paste0("0", data$a_lkf)[nchar(data$a_lkf)== 5]
  ##### Konvertering av de två länen i Uppsala/Örebro till Sydöstra ######
  data$a_lkf <- as.character(data$a_lkf)
  data$region <- as.character(data$region)
  data$region[substr(as.character(data$a_lkf), 1, 2) %in% c("19", "04")] <- "Region Sydöstra"
  ########################################################################
}

########## Ersätt inrapporterade klinik/sjukhus om Hemsjukhus = "Nej" ###############
data$a_rappsjhemsj_beskrivning <- as.character(data$a_rappsjhemsj_beskrivning)
data$a_inrappsjh <- as.character(data$a_inrappsjh)
data$sjukhuskod <- as.character(data$sjukhuskod)
data$a_hemklinik <- as.character(data$a_hemklinik)
data$a_inrappklk <- as.character(data$a_inrappklk)

data$a_inrappsjh[data$a_rappsjhemsj_beskrivning == "Nej" & !is.na(data$sjukhuskod) ] <- data$sjukhuskod[data$a_rappsjhemsj_beskrivning == "Nej" & !is.na(data$sjukhuskod)]
data$a_inrappklk[data$a_rappsjhemsj_beskrivning == "Nej" & !is.na(data$a_hemklinik) ] <- data$a_hemklinik[data$a_rappsjhemsj_beskrivning == "Nej" & !is.na(data$a_hemklinik) ]
####################################################################################


data$regionny[data$region=="Region Norr"]<-1
data$regionny[data$region=="Region Uppsala/Örebro"]<-2
data$regionny[data$region=="Region Sthlm/Gotland"]<-3
data$regionny[data$region=="Region Väst"]<-4
data$regionny[data$region=="Region Sydöstra"]<-5
data$regionny[data$region=="Region Syd"]<-6

data$region.f<-factor(data$regionny,exclude=NULL)
region_list <- list(
  "Norra"=1,
  "Uppsala/Örebro"=2,
  "Sthlm/Gotland"=3,
  "Västra"=4,
  "Sydöstra"=5,
  "Södra"=6
)
levels(data$region.f) <- region_list

################################################################################
#                                                                              #
#              Gemensam bearbetningar för indikatorer (Blankett 4              #
#                                                                              #
################################################################################
data_rec <- df_rec
names(data_rec)<-tolower(colnames(data_rec))  # Gemener till alla variabelnamn
data_rec$region<-trim(as.character(data_rec$region))
data_rec$a_diadat_ar <- substring(data_rec$a_diadat,1,4)

Länindelning<- param[["Länindelning"]]
if(Länindelning == "Sydöstra"){
  data_rec$a_lkf[nchar(data_rec$a_lkf)== 5] <- paste0("0", data_rec$a_lkf)[nchar(data_rec$a_lkf)== 5]
  ##### Konvertering av de två länen i Uppsala/Örebro till Sydöstra ######
  data_rec$a_lkf <- as.character(data_rec$a_lkf)
  data_rec$region <- as.character(data_rec$region)
  data_rec$region[substr(as.character(data_rec$a_lkf), 1, 2) %in% c("19", "04")] <- "Region Sydöstra"
  ########################################################################
}



data_rec$regionny[data_rec$region=="Region Norr"]<-1
data_rec$regionny[data_rec$region=="Region Uppsala/Örebro"]<-2
data_rec$regionny[data_rec$region=="Region Sthlm/Gotland"]<-3
data_rec$regionny[data_rec$region=="Region Väst"]<-4
data_rec$regionny[data_rec$region=="Region Sydöstra"]<-5
data_rec$regionny[data_rec$region=="Region Syd"]<-6

data_rec$region.f<-factor(data_rec$regionny,exclude=NULL)
region_list <- list(
  "Norra"=1,
  "Uppsala/Örebro"=2,
  "Sthlm/Gotland"=3,
  "Västra"=4,
  "Sydöstra"=5,
  "Södra"=6
)
levels(data_rec$region.f) <- region_list




######## PARAMETRAR ########
if (is.inca()) {
  Från<- as.numeric(param[["Från"]])
  Till<- as.numeric(param[["Till"]])
  Diagnos<- param[["Diagnos"]]
} else {
  ## Lokalt
  Från<- 2008
  Till<- 2014
  Diagnos<- "Cervix"
}
############################


# Subsetta diagnoser
data <- subset(data, a_icd_gruppnamn %in% Diagnos)
data_rec <- subset(data_rec, a_icd_gruppnamn %in% Diagnos)
Diagnos <- paste(Diagnos, collapse=",")









############################ ############################ #############################
#                                                                                     #
#                                                                                     #
#                           Indikator 1: Antal blankett 1                             #
#                                                                                     #
#                                                                                     #
############################ ############################ #############################

# Subsettar enbart de med inrapporterat datum
data1 <- data %>%
  filter(!is.na(a_inrappdatum) & a_inrappdatum !="" & a_diadat_ar != "" & a_diadat_ar >= 2011)


# Lägga till klinik om användaren loggar in på en sådan med data
unit <- nchar(data1$userunitcode[1])
klinik <- data1[0, ]

if (unit != 1 ) klinik<- subset(data1,(data1$userunitcode==data1$a_inrappklk &
                                         data1$userparentunitcode==data1$a_inrappsjh))
if (unit != 1 & nrow(klinik) > 0) hemregion <- sort(as.character(klinik$region.f),decreasing=TRUE)[1]

if (unit != 1 & nrow(klinik) > 0) levels(data1$region.f) <- sub(hemregion, paste(hemregion,"(exkl. din klinik)"), levels(data1$region.f))


if (unit != 1 & nrow(klinik) > 0) data1<- subset(data1,(!(data1$userunitcode==data1$a_inrappklk &
                                                            data1$userparentunitcode==data1$a_inrappsjh) | (is.na(data1$a_inrappklk) | is.na(data1$a_inrappsjh))))

if (unit != 1 & nrow(klinik) > 0) klinik$region.f<-"Din klinik"
if (unit != 1 & nrow(klinik) > 0) data1<-rbind(data1,klinik)



data1 <- data1 %>%
  select(a_diadat_ar, region.f)

# Skapa kategorivariabel i.e. år till JS format
kat1 <- levels(as.factor(data1$a_diadat_ar))
kat1 <- paste0("var kat1 = ['", paste(kat1,collapse = "','"), "'];" )

# Skapa regionsvariabel med värden till JS format
if(nrow(data1) != 0){
data1 <- dcast(data1, region.f ~ a_diadat_ar, value.var = "region.f", fun.aggregate = length)
data1$data <- do.call(paste, data1[2:ncol(data1)])
data1$data <- gsub(" ", ",", data1$data)
data1$data <- paste0("[",data1$data,"]")
data1 <- data1[c("region.f", "data")]
names(data1) <- c("name", "data")
ser1 <- paste("var","ser1","=",toJSON(data1), ";")
ser1 <- gsub('\\"\\[', '[', ser1  )
ser1 <- gsub('\\]\\"', ']', ser1  )
}
if(nrow(data1) == 0){
  ser1 <- "var ser1 = [] ;"
}
# Skapa titel
titel1 <- paste("Antal anmälningsblanketter")
titel1 <- paste("var titel1 =", "'",titel1,"'", ";")
subtitel1 <- paste("(Urval: ",Diagnos,")", sep ="")
subtitel1 <- paste("var subtitel1 =", "'",subtitel1,"'", ";")


# Namn på axlar
yaxis1 <- paste0("var yaxis1 = ","'" ,"Antal blanketter", "'" ,";")
xaxis1 <- paste0("var xaxis1 = ","'", "Diagnosår","'" ,";")









############################ ############################ #############################
#                                                                                     #
#                                                                                     #
#                           Indikator 2: Antal blankett 2                             #
#                                                                                     #
#                                                                                     #
############################ ############################ #############################

# Subsettar enbart de med inrapporterat datum
data2 <- data %>%
  filter(!is.na(k_inrappdat) & k_inrappdat !="" & a_diadat_ar != "" & a_diadat_ar >= 2011)

# Lägga till klinik om användaren loggar in på en sådan med data
unit <- nchar(data2$userunitcode[1])
klinik <- data2[0, ]


if (unit != 1 ) klinik<- subset(data2,(data2$userunitcode==data2$k_inrappenhklkkod &
                                         data2$userparentunitcode==data2$k_inrappenhsjhkod))
if (unit != 1 & nrow(klinik) > 0) hemregion <- sort(as.character(klinik$region.f),decreasing=TRUE)[1]

if (unit != 1 & nrow(klinik) > 0) levels(data2$region.f) <- sub(hemregion, paste(hemregion,"(exkl. din klinik)"), levels(data2$region.f))


if (unit != 1 & nrow(klinik) > 0) data2<- subset(data2,(!(data2$userunitcode==data2$k_inrappenhklkkod &
                                         data2$userparentunitcode==data2$k_inrappenhsjhkod) | (is.na(data2$k_inrappenhklkkod) | is.na(data2$k_inrappenhsjhkod))))

if (unit != 1 & nrow(klinik) > 0) klinik$region.f<-"Din klinik"
if (unit != 1 & nrow(klinik) > 0) data2<-rbind(data2,klinik)

data2 <- data2 %>%
  select(a_diadat_ar, region.f)

# Skapa kategorivariabel i.e. år till JS format
kat2 <- levels(as.factor(data2$a_diadat_ar))
kat2 <- paste0("var kat2 = ['", paste(kat2,collapse = "','"), "'];" )

# Skapa regionsvariabel med värden till JS format
if(nrow(data2) != 0){
data2 <- dcast(data2, region.f ~ a_diadat_ar, value.var = "region.f", fun.aggregate = length)
data2$data <- do.call(paste, data2[2:ncol(data2)])
data2$data <- gsub(" ", ",", data2$data)
data2$data <- paste0("[",data2$data,"]")
data2 <- data2[c("region.f", "data")]
names(data2) <- c("name", "data")
ser2 <- paste("var","ser2","=",toJSON(data2), ";")
ser2 <- gsub('\\"\\[', '[', ser2  )
ser2 <- gsub('\\]\\"', ']', ser2  )
}
if(nrow(data2) == 0){
  ser2 <- "var ser2 = [] ;"
}
# Skapa titel
titel2 <- paste("Antal kirurgiblanketter")
titel2 <- paste("var titel2 =", "'",titel2,"'", ";")
subtitel2 <- paste("(Urval: ",Diagnos,")", sep ="")
subtitel2 <- paste("var subtitel2 =", "'",subtitel2,"'", ";")


# Namn på axlar
yaxis2 <- paste0("var yaxis2 = ","'" ,"Antal blanketter", "'" ,";")
xaxis2 <- paste0("var xaxis2 = ","'", "Diagnosår","'" ,";")






############################ ############################ #############################
#                                                                                     #
#                                                                                     #
#                           Indikator 3: Antal blankett 3                             #
#                                                                                     #
#                                                                                     #
############################ ############################ #############################



# Subsettar enbart de med inrapporterat datum
data3 <- data %>%
  filter(!is.na(p_inrappdat) & p_inrappdat !="" & a_diadat_ar != "" & a_diadat_ar >= 2011)


# Lägga till klinik om användaren loggar in på en sådan med data
unit <- nchar(data3$userunitcode[1])
klinik <- data3[0, ]

if (unit != 1 ) klinik<- subset(data3,(data3$userunitcode==data3$p_inrappenhklkkod &
                                         data3$userparentunitcode==data3$p_inrappenhsjhkod))

if (unit != 1 & nrow(klinik) > 0) hemregion <- sort(as.character(klinik$region.f),decreasing=TRUE)[1]

if (unit != 1 & nrow(klinik) > 0) levels(data3$region.f) <- sub(hemregion, paste(hemregion,"(exkl. din klinik)"), levels(data3$region.f))


if (unit != 1 & nrow(klinik) > 0) data3<- subset(data3,(!(data3$userunitcode==data3$p_inrappenhklkkod &
                                                            data3$userparentunitcode==data3$p_inrappenhsjhkod) | (is.na(data3$p_inrappenhklkkod) | is.na(data3$p_inrappenhsjhkod))))

if (unit != 1 & nrow(klinik) > 0) klinik$region.f<-"Din klinik"
if (unit != 1 & nrow(klinik) > 0) data3<-rbind(data3,klinik)

data3 <- data3 %>%
  select(a_diadat_ar, region.f)



# Skapa kategorivariabel i.e. år till JS format
kat3 <- levels(as.factor(data3$a_diadat_ar))
kat3 <- paste0("var kat3 = ['", paste(kat3,collapse = "','"), "'];" )

# Skapa regionsvariabel med värden till JS format
if(nrow(data3) != 0){
data3 <- dcast(data3, region.f ~ a_diadat_ar, value.var = "region.f", fun.aggregate = length)
data3$data <- do.call(paste, data3[2:ncol(data3)])
data3$data <- gsub(" ", ",", data3$data)
data3$data <- paste0("[",data3$data,"]")
data3 <- data3[c("region.f", "data")]
names(data3) <- c("name", "data")
ser3 <- paste("var","ser3","=",toJSON(data3), ";")
ser3 <- gsub('\\"\\[', '[', ser3  )
ser3 <- gsub('\\]\\"', ']', ser3  )
}
if(nrow(data3) == 0){
  ser3 <- "var ser3 = [] ;"
}
# Skapa titel
titel3 <- paste("Antal primärbehandlingsblanketter")
titel3 <- paste("var titel3 =", "'",titel3,"'", ";")
subtitel3 <- paste("(Urval: ",Diagnos,")", sep ="")
subtitel3 <- paste("var subtitel3 =", "'",subtitel3,"'", ";")


# Namn på axlar
yaxis3 <- paste0("var yaxis3 = ","'" ,"Antal blanketter", "'" ,";")
xaxis3 <- paste0("var xaxis3 = ","'", "Diagnosår","'" ,";")







############################ ############################ #############################
#                                                                                     #
#                                                                                     #
#                           Indikator 4: Antal blankett 4                             #
#                           (O.B.S. här använder vi en annan DF)                      #
#                                                                                     #
############################ ############################ #############################



# Subsettar enbart de med inrapporterat datum, då denna data tages från annan DF så behöver vi även sortera diagnosår
data4 <- data_rec %>%
  filter(!is.na(r_inrappdat) & a_diadat_ar >= Från  & a_diadat_ar <= Till & r_inrappdat !="" & a_diadat_ar != "" & a_diadat_ar >= 2011)


# Lägga till klinik om användaren loggar in på en sådan med data
unit <- nchar(data4$userunitcode[1])
klinik <- data4[0, ]


if (unit != 1 ) klinik<- subset(data4,(data4$userunitcode==data4$r_inrappenhklkkod &
                                         data4$userparentunitcode==data4$r_inrappenhsjhkod))

if (unit != 1 & nrow(klinik) > 0) hemregion <- sort(as.character(klinik$region.f),decreasing=TRUE)[1]

if (unit != 1 & nrow(klinik) > 0) levels(data4$region.f) <- sub(hemregion, paste(hemregion,"(exkl. din klinik)"), levels(data4$region.f))


if (unit != 1 & nrow(klinik) > 0) data4<- subset(data4,(!(data4$userunitcode==data4$r_inrappenhklkkod &
                                                            data4$userparentunitcode==data4$r_inrappenhsjhkod) | (is.na(data4$r_inrappenhklkkod) | is.na(data4$r_inrappenhsjhkod))))

if (unit != 1 & nrow(klinik) > 0) klinik$region.f<-"Din klinik"
if (unit != 1 & nrow(klinik) > 0) data4<-rbind(data4,klinik)



data4 <- data4 %>%
  select(a_diadat_ar, region.f)



# Skapa kategorivariabel i.e. år till JS format
kat4 <- levels(as.factor(data4$a_diadat_ar))
kat4 <- paste0("var kat4 = ['", paste(kat4,collapse = "','"), "'];" )

# Skapa regionsvariabel med värden till JS format
if(nrow(data4) != 0){
data4 <- dcast(data4, region.f ~ a_diadat_ar, value.var = "region.f", fun.aggregate = length)
data4$data <- do.call(paste, data4[2:ncol(data4)])
data4$data <- gsub(" ", ",", data4$data)
data4$data <- paste0("[",data4$data,"]")
data4 <- data4[c("region.f", "data")]
names(data4) <- c("name", "data")
ser4 <- paste("var","ser4","=",toJSON(data4), ";")
ser4 <- gsub('\\"\\[', '[', ser4  )
ser4 <- gsub('\\]\\"', ']', ser4  )
}
if(nrow(data4) == 0){
  ser4 <- "var ser4 = [] ;"
}
# Skapa titel
titel4 <- paste("Antal recidivblanketter")
titel4 <- paste("var titel4 =", "'",titel4,"'", ";")
subtitel4 <- paste("(Urval: ",Diagnos,")", sep ="")
subtitel4 <- paste("var subtitel4 =", "'",subtitel4,"'", ";")


# Namn på axlar
yaxis4 <- paste0("var yaxis4 = ","'" ,"Antal blanketter", "'" ,";")
xaxis4 <- paste0("var xaxis4 = ","'", "Diagnosår","'" ,";")






############################ ############################ #############################
#                                                                                     #
#                                                                                     #
#                           Indikator 5: Antal blankett 5                             #
#                                                                                     #
#                                                                                     #
############################ ############################ #############################

# Subsettar enbart de med inrapporterat datum
data5 <- data %>%
  filter(!is.na(u_inrappdat) & u_inrappdat !="" & a_diadat_ar != "" & a_diadat_ar >= 2011)

# Lägga till klinik om användaren loggar in på en sådan med data
unit <- nchar(data5$userunitcode[1])
klinik <- data5[0, ]

if (unit != 1 ) klinik<- subset(data5,(data5$userunitcode==data5$u_inrappenhklkkod &
                                         data5$userparentunitcode==data5$u_inrappenhsjhkod))

if (unit != 1 & nrow(klinik) > 0) hemregion <- sort(as.character(klinik$region.f),decreasing=TRUE)[1]

if (unit != 1 & nrow(klinik) > 0) levels(data5$region.f) <- sub(hemregion, paste(hemregion,"(exkl. din klinik)"), levels(data5$region.f))


if (unit != 1 & nrow(klinik) > 0) data5<- subset(data5,(!(data5$userunitcode==data5$u_inrappenhklkkod &
                                                            data5$userparentunitcode==data5$u_inrappenhsjhkod) | (is.na(data5$u_inrappenhklkkod) | is.na(data5$u_inrappenhsjhkod))))

if (unit != 1 & nrow(klinik) > 0) klinik$region.f<-"Din klinik"
if (unit != 1 & nrow(klinik) > 0) data5<-rbind(data5,klinik)


data5 <- data5 %>%
  select(a_diadat_ar, region.f)



# Skapa kategorivariabel i.e. år till JS format
kat5 <- levels(as.factor(data5$a_diadat_ar))
kat5 <- paste0("var kat5 = ['", paste(kat5,collapse = "','"), "'];" )

# Skapa regionsvariabel med värden till JS format
if(nrow(data5) != 0){
data5 <- dcast(data5, region.f ~ a_diadat_ar, value.var = "region.f", fun.aggregate = length)
data5$data <- do.call(paste, data5[2:ncol(data5)])
data5$data <- gsub(" ", ",", data5$data)
data5$data <- paste0("[",data5$data,"]")
data5 <- data5[c("region.f", "data")]
names(data5) <- c("name", "data")
ser5 <- paste("var","ser5","=",toJSON(data5), ";")
ser5 <- gsub('\\"\\[', '[', ser5  )
ser5 <- gsub('\\]\\"', ']', ser5  )
}
if(nrow(data5) == 0){
  ser5 <- "var ser5 = [] ;"
}
# Skapa titel
titel5 <- paste("Antal uppföljningsblanketter")
titel5 <- paste("var titel5 =", "'",titel5,"'", ";")
subtitel5 <- paste("(Urval: ",Diagnos,")", sep ="")
subtitel5 <- paste("var subtitel5 =", "'",subtitel5,"'", ";")


# Namn på axlar
yaxis5 <- paste0("var yaxis5 = ","'" ,"Antal blanketter", "'" ,";")
xaxis5 <- paste0("var xaxis5 = ","'", "Diagnosår","'" ,";")








############################ ############################ #############################
#                                                                                     #
#                                                                                     #
#                           Indikator 6: Täckningsgrad blankett 2                     #
#                                                                                     #
#                                                                                     #
############################ ############################ #############################


data6 <- subset(data, a_primop_beskrivning != "Planeras ej" &
                  a_diadat_ar >= 2011, c("k_inrappdat","a_diadat_ar", "region.f")  )

### Parametrar för återanvänding av skript ####
rappdatum <- "k_inrappdat"


###################### Bearbetning av data för att härleda täckningsgrad ######################
# Skapa kategorivariabel i.e. år till JS format
kat6 <- levels(as.factor(data6$a_diadat_ar))
kat6 <- paste0("var kat6 = ['", paste(kat6,collapse = "','"), "'];" )
# Finns den aktuella blanketten eller inte
data6$rappdatum <- !is.na(as.Date(data6[[rappdatum]], format = "%Y-%m-%d"))
if(nrow(data6) != 0){
data6 <- dcast(data6, region.f ~ a_diadat_ar, fun.aggregate = mean, value.var = "rappdatum", drop = FALSE)
# Sätt eventuellt NAs på grund av 0 till 0 samt runda av täckningsgrader
data6[2:ncol(data6)] <- round(data6[2:ncol(data6)]*100,0)
data6[is.na(data6)] <- "null"

# Konvertera till JSON format
data6$data <- do.call(paste, data6[2:ncol(data6)])
data6$data <- gsub(" ", ",", data6$data)
data6$data <- paste0("[",data6$data,"]")
data6 <- data6[c("region.f", "data")]
names(data6) <- c("name", "data")
ser6 <- paste("var","ser6","=",toJSON(data6), ";")
ser6 <- gsub('\\"\\[', '[', ser6  )
ser6 <- gsub('\\]\\"', ']', ser6  )
}
if(nrow(data6) == 0){
  ser6 <- "var ser3 = [] ;"
}

# Skapa titel
titel6 <- paste("Intern täckningsgrad (%) för kirurgiblanketten")
titel6 <- paste("var titel6 =", "'",titel6,"'", ";")
subtitel6 <- paste("(Urval: ",Diagnos,")", sep ="")
subtitel6 <- paste("var subtitel6 =", "'",subtitel6,"'", ";")


yaxis6 <- paste0("var yaxis6 = ","'" ,"Täckningsgrad (%)", "'" ,";")
xaxis6 <- paste0("var xaxis6 = ","'", "Diagnosår","'" ,";")






############################ ############################ #############################
#                                                                                     #
#                                                                                     #
#                           Indikator 7: Täckningsgrad blankett 3                     #
#                                                                                     #
#                                                                                     #
############################ ############################ #############################


data7 <- subset(data, a_diadat_ar >= 2011, c("p_inrappdat","a_diadat_ar", "region.f")  )

### Parametrar för återanvänding av skript ####
rappdatum <- "p_inrappdat"


###################### Bearbetning av data för att härleda täckningsgrad ######################
# Skapa kategorivariabel i.e. år till JS format
kat7 <- levels(as.factor(data7$a_diadat_ar))
kat7 <- paste0("var kat7 = ['", paste(kat7,collapse = "','"), "'];" )
# Finns den aktuella blanketten eller inte
data7$rappdatum <- !is.na(as.Date(data7[[rappdatum]], format = "%Y-%m-%d"))
if(nrow(data7) != 0){
data7 <- dcast(data7, region.f ~ a_diadat_ar, fun.aggregate = mean, value.var = "rappdatum", drop = FALSE)
# Sätt eventuellt NAs på grund av 0 till 0 samt runda av täckningsgrader
data7[2:ncol(data7)] <- round(data7[2:ncol(data7)]*100,0)
data7[is.na(data7)] <- "null"

# Konvertera till JSON format
data7$data <- do.call(paste, data7[2:ncol(data7)])
data7$data <- gsub(" ", ",", data7$data)
data7$data <- paste0("[",data7$data,"]")
data7 <- data7[c("region.f", "data")]
names(data7) <- c("name", "data")
ser7 <- paste("var","ser7","=",toJSON(data7), ";")
ser7 <- gsub('\\"\\[', '[', ser7  )
ser7 <- gsub('\\]\\"', ']', ser7  )
}
if(nrow(data7) == 0){
  ser7 <- "var ser7 = [] ;"
}
# Skapa titel
titel7 <- paste("Intern täckningsgrad (%) för primärbehandlingsblanketten")
titel7 <- paste("var titel7 =", "'",titel7,"'", ";")
subtitel7 <- paste("(Urval: ",Diagnos,")", sep ="")
subtitel7 <- paste("var subtitel7 =", "'",subtitel7,"'", ";")


yaxis7 <- paste0("var yaxis7 = ","'" ,"Täckningsgrad (%)", "'" ,";")
xaxis7 <- paste0("var xaxis7 = ","'", "Diagnosår","'" ,";")






############################ ############################ #############################
#                                                                                     #
#                                                                                     #
#                           Indikator 8: Täckningsgrad blankett 4                     #
#                                                                                     #
#                                                                                     #
############################ ############################ #############################

#  Då denna data tages från annan DF så behöver vi även sortera diagnosår
data8 <- subset(data_rec, !is.na(u_datreci)
                & u_datreci != "" & a_diadat_ar >= Från &
                  a_diadat_ar <= Till & a_diadat_ar >= 2011, c("r_inrappdat","a_diadat_ar", "region.f")  )

### Parametrar för återanvänding av skript ####
rappdatum <- "r_inrappdat"


###################### Bearbetning av data för att härleda täckningsgrad ######################
# Skapa kategorivariabel i.e. år till JS format
kat8 <- levels(as.factor(data8$a_diadat_ar))
kat8 <- paste0("var kat8 = ['", paste(kat8,collapse = "','"), "'];" )
# Finns den aktuella blanketten eller inte
data8$rappdatum <- !is.na(as.Date(data8[[rappdatum]], format = "%Y-%m-%d"))
if(nrow(data8) != 0){
data8 <- dcast(data8, region.f ~ a_diadat_ar, fun.aggregate = mean, value.var = "rappdatum", drop = FALSE)
# Sätt eventuellt NAs på grund av 0 till 0 samt runda av täckningsgrader
data8[2:ncol(data8)] <- round(data8[2:ncol(data8)]*100,0)
data8[is.na(data8)] <- "null"

# Konvertera till JSON format
data8$data <- do.call(paste, data8[2:ncol(data8)])
data8$data <- gsub(" ", ",", data8$data)
data8$data <- paste0("[",data8$data,"]")
data8 <- data8[c("region.f", "data")]
names(data8) <- c("name", "data")
ser8 <- paste("var","ser8","=",toJSON(data8), ";")
ser8 <- gsub('\\"\\[', '[', ser8  )
ser8 <- gsub('\\]\\"', ']', ser8  )
}
if(nrow(data8) == 0){
  ser8 <- "var ser8 = [] ;"
}
# Skapa titel
titel8 <- paste("Intern täckningsgrad (%) för recidivblanketten")
titel8 <- paste("var titel8 =", "'",titel8,"'", ";")
subtitel8 <- paste("(Urval: ",Diagnos,")", sep ="")
subtitel8 <- paste("var subtitel8 =", "'",subtitel8,"'", ";")


yaxis8 <- paste0("var yaxis8 = ","'" ,"Täckningsgrad (%)", "'" ,";")
xaxis8 <- paste0("var xaxis8 = ","'", "Diagnosår","'" ,";")







############################ ############################ #############################
#                                                                                     #
#                                                                                     #
#                           Indikator 9: Täckningsgrad blankett 5                     #
#                                                                                     #
#                                                                                     #
############################ ############################ #############################


data9 <- subset(data,  a_diadat_ar >= 2011, c("u_inrappdat","a_diadat_ar", "region.f")  )

### Parametrar för återanvänding av skript ####
rappdatum <- "u_inrappdat"


###################### Bearbetning av data för att härleda täckningsgrad ######################
# Skapa kategorivariabel i.e. år till JS format
kat9 <- levels(as.factor(data9$a_diadat_ar))
kat9 <- paste0("var kat9 = ['", paste(kat9,collapse = "','"), "'];" )
# Finns den aktuella blanketten eller inte
data9$rappdatum <- !is.na(as.Date(data9[[rappdatum]], format = "%Y-%m-%d"))
if(nrow(data9) != 0){
data9 <- dcast(data9, region.f ~ a_diadat_ar, fun.aggregate = mean, value.var = "rappdatum", drop = FALSE)
# Sätt eventuellt NAs på grund av 0 till 0 samt runda av täckningsgrader
data9[2:ncol(data9)] <- round(data9[2:ncol(data9)]*100,0)
data9[is.na(data9)] <- "null"

# Konvertera till JSON format
data9$data <- do.call(paste, data9[2:ncol(data9)])
data9$data <- gsub(" ", ",", data9$data)
data9$data <- paste0("[",data9$data,"]")
data9 <- data9[c("region.f", "data")]
names(data9) <- c("name", "data")
ser9 <- paste("var","ser9","=",toJSON(data9), ";")
ser9 <- gsub('\\"\\[', '[', ser9  )
ser9 <- gsub('\\]\\"', ']', ser9  )
}
if(nrow(data9) == 0){
  ser9 <- "var ser9 = [] ;"
}
# Skapa titel
titel9 <- paste("Intern täckningsgrad (%) för uppföljningsblanketten")
titel9 <- paste("var titel9 =", "'",titel9,"'", ";")
subtitel9 <- paste("(Urval: ",Diagnos,")", sep ="")
subtitel9 <- paste("var subtitel9 =", "'",subtitel9,"'", ";")


yaxis9 <- paste0("var yaxis9 = ","'" ,"Täckningsgrad (%)", "'" ,";")
xaxis9 <- paste0("var xaxis9 = ","'", "Diagnosår","'" ,";")










############################ ############################ #############################
#                                                                                     #
#                                                                                     #
#                           Sammanslagning av del1 samt indikatorerna                 #
#                                                                                     #
#                                                                                     #
############################ ############################ #############################

# Ladda in del1
#Lokalt
#
if(is.inca()) {
  del1 <- scan("D:/R-Scripts/Väst/Oc5stafch/Rapportmallar/Cervix/Blanketter/del1_v2.txt", what="", sep="\n", quiet=TRUE,encoding="UTF-8")
} else {
  del1 <- scan("del1_v2(offline).txt", what="", sep="\n", quiet=TRUE)
}


#Rapportinformation
if(is.inca()) {
info_register <- param[["info_register"]]
info_typ<- param[["info_typ"]]
info_info<- paste("Urval: ",Diagnos, sep ="")
} else {
  # Lokalt
  info_register <- "Registernamn"
  info_typ<- "Rapportnamn"
  info_info<- paste("Urval: ",Diagnos, sep ="")
}



info_register <- paste0("document.getElementById('",'register', "').innerHTML='",info_register,"';")
info_typ <- paste0("document.getElementById('",'typ av rapport', "').innerHTML='",info_typ,"';")
info_info <- paste0("document.getElementById('",'information', "').innerHTML='",info_info,"';")

outfile <- file("./output.html","w",encoding="UTF-8")
cat(paste("\n",del1) ,file=outfile,append=TRUE)
cat(paste("\n","<script>") ,file=outfile,append=TRUE)
# Indikator 1
cat(paste("\n",kat1) ,file=outfile,append=TRUE)
cat(paste("\n",ser1) ,file=outfile,append=TRUE)
cat(paste("\n",titel1) ,file=outfile,append=TRUE)
cat(paste("\n",subtitel1) ,file=outfile,append=TRUE)
cat(paste("\n",yaxis1) ,file=outfile,append=TRUE)
cat(paste("\n",xaxis1) ,file=outfile,append=TRUE)
# Indikator 2
cat(paste("\n",kat2) ,file=outfile,append=TRUE)
cat(paste("\n",ser2) ,file=outfile,append=TRUE)
cat(paste("\n",titel2) ,file=outfile,append=TRUE)
cat(paste("\n",subtitel2) ,file=outfile,append=TRUE)
cat(paste("\n",yaxis2) ,file=outfile,append=TRUE)
cat(paste("\n",xaxis2) ,file=outfile,append=TRUE)
# Indikator 3
cat(paste("\n",kat3) ,file=outfile,append=TRUE)
cat(paste("\n",ser3) ,file=outfile,append=TRUE)
cat(paste("\n",titel3) ,file=outfile,append=TRUE)
cat(paste("\n",subtitel3) ,file=outfile,append=TRUE)
cat(paste("\n",yaxis3) ,file=outfile,append=TRUE)
cat(paste("\n",xaxis3) ,file=outfile,append=TRUE)
# Indikator 4
cat(paste("\n",kat4) ,file=outfile,append=TRUE)
cat(paste("\n",ser4) ,file=outfile,append=TRUE)
cat(paste("\n",titel4) ,file=outfile,append=TRUE)
cat(paste("\n",subtitel4) ,file=outfile,append=TRUE)
cat(paste("\n",yaxis4) ,file=outfile,append=TRUE)
cat(paste("\n",xaxis4) ,file=outfile,append=TRUE)
# Indikator 5
cat(paste("\n",kat5) ,file=outfile,append=TRUE)
cat(paste("\n",ser5) ,file=outfile,append=TRUE)
cat(paste("\n",titel5) ,file=outfile,append=TRUE)
cat(paste("\n",subtitel5) ,file=outfile,append=TRUE)
cat(paste("\n",yaxis5) ,file=outfile,append=TRUE)
cat(paste("\n",xaxis5) ,file=outfile,append=TRUE)
# Indikator 6
cat(paste("\n",kat6) ,file=outfile,append=TRUE)
cat(paste("\n",ser6) ,file=outfile,append=TRUE)
cat(paste("\n",titel6) ,file=outfile,append=TRUE)
cat(paste("\n",subtitel6) ,file=outfile,append=TRUE)
cat(paste("\n",yaxis6) ,file=outfile,append=TRUE)
cat(paste("\n",xaxis6) ,file=outfile,append=TRUE)
# Indikator 7
cat(paste("\n",kat7) ,file=outfile,append=TRUE)
cat(paste("\n",ser7) ,file=outfile,append=TRUE)
cat(paste("\n",titel7) ,file=outfile,append=TRUE)
cat(paste("\n",subtitel7) ,file=outfile,append=TRUE)
cat(paste("\n",yaxis7) ,file=outfile,append=TRUE)
cat(paste("\n",xaxis7) ,file=outfile,append=TRUE)
# Indikator 8
cat(paste("\n",kat8) ,file=outfile,append=TRUE)
cat(paste("\n",ser8) ,file=outfile,append=TRUE)
cat(paste("\n",titel8) ,file=outfile,append=TRUE)
cat(paste("\n",subtitel8) ,file=outfile,append=TRUE)
cat(paste("\n",yaxis8) ,file=outfile,append=TRUE)
cat(paste("\n",xaxis8) ,file=outfile,append=TRUE)
# Indikator 9
cat(paste("\n",kat9) ,file=outfile,append=TRUE)
cat(paste("\n",ser9) ,file=outfile,append=TRUE)
cat(paste("\n",titel9) ,file=outfile,append=TRUE)
cat(paste("\n",subtitel9) ,file=outfile,append=TRUE)
cat(paste("\n",yaxis9) ,file=outfile,append=TRUE)
cat(paste("\n",xaxis9) ,file=outfile,append=TRUE)
# Information i rapporten
cat(paste("\n",info_register) ,file=outfile,append=TRUE)
cat(paste("\n",info_typ) ,file=outfile,append=TRUE)
cat(paste("\n",info_info) ,file=outfile,append=TRUE)
# Ladda in funktionerna i skriptet
cat(paste("\n","Stapeldiagram('container1', titel1, subtitel1, kat1, xaxis1, yaxis1, ser1);
          ") ,file=outfile,append=TRUE)
cat(paste("\n","Stapeldiagram('container2', titel2, subtitel2, kat2, xaxis2, yaxis2, ser2);
          ") ,file=outfile,append=TRUE)
cat(paste("\n","Stapeldiagram('container3', titel3, subtitel3, kat3, xaxis3, yaxis3, ser3);
          ") ,file=outfile,append=TRUE)
cat(paste("\n","Stapeldiagram('container4', titel4, subtitel4, kat4, xaxis4, yaxis4, ser4);
          ") ,file=outfile,append=TRUE)
cat(paste("\n","Stapeldiagram('container5', titel5, subtitel5, kat5, xaxis5, yaxis5, ser5);
          ") ,file=outfile,append=TRUE)
cat(paste("\n","Tckdiagram('container6', titel6, subtitel6, kat6, xaxis6, yaxis6, ser6);
          ") ,file=outfile,append=TRUE)
cat(paste("\n","Tckdiagram('container7', titel7, subtitel7, kat7, xaxis7, yaxis7, ser7);
          ") ,file=outfile,append=TRUE)
cat(paste("\n","Tckdiagram('container8', titel8, subtitel8, kat8, xaxis8, yaxis8, ser8);
          ") ,file=outfile,append=TRUE)
cat(paste("\n","Tckdiagram('container9', titel9, subtitel9, kat9, xaxis9, yaxis9, ser9);
          ") ,file=outfile,append=TRUE)

cat(paste("\n","</script> </body> </html>") ,file=outfile,append=TRUE)
close(outfile)
