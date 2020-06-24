#-------------------TRAITEMENT DES DONNEES
#-------------------LES DONNES QUANTITATIVES OU TRANSFORMEE EN QUANTITATIVES
setwd("~/R/Projet Big Data Mining")
#install.packages("vroom", 
#                 dependencies = TRUE, repos = "https://cran.rstudio.com")
#install.packages("stringr", dependencies=TRUE)
#install.packages("sqldf", dependencies=TRUE)
library(sqldf)
library(vroom)
library(stringr)
source('~/R/Projet Big Data Mining/extract_function.R')

#CLASS
df_class <- read.csv("~/R/Projet Big Data Mining/dataset/education-used-done/class.csv",header = FALSE)
#Creer la variable class: le nombre total des classes suivis
class <- df_class$V6

#Liste des etudiants
etudiants <- df_class["V1"]
#Nombre des etudiants
print(length(etudiants))

#DEADLINES
df_deadlines <- read.csv("~/R/Projet Big Data Mining/dataset/education-used-done/deadlines.csv",header = TRUE)
df_deadlines_missed <- cbind(df_deadlines["uid"],df_deadlines["Deadlines"])
#Creer les variables deadlines = nombre des deadlines pendant la periode
deadlines <- sqldf("SELECT *
                 FROM etudiants
                 LEFT JOIN df_deadlines_missed
                 ON etudiants.V1 = df_deadlines_missed.uid")

#GRADES
df_grades <- read.csv("~/R/Projet Big Data Mining/dataset/education-used-done/grades.csv",header = TRUE)
#Creer les variables deadlines = nombre des deadlines pendant la periode
grades <- sqldf("SELECT *
                 FROM etudiants
                 LEFT JOIN df_grades
                 ON etudiants.V1 = df_grades.uid")

#PIAZZA
#Creer les variables deadlines = nombre des deadlines pendant la periode. Ici, pas de valeurs manquantes
piazza <- read.csv("~/R/Projet Big Data Mining/dataset/education-used-done/piazza.csv",header = TRUE)
print(nrow(piazza))

#Creer une fonction pour extraire les dossiers
extract <- function(path,pattern,variable,colnames=TRUE,etudiants){
  setwd(path)
  #Preparer le dossier des fichiers dinning
  list_of_files <- list.files(path = "path", recursive = TRUE,
                              pattern = pattern, 
                              full.names = FALSE)
  #Mettre tout le contenu dans une dataframe avec une colonne de nom de fichier
  df <- vroom(list_of_files, delim = ",",col_names = colnames, id = "FileName")
  #Simplifier le nom des fichiers
  df$FileName <- str_sub(as.character(df$FileName),-7,-5)
  #Tableau de frequence par etudiants
  missed <- table(df$FileName,unlist(df[-1]))
  
  #Transformer le tableau en dataframe pour faire une jointure
  etudiants_df <- row.names(missed)
  df_missed <- cbind(etudiants_df,as.data.frame.matrix(missed))
  rownames(df_missed) <- NULL
  
  #Creer les variables
  df_final <- sqldf("SELECT *
                 FROM etudiants
                 LEFT JOIN df_missed
                 ON etudiants.V1 = df_missed.etudiants_df")
  return(df_final)
  
}

#DINNING
#Preparer le dossier des fichiers dinning
list_of_files <- list.files(path = "~/R/Projet Big Data Mining/dataset/dinning-used", recursive = TRUE,
                            pattern = "\\.txt$", 
                            full.names = TRUE)
#Mettre tout le contenu dans une dataframe avec une colonne de nom de fichier
df_dinning <- vroom(list_of_files, delim = ",",col_names = FALSE, id = "FileName")
#Simplifier le nom des fichiers
df_dinning$FileName <- str_sub(as.character(df_dinning$FileName),-7,-5)

#Tableau de frequence des repas par etudiants
dinning_missed <- table(df_dinning$FileName,df_dinning$X3)
print(nrow(dinning_missed))
#Il manque quelques etudiants pour cette variable. On cherche a ajouter ces etudiants manquants

#Transformer le tableau en dataframe pour faire une jointure
etudiants_dinning <- row.names(dinning_missed)
df_dinning_missed <- cbind(etudiants_dinning,as.data.frame.matrix(dinning_missed))
rownames(df_dinning_missed) <- NULL

#Creer les variables dinning = nombre des repas pendant la periode
dinning <- sqldf("SELECT *
                 FROM etudiants
                 LEFT JOIN df_dinning_missed
                 ON etudiants.V1 = df_dinning_missed.etudiants_dinning")

#ACTIVITY
#Preparer le dossier des fichiers dinning
setwd("~/R/Projet Big Data Mining/dataset/sensing/activity")
list_of_files <- list.files(path = "~/R/Projet Big Data Mining/dataset/sensing/activity", recursive = TRUE,
                            pattern = "\\.csv$", 
                            full.names = FALSE)
#Mettre tout le contenu dans une dataframe avec une colonne de nom de fichier
df_activity <- vroom(list_of_files, delim = ",",col_names = TRUE, id = "FileName")
#Simplifier le nom des fichiers
df_activity$FileName <- str_sub(as.character(df_activity$FileName),-7,-5)

#Tableau de frequence par etudiants
activity_missed <- table(df_activity$FileName,unlist(df_activity["activity inference"]))
print(activity_missed)
#Il manque quelques etudiants pour cette variable. On cherche a ajouter ces etudiants manquants

#Transformer le tableau en dataframe pour faire une jointure
etudiants_activity <- row.names(activity_missed)
df_activity_missed <- cbind(etudiants_activity,as.data.frame.matrix(activity_missed))
rownames(df_activity_missed) <- NULL

#Creer les variables activity
activity <- sqldf("SELECT *
                 FROM etudiants
                 LEFT JOIN df_activity_missed
                 ON etudiants.V1 = df_activity_missed.etudiants_activity")
colnames(activity) <- c("V1","V2","activity0","activity1", "activity2", "activity3")

#AUDIO
audio <- extract(path = "~/R/Projet Big Data Mining/dataset/sensing/audio",
                 pattern ="\\.csv$",variable = "audio inference",etudiants = etudiants)

list_of_files <- list.files(path = "~/R/Projet Big Data Mining/dataset/sensing/audio", recursive = TRUE,
                            pattern = "\\.csv$", 
                            full.names = FALSE)
#Mettre tout le contenu dans une dataframe avec une colonne de nom de fichier
df <- vroom(list_of_files, delim = ",",col_names = TRUE, id = "FileName")
#Simplifier le nom des fichiers
df$FileName <- str_sub(as.character(df$FileName),-7,-5)
#Tableau de frequence par etudiants
audio <- table(df$FileName,unlist(df["audio inference"]))
colnames(audio) <- c("V1","V2","audio0","audio1", "audio2")

#GPS
gps <- as.data.frame.matrix(gps)
colnames(gps) <- c("moving","stationary")

deadlines <- deadlines$Deadlines

#FINAL
final <-cbind(activity[,c(-1,-2)],audio[,c(-1,-2)],class,dark,deadlines,dinning[,c(-1,-2)],gps,grades[,c(-1,-2)],phonelock,piazza[,-1])

#Traitement les valeurs manquantes
#Combien de valeurs manquantes ?
print(colSums(is.na(final)))
install.packages("mice")
library(mice)
print(md.pattern(final))
install.packages("VIM")
library(VIM)
mice_plot <- aggr(final[,1:10], col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(final[,1:10]), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))
mice_plot <- aggr(final[,11:16], col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(final[,11:16]), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))
mice_plot <- aggr(final[,17:26], col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(final[,17:26]), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))
#Ici, nous avons que des variables continues
final_imputed <- mice(final, m=5, method = 'norm', maxit = 50, seed = 500)
summary(final_imputed)

#missForest
install.packages('missForest')
library(missForest)

final_imputed <- missForest(final)
print(final_imputed$ximp)
print(final_imputed$OOBerror)

#install package and load library
install.packages("mi")
library(mi)
mi_final <- mi(final, seed=335)
summary(mi_final)
print(mi_final@data)

#De tous les trois package, il nous semble que missForest marche le mieux

#Exporter les donnees en fichier csv
write.csv(final_imputed$ximp,file = "quantitatives.csv")

#-------------------LES DONNEES SURVEY

#BIGFIVE
df_bigfive <- read.csv("~/R/Projet Big Data Mining/dataset/survey-used/BigFive.csv",header = TRUE)
bigfive_pre <- subset(df_bigfive,df_bigfive$type=="pre")
bigfive_post <- subset(df_bigfive,df_bigfive$type=="post")
#Creer des lignes manquantes par rapport à la liste des etudiants
bigfive_pre <- sqldf("SELECT *
FROM etudiants
LEFT JOIN bigfive_pre
ON etudiants.V1 = bigfive_pre.uid")
bigfive_post <- sqldf("SELECT *
FROM etudiants
LEFT JOIN bigfive_post
ON etudiants.V1 = bigfive_post.uid")

#FlourishingScale
df_flourishing <- read.csv("~/R/Projet Big Data Mining/dataset/survey-used/FlourishingScale.csv",header = TRUE)
#Pre-survey
FlourishingScale_pre <- subset(df_bigfive,df_bigfive$type=="pre")
#Creer les variables
FlourishingScale_pre <- sqldf("SELECT *
FROM etudiants
LEFT JOIN FlourishingScale_pre
ON etudiants.V1 = FlourishingScale_pre.uid")
#Post-survey
FlourishingScale_post <- subset(df_bigfive,df_bigfive$type=="post")
#Creer les variables
FlourishingScale_post <- sqldf("SELECT *
FROM etudiants
LEFT JOIN FlourishingScale_post
ON etudiants.V1 = FlourishingScale_post.uid")

#LonelinessScale
df_LonelinessScale <- read.csv("~/R/Projet Big Data Mining/dataset/survey-used/LonelinessScale.csv",header = TRUE)
#Pre-survey
LonelinessScale_pre <- subset(df_bigfive,df_bigfive$type=="pre")
#Creer les variables
LonelinessScale_pre <- sqldf("SELECT *
FROM etudiants
LEFT JOIN LonelinessScale_pre
ON etudiants.V1 = LonelinessScale_pre.uid")
#Post-survey
LonelinessScale_post <- subset(df_bigfive,df_bigfive$type=="post")
#Creer les variables
LonelinessScale_post <- sqldf("SELECT *
FROM etudiants
LEFT JOIN LonelinessScale_post
ON etudiants.V1 = LonelinessScale_post.uid")

#Panas
df_panas <- read.csv("~/R/Projet Big Data Mining/dataset/survey-used/panas.csv",header = TRUE)
#Pre-survey
panas_pre <- subset(df_panas,df_panas$type=="pre")
panas_pre <- data.frame(sapply(panas_post,factor))
#Creer les variables
panas_pre <- sqldf("SELECT *
FROM etudiants
LEFT JOIN panas_pre
ON etudiants.V1 = panas_pre.uid")
panas_pre <- panas_pre[,-2]
#Post-survey
panas_post <- subset(df_panas,df_panas$type=="post")
panas_post <- data.frame(sapply(panas_post,factor))
#Creer les variables
panas_post <- sqldf("SELECT *
FROM etudiants
LEFT JOIN panas_post
ON etudiants.V1 = panas_post.uid")

#PerceivedStressScale
df_PerceivedStressScale <- read.csv("~/R/Projet Big Data Mining/dataset/survey-used/PerceivedStressScale.csv",header = TRUE)
#Pre-survey
PerceivedStressScale_pre <- subset(df_PerceivedStressScale,df_PerceivedStressScale$type=="pre")
#Creer les variables bigfive
PerceivedStressScale_pre <- sqldf("SELECT *
FROM etudiants
LEFT JOIN PerceivedStressScale_pre
ON etudiants.V1 = PerceivedStressScale_pre.uid")
#Post-survey
PerceivedStressScale_post <- subset(df_PerceivedStressScale,df_PerceivedStressScale$type=="post")
#Creer les variables bigfive
PerceivedStressScale_post <- sqldf("SELECT *
FROM etudiants
LEFT JOIN PerceivedStressScale_post
ON etudiants.V1 = PerceivedStressScale_post.uid")

#PHQ9
df_PHQ9 <- read.csv("~/R/Projet Big Data Mining/dataset/survey-used/PHQ-9.csv",header = TRUE)
#Pre-survey
PHQ9_pre <- subset(df_PHQ9,df_PHQ9$type=="pre")
#Creer les variables bigfive
PHQ9_pre <- sqldf("SELECT *
FROM etudiants
LEFT JOIN PHQ9_pre
ON etudiants.V1 = PHQ9_pre.uid")
#Post-survey
PHQ9_post <- subset(df_PHQ9,df_PHQ9$type=="post")
#Creer les variables bigfive
PHQ9_post <- sqldf("SELECT *
FROM etudiants
LEFT JOIN PHQ9_post
ON etudiants.V1 = PHQ9_post.uid")

#PSQI
df_PSQI <- read.csv("~/R/Projet Big Data Mining/dataset/survey-used/PSQI.csv",header = TRUE)
#Pre-survey
PSQI_pre <- subset(df_PSQI,df_PSQI$type=="pre")
#Post-survey
PSQI_post <- subset(df_PSQI,df_PSQI$type=="post")
#Creer les variables
PSQI_pre <- sqldf("SELECT *
FROM etudiants
LEFT JOIN PSQI_pre
ON etudiants.V1 = PSQI_pre.uid")
PSQI_post <- sqldf("SELECT *
FROM etudiants
LEFT JOIN PSQI_post
ON etudiants.V1 = PSQI_post.uid")

#VR_12
df_VR_12 <- read.csv("~/R/Projet Big Data Mining/dataset/survey-used/VR_12.csv",header = TRUE)
#Pre-survey
VR_12_pre <- subset(df_VR_12,df_VR_12$type=="pre")
#Creer les variables
VR_12_pre <- sqldf("SELECT *
FROM etudiants
LEFT JOIN VR_12_pre
ON etudiants.V1 = VR_12_pre.uid")
#Post-survey
VR_12_post <- subset(df_VR_12,df_VR_12$type=="post")
#Creer les variables
VR_12_post <- sqldf("SELECT *
FROM etudiants
LEFT JOIN VR_12_post
ON etudiants.V1 = VR_12_post.uid")

#SURVEY-PRE
#Traiter les NA
#library(missForest)
#survey_imputed <- missForest(survey)
#library(mice)
#survey_mice  <- mice(survey, m=5, method = 'polyreg', maxit = 50, seed = 500)
#Aucun package marche
survey <- cbind(bigfive_pre,FlourishingScale_pre,LonelinessScale_pre,panas_pre,PerceivedStressScale_pre,PHQ9_pre,PSQI_pre,VR_12_pre)

#SURVEY-POST
survey_test <- cbind(bigfive_post,FlourishingScale_post,LonelinessScale_post,panas_post,PerceivedStressScale_post,PHQ9_post,PSQI_post,VR_12_post)
#Supprimer les colonnes etudiants en double
df_survey_1 <- survey[, grep("^(type)", names(survey), value = TRUE, invert = TRUE)]
df_survey_2 <- df_survey_1[, grep("^(V1)", names(df_survey_1), value = TRUE, invert = TRUE)]
df_survey_3 <- df_survey_2[, grep("^(uid)", names(df_survey_2), value = TRUE, invert = TRUE)]
df_survey_3 <- data.frame(sapply(df_survey_3,factor))
df_survey_test1 <- survey_test[, grep("^(type)", names(survey_test), value = TRUE, invert = TRUE)]
df_survey_test2 <- df_survey_test1[, grep("^(V1)", names(df_survey_test1), value = TRUE, invert = TRUE)]
df_survey_test3 <- df_survey_test2[, grep("^(uid)", names(df_survey_test2), value = TRUE, invert = TRUE)]
df_survey_test3 <- data.frame(sapply(df_survey_test3,factor))
row.names(df_survey_3) <- unlist(etudiants)
row.names(df_survey_test3) <- unlist(etudiants)

#Supprimer les lignes completement vide
df_survey_3 <- subset(df_survey_3, !is.na(df_survey_3$I.see.myself.as.someone.who........1..Is.talkative))
df_survey_test3 <- subset(df_survey_test3, !is.na(df_survey_test3$I.see.myself.as.someone.who........1..Is.talkative))

#Essayons d'imputer avec les modes
#Creer une fonction mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#Creer une fonction qui rempli les NA
fillmode <- function(x){
  return(ifelse(x=="" | is.na(x),getmode(x),x))
}
survey_imputed <- data.frame(apply(df_survey_3,2,fillmode))
survey_imputed <- data.frame(sapply(survey_imputed,factor))
survey_test_imputed <- data.frame(apply(df_survey_test3,2,fillmode))
survey_test_imputed <- data.frame(sapply(survey_test_imputed,factor))
test <- subset(survey_test_imputed,select = -c(During.the.past.month..how.would.you.rate.your.sleep.quality.overall.
))
test <- data.frame(sapply(test,factor))

#-------------------EMA
# Comme EMA est un dossier contenant lui-meme plusieurs dossiers, on va implementer
# des fonctions et des boucles pour faire les traitements "automatiquement".
#-----Etape 1-----#
# ConcatÃ©ner pour chaque dossier, c est a dire pour chaque question, 
# les rÃ©ponses de tous les Ã©tudiants dans un dataframe 7
# (=transformer json en df + concat)
#-----Etape 2-----#
#Gestion des valeurs manquantes
#--------------------------------------------------------------

#librairies utiles
#lecture de fichiers json
library(rjson)
library(jsonlite)
#concat fichier 
library(dplyr)
library(plyr)
library(rowr)
#traitement des NA
library(missForest)
library(mi)
library(VIM)
library(mice)
#definition du chemin
setwd("C:/Users/ane-y/Downloads/dataset/dataset/EMA/response")
#liste des dossiers 
dossiers=list.dirs('.', full.names=T,recursive=F)
n_dossiers=length(dossiers)
json_non_utilise=NULL
noms=NULL
#fonction qui pour un dossier donne concatene les fichiers a l interieur
creation_base<-function(doss){
  temp_dir=list.files(doss)
  base=NULL
  nb_lignes=c(0)
  for (i in 1:length(temp_dir)) {
    chemin=paste(doss,temp_dir, sep="/")
    temp=fromJSON(txt = chemin[i],simplifyMatrix = TRUE)
    name_indiv=substr(temp_dir[i],nchar(temp_dir[i])-7,nchar(temp_dir[i])-5)
    temp2=cbind(name_indiv,temp)
    if (is.data.frame(temp)==T){
      print(paste("Tour de boucle :", i))
      print(paste("Nom du fichier :", temp_dir[i]))
      print("")
      base = rbind.fill(base, 
                        temp2
      )
      nb_lignes=append(nb_lignes,nrow(base))
      print(paste("Le fichier", temp_dir[i], "est passe !"))
      print(paste("Nombre de lignes :",nrow((base))))
      print("---------------")
    }
    else {
      print(paste("Tour de boucle :", i))
      print(paste("Nom du fichier :", temp_dir[i]))
      print("")
      json_non_utilise=append(json_non_utilise,temp_dir[i])
      print(paste("Le fichier", temp_dir[i], "n'est pas un dataframe !!"))
      print("---------------")
    }
  }
  return(base)
}
#fonction pour transformer colonne chr en factor
chr_to_factor<-function(base){
  character_vars <- lapply(base, class) == "character"
  base[, character_vars] <- lapply(base[, character_vars], as.factor)
  return(base)
}

#concatenation dossiers par dossiers de tous les fichiers
for (j in 1:n_dossiers){
  #creation de toutes les df pour chaque dossier
  nom_temp=paste("df",substr(dossiers[j],3,nchar(dossiers[j])),sep="_")
  noms=append(noms,noquote(nom_temp))
  assign(nom_temp,creation_base(dossiers[j]))
}

#transformer chaque char dans les df en factor
{
  df_Activity=chr_to_factor(df_Activity)
  `df_Administration's response`=chr_to_factor(`df_Administration's response`)
  df_Behavior=chr_to_factor(df_Behavior)
  `df_Boston Bombing`=chr_to_factor(`df_Boston Bombing`)
  `df_Cancelled Classes`=chr_to_factor(`df_Cancelled Classes`)
  df_Class=chr_to_factor(df_Class)
  `df_Class 2`=chr_to_factor(`df_Class 2`)
  df_Comment=chr_to_factor(df_Comment)
  `df_Dartmouth now`=chr_to_factor(`df_Dartmouth now`)
  df_Dimensions=chr_to_factor(df_Dimensions)
  `df_Dimensions protestors`=chr_to_factor(`df_Dimensions protestors`)
  `df_Dining Halls`=chr_to_factor(`df_Dining Halls`)
  `df_Do Campbell's jokes suck_`=chr_to_factor(`df_Do Campbell's jokes suck_`)
  df_Events=chr_to_factor(df_Events)
  df_Exercise=chr_to_factor(df_Exercise)
  `df_Green Key 1`=chr_to_factor(`df_Green Key 1`)
  `df_Green Key 2`=chr_to_factor(`df_Green Key 2`)
  df_Lab=chr_to_factor(df_Lab)
  df_Mood=chr_to_factor(df_Mood)
  `df_Mood 1`=chr_to_factor(`df_Mood 1`)
  `df_Mood 2`=chr_to_factor(`df_Mood 2`)
  df_PAM=chr_to_factor(df_PAM)
  df_QR_Code=chr_to_factor(df_QR_Code)
  df_Sleep=chr_to_factor(df_Sleep)
  df_Social=chr_to_factor(df_Social)
  df_Stress=chr_to_factor(df_Stress)
  `df_Study Spaces`=chr_to_factor(`df_Study Spaces`)
}


#traiter les NA : 
#mi : TROP LONG
# 
#   mi_Activity=mi(select(df_Activity,-location,-Social2,-null), seed=335)
#   mi_Administration=mi(`df_Administration's response`[,3:length(`df_Administration's response`)], seed=335)
#   mi_Behavior=mi(df_Behavior[,3:length(df_Behavior)], seed=335)
#   mi_BostonBombing=mi(`df_Boston Bombing`[,3:length(`df_Boston Bombing`)], seed=335)
#   mi_CancelledClasses=mi(`df_Cancelled Classes`[,3:length(`df_Cancelled Classes`)], seed=335)
#   mi_Class=mi(df_Class[,3:length(df_Class)], seed=335)
#   mi_Class2=mi(`df_Class 2`[,3:length(`df_Class 2`)], seed=335)
#   mi_Comment=mi(df_Comment[,3:length(df_Comment)], seed=335)
#   mi_Dartmouth_now=mi(`df_Dartmouth now`[,3:length(`df_Dartmouth now`)], seed=335)
#   mi_Dimensions=mi(df_Dimensions[,3:length(df_Dimensions)], seed=335)
#   mi_Dimensions_protestors=mi(`df_Dimensions protestors`[,3:length(`df_Dimensions protestors`)], seed=335)
#   mi_DiningHalls=mi(`df_Dining Halls`[,3:length(`df_Dining Halls`)], seed=335)
#   mi_Campbell_jokes=mi(`df_Do Campbell's jokes suck_`[,3:length(`df_Do Campbell's jokes suck_`)], seed=335)
#   mi_Events=mi(df_Events[,3:length(df_Events)], seed=335)
#   mi_Exercise=mi(df_Exercise[,3:length(df_Exercise)], seed=335)
#   mi_GreenKey1=mi(`df_Green Key 1`[,3:length(`df_Green Key 1`)], seed=335)
#   mi_GreenKey2=mi(`df_Green Key 2`[,3:length(`df_Green Key 2`)], seed=335)
#   mi_Lab=mi(df_Lab[,3:length(df_Lab)], seed=335)
#   mi_Mood=mi(df_Mood[,3:length(df_Mood)], seed=335)
#   mi_Mood1=mi(`df_Mood 1`[,3:length(`df_Mood 1`)], seed=335)
#   mi_Mood2=mi(`df_Mood 2`[,3:length(`df_Mood 2`)], seed=335)
#   mi_PAM=mi(df_PAM[,3:length(df_PAM)], seed=335)
#   mi_QR_Code=mi(df_QR_Code[,3:length(df_QR_Code)], seed=335)
#   mi_Sleep=mi(df_Sleep[,3:length(df_Sleep)], seed=335)
#   mi_Social=mi(df_Social[,3:length(df_Social)], seed=335)
#   mi_Stress=mi(df_Stress[,3:length(df_Stress)], seed=335)
#   mi_StudySpaces=mi(`df_Study Spaces`[,3:length(`df_Study Spaces`)], seed=335)
# 

#missForest
{
  mF_Activity=missForest(select(df_Activity,-location,-Social2,-null))
  mF_Administration=missForest(select(`df_Administration's response`,-location))
  mF_Behavior=missForest(select(df_Behavior,-location,-null))
  mF_BostonBombing=missForest(select(`df_Boston Bombing`,-location))
  mF_CancelledClasses=missForest(select(`df_Cancelled Classes`,-location))
  #mF_Class=missForest(select(df_Class) trop de levels
  mF_Class2=missForest(select(`df_Class 2`,-location))
  #mF_Comment=missForest(select(df_Comment) trop de levels
  mF_Dartmouth_now=missForest(`df_Dartmouth now`)
  mF_Dimensions=missForest(df_Dimensions)
  mF_Dimensions_protestors=missForest(`df_Dimensions protestors`)
  mF_DiningHalls=missForest(select(`df_Dining Halls`,-location))
  #mF_Campbell_jokes=missForest(`df_Do Campbell's jokes suck_`)
  #mF_Events=missForest(df_Events) trop de levels
  mF_Exercise=missForest(select(df_Exercise,-location))
  mF_GreenKey1=missForest(`df_Green Key 1`)
  mF_GreenKey2=missForest(`df_Green Key 2`)
  mF_Lab=missForest(select(df_Lab,-location))
  mF_Mood=missForest(select(df_Mood,-location))
  mF_Mood1=missForest(select(`df_Mood 1`,-location))
  mF_Mood2=missForest(select(`df_Mood 2`,-location))
  mF_PAM=missForest(df_PAM)
  mF_QR_Code=missForest(df_QR_Code)
  mF_Sleep=missForest(select(df_Sleep,-location,-null))
  mF_Social=missForest(select(df_Social,-location,-null))
  mF_Stress=missForest(select(df_Stress,-location,-null))
  mF_StudySpaces=missForest(select(`df_Study Spaces`,-location,-place))
}

#variables "propres" sans NA
{
  mF_Activity_propre=mF_Activity$ximp
  mF_Administration_propre=mF_Administration$ximp
  mF_Behavior_propre=mF_Behavior$ximp
  mF_BostonBombing_propre=mF_BostonBombing$ximp
  mF_CancelledClasses_propre=mF_CancelledClasses$ximp
  mF_Class2_propre=mF_Class2$ximp
  mF_Dartmouth_now_propre=mF_Dartmouth_now$ximp
  mF_Dimensions_propre=mF_Dimensions$ximp
  mF_Dimensions_protestors_propre=mF_Dimensions_protestors$ximp
  mF_DiningHalls_propre=mF_DiningHalls$ximp
  mF_Exercise_propre=mF_Exercise$ximp
  mF_GreenKey1_propre=mF_GreenKey1$ximp
  mF_GreenKey2_propre=mF_GreenKey2$ximp
  mF_Lab_propre=mF_Lab$ximp
  mF_Mood_propre=mF_Mood$ximp
  mF_Mood1_propre=mF_Mood1$ximp
  mF_Mood2_propre=mF_Mood2$ximp
  mF_PAM_propre=mF_PAM$ximp
  mF_QR_Code_propre=mF_QR_Code$ximp
  mF_Sleep_propre=mF_Sleep$ximp
  mF_Social_propre=mF_Social$ximp
  mF_Stress_propre=mF_Stress$ximp
  mF_StudySpaces_propre=mF_StudySpaces$ximp
}

#concat de la base propre
EMA=cbind.fill(mF_Activity_propre,
               mF_Administration_propre,
               mF_Behavior_propre,
               mF_BostonBombing_propre,
               mF_CancelledClasses_propre,
               mF_Class2_propre,
               mF_Dartmouth_now_propre,
               mF_Dimensions_propre,
               mF_Dimensions_protestors_propre,
               mF_DiningHalls_propre,
               mF_Exercise_propre,
               mF_GreenKey1_propre,
               mF_GreenKey2_propre,
               mF_Lab_propre,
               mF_Mood_propre,
               mF_Mood1_propre,
               mF_Mood2_propre,
               mF_PAM_propre,
               mF_QR_Code_propre,
               mF_Sleep_propre,
               mF_Social_propre,
               mF_Stress_propre,
               mF_StudySpaces_propre)

write.csv2(EMA,"ema.csv")

#Supprimer les donnees jugee non-relevantes
#CancelledClasses: supprime
mF_Dartmouth_now_propre <- subset(mF_Dartmouth_now_propre,select=-c(location))
#Dimensions: supprime
mF_Dimensions_protestors_propre <- subset(mF_Dimensions_protestors_propre,select=-c(location))
#GreenKey1,2: supprime
mF_Mood2_propre <- subset(mF_Mood2_propre,select=-c(null))
#QR_Code: supprime

#Construire une fonction pour traiter chaque dossier EMA: 
#1/Regrouper les reponses de chaque etudiant par leur mode
#2/Faire une jointure avec la liste des etudiants pour faire apparaitre les etudiants manquants
ema <- function(X){
  #Fonction calculer mode
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  #Regrouper les reponses par leur mode
  ema_aggregated <- aggregate(subset(X,select=-c(resp_time)),by=list(X[,"name_indiv"]),FUN=getmode)[,-1]
  
  #Jointure avec la liste des etudiants
  res <- etudiants %>% left_join(ema_aggregated,by=c("etudiants" = "name_indiv"))
  
  return(res)
}


#La liste de tous les objets dans l'environnement global, sauf le dataframe "etudiants"
my_envr <- mget(ls())[-2]
lst_ema <- list()
for (i in my_envr){
  if (is.data.frame(i)){
    lst_ema <- append(lst_ema,ema(i))
  }
}

#Preparer le dataframe final
df_ema <- data.frame(lst_ema)
#Supprimer les colonnes etudiants en double
df_ema_final <- df_ema[, grep("^(etudiants)", names(df_ema), value = TRUE, invert = TRUE)]
row.names(df_ema_final) <- unlist(etudiants)
#Il existe plein de "null" à convertir en NA
df_ema_final <- apply(df_ema_final,2,function(x){ifelse(x=="null",NA,x)})

#Traiter les NA
library(missForest)

ema_imputed <- missForest(data.frame(df_ema_final))
write.csv(ema_imputed$ximp,"ema_imputed.csv")

#-----------------Fusionner EMA + Survey + Variables quantitatives 
data_final <- cbind(quantitatives[,-1],survey_imputed[,-1],ema_imputed[,-1])
row.names(data_final) <- quantitatives[,1]
write.csv(data_final,"data_final.csv")
write.csv(survey_imputed,"survey_imputed.csv")

#-----------------CLUSTERING SANS ACP avec donnees quantitatives
#centrage réduction des données pour éviter que variables à forte variance pèsent indûment sur les résultats: data.cr <- scale(data)
final_scale <- scale(final_imputed$ximp)

#K-Means
model <- kmeans(final_scale,centers=4,nstart=10)
plot(final_scale,col=model$cluster)
#Scree plot: 
inertie.explique <- 0 
# For 1 to 15 cluster centers
for (i in 1:15) {
  model <- kmeans(final_imputed$ximp, centers = i, nstart=20)
  inertie.explique[i] <- model$betweenss/model$totss} #(alternative: tot.withinss)
plot(1:15,inertie.explique,type="b",xlab="Nb. de groupes",ylab="% inertie expliquée")

#CAH
#matrice des distances euclidiennes entre individus
d.data <- dist(final_scale)
cah.ward <- hclust(d.data,method="ward.D2") #CAH - critère de Ward
plot(cah.ward) #affichage dendrogramme
clusters <- cutree(cah.ward,k=5) #Cut by height or by nb of clusters
plot(final_scale,col=clusters)

#-----------------------------CLUSTERING AVEC ACP
#Clustering avec des variables quantitatives
#ACP
acp.final <- prcomp(quantitatives,scale = TRUE, center = TRUE)
print(acp.final$x)
biplot(acp.final)
#Screeplot
pr.var <-acp.final$sdev^2
pve <- pr.var / sum(pr.var)
plot(cumsum(pve),xlab='Principal Component', ylab='Proportion of Variance Explained', ylim=c(0,1),type='b')
#Prenons 8 composants principaux
data_acp <- acp.final$x[,1:8]

#CAH
cah.ward <- hclust(dist(data_acp),method="ward.D2") #CAH - critère de Ward
plot(cah.ward) #affichage dendrogramme
clusters <- cutree(cah.ward,k=5) #Cut by height or by nb of clusters
plot(data_acp,col=clusters,type="n")
text(data_acp,labels=row.names(data_acp),col=clusters)

#K-MEANS
model <- kmeans(data_acp,centers=4,nstart=10)
plot(data_acp,col=model$cluster,type="n")
text(data_acp,labels=row.names(data_acp),col=model$cluster)

#Clustering avec 235 variables

#AFDM ade4
library(ade4)
adfm_data <- dudi.mix(data_final,scannf = FALSE, nf=10)$li
row.names(adfm_data) <- row.names(data_final)
#K-MEANS
model_235_afdm <- kmeans(adfm_data,centers=4,nstart=10)
plot(adfm_data[,1:2],col=model_235_afdm$cluster,type="n")
text(adfm_data,labels=row.names(adfm_data),col=model_235_afdm$cluster)
#CAH
cah.ward <- hclust(dist(adfm_data),method="ward.D2") #CAH - critère de Ward
plot(cah.ward) #affichage dendrogramme
clusters <- cutree(cah.ward,k=5) #Cut by height or by nb of clusters
plot(adfm_data,col=clusters,type="n")
text(adfm_data,labels=row.names(adfm_data),col=clusters)

#AFDM FactoMineR
library(FactoMineR)
data_235_FactoMineR<- FAMD(data_final,ncp=10)$ind$coord
#K-MEANS
model_235_FactoMineR <- kmeans(data_235_FactoMineR,centers=4,nstart=10)
plot(data_235_FactoMineR,col=model_235_FactoMineR$cluster,type="n")
text(data_235_FactoMineR,labels=row.names(data_235_FactoMineR),col=model_235_FactoMineR$cluster)
#CAH
cah.ward <- hclust(dist(data_235_FactoMineR),method="ward.D2") #CAH - critère de Ward
plot(cah.ward) #affichage dendrogramme
clusters <- cutree(cah.ward,k=5) #Cut by height or by nb of clusters
plot(data_235_FactoMineR,col=clusters,type="n")
text(data_235_FactoMineR,labels=row.names(data_235_FactoMineR),col=clusters)

#-----------------------------PREDICTION
#Etre sur que toutes les variables en factor
train <- data.frame(sapply(train,factor))
test <- data.frame(sapply(test,factor))

#Mise à niveau (levels) des variables trains
totalData <- rbind(train, test)
for (i in 1:length(names(totalData))) {
  levels(train[, i]) <- levels(totalData[, i])
}
#Tester les levels
levels(train[,11])
levels(totalData[,11])
levels(test[,11])

train <- totalData[1:46,]

#Creer les donnes test sans variables a predire
test_dependants <- subset(test,select=-c(During.the.past.month..how.would.you.rate.your.sleep.quality.overall.))

#Naives Bayes
library(naivebayes)
model <- naive_bayes(During.the.past.month..how.would.you.rate.your.sleep.quality.overall.
                     ~ ., data = train)
prediction <- predict(model, test_dependants)

#Regression logistique
model <- glm(During.the.past.month..how.would.you.rate.your.sleep.quality.overall.
             ~ ., data = totalData,family=binomial)
prediction <- predict(model, test_dependants)
print(prediction)

#Arbre de decision
library(rpart)
model <- rpart(During.the.past.month..how.would.you.rate.your.sleep.quality.overall.
               ~ ., data = train)
prediction <- predict(model, test_dependants,type = 'class')

#Evaluer le resultat
table(test$During.the.past.month..how.would.you.rate.your.sleep.quality.overall.,prediction)
library(mltest)
View(ml_test(prediction,test$During.the.past.month..how.would.you.rate.your.sleep.quality.overall.,output.as.table = TRUE))


