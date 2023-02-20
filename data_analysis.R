library(rsnps)
library(caret)
library(dplyr)
library(snpReady)


#NICOTINE DEPENDENCE
nicotine$user_id<-as.factor(nicotine$user_id)
nicotine$variation<-as.factor(nicotine$variation)

levels(nicotine$variation)[levels(nicotine$variation)=="6 cigarettes/day"] <- "Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Binge Smoker - 2 or 3 times a week, 10-20 at a time."] <- "Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="chain smoker"] <- "Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Chain smoker"] <- "Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Smoker. 10 cigarettes/day"] <- "Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Current smoker. ~10/day"] <- "Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Smoker. 4 cigarettes/day. Nicotine lozenges daily.  Trying to quit for 4 years."] <- "Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Smoker. 15 cigarettes/day. Trying to quit."] <- "Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Vape nicotine"] <- "Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Smoker. 15 cigarettes/day. trying to quit."] <- "Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Smoker. 10 cigarettes/Day"] <- "Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Smoker, 20 a day."] <- "Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Smoker, 2 a day."] <- "Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Made of nicotine"] <- "Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="excessive 3pks/day"] <- "Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="dipper, <1 can/day"] <- "Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Dipper, <1 can/day"] <- "Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Snuff"] <- "Smoker"

levels(nicotine$variation)[levels(nicotine$variation)=="Don't smoke"] <- "Non Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Never - smoker "] <- "Non Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Non-smoker"] <- "Non Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Never"] <- "Non Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Never-non-smoker "] <- "Non Smoker"

levels(nicotine$variation)[levels(nicotine$variation)=="ex-smoker for 11 years no patches or gum"] <- "Ex Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Ex smoker. 40 cigarettes a day"] <- "Ex Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="ex smoker. 40 cigarettes a day"] <- "Ex Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Ex smoker  3 cigarettes/Day"] <- "Ex Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Ex smoker now use electronic cigarettes"] <- "Ex Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Ex smoker  3 cigarettes/day"] <- "Ex Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Ex smoker - now vape"] <- "Ex Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="ex smoker - now vape"] <- "Ex Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Ex-Smoker. 25 cigarettes/day"] <- "Ex Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Ex-Smoker. 15 cigarettes/day"] <- "Ex Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Ex-Smoker. 15-16 cigarettes/day"] <- "Ex Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Ex-smoker, Still Use Nicotine Gum"] <- "Ex Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Ex-smoker, 7 cigarettes/Day"] <- "Ex Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="ex-smoker, 7 cigarettes/day"] <- "Ex Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Ex-smoker, 10 cigarettes/day"] <- "Ex Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Ex-smoker, 2 a day"] <- "Ex Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Ex-smoker, 7 cigarettes/day"] <- "Ex Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Ex-smoker, still use nicotine gum"] <- "Ex Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Ex-smoker. 15-16 cigarettes/day"] <- "Ex Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Ex-smoker. 15-16 cigarettes/Day"] <- "Ex Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Ex-smoker. 15 cigarettes/day"] <- "Ex Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Ex-smoker. 15 cigarettes/Day"] <- "Ex Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Ex-smoker. 60 cigarettes a day"] <- "Ex Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Ex-smoker. 25 cigarettes/day"] <- "Ex Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Ex smoker now use electronic cigarettes "] <- "Ex Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="ex smoker  3 cigarettes/day"] <- "Ex Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Ex social smoker"] <- "Ex Smoker"




############################## DUBBI ##################################
levels(nicotine$variation)[levels(nicotine$variation)=="Occasional cigar"] <- "Non Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Occasional Cigar"] <- "Non Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="have tried cigarettes - current non smoker  no addiction to nicotine"] <- "Non Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Have tried cigarettes - current non smoker  no addiction to nicotine"] <- "Non Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Social smoker, no addiction"] <- "Non Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="Social Smoker, No addiction"] <- "Non Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="na"] <- "Non Smoker"


levels(nicotine$variation)[levels(nicotine$variation)=="Gg"] <- "Smoker"
levels(nicotine$variation)[levels(nicotine$variation)=="GG"] <- "Smoker"

#SEX
gender<-phenotypes_byid(phenotypeid=60, return_ = 'users')
gender$user_id<-as.factor(gender$user_id)
gender$variation<-as.factor(gender$variation)
colnames(gender)<-c("user_id", "sex")
levels(gender$sex)[levels(gender$sex)=="female"] <- "Female"
levels(gender$sex)[levels(gender$sex)=="Female but feel non-gendered"] <- "Female"
levels(gender$sex)[levels(gender$sex)=="female but feel non-gendered"] <- "Female"
levels(gender$sex)[levels(gender$sex)=="Non-binary xx / genderqueer female-bodied"] <- "Female"
levels(gender$sex)[levels(gender$sex)=="Non-binary XX / genderqueer female-bodied"] <- "Female"
levels(gender$sex)[levels(gender$sex)=="woman"] <- "Female"
levels(gender$sex)[levels(gender$sex)=="Woman"] <- "Female"
levels(gender$sex)[levels(gender$sex)=="Female.  uploaded only mito."] <- "Female"
levels(gender$sex)[levels(gender$sex)=="Female.  Uploaded only mito."] <- "Female"
levels(gender$sex)[levels(gender$sex)=="Transgender ftm"] <- "Female"
levels(gender$sex)[levels(gender$sex)=="Transgender FtM"] <- "Female"

levels(gender$sex)[levels(gender$sex)=="male"] <- "Male"
levels(gender$sex)[levels(gender$sex)=="Male but genderfluid"] <- "Male"
levels(gender$sex)[levels(gender$sex)=="Man"] <- "Male"

levels(gender$sex)[levels(gender$sex)=="Love it"] <- NA

gender$sex<-droplevels(gender$sex) 


########################################################################
table(nicotine$variation)

#creo dataframe con user snp e fenotipo (smoker ex-smoker non-smoker)
#df = merge(x=matrice,y=nicotine,by="user_id",all.x=TRUE)
#df[1:15,8959830:8959837]
#table(df$variation)

#creo dataframe con user snp e fenotipo (smoker non-smoker)
df_dicot<-df
levels(df_dicot$variation)[levels(df_dicot$variation)=="Ex Smoker"] <- "Smoker"
table(df_dicot$variation) #120 Smoker 191 Non smoker

#unisco il sesso al dataframe con risposta dicotomica
df_dicot<-merge(x=df_dicot,y=gender,by="user_id",all.x=TRUE)
df_dicot$sex<-droplevels(df_dicot$sex) 

#rimuovo le osservazioni 159 e 260 perchè ho visto dalle analisi dei dataset spezzati che sono problematiche
df_dicot<-df_dicot[-c(159,260),]

############################################################################################################################################################
#########################################################df_dicot_1 : Colonne fino a 100000#################################################################
#prime 100mila colonne
df_dicot_1<-df_dicot[,1:100000]
#unisco il fenotipo
df_dicot_1<-merge(x=df_dicot[,c(1,8959837)],y=df_dicot_1,by="user_id",all.x=TRUE)
#unisco il sesso
df_dicot_1<-merge(x=df_dicot[,c(1,8959838)],y=df_dicot_1,by="user_id",all.x=TRUE)
#################################### PULIZIA DATASET #########################################

#Gli SNP con NA significa che quello snp non è stato sequenziato per quel genoma, se invece c'è il "--" lo snp è stato sequenziato ma
#non è presente il valore. 

#Ricodifico "--" come NA

for (i in 1:100000) {
  levels(df_dicot_1[,i])[levels(df_dicot_1[,i])=="--"] <-NA
  print(i)
}

levels(df_dicot_1[,99999])
df_dicot_1[c(159,260),c(1,2,4)]
which(df_dicot_1[,4]=='CT')
droplevels(df_dicot_1[,4])
#rimuovo la riga 159 e 260 (user 45 e 77) perchè codificate male (CT e TC sullo snp rs3094315 + altri problemi analoghi) 
df_dicot_1<-df_dicot_1[-c(159,260),]
#Rimuovo le colonne con +80% di NA

#dat<-df_dicot_1[1:15,c(2:10,99998:100000)]
#dat<-as.matrix(dat)
#dat[7,1]<-NA
#colMeans((is.na(dat)))
#dat.ready <- raw.data(data = as.matrix(dat), frame = "wide", base = TRUE,
#                      sweep.sample = 0.99, call.rate = 0.95, 
#                      maf = 0.05, imput = FALSE)


#dat[,which(colMeans(!is.na(dat)) > 0.2)]
#rimuovo le colonne che hanno più dell'80% di NA
df_dicot_1<-df_dicot_1[,which(colMeans(!is.na(df_dicot_1)) > 0.2)]
colMeans(is.na(df_dicot_1))
#rimosse 530 colonne


#Rimuovo le variabili con varianza pari a zero o quasi

remove<-nearZeroVar(df_dicot_1)
df_dicot_1<-df_dicot_1[,-remove]
#rimosse 13951 colonne
#dim(df_dicot_1)=(309,85521)

#########################    ################################    ################################   ###################################
#rieseguo pulizia
for (i in 1:85521) {
  levels(df_dicot_1[,i])[levels(df_dicot_1[,i])=="--"] <-NA
  df_dicot_1[,i]<-droplevels(df_dicot_1[,i])
  print(i)
}

df_dicot_1<-df_dicot_1[,which(colMeans(!is.na(df_dicot_1)) > 0.2)]
dim(df_dicot_1) #309 85502

######################     ###################################    ################################

#Ricodifico genotipo in numeri in base al reference genome
library(snpReady)



#########################################################################################################################################################
#############################################################df_dicot_2 : da 100001 a 300000  ###########################################################

df_dicot_2<-df_dicot[,c(1,8959837,100001:300000)]
df_dicot_2<-merge(x=df_dicot[,c(1,8959838)],y=df_dicot_2,by="user_id",all.x=TRUE)
#Ricodifico "--" come NA
for (i in 2:200002) {
  levels(df_dicot_2[,i])[levels(df_dicot_2[,i])=="--"] <-NA
  print(i)
}

#rimuovo la riga 159 e 260 (user 45 e 77) perchè codificate male (CT e TC sullo snp rs3094315 + altri problemi analoghi) 
df_dicot_2<-df_dicot_2[-c(159,260),]

#mantengo le colonne che hanno più del 20% di dati (rimuovo quelle con +80% di NA)
df_dicot_2<-df_dicot_2[,which(colMeans(!is.na(df_dicot_2)) > 0.2)]
#rimosse 1056 colonne

#Rimuovo le variabili con varianza pari a zero o quasi
remove<-nearZeroVar(df_dicot_2)
df_dicot_2<-df_dicot_2[,-remove]
#rimosse 23322 colonne
##dim(df_dicot_2)=(309,175625)


#########################    ################################    ################################   ###################################
#rieseguo pulizia
for (i in 1:175625) {
  levels(df_dicot_2[,i])[levels(df_dicot_2[,i])=="--"] <-NA
  df_dicot_2[,i]<-droplevels(df_dicot_2[,i])
  print(i)
}

df_dicot_2<-df_dicot_2[,which(colMeans(!is.na(df_dicot_2)) > 0.2)]
dim(df_dicot_2) #(309,175625)

######################     ###################################    ################################



#########################################################################################################################################################
#############################################################df_dicot_3 : da 300001 a 600000  ###########################################################

df_dicot_3<-df_dicot[,c(1,8959837,300001:600000)]
#unisco il sesso
df_dicot_3<-merge(x=df_dicot[,c(1,8959838)],y=df_dicot_3,by="user_id",all.x=TRUE)

#Ricodifico "--" come NA
for (i in 2:300002) {
  levels(df_dicot_3[,i])[levels(df_dicot_3[,i])=="--"] <-NA
  print(i)
}

#rimuovo la riga 159 e 260 (user 45 e 77) perchè codificate male (CT e TC sullo snp rs3094315 + altri problemi analoghi) 
df_dicot_3<-df_dicot_3[-c(159,260),]

#mantengo le colonne che hanno più del 20% di dati (rimuovo quelle con +80% di NA)
df_dicot_3<-df_dicot_3[,which(colMeans(!is.na(df_dicot_3)) > 0.2)]
#rimosse 1441 colonne

#Rimuovo le variabili con varianza pari a zero o quasi
remove<-nearZeroVar(df_dicot_3)
df_dicot_3<-df_dicot_3[,-remove]
#rimosse 34058 colonne
##dim(df_dicot_3)=(309,264504)

#########################    ################################    ################################   ###################################
#rieseguo pulizia
for (i in 1:264504) {
  levels(df_dicot_3[,i])[levels(df_dicot_3[,i])=="--"] <-NA
  df_dicot_3[,i]<-droplevels(df_dicot_3[,i])
  print(i)
}

df_dicot_3<-df_dicot_3[,which(colMeans(!is.na(df_dicot_3)) > 0.2)]
dim(df_dicot_3) #(309,264504)

######################     ###################################    ################################



#########################################################################################################################################################
#############################################################df_dicot_4 : da 600001 a 800000  ###########################################################

df_dicot_4<-df_dicot[,c(1,8959837,600001:800000)]
#unisco il sesso
df_dicot_4<-merge(x=df_dicot[,c(1,8959838)],y=df_dicot_4,by="user_id",all.x=TRUE)
#Ricodifico "--" come NA
for (i in 2:200002) {
  levels(df_dicot_4[,i])[levels(df_dicot_4[,i])=="--"] <-NA
  #print(i)
}

#rimuovo la riga 159 e 260 (user 45 e 77) perchè codificate male (CT e TC sullo snp rs3094315 + altri problemi analoghi) 
df_dicot_4<-df_dicot_4[-c(159,260),]

#mantengo le colonne che hanno più del 20% di dati (rimuovo quelle con +80% di NA)
df_dicot_4<-df_dicot_4[,which(colMeans(!is.na(df_dicot_4)) > 0.2)]
#rimosse 889 colonne

#Rimuovo le variabili con varianza pari a zero o quasi
remove<-nearZeroVar(df_dicot_4)
df_dicot_4<-df_dicot_4[,-remove]
#rimosse 26627 colonne
##dim(df_dicot_4)=(309,172487)



#########################    ################################    ################################   ###################################
#rieseguo pulizia
for (i in 1:172487) {
  levels(df_dicot_4[,i])[levels(df_dicot_4[,i])=="--"] <-NA
  df_dicot_4[,i]<-droplevels(df_dicot_4[,i])
  print(i)
}

df_dicot_4<-df_dicot_4[,which(colMeans(!is.na(df_dicot_4)) > 0.2)]
##dim(df_dicot_4)=(309,172487)


######################     ###################################    ################################








#########################################################################################################################################################
#############################################################df_dicot_5 : da 800001 a 1000000  ###########################################################

df_dicot_5<-df_dicot[,c(1,8959838,8959837,800001:1000000)]

#Ricodifico "--" come NA
for (i in 4:200003) {
  levels(df_dicot_5[,i])[levels(df_dicot_5[,i])=="--"] <-NA
  print(i)
}

#mantengo le colonne che hanno più del 20% di dati (rimuovo quelle con +80% di NA)
df_dicot_5<-df_dicot_5[,which(colMeans(!is.na(df_dicot_5)) > 0.2)]
#rimosse 33120 colonne

#Rimuovo le variabili con varianza pari a zero o quasi
remove<-nearZeroVar(df_dicot_5)
df_dicot_5<-df_dicot_5[,-remove]
#rimosse 24024 colonne
##dim(df_dicot_5)=(309,142859)

save(df_dicot_5, file = "df_dicot_5.RData")
rm(df_dicot_5)
rm(remove)




#########################    ################################    ################################   ###################################
#rieseguo pulizia
for (i in 1:142859) {
  levels(df_dicot_5[,i])[levels(df_dicot_5[,i])=="--"] <-NA
  df_dicot_5[,i]<-droplevels(df_dicot_5[,i])
  print(i)
}

df_dicot_5<-df_dicot_5[,which(colMeans(!is.na(df_dicot_5)) > 0.2)]
##dim(df_dicot_5)=(309,142859)

######################     ###################################    ################################




#########################################################################################################################################################
#############################################################df_dicot_6 : da 1000001 a 1200000  ###########################################################

df_dicot_6<-df_dicot[,c(1,8959838,8959837,1000001:1200000)]

#Ricodifico "--" come NA
for (i in 4:200003) {
  levels(df_dicot_6[,i])[levels(df_dicot_6[,i])=="--"] <-NA
  #print(i)
}

#mantengo le colonne che hanno più del 20% di dati (rimuovo quelle con +80% di NA)
d1<-dim(df_dicot_6)[2]
df_dicot_6<-df_dicot_6[,which(colMeans(!is.na(df_dicot_6)) > 0.2)]
d2<-dim(df_dicot_6)[2]
na6<-d2-d1 
rm(d1)
rm(d2)
#rimosse 134322 colonne

#Rimuovo le variabili con varianza pari a zero o quasi
remove6<-nearZeroVar(df_dicot_6)
df_dicot_6<-df_dicot_6[,-remove6]
#rimosse 44453 colonne
dim6<-dim(df_dicot_6)
##dim(df_dicot_6)=(309,21228)

save(df_dicot_6, file = "df_dicot_6.RData")
rm(df_dicot_6)



#########################    ################################    ################################   ###################################
#rieseguo pulizia
for (i in 1:21228) {
  levels(df_dicot_6[,i])[levels(df_dicot_6[,i])=="--"] <-NA
  df_dicot_6[,i]<-droplevels(df_dicot_6[,i])
  print(i)
}

df_dicot_6<-df_dicot_6[,which(colMeans(!is.na(df_dicot_6)) > 0.2)]
##dim(df_dicot_6)=(309,21228)

######################     ###################################    ################################




#########################################################################################################################################################
#############################################################df_dicot_7 : da 1200001 a 1400000  ###########################################################

df_dicot_7<-df_dicot[,c(1,8959838,8959837,1200001:1400000)]
df_dicot_7<-df_dicot_7[,which(colMeans(!is.na(df_dicot_7)) > 0.2)]
#Ricodifico "--" come NA

levels(df_dicot_7[,4])[levels(df_dicot_7[,4])=="--"] <-NA
df_dicot_7[,4]<-droplevels(df_dicot_7[,4])
#mantengo le colonne che hanno più del 20% di dati (rimuovo quelle con +80% di NA)

df_dicot_7<-df_dicot_7[,which(colMeans(!is.na(df_dicot_7)) > 0.2)]

#rimosse 199999 tutte colonne

#Rimuovo le variabili con varianza pari a zero o quasi
remove7<-nearZeroVar(df_dicot_7)
#dim(df_dicot_7)=(309,4)   un solo snp
save(df_dicot_7, file = "df_dicot_7.RData")
rm(df_dicot_7)

#########################    ################################    ################################   ###################################


######################     ###################################    ################################


#########################################################################################################################################################
#############################################################df_dicot_8 : da 1400001 a 1600000  ###########################################################

df_dicot_8<-df_dicot[,c(1,8959838,8959837,1400001:1600000)]
df_dicot_8<-df_dicot_8[,which(colMeans(!is.na(df_dicot_8)) > 0.2)]
#rimosse tutte le colonne

#########################################################################################################################################################
#############################################################df_dicot_9 : da 1600001 a 1800000  ###########################################################

df_dicot_9<-df_dicot[,c(1,8959838,8959837,1600001:1800000)]

#Ricodifico "--" come NA
for (i in 4:334) {
  levels(df_dicot_9[,i])[levels(df_dicot_9[,i])=="--"] <-NA
  print(i)
}

#mantengo le colonne che hanno più del 20% di dati (rimuovo quelle con +80% di NA)
d1<-dim(df_dicot_9)[2]
df_dicot_9<-df_dicot_9[,which(colMeans(!is.na(df_dicot_9)) > 0.2)]
d2<-dim(df_dicot_9)[2]
na9<-d1-d2 
rm(d1)
rm(d2)
#rimosse 1999682 colonne

#Rimuovo le variabili con varianza pari a zero o quasi
remove9<-nearZeroVar(df_dicot_9)
df_dicot_9<-df_dicot_9[,-remove9]
#rimosse 154 colonne
dim9<-dim(df_dicot_9)
##dim(df_dicot_9)=(309,167)



#########################    ################################    ################################   ###################################
#rieseguo pulizia
for (i in 1:167) {
  levels(df_dicot_9[,i])[levels(df_dicot_9[,i])=="--"] <-NA
  df_dicot_9[,i]<-droplevels(df_dicot_9[,i])
  print(i)
}

df_dicot_9<-df_dicot_9[,which(colMeans(!is.na(df_dicot_9)) > 0.2)]
##dim(df_dicot_9)=(309,167)

######################     ###################################    ################################





#########################################################################################################################################################
#############################################################df_dicot_10 : da 1800001 a 2000000  ########################################################

df_dicot_10<-df_dicot[,c(1,8959838,8959837,1800001:2000000)]
df_dicot_10<-df_dicot_10[,which(colMeans(!is.na(df_dicot_10)) > 0.2)]
#rimosse tutte le colonne

#########################################################################################################################################################
#############################################################df_dicot_11 : da 2000001 a 2200000  ########################################################

df_dicot_11<-df_dicot[,c(1,8959838,8959837,2000001:2200000)]
df_dicot_11<-df_dicot_11[,which(colMeans(!is.na(df_dicot_11)) > 0.2)]
#rimosse tutte le colonne

#########################################################################################################################################################
#############################################################df_dicot_12 : da 2200001 a 2400000  ########################################################

df_dicot_12<-df_dicot[,c(1,8959838,8959837,2200001:2400000)]
df_dicot_12<-df_dicot_12[,which(colMeans(!is.na(df_dicot_12)) > 0.2)]
#rimosse tutte le colonne

#########################################################################################################################################################
#############################################################df_dicot_13 : da 2400001 a 2600000  ########################################################

df_dicot_13<-df_dicot[,c(1,8959838,8959837,2400001:2600000)]
df_dicot_13<-df_dicot_13[,which(colMeans(!is.na(df_dicot_13)) > 0.2)]
#rimosse tutte le colonne

#########################################################################################################################################################
#############################################################df_dicot_14 : da 2600001 a 3600000  ########################################################

df_dicot_14<-df_dicot[,c(1,8959838,8959837,2600001:3600000)]
df_dicot_14<-df_dicot_14[,which(colMeans(!is.na(df_dicot_14)) > 0.2)]
#rimosse tutte le colonne

#########################################################################################################################################################
#############################################################df_dicot_15 : da 3600001 a 4600000  ########################################################

df_dicot_15<-df_dicot[,c(1,8959838,8959837,3600001:4600000)]
df_dicot_15<-df_dicot_15[,which(colMeans(!is.na(df_dicot_15)) > 0.2)]
#rimosse tutte le colonne

#########################################################################################################################################################
#############################################################df_dicot_16 : da 4600001 a 5600000  ########################################################

df_dicot_16<-df_dicot[,c(1,8959838,8959837,4600001:5600000)]
df_dicot_16<-df_dicot_16[,which(colMeans(!is.na(df_dicot_16)) > 0.2)]
#rimosse tutte le colonne

#########################################################################################################################################################
#############################################################df_dicot_17 : da 5600001 a 7600000  ########################################################

df_dicot_17<-df_dicot[,c(1,8959838,8959837,5600001:7600000)]
df_dicot_17<-df_dicot_17[,which(colMeans(!is.na(df_dicot_17)) > 0.2)]
#rimosse tutte le colonne

#########################################################################################################################################################
#############################################################df_dicot_18 : da 7600001 a 8959838  ########################################################

df_dicot_18<-df_dicot[,c(1,8959838,8959837,7600001:8959836)]
df_dicot_18<-df_dicot_18[,which(colMeans(!is.na(df_dicot_18)) > 0.2)]
#rimosse tutte le colonne


#########################################################################################################################################################
#########################################################################################################################################################
#####################################################################unisco in un solo dataset gli snp rimasti dopo la pulizia###########################
#########################################################################################################################################################

pulito<-merge(x=df_dicot_1,y=df_dicot_2[,-c(2:3)],by="user_id",all.x=TRUE)
pulito<-merge(x=pulito,y=df_dicot_3[,-c(2:3)],by="user_id",all.x=TRUE)
pulito<-merge(x=pulito,y=df_dicot_4[,-c(2:3)],by="user_id",all.x=TRUE)
pulito<-merge(x=pulito,y=df_dicot_5[,-c(2:3)],by="user_id",all.x=TRUE)
pulito<-merge(x=pulito,y=df_dicot_6[,-c(2:3)],by="user_id",all.x=TRUE)
pulito<-merge(x=pulito,y=df_dicot_7[,-c(2:3)],by="user_id",all.x=TRUE)
pulito<-merge(x=pulito,y=df_dicot_9[,-c(2:3)],by="user_id",all.x=TRUE)
save(pulito, file="pulito.RData")

#########################################################################################################################################################
#########################################################################################################################################################
#########################################################################################################################################################
#sono presenti alcune colonne di introni codificate con "I" e "II". Rimuovo tutto
#rimuovo le colonne che non sono codificate con le basi A G C T (introni codificati con I)
r<-vector()
for (i in 1:862355) {
  if ("I" %in% levels(as.factor(pulito[,i]))){
    r<-c(r,i)
  }
}
dim(pulito) #309 862355
pulito<-pulito[,-c(r)] #rimuovo 543 colonne
dim(pulito) #309 861812

#rimuovo le colonne che non sono codificate con le basi A G C T (introni codificati con II)
r2<-vector()
for (i in 1:861615) {
  if ("II" %in% levels(as.factor(pulito[,i]))){
    r2<-c(r2,i)
  }
}
pulito<-pulito[,-c(r2)] #rimuovo 197 colonne
dim(pulito) #309 861615


#rimuovo le colonne che non sono codificate con le basi A G C T (introni codificati con DD)
r3<-vector()
for (i in 1:861615) {
  if ("D" %in% levels(as.factor(pulito[,i])) ||
      "DD" %in% levels(as.factor(pulito[,i])) ||
      "DI" %in% levels(as.factor(pulito[,i]))){
    r3<-c(r3,i)
  }
}



pulito<-pulito[,-c(r3)] #rimuovo 213 colonne
dim(pulito) #309 861402
pulito<-pulito[-239,] #questo soggetto ha solo una lettera su tutto il genoma
dim(pulito) #308 861402

#prendo gli snp nei cui livelli è presente 0
r4<-vector()
for (i in 1:861615) {
  if ("0" %in% levels(as.factor(pulito[,i])))
    {
    r4<-c(r4,i)
  }
}

#elimino per questi snp i livelli non usati, così da vedere se effettivamente ci sono ancora 0 nel dataset

for (i in c(r4)) {
  pulito[,i]<-droplevels(pulito[,i])
  print(i)
}

#rieseguo per vedere se ho ancora 0 tra i livelli
r4<-vector()
for (i in 1:861615) {
  if ("0" %in% levels(as.factor(pulito[,i])))
  {
    r4<-c(r4,i)
  }
}

#un genotipo è codificato male "CC ", lo ricodifico
pulito[126,"rs17822931"]<-"CC"
pulito[,"rs17822931"]<-droplevels(pulito[,"rs17822931"])
pulito[126,"rs7903146"]<-"CT"
pulito[,"rs7903146"]<-droplevels(pulito[,"rs7903146"])

#rinomino le variabili il cui nome è estratto male "rs4873...2364"
for (i in 1:861402) {
  if (grepl("\\...",colnames(pulito)[i])==TRUE){
    colnames(pulito)[i]<-gsub("\\...*", "",colnames(pulito)[i])
    print(i)
  }
}
save(pulito,file = "pulito.RData")
dim(pulito) #308 857909 (dopo aver rimosso le variabili nearzerovar della riga 782)


coded<-as.matrix(pulito)
coded<-coded[,-c(1:3)] #tolgo le prime tre colonne user_id sex variation

dim(coded) #308 861399
#ricodifico i dati in pulito in numeri
coded<-raw.data(coded,frame = "wide", hapmap = NULL, base = TRUE, sweep.sample = 1,
                call.rate = 0, maf = 0, imput = FALSE, outfile = "012")

dim(coded$M.clean) #308 861399
#tengo solo la matrice di dati ed elimino il report della funzione
coded<-coded$M.clean
#riattacco le prime tre colonne
coded<-cbind(pulito[,1:3],coded)

########################################################################################################################################################
############################################################## ANALISI ESPLORATIVA #####################################################################
if (!requireNamespace("BiocManager", quietly=TRUE))
  install.packages("BiocManager")
BiocManager::install("gdsfmt")
BiocManager::install("SNPRelate")

#rinomino gli SNP il cui nome è stato estratto male
grepl("\\...",colnames(coded)[245232])
grepl("\\...",colnames(coded)[245233])

system.time(for (i in 1:857909) {
  if (grepl("\\...",colnames(coded)[i])==TRUE){
  colnames(coded)[i]<-gsub("\\...*", "",colnames(coded)[i])
  print(i)
  }
  })


library(gdsfmt)
library(SNPRelate)

#rimuovo 1 soggetto con quasi tutti NA
coded<-coded[-207,] #dim coded = 307 x 857909
pulito<-pulito[-207,] #dim pulito = 307 x 857909

#rimuovo i soggetti che hanno tutti NA
r<-which(rowMeans(is.na(coded)) > 0.90) #tolgo nove osservazioni
coded<-coded[-r,] #298 osservazioni
pulito<-pulito[-r,]

#la funzione snpgdsCreateGeno ha bisogno che l'input di genmat sia una matrice numerica
#quindi elimino le prime tre colonne della matrice coded e tengo in coded.matrix solo gli snp in classe numeric
coded.matrix<-as.matrix(coded[,4:857909])
class(coded.matrix[,1]) #numeric

#creo il file gds utile per poter utilizzare il pacchetto snpRelate
snpgdsCreateGeno("test.gds", genmat = coded.matrix,
                 sample.id = coded[,1], snp.id = colnames(coded)[4:857909],
                 snpfirstdim=FALSE)
genofile <- snpgdsOpen("test.gds", readonly = FALSE)

#aggiungo il sesso alle informazioni sul campione 
samp.annot <-data.frame(sex = coded[,2])
add.gdsn(genofile, "sample.annot", samp.annot)
#vediamo il sesso dei primi 6 soggetti
head(read.gdsn(index.gdsn(genofile, "sample.annot")))

#matrice dei genotipi
g <- snpgdsGetGeno(genofile)


#### PCA ####
snp.id<-read.gdsn(index.gdsn(genofile, "snp.id"))
pca <- snpgdsPCA(genofile, snp.id=snp.id, num.thread=2)
pc.percent <- pca$varprop*100
head(round(pc.percent, 2))
tab <- data.frame(sample.id = pca$sample.id,
                  EV1 = pca$eigenvect[,1],    # the first eigenvector
                  EV2 = pca$eigenvect[,2],    # the second eigenvector
                  stringsAsFactors = FALSE)
head(tab)
plot(tab$EV2, tab$EV1, xlab="Autovettore 2", ylab="Autovettore 1")
#c'è un qualche pattern, proviamo a inserire il sesso

sample.id <- read.gdsn(index.gdsn(genofile, "sample.id"))
pop.sex<-read.gdsn(index.gdsn(genofile, "sample.annot/sex"))
head(cbind(sample.id, pop.sex))

#unisco in unico dataframe l'user_id (sample_id) il sesso, e i primi due autovettori
tab <- data.frame(sample.id = pca$sample.id,
                  pop = factor(pop.sex)[match(pca$sample.id, sample.id)],
                  EV1 = pca$eigenvect[,1],    # the first eigenvector
                  EV2 = pca$eigenvect[,2],    # the second eigenvector
                  stringsAsFactors = FALSE)
head(tab)

plot(tab$EV2, tab$EV1, col=as.integer(tab$pop), xlab="Autovettore 2", ylab="Autovettore 1")
legend("topleft", legend=levels(tab$pop), pch="o", col=1:nlevels(tab$pop))
#il secondo autovalore discrimina per maschi e femmine


#Plot the principal component pairs for the first four PCs:
lbls <- paste("PC", 1:4, "\n", format(pc.percent[1:4], digits=2), "%", sep="")
pairs(pca$eigenvect[,1:4], col=tab$pop, labels=lbls)


#Parallel coordinates plot for the top principal components:
library(MASS)

datpop <- factor(pop.sex)[match(pca$sample.id, sample.id)]
parcoord(pca$eigenvect[,1:16], col=datpop)


#### MDS ####
#For n study individuals, snpgdsIBS() can be used to create a n×n matrix of genome-wide average IBS(Identity By State) pairwise identities:
ibs <- snpgdsIBS(genofile, num.thread=2)
ibs$sample.id[257]
ibs$ibs[1:2,1:2]
dim(ibs$ibs) #298 x 298
loc <- cmdscale(1 - ibs$ibs, k = 2)
x <- loc[, 1]; y <- loc[, 2]
pop_sex <- as.factor(pop.sex)

plot(x, y, col=pop_sex, xlab = "", ylab = "",
     main = "Multidimensional Scaling Analysis (IBS)")
legend("bottomleft", legend=levels(pop_sex), pch="o", text.col=1:nlevels(pop_sex))



#################################################### fine esplorativa #########################################################
###############################################################################################################################

#ci sono ancora variabili con zero varianza, le rimuovo da coded
remove<-nearZeroVar(coded)
coded<-coded[,-remove] #3493 variabili 
dim(coded) #308 857909

#li tolgo anche da pulito
pulito<-pulito[,-c(remove)] #308 x 857909

remove<-nearZeroVar(coded)

################################### PRIMI MODELLI ##########################
############################################################################
############################### Logistico con solo 1 snp alla volta ########

pval<-numeric(857909) #names(pval) per indicare il nome dello snp, poi estraggo elemento con pval["name"]
stat<-numeric(857909)
coeff<-numeric(857909)

coded[1:10,1:15]
for (i in 4:857909) {
  fit<-glm(coded[,3]~coded[,i], family = "binomial")
  pval[i]<-summary(fit)$coefficients[2,4]
  names(pval)[i]<-colnames(coded)[i]
  stat[i]<-summary(fit)$coefficients[2,3]
  names(stat)[i]<-colnames(coded)[i]
  coeff[i]<-summary(fit)$coefficients[2,1]
  names(coeff)[i]<-colnames(coded)[i]
  print(i)
}

pval[1:4]
#tolgo i primi tre elementi perchè vuoti
pval<-pval[-(1:3)] #length(pval)= 857906
stat<-stat[-c(1:3)] #length(stat)= 857906
coeff<-coeff[-c(1:3)] #length(coeff)= 857906

save(pval,file = "pval.RData")
save(stat,file = "stat.RData")
save(coeff,file = "coeff.RData")

sorted<-sort(pval) #i più piccoli sono 10^-5

################################################################################################################################
############################################# Creo matrice con posizione snp sul cromosoma #####################################
lista<-colnames(coded)[4:857909]
matrice_snp<-matrix(nrow = 857906, ncol = 1)
colnames(matrice_snp)<- "rsid"
matrice_snp[,1]<-lista



#lo faccio prima per il primo dataset [[8]] e poi vado a riempire i vuoti 
#estraggo il dataset dell'user 8 senza il genotipo
data23andme.8<-dati23andme[[8]][,-4]

dim(matrice_snp) #857906      1
#unisco la matrice con gli snp sulle righe al cromosoma e posizione usando come chiave il nome dello SNP
matrice_snp<-merge(matrice_snp,data23andme.8, by="rsid", all.x=TRUE)
dim(matrice_snp) #857906  3


sum(is.na(matrice_snp$chromosome))
#restano 20668 snp di cui trovare la posizione

dati23andme.11<-dati23andme[[11]][,-4]

matrice_snp$chromosome[is.na(matrice_snp$chromosome)]<-dati23andme.11[,2][match(matrice_snp$rsid,dati23andme.11[,1])][which(is.na(matrice_snp$chromosome))]
matrice_snp$position[is.na(matrice_snp$position)]<-dati23andme.11[,3][match(matrice_snp$rsid,dati23andme.11[,1])][which(is.na(matrice_snp$position))]
sum(is.na(matrice_snp$chromosome))
#20654 NA 

dati23andme.14<-dati23andme[[14]][,-4]
matrice_snp$chromosome[is.na(matrice_snp$chromosome)]<-dati23andme.14[,2][match(matrice_snp$rsid,dati23andme.14[,1])][which(is.na(matrice_snp$chromosome))]
matrice_snp$position[is.na(matrice_snp$position)]<-dati23andme.14[,3][match(matrice_snp$rsid,dati23andme.14[,1])][which(is.na(matrice_snp$position))]
sum(is.na(matrice_snp$chromosome))
#restano 20476 snp di cui trovare la posizione



for (i in 15:10408) {
  if (is.null(dati23andme[[i]])==FALSE){
    matrice_snp$chromosome[is.na(matrice_snp$chromosome)]<-dati23andme[[i]][,2][match(matrice_snp$rsid,dati23andme[[i]][,1])][which(is.na(matrice_snp$chromosome))]
    matrice_snp$position[is.na(matrice_snp$position)]<-dati23andme[[i]][,3][match(matrice_snp$rsid,dati23andme[[i]][,1])][which(is.na(matrice_snp$position))]
    print(i)
    }
}
sum(is.na(matrice_snp$chromosome))
#rimossi tutti gli NA, ora tutti gli SNP hanno cromosoma e posizione
  
#Aggiungo il pvalue (da qui posso aggiungere anche coeff e stat ma per ora non ci servono)

pval<-as.data.frame(pval)
pval$rsid<-row.names(pval)
pval[1:10,]

#coeff<-as.data.frame(coeff)
#coeff$rsid<-row.names(coeff)
#coeff

#stat<-as.data.frame(stat)
#stat$rsid<-row.names(stat)
#stat

matrice_snp[1:10,]
#unisco al dataset matrice_snp la colonna col pvalue
matrice_snp<-merge(matrice_snp,pval, by='rsid', all.x=TRUE)

###############################################################################################################
####################################### GRAFICI RISULTATI MODELLI #############################################

###Manhattan Plot
install.packages("CMplot")
library("CMplot")
#prova_snp<-matrice_snp[473260:473270,-c(2,3,4)]

#prova_pval<-pval


#SNP-Density Plotting
CMplot(matrice_snp,type="p",plot.type="d",bin.size=1e6,chr.den.col=c("darkgreen", "yellow", "red"),file="jpg",memo="",dpi=300,
       main="illumilla_60K",file.output=FALSE,verbose=TRUE,width=9,height=6)
CMplot(matrice_snp,type="p",plot.type="d",bin.size=1e6,chr.den.col=c("darkgreen", "yellow", "red"),file="jpg",memo="",dpi=300,
       main="illumilla_60K",file.output=FALSE,verbose=TRUE,width=9,height=6)
#Rectangular-Manhattan Plotting pval
CMplot(matrice_snp,type="p",plot.type="m",LOG10=TRUE,threshold=1e-4,threshold.col="black", threshold.lty=1,
       threshold.lwd=1,file="jpg",memo="",dpi=300, file.output=F,verbose=TRUE,width=14,height=6, cex.lab = 1, main= "Manhattan plot")
# 'chr.labels.angle': adjust the angle of labels of x-axis (-90 < chr.labels.angle < 90).


CMplot(matrice_snp, plot.type="m", LOG10=TRUE, ylim=NULL, threshold=c(1e-5,1e-4),threshold.lty=c(1,2),
       threshold.lwd=c(1,1), threshold.col=c("black","grey"), amplify=TRUE,bin.size=1e6,
       chr.den.col=c("darkgreen", "yellow", "red"),signal.col=c("red","green"),signal.cex=c(1.5,1.5),
       signal.pch=c(19,19),file="jpg",memo="",dpi=300,file.output=TRUE,verbose=TRUE,
       width=14,height=6)
#QQ-plot
CMplot(matrice_snp,plot.type="q",box=FALSE,file="jpg",memo="",dpi=300,
       conf.int=TRUE,conf.int.col=NULL,threshold.col="red",threshold.lty=2,
       file.output=FALSE,verbose=TRUE,width=5,height=5, cex.lab = 1, main = "QQ-plot")




############################################################################################################################
########################################### MODELLO CON FACTOR + SESSO + MDS ###############################################
pval_factor<-numeric(857909)


#fitto il modello con snp come fattore + sesso + mds1 + mds2
#fitto il modello con sesso + mds1 + mds2 (solo sui soggetti per cui faccio il primo modello)
#faccio l'anova dei due modelli e ne prendo il pvalue
for (i in 857742:857909) {
  fit<-glm(coded[,3]~as.factor(coded[,i])+ coded[,2]+x+y, family = "binomial")
  fit1<-glm(coded[,3]~ coded[,2]+x+y, family = "binomial",subset=!is.na(coded[,i]))
  anova<-anova(fit1,fit, test = "LRT")
  pval_factor[i]<-anova$`Pr(>Chi)`[2]
  names(pval_factor)[i]<-colnames(coded)[i]
  print(i)
}

sorted_factor<-sort(pval_factor)
#tengo solo i pvalue diversi da 0
pval_factor<-pval_factor[pval_factor!= 0]
#tengo solo i pvalue che non sono NA
pval_factor<-pval_factor[is.na(pval_factor) == FALSE]
pval_factor<-as.data.frame(pval_factor)
pval_factor$rsid<-row.names(pval_factor)

save(pval_factor, file="pval_factor_senzaNAe0.RData")

matrice_snp<-merge(matrice_snp,pval_factor, by='rsid', all.x=TRUE)

#Manhattan plot
CMplot(matrice_snp[,-4],type="p",plot.type="m",LOG10=TRUE,threshold=1e-4,threshold.col="black", threshold.lty=1,
       threshold.lwd=1,file="jpg",memo="",dpi=300, file.output=F,verbose=TRUE,width=12,height=6, cex.lab = 1)
#QQ-plot
CMplot(matrice_snp[,-4],plot.type="q",box=FALSE,file="jpg",memo="",dpi=300,
       conf.int=TRUE,conf.int.col=NULL,threshold.col="red",threshold.lty=2,
       file.output=FALSE,verbose=TRUE,width=5,height=5, cex.lab = 1, main = "QQ-plot")
############################################################################################################################
####################################### LASSO e ELASTICNET CON glmnet ################################################################


###SELEZIONE DATI PER LASSO ED ELASTIC NET

#tengo solo gli snp con meno del 50% di NA
coded_50NA<-coded[,which(colMeans(!is.na(coded)) > 0.5)]

#tutti gli snp ordinati per pvalue
sorted_factor<-pval_factor[order(pval_factor$pval_factor),]
#solo gli snp con missing<50% ordinati per pvalue
sorted_50NA<-sorted_factor[colnames(coded_50NA)[-c(1:3)],]
sorted_50NA<-sorted_50NA[order(sorted_50NA$pval_factor),]


#primi_mille<-sorted_factor[1:1000,]
#primi_duemila<-sorted_factor[1:2000,]
primi_mille_50NA<-sorted_50NA[1:1000,]


#a<-coded[,primi_mille$rsid]
#dati_lasso<-coded[,primi_duemila$rsid]
dati_lasso_50NA<-coded[,primi_mille_50NA$rsid]


BiocManager::install("impute")
library(impute)
sum(is.na(dati_lasso_50NA)) #41138 missing da imputare
#sum(is.na(a)) #84121 missing da imputare


#b<-impute.knn(t(a) ,k = 10, rowmax = 0.8, colmax = 0.9, maxp = 100, rng.seed=362436069)
#temp<-impute.knn(t(dati_lasso) ,k = 10, rowmax = 0.8, colmax = 0.9, maxp = 100, rng.seed=362436069)
temp_50NA<-impute.knn(t(dati_lasso_50NA) ,k = 10, rowmax = 0.5, colmax = 0.75, maxp = 100, rng.seed=362436069)


#c<-round(t(b$data))
#dati_imputati<-round(t(temp$data))
dati_imputati_50NA<-round(t(temp_50NA$data))


##Lasso

library(glmnet)
#calcolo il lambda ottimale tramite cross validation per il lasso (alpha=1)
set.seed(123)
#d<-cv.glmnet(c, coded[,3], family = "binomial", nfold = 10, type.measure = "deviance", alpha = 1)
#cv1 <- cv.glmnet(dati_imputati, coded[,3], family = "binomial", nfold = 10, type.measure = "deviance", alpha = 1)
cv1_50NA <-cv.glmnet(dati_imputati_50NA, coded[,3], family = "binomial", nfold = 10, type.measure = "deviance", alpha = 1)
save(cv1_50NA,file="cv1_50NA.RData")

#plot(d)
#plot(cv1)
plot(cv1_50NA, ylab="Devianza binomiale")


#e<-glmnet(c, coded[,3], family = "binomial", lambda = d$lambda.1se, alpha = 1)
#lasso <- glmnet(dati_imputati, coded[,3], family = "binomial", lambda = cv1$lambda.1se, alpha = 1)
lasso_50NA <- glmnet(dati_imputati_50NA, coded[,3], family = "binomial", lambda = cv1_50NA$lambda.1se, alpha = 1)
save(lasso_50NA,file="lasso_50NA.RData")

#f<-which(coef(e)!=0) #110
#p_lasso<-which(coef(lasso)!=0) #128 variabili
p_lasso_50NA<-which(coef(lasso_50NA)!=0) #105
#lambda=0.03114676

#sort(coef(e))
sort(coef(lasso_50NA))
coef_lasso_50NA<-coef(lasso_50NA)
nomi_lasso<-names(coef_lasso_50NA[p_lasso_50NA,])
save(nomi_lasso,file = "nomi_lasso.Rdata" )
coef_lasso_50NA[order(coef_lasso_50NA[,1]),]

#Cross validation ripetuta 20 volte per trovare gli snp che risultano sempre diversi da 0
#fit<-cv.glmnet(dati_imputati_50NA, coded[,3], family="binomial", nfold = 10, type.measure = "deviance", alpha = 1)
#sim <- cbind(as.numeric(coef(fit)), 
#             replicate(20, as.numeric(coef(cv.glmnet(dati_imputati_50NA, coded[,3], family="binomial", nfold = 10, type.measure = "deviance", alpha = 1)))))
#k=1000
#plot(1:k, rowMeans(sim[-1,] != 0) + runif(k, -0.025, 0.025), 
#     xlab="Coefficient Index", ylab="Frequency not zero (jittered)",
#     main="Results of Repeated Cross-Validated Lasso Fits")
#abline(h=0.9)
#sum(rowMeans(sim[-1,] != 0)==1)
#95 variabili vengono sempre diverse da zero

#quali sono le righe sempre diverse da 0
#diversidazero<-which(rowMeans(sim!=0)==1)
#estraggo i nomi di questi snp
#snp_diversidazero<-names(coef(lasso_50NA)[diversidazero,])[-1] #tolgo l'intercetta

#faccio il manhattan plot per vedere qual è il cromosoma più interessato
#table(matrice_snp[matrice_snp$rsid%in%snp_diversidazero,]$chromosome)


##Elastic Net
install.packages("glmnetUtils")
library(glmnetUtils)
#g<-cva.glmnet(c,coded[,3], family="binomial", nfolds = 10)
#elastic_net<-cva.glmnet(dati_imputati,coded[,3], family="binomial", nfolds = 10)
cv.elastic_net<-cva.glmnet(dati_imputati_50NA,coded[,3], family="binomial", nfolds = 10)
save(cv.elastic_net, file="cv.elastic_net.RData")

#
plot(cv.elastic_net$modlist[[9]], ylab="Devianza binomiale")
plot(cv.elastic_net$modlist[[7]])
cv.elastic_net$modlist[[9]]$lambda.1se
#alpha=0.512 lambda=0.04002439

#plot(g)
#plot(elastic_net)
plot(cv.elastic_net)

elastic_net_50NA <- glmnet(dati_imputati_50NA, coded[,3], family = "binomial", 
                           lambda = cv.elastic_net$modlist[[9]]$lambda.1se, alpha =  0.512)
save(elastic_net_50NA,file="elastic_net_50NA.RData")

p_elastic_50NA<-which(coef(elastic_net_50NA)!=0) #160 variabili


coef_elastic_50NA<-coef(elastic_net_50NA)
save(coef_elastic_50NA, file="coef_elastic_50NA.RData")
coef_elastic_50NA[order(coef_elastic_50NA[,1]),]
nomi_elastic<-names(coef_elastic_50NA[p_elastic_50NA,])
nomi_elastic<-nomi_elastic[-1]
save(nomi_elastic, file="nomi_elastic.RData")
#################################################################################################################################
################################################# fused lasso with penalized #####################################################

#vedo quale è il cromosoma con più snp tra i primi mille più significativi
sort(table(matrice_snp[matrice_snp$rsid%in%primi_mille_50NA$rsid,][,2]))
#il 2 è quello più numeroso, ne ha 112

snp_cr2<-matrice_snp[which(matrice_snp$chromosome=="2") ,]
snp_cr2<-snp_cr2$rsid

#prendo da coded solo gli snp sul cromosoma 2
coded_cr2<-coded[,snp_cr2]
#rimuovo quelli con più di 50% di NA
coded_cr2<-coded_cr2[,which(colMeans(!is.na(coded_cr2)) > 0.5)]

library(impute)
#imputo i valori mancanti
coded_cr2<-impute.knn(t(coded_cr2) ,k = 10, rowmax = 0.8, colmax = 0.9, maxp = 100, rng.seed=362436069)
coded_cr2<-round(t(coded_cr2$data))
#tengo da matrice snp solo quelli sul cromosoma due per poterli poi ordinare
matrice_snp_cr2<-matrice_snp[matrice_snp$rsid%in%snp_cr2,]

#tengo in matrice snp quelli sul cromosoma due che hanno meno di 50% di NA
tenere<-intersect(colnames(coded_cr2), matrice_snp_cr2$rsid)
matrice_snp_cr2<-matrice_snp_cr2[matrice_snp_cr2$rsid%in%tenere,]
#ordino per posizione
matrice_snp_cr2<-matrice_snp_cr2[order(matrice_snp_cr2$position),]
snp_ordinati_cr2<-matrice_snp_cr2$rsid

#riordino le colonne di coded_cr2 in base alla posizione
coded_cr2<-coded_cr2[,snp_ordinati_cr2]
dim(coded_cr2) #298 45085

pvalini<-which(pval_factor$pval_factor<0.05)
pvalini<-pval_factor[pvalini,]
piccoli<-pvalini$rsid
keep<-intersect(colnames(coded_cr2),piccoli)
pos<-matrice_snp_cr2[matrice_snp_cr2$rsid%in%keep,3]
#tengo solo gli snp sul cromosoma due che hanno pvalue<0.05 nel modello con
#factor + sex + mds
coded_cr2<-coded_cr2[,keep]
dim(coded_cr2)
#298 2648
save(coded_cr2, file="coded_cr2.RData")

library(penalized)
fit1 <- profL1(coded[,3], coded_cr2,fold=10, plot=TRUE)
fit2 <- profL2(coded[,3], coded_cr2,fold=fit1$fold,
               minl = 0.01, maxl = 1000)
save(fit1,file = "fit1.RData")
save(fit2,file = "fit2.RData")

plot(fit2$lambda, fit2$cvl, type="l", log="x", xlab="lambda 2", ylab="verosimiglianza cross-validata")
plotpath(fit2$fullfit, log="x")
plot(fit1$lambda, fit1$cvl, type="l", xlab="lambda 1", ylab="verosimiglianza cross-validata")

opt1 <- optL1(coded[,3], coded_cr2, fold=fit1$fold)
opt1$lambda
#2.520752

opt2 <- optL2(coded[,3], coded_cr2, fold=fit2$fold)

opt2$lambda
#23.38068
save(opt1, file="opt1.RData")
save(opt2, file="opt2.RData")
fit_b<-penalized(coded[,3], coded_cr2, lambda1 = 2.520752, lambda2=23.38068,fusedl=TRUE)
save(fit_b,file = "fit_b.RData")

#estraggo coefficienti del fused lasso e li trasformo in dataframe con coef e rsid
coef_fused<-coefficients(fit_b, "all")
coef_fused<-as.data.frame(coef_fused)
coef_fused$rsid<-row.names(coef_fused)
save(coef_fused, file="coef_fused.RData")

p_fused<-coefficients(fit_b) 
save(p_fused,file = "p_fused.RData")
nomi_fused<-names(p_fused)[-1] #218 predittori
save(nomi_fused,file = "nomi_fused.RData")

#unisco le due matrici per aggiungere ai coefficienti la posizione
fused_snp<-merge(coef_fused,matrice_snp, by='rsid', all.x=TRUE)
fused_snp<-fused_snp[-1,]
plot(fused_snp$position, fused_snp$coef_fused, type="h", 
     xlab="Posizione sul cromosoma 2", ylab="Coefficiente fused lasso",las = 2, cex.axis = 0.54)
install.packages("Hmisc")
#library(Hmisc)
#minor.tick(nx=2, ny=2, x.args = fused_snp$position, y.args = list())



####RIFACCIO IL LASSO SULLO STESSO DATASET DEL FUSED LASSO
cv1_cr2 <-cv.glmnet(coded_cr2, coded[,3], family = "binomial", nfold = 10, type.measure = "deviance", alpha = 1)
save(cv1_cr2, file="cv1_cr2.RData")
plot(cv1_cr2, ylab="Devianza binomiale")

#lambda=0.05854
lasso_cr2 <- glmnet(coded_cr2, coded[,3], family = "binomial", lambda = cv1_cr2$lambda.1se, alpha = 1)
save(lasso_cr2, file="lasso_cr2.RData")
p_lasso_cr2<-which(coef(lasso_cr2)!=0) #67 variabili
coef_lasso_cr2<-coef(lasso_cr2)

coef_lasso_cr2<-as.matrix(coef_lasso_cr2)
coef_lasso_cr2<-as.data.frame(coef_lasso_cr2)
coef_lasso_cr2$rsid<-row.names(coef_lasso_cr2)

lasso_snp_cr2<-merge(coef_lasso_cr2,matrice_snp, by='rsid', all.x=TRUE)
lasso_snp_cr2<-lasso_snp_cr2[-1,]


plot(lasso_snp_cr2$position, lasso_snp_cr2$s0, type='h', las=2,
     xlab="Posizione sul cromosoma 2",ylab="Coefficiente lasso", cex.axis=0.54)




####RIFACCIO L'ELASTIC NET SULLO STESSO DATASET DEL FUSED LASSO

cv.elastic_net_cr2<-cva.glmnet(coded_cr2,coded[,3], family="binomial", nfolds = 10)
save(cv.elastic_net_cr2, file = "cv.elastic_net_cr2.RData")
plot(cv.elastic_net_cr2$modlist[[9]], ylab="Devianza binomiale")
plot(cv.elastic_net_cr2)
cv.elastic_net_cr2$modlist[[9]]$lambda.1se
#alpha=0.512 lambda=0.04109
elastic_net_cr2 <- glmnet(coded_cr2, coded[,3], family = "binomial", 
                           lambda = cv.elastic_net_cr2$modlist[[9]]$lambda.1se, alpha =  0.512)
save(elastic_net_cr2,file = "elastic_net_cr2.RData")
p_elastic_cr2<-which(coef(elastic_net_cr2)!=0) #208 variabili


coef_elastic_cr2<-coef(elastic_net_cr2)
coef_elastic_cr2<-as.matrix(coef_elastic_cr2)
coef_elastic_cr2<-as.data.frame(coef_elastic_cr2)
coef_elastic_cr2$rsid<-row.names(coef_elastic_cr2)


elastic_snp_cr2<-merge(coef_elastic_cr2,matrice_snp, by='rsid', all.x=TRUE)
elastic_snp_cr2<-elastic_snp_cr2[-1,]

plot(elastic_snp_cr2$position, elastic_snp_cr2$s0, type='h', las=2,
     xlab="Posizione sul cromosoma 2",ylab="Coefficiente elastic net", cex.axis=0.54)
















### FACCIO IL GRAFICO DEI COEFFICIENTI SUL CROMOSOMA DUE ESTRATTI CON IL LASSO
axis(1, at = fused_snp$position, tick=FALSE,las=2)
coef_lasso_50NA<-as.matrix(coef_lasso_50NA)
coef_lasso_50NA<-as.data.frame(coef_lasso_50NA)
coef_lasso_50NA$rsid<-row.names(coef_lasso_50NA)
save(coef_lasso_50NA,file="coef_lasso_50NA.RData")


#unisco le due matrici per aggiungere ai coefficienti la posizione
lasso_snp<-merge(coef_lasso_50NA,matrice_snp, by='rsid', all.x=TRUE)
lasso_snp<-lasso_snp[-1,]
#plotto i risultati del lasso
lasso_snp_cr2<-lasso_snp[lasso_snp$chromosome==2,]
p_lasso_cr2<-which(lasso_snp_cr2$s0!=0)
options("scipen"=100, "digits"=4)
plot(lasso_snp_cr2$position, lasso_snp_cr2$s0, type='h', las=2,
     xlab="Posizione sul cromosoma 2",ylab="Coefficiente lasso", cex.axis=0.54)


prop.table(table(coded[,2],coded[,3]),2)



### FACCIO IL GRAFICO DEI COEFFICIENTI SUL CROMOSOMA DUE ESTRATTI CON ELASTIC NET
coef_elastic_50NA<-as.matrix(coef_elastic_50NA)
coef_elastic_50NA<-as.data.frame(coef_elastic_50NA)
coef_elastic_50NA$rsid<-row.names(coef_elastic_50NA)
save(coef_elastic_50NA,file="coef_elastic_50NA.RData")


#unisco le due matrici per aggiungere ai coefficienti la posizione
elastic_snp<-merge(coef_elastic_50NA,matrice_snp, by='rsid', all.x=TRUE)
elastic_snp<-elastic_snp[-1,]
elastic_snp_cr2<-elastic_snp[elastic_snp$chromosome==2,]
save(elastic_snp_cr2,file="elastic_snp_cr2.RData")

plot(elastic_snp_cr2$position, elastic_snp_cr2$s0, type='h', las=2,
     xlab="Posizione sul cromosoma 2",ylab="Coefficiente elastic net", cex.axis=0.54)

elastic_snp_cr2[order(elastic_snp_cr2$s0),]
#più grande negativo: rs1548632 -0.152973
#più grande positivo: rs7577996  0.334095
                    # rs7557776  0.288590


elastic_snp_cr2[elastic_snp_cr2$rsid%in%c("rs10420685","rs11998813"),]
