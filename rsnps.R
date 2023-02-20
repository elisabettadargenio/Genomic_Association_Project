library(rsnps)

#Get openSNP genotype data for all users at a particular snp
x <- allgensnp(snp='rs4477212')
tail(x)
?allgensnp #vedi: see also

#rs4477212_nic <- subset(x, x$id%in%user_con_genoma)


#Get all openSNP phenotypes, their variations, and how many users have data available for a given phenotyp
y<-allphenotypes(df=TRUE)

datalist <- allphenotypes()
sort(names(datalist))
datalist[["Nicotine dependence"]]
datalist[["Sex"]] #id=60
data <- users(df=TRUE)
dim(data[[1]])
data[[1]][24:25,]
#sort(data[[1]])

#Vedi con cosa hanno estratto i dati!
data[[1]][order(data[[1]]$genotypes.filetype),][10420:13000,]
table(data[[1]]$genotypes.filetype)

##data[[1]] # users with links to genome data
##data[[2]] # users without links to genome data

#SCARICA GENOTYPE DALL'url DELL'USER
#id=22
#gen_Holston<-fetch_genotypes(url = data[[1]][which(data[[1]]$id==22),"genotypes.download_url"], rows = -1)

genotypes(snp='rs9939609', userid=1)
query<-ncbi_snp_query("rs7537756")
query$ancestral_allele


#Nicotine dependence -> id=20
nicotine<-phenotypes_byid(phenotypeid=20, return_ = 'users')
table(nicotine[,2])
users_nicotine<-nicotine[,1]

user_con_genoma<-intersect((data[[1]]$id), users_nicotine)
sort(user_con_genoma)



##################################### 23andme ################################################
#5078=numero righe data[[1]]

#inizializzo LISTA in cui inserisco i genomi estratti da 23andme
dati23andme <- vector(mode = "list", length = 10443)


#estraggo il numero delle righe dei genomi estratti da 23andme
indici_23andme<-which(data[[1]]$genotypes.filetype=="23andme")
#trovo il numero di id degli user corrispondenti alle righe estratte sopra
id_23andme<-data[[1]][indici_23andme,"id"]
#vedo i valori su cui fare girare il ciclo for
tail(sort(id_23andme))
#questi sono gli id dei soggetti con genoma estratto da 23andme che hanno risposto alla domanda sulla nicotina
sort(intersect(user_con_genoma, id_23andme))

#ESTRAGGO IL GENOMA DI TUTTI GLI USER DI 23ANDME CHE HANNO RISPOSTO ALLA NICOTINA, ne escludo alcuni problematici
for (i in 1:10500) {
  if (i %in% user_con_genoma & i %in% id_23andme & i!=158 & i!=1059 & i!=2322
      & i!=2398 & i!=3085 & i!=3179 & i!=3343 & i!=3371 & i!=3730 & i!=3806
      & i!=4241 & i!=4290 & i!=6613 & i!=6677 & i!=6832 & i!=5211) {
  dati23andme[[i]]<- as.matrix(fetch_genotypes(url = data[[1]][which(data[[1]]$id==i),"genotypes.download_url"], rows = -1))
  }
} #311 USER CON GENOMA



##################################### ancestry #############################################

indici_ancestry<-which(data[[1]]$genotypes.filetype=="ancestry")
id_ancestry<-data[[1]][indici_ancestry,"id"]
sort(id_ancestry) #da 994 a 10442
sort(intersect(user_con_genoma, id_ancestry)) #72 utenti
datiancestry <- vector(mode = "list", length = 10443)


for (i in 1:10444) {
  if (i %in% user_con_genoma & i %in% id_ancestry & i!=3733 & i!=4207 & i!=4224) {
    datiancestry[[i]]<- as.matrix(fetch_genotypes(url = data[[1]][which(data[[1]]$id==i),"genotypes.download_url"], rows = -1))
  }
} #69 USER CON GENOMA (allele1, allele2)
#PROBLEMA: HA due righe di intestazione colonne, quindi il singolo genoma ha come intestazione
#rsid chromosome position genotype <NA>  e come prima riga
#rsid chromosome position allele1 allele2


##################################### ftdna-illumina #############################################

indici_ftdna<-which(data[[1]]$genotypes.filetype=="ftdna-illumina")
id_ftdna<-data[[1]][indici_ftdna,"id"]
sort(id_ftdna) #da 71 a 10428
sort(intersect(user_con_genoma, id_ftdna)) #46 utenti
datiftdna <- vector(mode = "list", length = 10443)


#NON FUNZIONA la funzione fetch_genotypes CON ftdna-illumina, saranno da inserire manualmente i file
#for (i in 1:10443) {
#  if (i %in% user_con_genoma & i %in% id_ftdna & i!=579 & i!=1111 & i!=1491 & i!=2506 & i!=2953
#      & i!=3267 & i!=3357 & i!=3998 & i!=4198 & i!=4360 & i!=4696 & i!=4738 & i!=4917 & i!=4924
#      & i!=5024 & i!=5283 & i!=6874 & i!=7372 & i!=7527 & i!=7616 & i!=8085 & i!=8132 & i!=9429 
#      & i!=9941) {
#    datiftdna[[i]]<- as.matrix(fetch_genotypes(url = data[[1]][which(data[[1]]$id==i),"genotypes.download_url"], rows = -1))
#  }
#} 

#22 USER DISPONIBILI MA DA INSERIRE A MANO



##################################### genes-for-good #############################################
#NO DATA DISPONIBILE



##################################### IYG #############################################
indici_IYG<-which(data[[1]]$genotypes.filetype=="IYG")
id_IYG<-data[[1]][indici_IYG,"id"]
sort(intersect(user_con_genoma, id_IYG)) #822 e 835
#non hanno intestazione colonne, vanno inseriti manualmente


##################################### decodeme #############################################
#NO DATA DISPONIBILE



library(tidyverse)
prova<-vector()


#CREO vettore con nomi snp
for (i in 1:10408){
  if (is.null(dati23andme[[i]])==FALSE) {
  prova<-c(prova,dati23andme[[i]][,1])
  prova<-unique(prova)
  }
}



#creo matrice vuota con intestazione colonne=nomi snp
matricione<- matrix(nrow = 311, ncol = length(prova))
#assegno alle colonne i nomi degli snp
colnames(matricione)<-prova
matricione[1:5,1:8]

matricione<-as.data.frame(matricione)
     

######################## MATRICI DI TEST #############################  
f<-matricione[1:5,1:8]
a<-vector(mode = "list", length = 6)

b<-matrix(nrow=3, ncol=4)
c<-matrix(nrow=4, ncol=4)
colnames(b)<-c('rsid','chromosome', 'position','genotype')
colnames(c)<-c('rsid','chromosome', 'position','genotype')
b[1,]<- c( "rs4477212",  "1", "72017" ,"AA")
b[2,]<- c("rs3094315"  ,"1","742429" ,"AG")
b[3,]<- c("rs3131972" , "1"  ,"742584", "AG")
c[1,]<-c("rs4477212" , "1","72017", "AA")
c[2,]<-c("rs3094315"  ,"1","742429", "AA" )
c[3,]<-c("rs3131972" , "1","742584" ,"GG")
c[4,]<-c("rs12124819", "1","766409", "AA")


a[[2]]<-b
a[[4]]<-c


h=0
for (i in 1:6) {
  if (is.null(a[[i]])==FALSE) {
    h=h+1
    for (j in 1:dim(a[[i]])[1]){ #j scorre sulle righe della singola matrice
      for (k in 1:length(colnames(f))){ #k scorre i nomi degli snp
        if (a[[i]][j,1]==colnames(f)[k]){
          f[h,1]<-i
          f[h,k]<-a[[i]][j,4]
          break
        }
      }
      
    }
    
  }
}


########################################################################


#aggiungo la colonna per l'user id
matricione<-cbind(user_id=NA,matricione)
a<-dati23andme[[8]]
a<-as.data.frame(a)
str(a)
dati23andme[[8]][1,4]
dati23andme[[8]][1,1]==colnames(matricione)[1]
h=0
for (i in 7:8) {
  if (is.null(dati23andme[[i]])==FALSE) {
    h=h+1
        for (j in 1:dim(dati23andme[[i]])[1]){ #j scorre sulle righe della singola matrice
      for (k in 1:length(colnames(matricione))){ #k scorre i nomi degli snp
        if (dati23andme[[i]][j,1]==colnames(matricione)[k]){
          matricione[h,1]<-i
          matricione[h,k]<-dati23andme[[i]][j,4]
          break
        }
      }
      
    }

  }
}







###################################################################
at<-vector(mode = "list")
for (i in 1:11){
  if (is.null(dati23andme[[i]])==FALSE){
    at[[i]]<- t(dati23andme[[i]]) 
    colnames(at[[i]])<- at[[i]][1,]
    at[[i]]<- cbind(user_id=i,at[[i]])
    at[[i]]<- at[[i]][4,]
  }
}
at[[8]]
library(dplyr)
dplyr::bind_rows(at[[8]], at[[11]])
matrice<- at[[1]]
for (i in 2:6) {
  if (is.null(a[[i]])==FALSE){
    matrice <- dplyr::bind_rows(matrice,at[[i]])
  }
}


