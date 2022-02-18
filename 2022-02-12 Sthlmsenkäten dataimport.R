# # Stockholmsenkäten dataimport 2022, magnus.p.johansson@ri.se

# SPSS data file from Stockholm Stad
library(foreign)
library(car)
library(readxl)
library(labelled)

setwd("C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022")
df <- read.spss("data/SE 2014-2020 KI Leifman.sav", to.data.frame = TRUE)
#df <- read.spss("data/2006-2012 Stockholmsenkäten 201126.sav", to.data.frame = TRUE)

# we have data from 2006 to 2020, every 2 years. Starting with 2014 as calibration dataset
df <- subset(df, ar == "2014") # N = 12540, 372 variables

# MJ note: it should be possible to export scaled (0-100 or similar) theta/SE values for all individuals for each year,
# then calculating average and confidence interval for all (to maximize parameter precision), THEN
# subsetting the different municipalities 

#replace all 99* codes with missing (NA)
for (i in 1:ncol(df)) {
  df[,i]<-car::recode(df[,i],"990:999=NA",as.factor=FALSE)
}

# verification sample should be representative of all years, sample 500 random individuals from each year
# first subset df.year == YYYY, then df.if<-rbind(df.year[sample(1:nrow(df.year), 500))
df.all <- read.spss("data/SE 2014-2020 KI Leifman.sav", to.data.frame = TRUE)
df.0612 <- read.spss("data/2006-2012 Stockholmsenkäten 201126.sav", to.data.frame = TRUE)
df.all<-rbind(df.all,df.0612)
# number to sample:
nsam<-500
for (i in c(2006,2008,2010,2012,2014,2016,2018,2020)) {
  df.year<-subset(df.all, ar == i)
  df.if<-rbind(df.if,df.year[sample(1:nrow(df.year), nsam, replace = F),])
}
summary(df.if$ar)
#replace all 99* codes with missing (NA)
for (i in 1:ncol(df.if)) {
  df.if[,i]<-car::recode(df.if[,i],"990:999=NA",as.factor=FALSE)
}
write.csv(df.if, file = "data/randomsampleAllYears.csv", row.names = F)
#replace all 99* codes with missing (NA)
for (i in 1:ncol(df.all)) {
  df.all[,i]<-car::recode(df.all[,i],"990:999=NA",as.factor=FALSE)
}
df.all<-subset(df.all[c(1,8,10,11,152:169,191:211,212:213,289:300,302:313)])
write.csv(df.all, file = "data/AllIndices.csv", row.names = F)
df<-df.all

# check this page for CTT code: https://eiko-fried.com/creating-basic-psychometric-summaries-in-r/ 
# and also the lavaan parts of the IRT/CAT/Concerto course

#write.csv(df, file = "Sthlm2014.csv", row.names=FALSE, na = "NA")

# vector for variables årtal, SkolSDO, årskurs, kön 
# AND variables included in indices for skola, IF, framtid, 
# psykiska/psykosomatiska besvär, trygghet/bostadsområde, 69 variables in all
##c(1,8,10,11,152:169,191:211,212:213,289:300,302:313)
#df<-subset(df[c(1,8,10,11,152:169,191:211,212:213,289:300,302:313)])

# om vi vill reversera så att hög poäng = låg risk i stället (ta bort #-tecknet på de tre raderna)
#for (i in 289:300) {
#  df[,i]<-recode(df[,i],"1=5;2=4;3=3;4=2;5=1",as.factor=FALSE)
#}

df<-read.csv("data/CompleteSample.csv")
df<-as.data.frame(df)
###### Individfaktorer

# IF

# koda om svaren för items som ingår i individfaktorer, F66a-F66u i data, fråga 67 i PDF
# variabler df[191:211]
# defininera svarskategorierna för att förenkla recode-koden
smd<-'Stämmer mycket dåligt'
sgd<-'Stämmer ganska dåligt'
sgb<-'Stämmer ganska bra'
smb<-'Stämmer mycket bra'
# låg poäng = låg risk 
#df$f66a<-recode(df$f66a,"smb=0;sgb=1;sgd=2;smd=3",as.factor=FALSE)
#a-g samma?
for (i in 191:197) {
  df[,i]<-car::recode(df[,i],"smb=0;sgb=1;sgd=2;smd=3",as.factor=FALSE)
}
#  h och p och u reverserade
df$f66h<-car::recode(df$f66h,"smb=3;sgb=2;sgd=1;smd=0",as.factor=FALSE)
for (i in 199:205) {
  df[,i]<-car::recode(df[,i],"smb=0;sgb=1;sgd=2;smd=3",as.factor=FALSE)
}
df$f66p<-car::recode(df$f66p,"smb=3;sgb=2;sgd=1;smd=0",as.factor=FALSE)
for (i in 207:210) {
  df[,i]<-car::recode(df[,i],"smb=0;sgb=1;sgd=2;smd=3",as.factor=FALSE)
}
df$f66u<-car::recode(df$f66u,"smb=3;sgb=2;sgd=1;smd=0",as.factor=FALSE)


df.all <- df # keep everything in df.all, we will need DIF variables, etc, separately from the item data

# for export to external analysis in Winsteps etc:
#df <- df.all[c(8,10,11,191:211)]
# change missing data (NA) to 999 for Mplus before export or blank for Winsteps
#write.csv(df, file = "data/IF2014.csv", row.names=FALSE, na = "")

# remove non-item data, then go to analysis script (separate for each index)
df <- df.all[191:211]
# setup item descriptions
itemlabels<-read.csv("data/IF_item_labels.csv")
#itemlabels <- read_excel("IFitems.xls")
var_label(df) <- itemlabels$items # attach labels/descriptions to each variable
#itemlabels<-as.data.frame(itemlabels)
#itemlabels$itemnr<-names(df)
#write.csv(itemlabels, file="IF item labels.csv", row.names = F)
#rvals<-labelled(c(0,1,2,3), c("Stämmer mycket dåligt"=0, "Stämmer ganska dåligt"=1, "Stämmer ganska bra"=2, "Stämmer mycket bra"=3))
#val_labels(rvals)
#val_labels(df) <- rvals
# any further data manipulation, such as merged response categories, is found in analysis file
df$f66t<-NULL
df$f66b<-NULL
df$f66l<-NULL # remove items
df$f66p<-NULL
df$f66u<-NULL
df$f66e<-NULL # remove items
df$f66h<-NULL
df$f66m<-NULL

df<-na.omit(df) # N=9756 complete



###############

# koda om svaren för items som ingår i psykiska/psykosomatiska besvär, F88-F99 i data, fråga 90-101 i PDF
# variabler df[289:300]
# låg poäng = låg risk 
df$F88<-recode(df$F88,"'Aldrig'=1;'Ungefär 1 gång/termin'=2;'Ungefär 1 gång/månad'=3;'Ungefär 1 gång/vecka'=4;'Flera gånger i veckan'=5",as.factor=FALSE)
df$F89<-recode(df$F89,"'Väldigt ofta'=5;'Ganska ofta'=4;'Ibland'=3;'Någon enstaka gång'=2;'Sällan'=1",as.factor=FALSE)
df$F90<-recode(df$F90,"'Sällan'=1;'Någon enstaka gång'=2;'Ibland'=3;'Ganska ofta'=4;'Väldigt ofta'=5",as.factor=FALSE)
df$F91<-recode(df$F91,"'Aldrig'=1;'Ungefär 1 gång/termin'=2;'Ungefär 1 gång/månad'=3;'Ungefär 1 gång/vecka'=4;'Flera gånger i veckan'=5",as.factor=FALSE)
df$F92<-recode(df$F92,"'Inte alls'=1;'Ganska lite'=2;'En del'=3;'Ganska mycket'=4;'Väldigt mycket'=5",as.factor=FALSE)
df$F93<-recode(df$F93,"'Aldrig'=1;'Ungefär 1 gång/termin'=2;'Ungefär 1 gång/månad'=3;'Ungefär 1 gång/vecka'=4;'Flera gånger i veckan'=5",as.factor=FALSE)
df$F94<-recode(df$F94,"'Väldigt ofta'=5;'Ganska ofta'=4;'Ibland'=3;'Någon enstaka gång'=2;'Nästan aldrig'=1",as.factor=FALSE)
df$F95<-recode(df$F95,"'Aldrig'=1;'Ungefär 1 kväll/termin'=2;'Ungefär 1 kväll/månad'=3;'Ungefär 1 kväll/vecka'=4;'Flera gånger i veckan'=5",as.factor=FALSE)
df$F96<-recode(df$F96,"'Nästan aldrig'=5;'Någon enstaka gång'=4;'Ibland'=3;'Ganska ofta'=2;'Oftast'=1",as.factor=FALSE)
df$F97<-recode(df$F97,"'Sällan'=1;'Någon enstaka gång'=2;'Ibland'=3;'Ganska ofta'=4;'Väldigt ofta'=5",as.factor=FALSE)
df$F98<-recode(df$F98,"'Aldrig'=1;'Ungefär 1 natt/termin'=2;'Ungefär 1 natt/månad'=3;'Ungefär 1 natt/vecka'=4;'Flera nätter i veckan'=5",as.factor=FALSE)
df$F99<-recode(df$F99,"'Sällan'=5;'Någon enstaka gång'=4;'Ibland'=3;'Ganska ofta'=2;'Väldigt ofta'=1",as.factor=FALSE)

# flytta ankare från 1 till 0 för lägsta kategori, behövs för vissa Rasch-program
for (i in 289:300) {
  df[,i]<-recode(df[,i],"1=0;2=1;3=2;4=3;5=4",as.factor=FALSE)
}

# fixa barplots för svarsfrekvenser
pdf("Plots/PSF12svarsfrekvenser.pdf")
for (i in 289:300) {
  barplot(table(df[,i]), col="lightblue",xlab=names(df[i]),ylab="Antal svar")
}
dev.off()



###########

# Skola
# f54 a-r (152:169) 
# 
# defininera svarskategorierna för att förenkla recode-koden (samma som IF-frågorna)
smd<-'Stämmer mycket dåligt'
sgd<-'Stämmer ganska dåligt'
sgb<-'Stämmer ganska bra'
smb<-'Stämmer mycket bra'
# låg poäng = låg risk 

# smd = 0 för f54e, f, i, k, m, o, q = 8 items, c(156:157,160,162,164,166,168)
# smd = 3 för f54a, b, c, d, g, h, j, l, n, p, r, = 10 items, c(152:155,158:159,161,163,165,167,169)

for (i in c(152:155,158:159,161,163,165,167,169)) {
  df[,i]<-recode(df[,i],"smb=0;sgb=1;sgd=2;smd=3",as.factor=FALSE)
}

for (i in c(156:157,160,162,164,166,168)) {
  df[,i]<-recode(df[,i],"smb=3;sgb=2;sgd=1;smd=0",as.factor=FALSE)
}

# exportera barplots för svarsfrekvenser
#pdf("Plots/SkolaSvarsfrekvenser.pdf")
#for (i in 152:169) {
#  barplot(table(df[,i]), col="lightblue",xlab=names(df[i]),ylab="Antal svar")
#}
#dev.off()

# exportera årtal (kolumn 1) och delskalan för vidare analys i annat program
df.skola <- df[c(152:169)]
# change missing data (NA) to 999 for Mplus before export or blank for Winsteps
#write.csv(df.skola, file = "skola2014-2020.csv", row.names=FALSE, na = "")





# resten av "skolrelaterat": , df$F56 (frånvaro), samt F65 a-c(188:190)betyg, ev. mobbning (f60 a-i?), 175:183
# ej fixade ännu





##############

#  Trygghet/bostadsområde, 102 (F100) och 103, items 302:313
# F100 egen fråga, f101a till l med fyra steg + "vet inte"
smd<-'Stämmer mycket dåligt'
sgd<-'Stämmer ganska dåligt'
sgb<-'Stämmer ganska bra'
smb<-'Stämmer mycket bra'
vetej<-'Vet inte'
# låg poäng = låg risk 

df$F100<-recode(df$F100,"'Mycket trygg'=0;'Ganska trygg'=1;'Ganska otrygg'=2;'Mycket otrygg'=3",as.factor=FALSE)
df$F100<-recode(df$F100,"'Går ej ut på kvällen av oro för att utsättas för brott'=4",as.factor=FALSE)
df$F100<-recode(df$F100,"'Går ej ut på kvällen av andra orsaker'=NA",as.factor=FALSE)
#barplot(table(df$F100), col="lightblue",xlab=names(df$F100),ylab="Antal svar")

# smd = 0 för f101a,def -- c(302,305:307)
# nedan reverserade
# smd = 3 för f101bc,ghijkl -- c(303:304,308:313)

for (i in c(302,305:307)) {
  df[,i]<-recode(df[,i],"smb=3;sgb=2;sgd=1;smd=0;vetej=NA",as.factor=FALSE)
}

for (i in c(303:304,308:313)) {
  df[,i]<-recode(df[,i],"smb=0;sgb=1;sgd=2;smd=3;vetej=NA",as.factor=FALSE)
}


#########

# framtidsfrågor, 68-69, heter f67 och f68 i data, variabler 212:213

df$F67<-recode(df$F67,"'Är mycket viktigt'=0;'Är ganska viktigt'=1;'Är varken viktigt eller oviktigt'=2;'Spelar nästan ingen roll'=3;'Spelar ingen roll alls'=4",as.factor=FALSE)
df$F68<-recode(df$F68,"'Mycket bättre'=0;'Lite bättre'=1;'Lika bra'=2;'Lite sämre'=3;'Mycket sämre'=4",as.factor=FALSE)

###############
# Kamrater, fråga 88 i PDF. 10 frågor (F86f "Är duktiga i skolan?"), df[278:287]
# 5 svarsalternativ
smd<-'Ingen'
sgd<-'Någon enstaka'
sgb<-'Ungefär hälften'
smb<-'De flesta'
vetej<-'Vet inte'

# smd = 0 för f86b,de,ghij -- c(279,281:282,284:287)
# nedan reverserade
# smd = 3 för f86acf -- c(278,280,283)

for (i in c(279,281:282,284:287)) {
  df[,i]<-recode(df[,i],"smb=3;sgb=2;sgd=1;smd=0;vetej=NA",as.factor=FALSE)
}

for (i in c(278,280,283)) {
  df[,i]<-recode(df[,i],"smb=0;sgb=1;sgd=2;smd=3;vetej=NA",as.factor=FALSE)
}
# exportera barplots för svarsfrekvenser
pdf("Plots/KamraterSvarsfrekvenser.pdf")
for (i in 278:287) {
  barplot(table(df[,i]), col="lightblue",xlab=names(df[i]),ylab="Antal svar")
}
dev.off()

# Problematiska kamratrelationer: f86b+f86d+f86e+f86g+f86g+f86i+f86h+f86j, sumscore
df$ProblKamrat<-rowSums(df[c(279,281:282,284:287)], na.rm = TRUE)


#################
# Brott, begått: f75a-s, 278:287

# brott, utsatt för: f80a-e, names(df[244:263])
#[1] "f78aa" "F78ab" "f78ac" "f78ad" "f78ba" "F78bb" "f78bc" "f78bd" "f78ca" "F78cb" "f78cc" "f78cd" "f78da"
#[14] "F78db" "f78dc" "f78dd" "f78ea" "F78eb" "f78ec" "f78ed"
# svarsalternativ
#f78aa = 'Nej', 'Ja, antal gånger'
#f78ab = siffra för antal gånger (0 recodas till NA)
#f78ac = 'Nej', 'Ja, antal gånger'
#f78ad = siffra för antal gånger (0 recodas till NA)

#pdf("Plots/brott-histogram.pdf")
#for (i in c(244:263)) {
#  barplot(table(df[i]), col="lightblue",xlab=names(df[i]),ylab="Antal svar")
#}
#dev.off()

# utsatt för brott ja/nej, c(244,248,252,256,260)
for (i in c(244,248,252,256,260)) {
  df[,i]<-recode(df[,i],"'Nej'=0;'Ja, antal gånger'=1",as.factor=FALSE)
}

# antal gånger-frågorna, *b och *d, 0 blir NA. c(245,247,249,251,253,255,257,259,261,263)
for (i in c(245,247,249,251,253,255,257,259,261,263)) {
  df[,i]<-recode(df[,i],"0=NA",as.factor=FALSE)
}

# anmält till polis, c(246,250,254,258,262)
for (i in c(246,250,254,258,262)) {
  df[,i]<-recode(df[,i],"'Nej'=0;'Ja, antal gånger'=1",as.factor=FALSE)
}

# utsatt för någon typ av brott, områden jämförda
#pdf("Plots/brott-histogram2.pdf")
#for (i in c(244,248,252,256,260)) {
#  barplot(table(df[i]), col="lightblue",xlab=names(df[i]),ylab="Antal svar")
#}
#dev.off()

#####################


## fritidsfrågor
# lärarledd fritidsaktivitet, 71 i PDF, f70 i data
# Brukar du delta i någon ledarledd fritidsaktivitet eller träning?
df$F70<-recode(df$F70,"'Ofta'=0;'Ibland'=1;'Sällan'=2;'Aldrig'=3",as.factor=FALSE)

# "Grad av prosocialt index", åk2, delat på stadsdelsområde och kön, 2020
# från "Figyr 11:13 som Lovisa skickade via chat
df.prosoc <- df[c(1,8,10,11,215,278,280,283)]
df.prosoc2020 <- subset(df.prosoc, ar==2020)


# vektor för årtal, SkolSDO, kön, skola, IF, framtid, psykiska/psykosomatiska besvär, trygghet/bostadsområde
##c(1,8,11,152:169,191:211,212:213,289:300,302:313)
df.tmp<-df[c(1,8,11,152:169,191:211,212:213,244:263,278:287,289:300,302:313)]
# subset a particular municipality for Collective Impact Husby analysis
#sub.data<-subset(df.tmp, SkolSDO=="Rinkeby-Kista")
# if needed, remove persons with 100% missing data
#sub.data <- na.omit(sub.data)
# export subset to CSV-file
write.csv(df.prosoc2020, file = "2021-12-08_RK_prosoc.csv", row.names=FALSE, na = "")




############


# Rasch-analys med R, kräver dataframe med enbart items
# subset av enbart 2014-enkäten och PSF-items
df.2014<-subset(df, ar==2014)
df.psf <- df[289:300]
df.skola <- df.2014[152:169]
# ta bort case som har 100% missing data:
df.psf <- filter(df.psf, rowSums(is.na(df.psf)) != ncol(df.psf))
#ladda bibliotek för Rasch/IRT
library(TAM)
library(WrightMap)

# if needed, remove persons with 100% missing data
df.skola <- na.omit(df.skola)
# run Rasch Rating Scale Model and put output into new variable
matrix.sa <- as.matrix(df.psf)
matrix.sa <- as.matrix(df.skola)
sa_model1 <- tam(matrix.sa, irtmodel = "PCM") #RSM also possible
sa_model1 <- tam.jml(matrix.sa)
#sa_model1 <- tam(matrix.sa)
# look at item parameters
sa_model1$xsi
# visualize distribution of item difficulties (not working)
#hist(sa_model1$xsi, breaks=10)
# get Thurstonian thresholds 
#sa_thresh <- tam.threshold(sa_model1)
# plot wright map
#IRT.WrightMap(sa_model1,show.thr.lab=TRUE)
# person ability (theta) estimates
sa_ability <- tam.wle(sa_model1)
person.ability <- sa_ability$theta
head(sa_ability)
# item fit statistics
sa_itemfit <- tam.fit(sa_model1)
# show in table
library(knitr)
kable(sa_itemfit$itemfit)

# plot expected score curves
pdf("Plots/psfALL-esc.pdf")
plot(sa_model1, export = F)
dev.off()

# plot item characteristic curves
#plot(sa_model1, type ="items")
pdf("Plots/psfALL-icc.pdf")
#plot(sa_model1, type = "items", export = F, high = 5, items = c(1:12))
plot(sa_model1, type = "items", export = F)
dev.off()

#write.csv(df, file = "jamy-geas_rscored.csv", row.names=FALSE)








##### Rasch with eRm, https://bookdown.org/chua/new_rasch_demo2/PC-model.html 
library(eRm)

# run rating scale model
# first remove persons with 100% missing data
df.psfComplete <- na.omit(df.psf)
rownames(df.psfComplete)<-NULL

sa_model2 <- RSM(df.psf)
sa_model2 <- PCM(df.skola)

plotPImap(sa_model2)

pdf("Plots/skola-iccPCM.pdf")
plotICC(sa_model2, ask = FALSE)
dev.off()

### Examine item difficulty values:
item.estimates <- thresholds(sa_model2)
item.estimates
item_difficulty <- item.estimates[["threshtable"]][["1"]]
item_difficulty

person.locations.estimate <- person.parameter(sa_model2)
item.fit <- itemfit(person.locations.estimate)
item.fit # view output
pfit <- personfit(person.locations.estimate)

### start code to plot std residuals
stresid <- item.fit$st.res
# before constructing the plots, find the max & min residuals:
max.resid <- ceiling(max(stresid))
min.resid <- ceiling(min(stresid))

for(item.number in 1:ncol(stresid)){
  plot(stresid[, item.number], ylim = c(min.resid, max.resid),
       main = paste("Standardized Residuals for Item ", item.number, sep = ""),
       ylab = "Standardized Residual", xlab = "Person Index")
  abline(h = 0, col = "blue")
  abline(h=2, lty = 2, col = "red")
  abline(h=-2, lty = 2, col = "red")
  legend("topright", c("Std. Residual", "Observed = Expected", "+/- 2 SD"), pch = c(1, NA, NA), 
         lty = c(NA, 1, 2),
         col = c("black", "blue", "red"), cex = .8)
}
### end code to plot residuals



df$F89<-recode(df$F89,"''=1;''=2;''=3;''=4;''=5",as.factor=FALSE)


#recode one sample variable with dichotomous response scale
df$pf82<-recode(df$F82,"'Ja'=1;'Nej'=0;990=NA;'Vet inte'=NA;'Ej svar'=NA",as.factor=FALSE)
library(skimr)
skim(df$pf82)
# reverse score 'inkonsekvens' parent items
#f83b
#f83d
#f83f




# network analysis tests for risk and protective factors literature review

library(psych)
library(psychometric)

library(plyr)
library(car)
library(ggplot2)
library(reshape2)
library(skimr)
library(Rfast)
library(lattice)
library(TAM)
library(WrightMap)
library(tidyverse)
library(knitr)
library(eRm)
library(readxl)
library(Hmisc)
########################
# probably the only libraries needed for network analysis, the rest are loaded later
library(dplyr)
library(tidyverse)
library(rlang)
setwd("C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/LST KI Leifman/Data2")
rsfaktorer <- file.choose()

rs.df <- read.table(rsfaktorer, encoding="iso-8859-1", header = TRUE, sep = ";")

# borrowed code below from https://www.jessesadler.com/post/network-analysis-with-r/

sources <- rs.df %>%
  distinct(Riskfaktor)  %>%
  dplyr::rename(label = Riskfaktor)

destinations <- rs.df %>%
  distinct(Utfall)  %>%
  dplyr::rename(label = Utfall)

nodes <- full_join(sources, destinations, by = "label")

nodes <- nodes %>% rowid_to_column("id")
                   
per_route <- rs.df %>%
  group_by(Riskfaktor, Utfall) %>%
  dplyr::summarise(count = n()) %>%
  ungroup()
                   
edges <- per_route %>%
  left_join(nodes, by = c("Riskfaktor" = "label")) %>%
  dplyr::rename(from = id)
                   
edges <- edges %>%
  left_join(nodes, by = c("Utfall" = "label")) %>%
  dplyr::rename(to = id)
                   
edges <- select(edges, from, to, count)

# another visualization option
library(tidygraph)
library(ggraph)

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
routes_igraph_tidy <- as_tbl_graph(routes_igraph)

ggraph(routes_tidy, layout = "nicely") +
  geom_node_point(colour = "red") +
  geom_edge_fan(aes(colour = from, 
                    edge_width = count)) +
  geom_edge_link(aes(width = count), alpha = 0.8, arrow = NULL, 
                 label_alpha = 0.8, edge_colour = "green") +
  geom_node_text(aes(label = label), repel = TRUE) +
  scale_edge_colour_gradient(low = "blue", high = "red") +
  labs(edge_width = "Antal") +
  theme_graph()

##############

# Interactive network graphs with visNetwork and networkD3
library(visNetwork)
library(networkD3)

visNetwork(nodes, edges, width = "100%", height = "900px", main="Risk- och skyddsfaktorer", 
           submain="Kopplade till preventionsstjärnan",
           footer= "Producerad av RISE i samarbete med Länsstyrelsen i Stockholm")

edges <- mutate(edges, width = count + 1)

visNetwork(nodes, edges, width = "100%", height = "900px", main="Risk- och skyddsfaktorer", 
           submain="Kopplade till preventionsstjärnan",
           footer= "Producerad av RISE i samarbete med Länsstyrelsen i Stockholm") %>% 
  visIgraphLayout(layout = "layout_with_fr") %>% 
  visEdges(arrows = "middle")

## networkD3

nodes_d3 <- mutate(nodes, id = id - 1)
edges_d3 <- mutate(edges, from = from - 1, to = to - 1)

forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
             NodeID = "label", Group = "id", Value = "count", legend = F,
             opacity = 1, fontSize = 16, zoom = TRUE, width = 1280, height = 960,
             arrows = T)

sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
              NodeID = "label", Value = "count", fontSize = 16, unit = "Antal")


## recommended link: https://kateto.net/network-visualization 
# https://kateto.net/sunbelt2021#interactive-network-visualizations

### below is mostly test things

# the "network" stuff can probably just be removed
library(network)
routes_network <- network(edges, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)
summary(routes_network)
# The plot() function with a network object uses the Fruchterman and Reingold 
# algorithm to decide on the placement of the nodes. You can change the layout 
# algorithm with the mode argument. 
plot(routes_network, vertex.cex = 3)
detach(package:network)
rm(routes_network)

# igraph is one visualization option
library(igraph)

routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

plot(routes_igraph, edge.arrow.size = 0.2)
plot(routes_igraph, layout = layout_nicely, edge.arrow.size = 0.2)

### to test various layouts, from https://kateto.net/netscix2016.html 
layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] 

# Remove layouts that do not apply to our graph.
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]

par(mfrow=c(3,3), mar=c(1,1,1,1))
for (layout in layouts) {
  print(layout)
  l <- do.call(layout, list(routes_igraph)) 
  plot(routes_igraph, edge.arrow.mode=0, layout=l, main=layout) 
  }
dev.off()  
###

# another visualization option
library(tidygraph)
library(ggraph)

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
routes_igraph_tidy <- as_tbl_graph(routes_igraph)

#test ggraph
ggraph(routes_tidy) + geom_edge_link() + geom_node_point() + theme_graph()

ggraph(routes_tidy, layout = "nicely") +
  geom_node_point(colour = "red") +
  geom_edge_fan(aes(colour = from, 
                    edge_width = count)) +
  geom_edge_link(aes(width = count), alpha = 0.8, arrow = NULL, 
                 label_alpha = 0.8, edge_colour = "green") +
  geom_node_text(aes(label = label), repel = TRUE) +
  scale_edge_colour_gradient(low = "blue", high = "red") +
  labs(edge_width = "Antal") +
  theme_graph()

#repel = true makes labels not overlap
ggraph(routes_tidy, layout = "nicely") + 
  geom_node_point() +
  geom_edge_link(aes(width = count), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Antal") +
  theme_graph()
