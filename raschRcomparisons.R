# Rasch analysis script to make comparisons between estimation methods, by magnus.p.johansson@ri.se
# https://orcid.org/0000-0003-1669-592X
# TAM, mirt, eRm, lordif within the R framework, and Winsteps 5.1 as an external reference (JMLE and CMLE)

setwd("C:/Users/magnuspjo/OneDrive - RISE/Dokument/CATcourse/Day 2 (IRT theory and practical)/IRT practical (practical 2)") #set working directory
cesd = read.csv('IRT_cesd.csv')
df <- cesd[,5:24]

## Reverse Scoring CESD items
library("car")
for (i in c(4,8,12,16)) {
  df[,i]<-recode(df[,i],"1=4;2=3;3=2;4=1",as.factor=FALSE)
}
df[,c(2,11,15,16,8)] <- NULL


# load GNI23 dataset, GNI toxicity subscale (7 items, 4 response categories),
# and WAAQ with 7 items and 7 response categories.

setwd("C:/Users/magnuspjo/OneDrive - RISE/Dokument/CATcourse") #set working directory

# load datafile to df and remove all non-item data
df<-read.csv("GNI23data.csv", sep = ";")
names(df)
#run either of the lines below to select one scale for analysis
df[,8:14]<-NULL # remove WAAQ items
df[,1:7]<-NULL # remove GNI toxic subscale items
df<-na.omit(df)
# WAAQ has N = 230, GNItoxic has 582

# some programs (TAM, eRm) needs 0 as the base response category. mirt?
library(car)
for (i in 1:ncol(df)) {
  df[,i]<-recode(df[,i],"1=0;2=1;3=2;4=3;5=4;6=5;7=6",as.factor=F) # adjust number of response options depending on data
}

# descriptives
for (i in 1:nrow(df)) {
  barplot(table(df[,i]), col="lightblue",xlab=names(df[i]),ylab="Number of responses")
}

library(mirt)# Loads `mirt`. Install if necessary.
mirt.rasch <- mirt(df, model=1, itemtype='Rasch') # unidimensional model
#plot(mirt.rasch, type="trace") # get ICCs
mitemfit<-mirt::itemfit(mirt.rasch, fit_stats = 'infit', method = 'ML') # get item fit 
mitemlocation<-cbind(gen.difficulty(mirt.rasch)) # get item locations
#mitemlocation<-cbind(mitemlocation) # transpose to column 
#mitemlocation<-as.data.frame(mirt.itemlocation)
#mitemfit
mirtall<-cbind(mitemfit[,2:5],mitemlocation)
mirtall

library(TAM)
df.m<-as.matrix(df)
df.tam <- tam(df.m, irtmodel = "PCM")
IRT.WrightMap(df.tam,show.thr.lab=TRUE)
#sa_itemfit <- tam.fit(df.tam)
sb_itemfit <- msq.itemfit(df.tam) # this gives item fit values, MNSQ/ZSTD
sb_itemfit$itemfit
tthresh <- round(tam.threshold(df.tam),3) # gives (Thurstonian) thresholds in a useful format
# calculate item mean location, based on thresholds
tthresh<-as.data.frame(tthresh)
tthresh$itemlocation <- round(rowMeans(tthresh[,1:3]),3)
# get all info in the same dataframe
titemfit<-as.data.frame(sb_itemfit$itemfit)
tamall<-cbind(tthresh,titemfit[,3:8])
tamall
delta_tau <- df.tam$item_irt


### eRm
library(eRm) # https://bookdown.org/chua/new_rasch_demo2/PC-model.html
df.erm <- PCM(df) # CML estimation.
#df.erm <- RSM(df) # CML estimation.
### Examine item difficulty values:
item.estimates <- eRm::thresholds(df.erm)
item.estimates
item_difficulty <- item.estimates[["threshtable"]][["1"]]
item_difficulty
item_difficulty<-as.data.frame(item_difficulty)
## Get threshold SEs values:
item.se <- item.estimates$se.thresh
item.se
person.locations.estimate <- person.parameter(df.erm)
summary(person.locations.estimate)
item.fit <- itemfit(person.locations.estimate)
item.fit # view output
item.fit.table <- cbind(item.fit[["i.outfitMSQ"]],item.fit[["i.infitMSQ"]],item.fit[["i.infitMSQ"]],item.fit[["i.infitZ"]])
pfit <- personfit(person.locations.estimate)
person.fit.table <- cbind(pfit[["p.outfitMSQ"]],pfit[["p.outfitMSQ"]],pfit[["p.outfitMSQ"]],pfit[["p.outfitMSQ"]])


### import Winsteps data
df.wsteps<-read.csv("2022-01-19WinstepsGNIntox.csv", sep = ";")
df.wsteps

#compare item locations, using scale() to normalize/center data
#comp.locations<-round(cbind(tamall$itemlocation,mirtall$mitemlocation,item_difficulty$Location),3)
comp.locations<-round(cbind(scale(tamall$itemlocation),scale(mirtall$mitemlocation),
                            scale(item_difficulty$Location),df.wsteps$WinstepsJMLElocation,df.wsteps$WinstepsCMLElocation),3)
comp.locations<-as.data.frame(comp.locations)
names(comp.locations) <- c("TAM", "mirt", "eRm", "JMLE", "CMLE")
comp.locations
write.csv(comp.locations, file = "GNIntoxLocations.csv")

# compare outfit
comp.outfit<-round(cbind(tamall$Outfit,mirtall$outfit,item.fit$i.outfitMSQ,df.wsteps$WinstepsJMLEoutfit,df.wsteps$WinstepsCMLEoutfit),3)
comp.outfit<-as.data.frame(comp.outfit)
names(comp.outfit) <- c("TAM", "mirt", "eRm", "JMLE", "CMLE")
comp.outfit
write.csv(comp.outfit, file = "GNIntoxOutfit.csv")

# compare outfit Z/t
comp.outfitZ<-round(cbind(tamall$Outfit_t,mirtall$z.outfit,item.fit$i.outfitZ,df.wsteps$WinstepsJMLEoutfitZ,df.wsteps$WinstepsCMLEoutfitZ),3)
comp.outfitZ<-as.data.frame(comp.outfit)
names(comp.outfitZ) <- c("TAM", "mirt", "eRm", "JMLE", "CMLE")
comp.outfitZ
write.csv(comp.outfitZ, file = "GNIntoxOutfitZ.csv")

# further calculations to be implemented



# Item fit was evaluated by analyzing the item fit residual statistics using the TAM package [40]. Item
# fit residuals between ±2.5 are deemed to indicate adequate fit to the model [10].
# MJ: quote taken from Robinson et al. (2019), comparing RUMM to R.

# DIF analysis
library("lordif")
df.gni<-read.csv("GNI23data_v1.1.csv", sep = ";")
gender<-df.gni$Sex

# Run Age-related DIF
ageDIF <- lordif(df, gender, criterion="Chisqr", alpha=0.01, minCell=5, model = "GPCM",
                 MonteCarlo = T, nr = 1000)
#print(ageDIF)
#summary(ageDIF)
plot(ageDIF, labels=c("Male", "Female"))

# DIF with eRm:
# https://bookdown.org/chua/new_rasch_demo2/DIF.html#r-lab-2-dif-analysis-with-the-partial-credit-model
subgroup_diffs <- Waldtest(df.erm, splitcr = gender)


# TAM: create EVC's separately for gender DIF visual analysis ?

df.sex<-cbind(df,gender)
names(df.sex)
df.male<-subset(df, gender == 1)
df.female<-subset(df, gender == 2)
df.male$gender <- NULL
df.female$gender <- NULL
df.male<-as.matrix(df.male)
df.female<-as.matrix(df.female)
df.tam.m <- tam(df.male, irtmodel = "PCM")
df.tam.f <- tam(df.female, irtmodel = "PCM")
plot(df.tam, item = 3)
plot(df.tam.m, item = 3)
plot(df.tam.f, item = 3)
# changes slope of curve, making it difficult to compare...


# https://rdrr.io/github/cswells1/MeasInv/ maybe offers a useful option?
#install.packages("devtools")
#devtools::install_github("cswells1/MeasInv") 
library("MeasInv")
df.sex$gender<-factor(df.sex$gender)
df.sex$gender<-car::recode(df.sex$gender,"1=0;2=1")
# Perform Mantel DIF procedure for polytomous item responses #
Mantel.output <- Mantel.poly(data = df.sex[,1:7], group = df.sex$gender,
                             focal.name = 2, ref.name = 1, sig.level = .01, purify = TRUE)
Mantel.output
# Perform GMH DIF procedure for polytomous item responses #
GMH.results <- GMH.poly(data = Likert.data[,1:12], group = Likert.data$group, 
                        sig.level = .05, purify = TRUE)

# Run Monte Carlo simulation to generate cut-off thresholds
#ageMC <- montecarlo(ageDIF, alpha=.01, nr=100) #this may take about an hour
#print(ageMC)
#summary(ageMC)
#plot(ageMC)


### local dependencies, residual correlations
# mirt
resid=residuals(mirt.rasch, type="Q3", digits=2)
diag(resid) <- NA # make the diagonal of correlation matrix NA instead of 1
# use function from ltm package to check for local deps, with p-value adjustment for multiple analysis
library("ltm")
rcor.test(resid, p.adjust = T) # only provides absolute correlations
# if significant correlations exist, check size compared to average:
mean(resid, na.rm=T) # get the mean residual value, ignoring NA
mirt.resid.limit<- mean(resid, na.rm=T) + 0.25 # create variable indicating dynamic cutoff
mirt.resid.limit #check value
Filter(function(x) any(x > mirt.resid.limit), resid) #identify values above cutoff, then find them in matrix
mirt.residualmatrix<-round(resid, 2) #store the corr matrix
mirt.residualmatrix


### unidimensionality check
# mokken scaling
library(mokken)
coefH(df) # H should be above 0.3
aisp(df) # should be 1

# CFA
library(lavaan)
cfa.model <- 'factor1 =~ GNI_1+GNI_2+GNI_3+GNI_4+GNI_5+GNI_6+GNI_21'
cfa.fit.dwls <- cfa(cfa.model, data=df, ordered=names(df)) # DWLS for ordinal data
summary(cfa.fit.dwls, fit.measures=T, standardized=T)
#cfa.fit.ml <- cfa(cfa.model, data = df) # ML for fun comparison
#summary(cfa.fit.ml, fit.measures=T, standardized=T)

# PCA of residuals, guided by https://bookdown.org/chua/new_rasch_demo2/MD-fit.html#conduct-a-pca-of-standardized-residual-correlations
# based on eRm residuals
library(psych)
std.resids <- item.fit$st.res
pca <- pca(std.resids, nfactors = ncol(df), rotate = "none")
pca$values # check eigenvalues, should be below 2.0
# plot if desired
contrasts <- c(pca$values[1], pca$values[2], pca$values[3], pca$values[4], pca$values[5])
plot(contrasts, ylab = "Eigenvalues for Contrasts", xlab = "Contrast Number", main = "Contrasts from PCA of Standardized Residual Correlations")


# response category tresholds
# mirt plot:
plot(mirt.rasch, type="trace", xlim = c(-6,6))
# TAM plot:
plot(df.tam, type = "items")
# add code to generate numerical comparisons for thresholds, scaled to center at 0


# person location comparisons



### ltm estimation; "approximate marginal Maximum Likelihood, using the 
# Gauss-Hermite quadrature rule for the approximation of the required integrals"
# however, ltm can only do Rasch for dichotomous data
require(ltm)
my2pl<-ltm(df~z1)
descript(df)
unidimTest(df)
# from https://statmath.wu.ac.at/research/talks/resources/PresIRT.pdf
#Descriptive Analysis with ltm
#. descript(): descriptive statistics relevant to IRT
#. rcor.test(): pairwise associations
#. biserial.cor(): biserial correlation
#. cronbach.alpha(): calculates Cronbach’s alpha
#. unidimTest(): unidimensionality check
# . item.fit() & person.fit(): item & person fit statistics
