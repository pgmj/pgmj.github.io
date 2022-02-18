# Stockholmsenkäten analysmall 2022, magnus.p.johansson@ri.se

# Struktur där varje punkt itereras tills acceptabla resultat uppnås. 
# Senare delar kan resultera i item-reduktion, varav hela processen tas om från början
# Kod kopieras till att återkomma när så behövs, för att det ska gå att följa filen kronologiskt
# •	dimensionalitet och residualkorrelationer
# •	hur svarskategorierna fungerar och ev. behov av förändringar
# •	hur väl frågorna fungerar i termer av ”item fit” till Rasch-modellen
# •	hur frågornas ”svårighetsgrad” matchar mot respondenternas egenskaper
# •	reliabilitet (över hela indexet, ”test information function”)
# •	invarians – att frågorna fungerar lika mellan kön och mättillfällen (och ev. mellan åk 9 och åk 2 på gymnasiet? ej specat i avtalet, men rimligen relevant)
# •	omvandling av summerade rådata till intervalldata med mätosäkerheter på individnivå

# item har etiketter f66a-f66u, fråga 67 i PDF-fil med frågor
# "Hur väl stämmer följande påståenden in på dig som person?" följs av frågorna, alla med samma svar
# 
# smd<-'Stämmer mycket dåligt'
# sgd<-'Stämmer ganska dåligt'
# sgb<-'Stämmer ganska bra'
# smb<-'Stämmer mycket bra'
# f66 h och p och u reverserade
# höga poäng = hög risk

# Deskriptiva data:
# barplots är folk vana att läsa, men de tar mycket plats_
for (i in 1:ncol(df)) {
  barplot(table(df[,i]), col="lightblue",main=names(df[i]),ylab="Antal svar", xlab=(itemlabels[i,]))
}
# code below from https://solomonkurz.netlify.app/post/2021-05-11-yes-you-can-fit-an-exploratory-factor-analysis-with-lavaan/ 
# overall distributions across all items 
library(tidyverse)
df %>% 
  pivot_longer(everything()) %>% 
  count(value) %>% 
  mutate(percent = (100 * n / sum(n)) %>% round(digits = 1))
# tile plot is efficient for comparison overview 
# (remember to change "breaks = 0:n" so that n is equal to max response options)
df %>% 
  pivot_longer(everything()) %>% 
  count(name, value) %>% 
  ggplot(aes(x = value, y = name, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c(expression(italic(n)), limits = c(0, NA)) +
  scale_x_continuous("Response", expand = c(0, 0), breaks = 0:4) +
  ggtitle("Items") +
  theme(axis.text.x = element_text(size = 6))

###
# •	dimensionalitet och residualkorrelationer
###

### unidimensionality, standard PCA
library(psych)
pca <- pca(df, nfactors = 7, rotate = "none")
pca$values # check eigenvalues
pca$loadings
contrasts <- c(pca$values[1], pca$values[2], pca$values[3], pca$values[4], pca$values[5])
plot(contrasts, ylab = "Eigenvalues for Contrasts", xlab = "Contrast Number", main = "Contrasts from PCA of Standardized Residual Correlations")

# PCA of residuals, guided by https://bookdown.org/chua/new_rasch_demo2/MD-fit.html#conduct-a-pca-of-standardized-residual-correlations
# based on eRm residuals
library(eRm)
df.omit.na<-na.omit(df) # eRm does not allow participants with all missing data
df.erm<-PCM(df.omit.na) # run PCM model
# get estimates
item.estimates <- eRm::thresholds(df.erm)
item_difficulty <- item.estimates[["threshtable"]][["1"]]
item_difficulty<-as.data.frame(item_difficulty)
item.se <- item.estimates$se.thresh
person.locations.estimate <- person.parameter(df.erm)
item.fit <- eRm::itemfit(person.locations.estimate)
item.fit
########### item fit indicates worst items as ehm, see code after mokken scaling

std.resids <- item.fit$st.res
# PCA of residuals
pca <- pca(std.resids, nfactors = 3, rotate = "oblimin")
pca$values # check eigenvalues, second should be below 2.0
# plot if desired
contrasts <- c(pca$values[1], pca$values[2], pca$values[3], pca$values[4], pca$values[5])
plot(contrasts, ylab = "Eigenvalues for Contrasts", xlab = "Contrast Number", main = "Contrasts from PCA of Standardized Residual Correlations")
# check loadings
pca$loadings
# make a plot of PC1 loadings and item locations! requires prcomp (instead of pca)
pca.loadings<-as.data.frame(res.pca$rotation)
plot(item_difficulty$Location, pca.loadings$PC1)
# try kmeans and cluster.plot?
pca.clusters<-kmeans(pca.loadings, 3, nstart = 25)
cluster.plot(pca.clusters$centers)
pca.clusters

# testing code, http://sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp
res.pca<-prcomp(std.resids, scale. = T)
library(factoextra)
fviz_eig(res.pca)
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
eig.val <- get_eigenvalue(res.pca)
eig.val
res.var <- get_pca_var(res.pca)
res.pca$rotation
a<-res.pca$rotation
a %>% as.data.frame %>% rownames_to_column %>% 
  select(rowname, PC1, PC2) %>% arrange(desc(PC1^2+PC2^2)) %>% head(10)

# testing code: identifying clusters
resid.clusters<-kmeans(std.resids, 3, nstart = 20)
table(resid.clusters$cluster)

# mokken scaling
library(mokken)
coefH(df.omit.na) # H should be above 0.3
aisp(df.omit.na) # A zero indicates an unscalable item. Items with same integer 
# belong to the same Mokken scale. 
# coefH below 0.3: efhiklmprsu
# according to aisp: ehm
df.mokken<-df.omit.na # temp dataframe
df.mokken$f66e<-NULL # remove items
df.mokken$f66h<-NULL
df.mokken$f66m<-NULL
coefH(df.mokken) # worst are lpu
aisp(df.mokken) # indicates lpu and s
df.mokken$f66l<-NULL
df.mokken$f66p<-NULL
df.mokken$f66u<-NULL
coefH(df.mokken) # worst are ks
aisp(df.mokken) # indicates s
df.mokken$f66s<-NULL
coefH(df.mokken) # ok, total removed are ehm, lpu, s (7 items)
aisp(df.mokken) # ok

mok.iio<-check.iio(df.omit.na)

########### item fit indicates worst items as ehm, remove and re-run eRm model
df$f66e<-NULL # remove items
df$f66h<-NULL
df$f66m<-NULL
df.omit.na<-na.omit(df) # eRm does not allow participants with all missing data
df.erm<-PCM(df.omit.na) # run PCM model
# get estimates
item.estimates <- eRm::thresholds(df.erm)
item_difficulty <- item.estimates[["threshtable"]][["1"]]
item_difficulty<-as.data.frame(item_difficulty)
item.se <- item.estimates$se.thresh
person.locations.estimate <- person.parameter(df.erm)
item.fit <- itemfit(person.locations.estimate)
item.fit 
# worst items are lpu
std.resids <- item.fit$st.res
# PCA on residuals
pca <- pca(std.resids, nfactors = 3, rotate = "oblimin")
pca$values # eigenvalue down to 2.16 now, previously was 2.58
df$f66l<-NULL # remove items
df$f66p<-NULL
df$f66u<-NULL

# new model:
df.omit.na<-na.omit(df) # eRm does not allow participants with all missing data
df.erm<-PCM(df.omit.na) # run PCM model
# get estimates
item.estimates <- eRm::thresholds(df.erm)
item_difficulty <- item.estimates[["threshtable"]][["1"]]
item_difficulty<-as.data.frame(item_difficulty)
item.se <- item.estimates$se.thresh
person.locations.estimate <- person.parameter(df.erm)
item.fit <- eRm::itemfit(person.locations.estimate)
item.fit 
# all items in 0.7-1.3 range MSQ
std.resids <- item.fit$st.res
# PCA on residuals
pca <- pca(std.resids, nfactors = 3, rotate = "oblimin")
pca$values # eigenvalue 1.99

# Winsteps CMLE shows high item fit for ehmpu, and on new analysis also l

# in sum, eRm, mokken and Winsteps all converge on the same unidimensional model
# mokken also wanted s removed, but let's see what the local dependencies give us
write.csv(df, file="IF2014_Omit_ehmplu.csv")

######
# make a nice table from mokken and eRm output, summarizing aisp, H, and  MSQ
mok.h<-coefH(df.omit.na) # H should be above 0.3
mok.a<-aisp(df.omit.na) # A zero indicates an unscalable item. Items with same integer indicates same dimension
mok.hi<-as.numeric(c(mok.h$Hi))
mok.hi
mok.out<-as.data.frame(cbind(mok.a,mok.hi[1:ncol(df.omit.na)]))
mok.out<-cbind(mok.out, item.fit$i.outfitMSQ, item.fit$i.infitMSQ)
colnames(mok.out)<-c("aisp", "coefH", "OutfitMSQ", "InfitMSQ")
mok.out$OutfitMSQ<-round(mok.out$OutfitMSQ,3)
mok.out$InfitMSQ<-round(mok.out$InfitMSQ,3)
mok.out
# formatted table that highlights cutoff values in red
formattable(mok.out, list(
  'coefH' = 
    formatter("span", style = ~ style(color = ifelse(coefH < 0.3, "red", "black"))),
  'OutfitMSQ' = 
    formatter("span", style = ~ style(color = ifelse(OutfitMSQ < 0.7, "red", 
                                                     ifelse(OutfitMSQ > 1.3, "red", "black")))),
  'InfitMSQ' = 
    formatter("span", style = ~ style(color = ifelse(InfitMSQ < 0.7, "red", 
                                                     ifelse(InfitMSQ > 1.3, "red", "black"))))
    ))


# For fun, let's run a CFA model and look at model fit as well as residual correlations
# CFA
library(lavaan)
cfa.model <- 'factor1 =~ f66a +f66b +f66c +f66d +f66f +f66g +f66i +f66j +f66k +f66n +f66o +f66q+f66r +f66s +f66t'
cfa.fit.dwls <- cfa(cfa.model, data=df, ordered=names(df)) # DWLS for ordinal data
summary(cfa.fit.dwls, fit.measures=T, standardized=T)
# show the 10 highest mod indices (unidimensional model will only have residual correlations, no crossloadings)
cfa.correlations<-modindices(cfa.fit.dwls) %>%
  arrange(desc(mi)) %>%
  head(10)

# let's get Rasch residual correlations - local dependencies
# we need mirt
library(mirt)
mirt.rasch <- mirt(df.omit.na, model=1, itemtype='Rasch') # unidimensional Rasch model
resid=residuals(mirt.rasch, type="Q3", digits=2)
diag(resid) <- NA # make the diagonal of correlation matrix NA instead of 1
dyn.cutoff<-mean(resid, na.rm=T) + 0.3 # create variable indicating dynamic cutoff at 0.3 above average
resid<-as.data.frame(resid)
for (i in 1:ncol(resid)) {
  resid[,i]<-round(resid[,i],2)
}
library(formattable)
# now get formattable to highlight relevant correlations according to dynamic cutoff:
sign_formatter <- formatter("span", style = x ~ style(color = ifelse(x > dyn.cutoff, "red", "black")))
# color_tile would be even better looking, but this will do for now
resid[upper.tri(resid)]<-NA # remove duplicate values in upper triangle
formattable(resid, list(area(col = 1:ncol(df.omit.na)) ~ sign_formatter))
# below just gives us toned background tiles depending on value
formattable(resid, list(area(col = 1:ncol(df.omit.na)) ~ color_tile("lightblue", "lightpink")))

formattable(itemlabels, list(area(row = c(5,8,13)) ~ color_tile("lightblue", "lightpink")))


### add a section with more data to make decisions on residual correlation pairs
# kolla item fit och location
item.fit
item_difficulty
plotPImap(df.erm)
# och mokken IIO output!

# ta bort nedan?
# use function from ltm package to check for local deps, with p-value adjustment for multiple analysis
library("ltm")
rcor.test(resid, p.adjust = T) # only provides absolute correlations
rcor.test(resid)
# if significant correlations exist, check size compared to average:
diag(resid) <- NA # make the diagonal of correlation matrix NA instead of 1
mean(resid, na.rm=T) # get the mean residual value, ignoring NA
mean(resid, na.rm=T) + 0.3 # create variable indicating dynamic cutoff
residuals(mirt.rasch, type="Q3", digits=2, suppress=+.18) # indicates b+n and c+t
cfa.correlations # shows same two pairs as worst offenders
# winsteps identifies the same two pairs
itemlabels # check which items
# b+n göra farliga saker vs göra dumma/farliga saker
# c+t slå till någon när arg/provocerad
# helt rimligt att ta bort en i vardera par, så kör vi en ny modell sedan
# kolla item fit och location
item.fit
item_difficulty
plotPImap(df.erm)
# t looks worse than c in terms of location/targeting
# b has a bit worse fit than n, but they are very close on all parameters
df$f66t<-NULL
df$f66b<-NULL

##### new model
df.omit.na<-na.omit(df) # eRm does not allow participants with all missing data
df.erm<-PCM(df.omit.na) # run PCM model
# get estimates
item.estimates <- eRm::thresholds(df.erm)
item_difficulty <- item.estimates[["threshtable"]][["1"]]
item_difficulty<-as.data.frame(item_difficulty)
item.se <- item.estimates$se.thresh
person.locations.estimate <- person.parameter(df.erm)
item.fit <- eRm::itemfit(person.locations.estimate)
item.fit 
std.resids <- item.fit$st.res
# PCA on residuals
pca <- pca(std.resids, nfactors = 3, rotate = "oblimin")
pca$values # eigenvalue down to 1.75

cfa.model <- 'factor1 =~ f66a +f66c +f66d +f66f +f66g +f66i +f66j +f66k +f66n +f66o +f66q+f66r +f66s'
cfa.fit.dwls <- cfa(cfa.model, data=df, ordered=names(df)) # DWLS for ordinal data
summary(cfa.fit.dwls, fit.measures=T, standardized=T)
# s has loading 0.41
# show the 10 highest mod indices (unidimensional model will only have residual correlations, no crossloadings)
cfa.correlations<-modindices(cfa.fit.dwls) %>%
  arrange(desc(mi)) %>%
  head(10)

# let's get Rasch residual correlations
mirt.rasch <- mirt(df.omit.na, model=1, itemtype='Rasch') # unidimensional model
resid=residuals(mirt.rasch, type="Q3", digits=2)
rcor.test(resid)
# if significant correlations exist, check size compared to average:
diag(resid) <- NA # make the diagonal of correlation matrix NA instead of 1
mean(resid, na.rm=T) # get the mean residual value, ignoring NA
mean(resid, na.rm=T) + 0.3 # create variable indicating dynamic cutoff
residuals(mirt.rasch, type="Q3", digits=2, suppress=+.2) # indicates b+n and c+t
cfa.correlations #


### svarskategorier
plot(mirt.rasch, type="trace") # get ICC for response options
plotICC(df.erm) # can use item.subset = "item")
#plot(df.tam, type = "items")


# •	hur frågornas ”svårighetsgrad” matchar mot respondenternas egenskaper

plotPImap(df.erm) # wright map sorted on item entry
plotPImap(df.erm, sorted = T) # wright map sorted on location
# lägg till TAM, behövs senare för reliabilitet
library(TAM)
df.m<-as.matrix(df)
df.tam <- tam(df.m, irtmodel = "PCM")
IRT.WrightMap(df.tam,show.thr.lab=TRUE)

# make separate histograms with theta and locations and see how they can be 
# combined, and add lines showing reliability

# persons
thetas<-as.data.frame(person.locations.estimate$theta.table)
head(thetas$`Person Parameter`)
pthetas<-thetas$`Person Parameter`
hist(pthetas, col=rgb(1,0,0,.5), xlim = c(-4,4))

# item thresholds
thresholds<-c(item_difficulty$`Threshold 1`,item_difficulty$`Threshold 2`,item_difficulty$`Threshold 3`)
hist(thresholds, xlim = c(-4,4), col=rgb(0,0,1,.5))

# create dataframe for ggplot, with both in the same variable
#create data frame with 0 rows and 3 columns
df.locations <- data.frame(matrix(ncol = 2, nrow = 0))
#provide column names
colnames(df.locations) <- c('type', 'locations')
# change type of data
df.locations$type<-as.character(df.locations$type)
df.locations$locations<-as.numeric(df.locations$locations)
# insert labels in accurate amounts (N+items)
df.locations[1:9756,1]<-paste0("Persons")
df.locations[9757:9795,1]<-paste0("Item thresholds")
# insert data from vectors with thetas and thresholds
df.locations$locations<-c(pthetas,thresholds)
# change type to class factor
df.locations$type<-as.factor(df.locations$type)

library(ggplot2)
ggplot() + 
  geom_histogram(data=subset(df.locations, type=="Persons"), aes(locations, fill="Persons", y= ..count..)) +
  geom_histogram(data=subset(df.locations, type=="Item thresholds"), aes(locations, fill="Item thresholds", y= -..count.. * 100)) + # 100 needs to be adjusted depending on sample size
  scale_x_continuous("Logits", breaks = -5:5) +
  scale_y_continuous("Antal personer", breaks = ) +
  scale_fill_hue("") +
  ggtitle("Person/item thresholds histogram")  
# add lines for reliability thresholds?

# plot infit t, can also be done for persons!
plotPWmap(df.erm, pmap = TRUE, imap = FALSE, horiz = F)

########################################### add numbers on extreme responses!!

######## item fit z stats

# test similar with eRm?

#zstd is inflated with large samples, reduce sample size in df with jz and re-check with yz random samples
jz = 9400 # number to remove from dataset. Recommended to reduce to about N = 400
yz = 25 # number of random samples
outfitZ<-c()
infitZ<-c()
for (i in 1:yz) {
  df.new <- df.omit.na[-sample(1:nrow(df.omit.na), jz), ]
  mirt.raschZ <- mirt(df.new, model=1, itemtype='Rasch') # unidimensional model
  mitemfit<-mirt::itemfit(mirt.raschZ, fit_stats = 'infit', method = 'ML', na.rm = T) # get item fit 
  outfitZ<-cbind(outfitZ,mitemfit$z.outfit)
  infitZ<-cbind(infitZ,mitemfit$z.infit)
}
mitemfit$mean.outfitZ<-cbind(round(rowMeans(outfitZ),3))
mitemfit$mean.infitZ<-cbind(round(rowMeans(infitZ),3))
mitemfit


# •	reliabilitet (över hela indexet, ”test information function”)

# TAM visar på EAP Reliability 0.811 (Winsteps ger 0.80)
plotINFO(df.erm) # get item+test information curves
plotINFO(df.erm, type = "test") # show only test information
abline(h = 3.33, col = "grey") # indicate TIF 3.33 = PSI 0.70
abline(h = 5, col = "grey") # indicate TIF 5 = PSI 0.80


# •	invarians – att frågorna fungerar lika mellan kön och mättillfällen (och ev. mellan åk 9 och åk 2 på gymnasiet? ej specat i avtalet, men rimligen relevant)
# DIF tests


# DIF with eRm requires no missing data in comparison variable
df.nona <- df.all[c(8,10,11,191:211)]
df.nona<-na.omit(df.nona)
df.nona.gender<-df.nona$Kön
df.nona$Kön<-NULL
df.nona.arskurs<-df.nona$ARSKURS
df.nona$ARSKURS<-NULL
df.nona.skolsdo<-df.nona$SkolSDO
df.nona$SkolSDO<-NULL
#names(df.nona) <- paste0("q", c(1:ncol(df.nona))) # for any set
df.nona$f66t<-NULL
df.nona$f66b<-NULL
df.nona$f66l<-NULL # remove items
df.nona$f66p<-NULL
df.nona$f66u<-NULL
df.nona$f66e<-NULL # remove items
df.nona$f66h<-NULL
df.nona$f66m<-NULL

df.erm.dif <- PCM(df.nona) # CML estimation.
df.erm.gof<-LRtest(df.erm.dif, splitcr = df.nona.gender)
df.erm.gof # check if overall significant - probably needs sample reduction!



# plot Andersens's Likelihood Ratio test, circles outside of the line indicate DIF for thresholds(?)
# no correction for multiple tests, though
plotGOF(df.erm.gof, conf = list(), tlab = "item",
        xlab = "Women", ylab = "Men")



# run similar DIF LR-test, but get significance tests on all thresholds
dif.wald<-Waldtest(df.erm.dif, splitcr = df.nona.gender)
# we need to adjust for multiple testing (non-working code below)
dif.wald.p<-as.data.frame(dif.wald$coef.table)
dif.wald$coef.table
dif.wald.p
p.adjust(dif.wald.p$`p-value`)


# we can get a random split variable by generating a vector of 1's and 2's
halfsplit<-sample(1:2,793, replace = T)
df.erm.split<-LRtest(df.erm.dif, splitcr = halfsplit)
df.erm.split # check if significant
# we could write a loop running this test x times and collecting how many have significant DIF?

# also run with splitcr = "median" and/or "mean"
df.erm.split<-LRtest(df.erm, splitcr = "median")
df.erm.split


# •	omvandling av summerade rådata till intervalldata med mätosäkerheter på individnivå