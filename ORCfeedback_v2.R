library(dplyr)
library(tidyverse)
library(car)
library(ggplot2)
library(reshape2)
library(readxl)
library(tidyr)

# set working directory
setwd("C:/Users/magnuspjo/RISE/Innovationspartnerskap Sveriges Stadsmissioner - General/Frågeformulär/ORC/MJ")

df <- read_excel("2022-01-10 orcssm2.xlsx")

# convert tibble to dataframe (for reverse scoring etc)
df <- as.data.frame(df)
#drop removed items
df$KLq8d <- NULL
df$KLq9d1 <- NULL
df$KMb4r <- NULL

# remove non-SSM responses (NA in variable Stadsmission)
df <- df[-c(111:152),]
# and remove respondent 53 with blank data
df <- df[-53,]

# import variable descriptions, first for items (repeat same procedure for others)
variabler <- read_excel("../ORC_2_utskicket_220110.xlsx", sheet = "Variabellista")
variabler<-as.data.frame(variabler)
stadsmissioner <- variabler %>% 
  separate(Stadsmission, c(NA, "SSM"), sep ="=") %>%
  select("SSM") %>%
  drop_na()

legend<-variabler[3:6,6]
legend.if<-variabler[3:6,28]

#drop removed items from variable descriptions
variabler$KLq8d <- NULL
variabler$KLq9d1 <- NULL
variabler$KMb4r <- NULL

# replace old or create new variable with location names instead of numbers 1-10
df$Stadsmission <- stadsmissioner$SSM[ match(df$Stadsmission, 1:10) ]

# make separate barplot PDFs for each local Stadsmission
for (j in stadsmissioner[c(1:4,8:10),1]) { # Skåne,Sthlm saknar data, Etuna för få N
  outputFile<-paste0("Stadsmissionen ",j,".pdf") # filnamn baserat på id för Stadsmission
  pdf(outputFile) # skapa/exportera till PDF
  df.temp<-subset(df, Stadsmission==j) # gör separat analys för varje stadsmission
  
  # demografiska variabler med olika svarsalternativ (se legend.text)
  barplot(table(df.temp[,3]), col="lightblue", sub=variabler[2,3],ylab="Antal", 
          main = paste0(j, " - ", names(df.temp[3])), 
          legend.text = variabler[3:5,3], args.legend = list(bty="n",cex=0.5, x="topleft"))
  barplot(table(df.temp[,4]), col="lightblue", sub=variabler[2,4],ylab="Antal", 
          main = paste0(j, " - ", names(df.temp[4])), 
          legend.text = variabler[3:7,4], args.legend = list(bty="n",cex=0.5, x="topleft"))
  barplot(table(df.temp[,5]), col="lightblue", sub=variabler[2,5],ylab="Antal", 
          main = paste0(j, " - ", names(df.temp[5])), 
          legend.text = variabler[3:6,5], args.legend = list(bty="n",cex=0.5, x="topleft"))
  for (i in 6:24) { # items från KL och KM
    barplot(table(df.temp[,i]), col="lightblue", sub=variabler[2,i],ylab="Antal", 
            main = paste0(j, " - enkätfråga ", names(df.temp[i])), legend.text = legend,
            args.legend = list(bty ="n", cex=0.5, x = "topleft"))
  }
  for (i in 25:28) { # items från IF har andra svarskategorier
    barplot(table(df.temp[,i]), col="lightblue", sub=variabler[2,i],ylab="Antal", 
            main = paste0(j, " - enkätfråga ", names(df.temp[i])), legend.text = legend.if,
            args.legend = list(bty ="n", cex=0.5, x = "topleft"))
  }
  dev.off()
}


## testkod
library(summarytools)
dfSummary(df[3:5])


### old code
# recode numerics to factors for demographics
df$Stadsmission <- car::recode(df$Stadsmission,"1='Göteborg';2='Kalmar';3='Linköping';4='Uppsala'; 5='Eskilstuna';6='Skåne';7='Stockholm';8='Umeå';9='Västerås';10='Örebro'")
df$Arbetsområde <- car::recode(df$Arbetsområde,"1='Arbetsintegrering';2='Dagverksamhet (Öppen mötesplats)';3='Ledning'")
df$Arbetsledare <- car::recode(df$Arbetsledare,"1='Arbetsledare';2='Teamledare';3='Chef';
                                4='Verksamhetsutvecklare';5='Nej'")

