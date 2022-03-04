library(dplyr)
library(tidyverse)
library(readxl)
library(tidyr)

# set working directory (copy from file explorer and replace \ with /)
setwd("C:/Users/magnuspjo/OneDrive - RISE/Dokument/IPS livskvalitet/BBQ")

# read datafile to dataframe
df <- read_excel("BBQonly.xlsx")
df.all <- df
# BBQ "viktade" finns i df[,59:64]
df <- df.all[,c(47,49,51,53,55,57)] # BBQ udda items, innehåller upplevelsefrågorna
names(df) <- paste0("q", c(1,3,5,7,9,11)) # for BBQ
library(eRm) # https://bookdown.org/chua/new_rasch_demo2/PC-model.html
df.erm <- PCM(df) # Partial Credit Model, CML estimation.
person.locations.estimate <- person.parameter(df.erm)
# omvandling av summerade rådata till intervalldata
# nedan ger transformering till 0-100
ple<-as.data.frame(print(person.locations.estimate))
rownames(ple)<-NULL
colnames(ple)<-c("Råpoäng","Logits","SE (Logits)")
ple[,2]<-round(ple[,2],2)
ple[,3]<-round(ple[,3],2)
# now rescale, https://www.rdocumentation.org/packages/scales/versions/0.4.1/topics/rescale 
library(scales)
ple$Intervallpoäng<-round(scales::rescale(ple[,2], to = c(0,100)),1) # set 0,100 to desired range
library(formattable)
formattable(ple) # check that the table looks ok
ordinalScale<-ple$Råpoäng
intervalScale<-ple$Intervallpoäng
df$rawSumScores<-df %>%
  mutate(rowSums(.)) %>%
  pull()
hist(df$rawSumScores)
df$intervalScores<-plyr::mapvalues(df$rawSumScores, ordinalScale, intervalScale)
hist(df$intervalScores, col=rgb(1,0,0,.5))
