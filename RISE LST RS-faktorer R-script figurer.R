###
### Detta script hör till en rapport producerad av RISE Social & Health Impact Center, magnus.p.johansson@ri.se på uppdrag av 
### Länsstyrelsen i Stockholm. Rapporten och tillhörande material finns att ladda ner på Open Science Framework: 
### https://osf.io/935b6/
### Denna fil är skyddad av Creative Commons Attribution 4.0 International Public License
### https://creativecommons.org/licenses/by/4.0/
###

# läs in de bibliotek som behövs för att köra scriptet (ev. kan de behöva installeras på din dator)
library(readxl)
library(dplyr)
library(tidyverse)
library(rlang)
library(ggplot2)
library(visNetwork)
library(networkD3)

# välj excelfil med data
rsfaktorer<-file.choose()
# läs in excelfilen till en dataframe
lst.data <- read_excel(rsfaktorer)

### Skapa subset av data för att välja kontext/nivå(er) och antingen risk eller skydd (RSfaktor), och om så önskas även utfall (Spets).
### Nedan finns flera exempel - bara en rad kan köras per figur (OBS att stor/liten bokstav är viktigt!)

# Nedanstående rad väljer riskfaktorer på individnivå
lst.kontext<-subset(lst.data, Kontext=="Individ" & RSfaktor=="Riskfaktor")

# här finns fler exempel på urval som kan göras
lst.kontext<-subset(lst.data, RSfaktor=="Riskfaktor") # här väljs riskfaktorer, och alla kontexter
lst.kontext<-subset(lst.data, RSfaktor=="Riskfaktor" & Spets=="Psyk. ohälsa") # alla riskfaktorer för psyk. ohälsa
lst.kontext<-subset(lst.data, Faktor=="Impulsivitet") # enbart riskfaktorn Impulsivitet
# nedan är en specifik uppsättning riskfaktorer utvalda
lst.kontext<-subset(lst.data, RSfaktor=="Riskfaktor" & Faktor %in% c('Impulsivitet','Föräldrarnas svårigheter','Låga skolprestationer','Problematiska kamratrelationer'))

# Nedan väljs skyddsfaktorer för specifika kontexter
lst.kontext<-subset(lst.data, RSfaktor=="Skyddsfaktor" & Kontext %in% c('Individ','Familj'))
lst.kontext<-subset(lst.data, RSfaktor=="Skyddsfaktor" & Kontext %in% c('Kamrater och fritid','Skola','Samhälle'))




### Sedan körs hela kodblocket nedan till filens slut, vilket resulterar i en figur med ett Sankey-diagram.
### Figuren kan exporteras som webbsida/HTML, vilket gör att det går att interagera med den med muspekaren,
### eller så exporteras den som bildfil.

### Önskar man få fram fler figurer är det bara att göra om subset-kommandot ovan och välja vilken 
### data man vill använda, och köra nedanstående kod igen.

# koden nedan är lånad från nedanstående källor, och modifierad:
# https://www.r-graph-gallery.com/322-custom-colours-in-sankey-diagram
# https://medium.com/@emtiazahmed.cs/sankey-diagram-step-by-step-using-r-b3e7bea53224 
# https://christophergandrud.github.io/networkD3/

# extrahera vektorer med unika Spetsar & RS-faktorer
spetsar <- lst.kontext %>%
  distinct(Spets)  %>%
  dplyr::rename(label = Spets)
faktorer <- lst.kontext %>%
  distinct(Faktor)  %>%
  dplyr::rename(label = Faktor)

# sammanfoga dem
rsfaktorer <- full_join(faktorer, spetsar, by = "label")
rsfaktorer <- rsfaktorer %>% rowid_to_column("id")

# skapa table som visar hur många gånger varje rsfaktor kopplas till en spets
per_route <- lst.kontext %>%
  group_by(Faktor, Spets) %>%
  dplyr::summarise(count = n()) %>%
  ungroup()

# ta fram variabler för nätverksmodeller och liknande visualisering
edges <- per_route %>%
  left_join(rsfaktorer, by = c("Faktor" = "label")) %>%
  dplyr::rename(from = id)
edges <- edges %>%
  left_join(rsfaktorer, by = c("Spets" = "label")) %>%
  dplyr::rename(to = id)

edges <- select(edges, from, to, count)
edges <- mutate(edges, width = count + 1)
nodes_d3 <- mutate(rsfaktorer, id = id - 1)
edges_d3 <- mutate(edges, from = from - 1, to = to - 1)
edges_d3$group <- per_route$Spets
nodes_d3$nodecolor <- c("allsamecolor")

# färgsättning av flöden (edges) utifrån spetsarna och rätblocken intill faktorer & spetsar i diagrammet
# kulörer lånade bl.a. från http://opencolor.tools/palettes/wesanderson/ 
my_color <- 'd3.scaleOrdinal() .domain(["Psyk. ohälsa","Utanförskap","Våld","Kriminalitet","Missbruk/ANDTS", "allsamecolor"]) 
              .range(["lightblue", "#F5CDB6", "#F7B0AA", "#FDDDA4", "#76A08A", "#FCD16B"])'
# färgkod #FCD16B för skyddsfaktorer (förvalt i koden ovanför) och #D8A49B för riskfaktorer 
# ändra "#FCD16B" på rad 91 till "#D8A49B" för att byta färg på rätblocken

# skapa ett interaktivt Sankey-diagram där spetsarna i preventionsstjärnan finns till höger.
sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
              NodeID = "label", Value = "count", fontSize = 20, unit = "Antal",
              fontFamily="sans-serif", LinkGroup = "group", colourScale = my_color,
              nodeWidth = 13, NodeGroup = "nodecolor", nodePadding = 18)
