### creating a CSV with item parameters for Concerto Platform to enable computer adaptive testing
### magnus.p.johansson@ri.se RISE Research Institutes of Sweden www.ri.se
### CC BY 4.0

library(tidyverse)
library(eRm)

# read raw questionnaire data, this uses SWEMWBS with 7 items
df <- read.csv("C:/Users/magnuspjo/OneDrive - RISE/Dokument/IPS livskvalitet/RegSthlm/regsthlmData/regsthlmSWEMWBSr2.csv")

# subset item data only
df.omit.na <- df %>%
  dplyr::select(starts_with("SWEMWBS")) %>%
  na.omit()

# run PCM Rasch model
df.erm<-PCM(df.omit.na) 

# get item estimates
item.estimates <- eRm::thresholds(df.erm)
item_difficulty <- as.data.frame(item.estimates[["threshtable"]][["1"]])

# create df with pX = item thresholds
concerto <- item_difficulty %>%
  select(starts_with("Threshold")) %>%
  add_column('id' = c(1:ncol(df.omit.na)), .before = "Threshold 1")
concols<-ncol(concerto)-1
names(concerto) <- c("id",paste0("p", c(1:concols)))

# read questionnaire item from separate file
itemlabels<-read.csv("C:/Users/magnuspjo/OneDrive - RISE/Dokument/IPS livskvalitet/RegSthlm/SWEMWBS_itemlabels.csv", encoding = "UTF-8")

# create FlatItem file for Concerto with prerequisite variables. Needs adjustment depending on response categories
concerto <- concerto %>%
  add_column('fixedIndex' = NA, .after = "id") %>%
  add_column('trait' = "SWEMWBS", .after = "fixedIndex") %>%
  add_column('question' = itemlabels$item, .after = "trait") %>%
  add_column('responseLabel1' = "Aldrig") %>% # these end up after pX variables
  add_column('responseValue1' = 0) %>%
  add_column('responseScore1' = 0) %>%
  add_column('responseTrait1' = NA) %>%
  add_column('responseLabel2' = "Sällan") %>%
  add_column('responseValue2' = 1) %>%
  add_column('responseScore2' = 1) %>%
  add_column('responseTrait2' = NA) %>%
  add_column('responseLabel3' = "Ibland") %>%
  add_column('responseValue3' = 2) %>%
  add_column('responseScore3' = 2) %>%
  add_column('responseTrait3' = NA) %>%
  add_column('responseLabel4' = "Oftast") %>%
  add_column('responseValue4' = 3) %>%
  add_column('responseScore4' = 3) %>%
  add_column('responseTrait4' = NA) %>%
  add_column('responseLabel5' = "Alltid") %>%
  add_column('responseValue5' = 4) %>%
  add_column('responseScore5' = 4) %>%
  add_column('responseTrait5' = NA) %>%
  add_column('type' = "options")

write.csv(concerto, file = "C:/Users/magnuspjo/OneDrive - RISE/Dokument/IPS livskvalitet/swemwbsFlatItems.csv", row.names = F, na = "", fileEncoding = "UTF-8")

# # read data from concerto
# WellCATdata<-read.csv("C:\\Users\\magnuspjo\\Downloads\\WellCATdata.csv")
# # get session id's connected to manually created individual ID codes to track longitudinal data
# WellCATdata %>%
#   filter(name == "ID")
# # create df for ggplot etc (this needs automation when we have large N, but the basics should be similar)
# df.stats<-WellCATdata %>%
#   filter(session_id %in% c("i74","i75")) %>%
#   filter(name %in% c("theta","sem")) %>%
#   select(value,session_id)