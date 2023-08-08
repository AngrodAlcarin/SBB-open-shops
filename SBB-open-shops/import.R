##install.packages("dplyr")
library(dplyr)

sbbopenshops<-read.csv(url("https://data.sbb.ch/api/explore/v2.1/catalog/datasets/offnungszeiten-shops/exports/csv?limit=-1&lang=de&timezone=UTC&use_labels=true&epsg=4326"), sep = ';')

sbbshops<-sbbopenshops %>% 
  select(c(1,5,7:13,17,34:37)) %>% 
  ##filter(!category %in% c("Öffentlicher Verkehr","Piktogramm (Übrige)", "Piktogramm SBB Schalter")) %>% 
  filter(!category %in% c("Öffentlicher Verkehr","Piktogramm (Übrige)")) %>% 
  mutate(logourl=paste("https://stations.sbb.cartaro-enterprise.com", logo,sep = ""),
         logourl2=paste("<img src=", "\"", logourl, "\""," width=\"80\"></img>",sep=""),
         category=case_when(category == "Piktogramm SBB Schalter" ~ "SBB Schalter", TRUE~category),
         category=case_when(category=="Services P"~"Services", 
                            category=="Services IM"~"Services",
                            category=="Services-Übrige"~"Services", TRUE~category))

nlevels(factor(sbbshops$Haltestellen.Name))
unique(sbbshops$Haltestellen.Name)
