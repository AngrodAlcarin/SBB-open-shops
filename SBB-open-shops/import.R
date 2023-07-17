install.packages("dplyr")
library(dplyr)

sbbopenshops<-read.csv(url("https://data.sbb.ch/api/explore/v2.1/catalog/datasets/offnungszeiten-shops/exports/csv?limit=-1&lang=de&timezone=UTC&use_labels=true&epsg=4326"), sep = ';')

sbbshops<-sbbopenshops %>% 
  select(c(5,7:13,17,34:37)) %>% 
  filter(!category %in% c("Öffentlicher Verkehr","Piktogramm (Übrige)", "Piktogramm SBB Schalter"))

         