library(dplyr)

sbbshops<-read.csv(url("https://data.sbb.ch/api/explore/v2.1/catalog/datasets/offnungszeiten-shops/exports/csv?limit=-1&lang=de&timezone=UTC&use_labels=true&epsg=4326"), sep = ';')

