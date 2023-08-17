##install.packages("dplyr")
##install.packages("tidyverse")
##install.packages("jsonlite")
##install.packages("lubridate")
library(dplyr)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(purrr)

sbbopenshops<-read.csv(url("https://data.sbb.ch/api/explore/v2.1/catalog/datasets/offnungszeiten-shops/exports/csv?limit=-1&lang=de&timezone=UTC&use_labels=true&epsg=4326"), sep = ';')

sbbshops<-sbbopenshops %>% 
  select(c(1,5,7:13,17,34:37)) %>% 
  ##filter(!category %in% c("Öffentlicher Verkehr","Piktogramm (Übrige)", "Piktogramm SBB Schalter")) %>% 
  filter(!category %in% c("Öffentlicher Verkehr","Piktogramm (Übrige)")) %>% 
  mutate(logourl=paste("https://stations.sbb.cartaro-enterprise.com", logo,sep = ""),
         logourl2=paste("<img src=", "\"", logourl, "\""," width=\"80\"></img>",sep=""),
         category=case_when(category=="Piktogramm SBB Schalter" ~ "SBB Schalter",
                            category=="Services P"~"Services", 
                            category=="Services IM"~"Services",
                            category=="Services-Übrige"~"Services",
                            category=="Kombinierte Mobilität"~"Mobilität", TRUE~category),
         subcategories=case_when(subcategories=="Dienstleistungen SBB Services"~"SBB Services",
                                 subcategories=="SBB Services"~"SBB Services",
                                 subcategories=="SBB"~"SBB Services",TRUE~subcategories),
         subcategories = strsplit(subcategories, "[\n /]+"))%>%
  mutate(openhours_list = strsplit(as.character(openhours), "\\]\\s*\\[", perl = TRUE)) %>%
  unnest(openhours_list) %>%
  mutate(openhours_list = paste0("[", openhours_list, "]")) %>%
  mutate(openhours_list = map(openhours_list, ~ suppressWarnings(fromJSON(.x))))%>%
  unnest_wider(openhours_list, names_sep = "_") %>% 
  mutate(openhours_table = map(openhours_list_1, ~ format_opening_hours(.x)))%>%
  unnest_wider(openhours_table, names_sep = "_")


