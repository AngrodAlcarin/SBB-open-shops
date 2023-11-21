##install.packages("tidyverse")
##install.packages("jsonlite")
##install.packages("lubridate")
library(tidyverse)
library(jsonlite)
library(lubridate)
library(readr)


sbbopenshops<-read_delim("SBB-open-shops/offnungszeiten-shops.csv",delim = ";", escape_double = TRUE,
                         trim_ws = TRUE,show_col_types=FALSE) %>% 
  mutate(Namelow=tolower(Name))

sbbnewshops<-read.csv(url("https://data.sbb.ch/api/explore/v2.1/catalog/datasets/offnungszeiten-shops/exports/csv?limit=-1&lang=de&timezone=UTC&use_labels=true&epsg=4326"), sep = ';')
##write_csv(sbbnewshops,"SBB-open-shops/sbbnewshops17oct23.csv") #archive current csv so it works even if it changes again
sbbnewlogo<-sbbnewshops %>% 
  mutate(Namelow=tolower(Name)) %>% 
  select(c(19,18,48)) %>% 
  rename(logourl=logo_svg,
         Namey=Name) %>% 
  distinct()
sbbmergedshops<-left_join(sbbopenshops,sbbnewlogo,by=join_by(Namelow),relationship="many-to-one",multiple="first")

sbbshops<-sbbmergedshops %>% 
  select(c(1,5,7:13,17,34:37,40)) %>% 
  filter(!category %in% c("Öffentlicher Verkehr","Piktogramm (Übrige)")) %>%
  mutate(logourl2=paste("<img src=", "\"", logourl, "\""," width=\"80\"></img>",sep=""),
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
  unnest_wider(openhours_list, names_sep = "_")

sbbshops<-sbbshops %>% 
  mutate(openhours_table = lapply(sbbshops$openhours_list_1, expand_openhours))
sbbshops<-sbbshops %>% 
  mutate(openhours_char= lapply(sbbshops$openhours_table, function(openhours) {
    if (!is.null(openhours) && is.data.frame(openhours)) {
      # Extract and format the open hours data
      openhours_text <- paste(openhours$day, openhours$time_from, openhours$time_to, sep = " - ")
      paste(openhours_text, collapse = "<br>")
    } else {
      ""
    }
  }),
  openhours_char=as.character(openhours_char))
