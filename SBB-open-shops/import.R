##install.packages("tidyverse")
##install.packages("jsonlite")
##install.packages("lubridate")
library(tidyverse)
library(jsonlite)
library(lubridate)
library(readr)


sbbopenshops<-read_delim("SBB-open-shops/offnungszeiten-shops.csv",delim = ";", escape_double = TRUE,
                         trim_ws = TRUE)

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
