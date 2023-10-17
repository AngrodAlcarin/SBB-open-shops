##install.packages("tidyverse")
##install.packages("jsonlite")
##install.packages("lubridate")
library(tidyverse)
library(jsonlite)
library(lubridate)
library(RJSONIO)

sbbopenshops<-read.csv(url("https://data.sbb.ch/api/explore/v2.1/catalog/datasets/offnungszeiten-shops/exports/csv?limit=-1&lang=de&timezone=UTC&use_labels=true&epsg=4326"), sep = ';')
write.csv2(sbbopenshops, "SBB-open-shops/sbbnewshops17oct23.csv") #archive current csv so it works even if it changes again

sbbshops<-sbbopenshops %>% 
  select(c(2,3,13,18,19,25,26,29,36,46,47)) %>% 
  filter(!category =="__dummy__") %>% 
  mutate(logourl2=paste("<img src=", "\"", logo_svg, "\""," width=\"80\"></img>",sep=""))%>%
  mutate(openhours_list = strsplit(as.character(openinghours), "\\]\\s*\\[", perl = TRUE)) %>%
  unnest(openhours_list) %>%
  mutate(openhours_list = paste0("[", openhours_list, "]")) %>%
  mutate(openhours_list = map(openhours_list, ~ suppressWarnings(fromJSON(.x))))%>%
  unnest_wider(openhours_list, names_sep = "_")

sbbshops<-sbbshops %>% 
  rowwise() %>%
  mutate(parsed_openinghours = list(fromJSON(openinghours))) %>%
  ungroup() %>%
  unnest(cols = parsed_openinghours)

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
