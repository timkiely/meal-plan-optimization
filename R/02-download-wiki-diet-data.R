
library(tidyverse)
library(rvest)


# source wiki table:
wiki_url <- "https://en.wikipedia.org/wiki/Dietary_Reference_Intake"
raw_html <- read_html(wiki_url)
all_tables <- raw_html %>% html_table()


# micros
micros <- all_tables[[1]]
micro_to_milli <- function(x) x*0.001

micro_table <- micros %>% 
  select(Nutrient
         , lower = `Highest RDA/AI`
         , upper = `UL[7]`
         , Unit
         ) %>% 
  mutate(upper = as.numeric(upper)) %>% 
  mutate(lower = ifelse(Unit == "µg", micro_to_milli(lower), lower)
         , upper = ifelse(Unit == "µg", micro_to_milli(upper), upper)
         ) %>% 
  mutate(Unit = "mg")
  
# macros
macros <- all_tables[[3]]

macro_table <- macros %>% 
  select(Substance
         , "Amount" = `Amount (males)`
         )

