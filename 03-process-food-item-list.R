

# this script takes the raw Nutrients file from the USDA:
# https://www.ars.usda.gov/northeast-area/beltsville-md-bhnrc/beltsville-human-nutrition-research-center/nutrient-data-laboratory/docs/sr28-download-files/
# and creates a subset of the data by rolling up foods into categories and eliminating some unwanted ones.
# food category nutriet values are the median of the foods under each category. 

# data dictionary found here:
# https://www.ars.usda.gov/ARSUserFiles/80400525/Data/SR/sr28/sr28_doc.pdf

library(tidyverse)
library(stringr)
nutiets <- read_csv("data/Nutrient Data.csv")

# the initial list has close to 8,000 foods. We want to reduce the number down to something manageable. 
# fortunately, the foods easily group themselves by the first word in the "Short Description" field. 
# for example, "Beef" "Chicken" etc. 

# we will want to group the foods by their high level categories. 
# there are about 1,200 high-level categories
nutiets %>% 
  mutate(firstword = substr(Shrt_Desc, 1, str_locate(Shrt_Desc,",")-1)) %>% 
  group_by(firstword) %>% 
  count() %>% 
  arrange(-n)

# the "NA" high level categories are inconsequential
# so we can filter them out
nutiets %>% 
  mutate(firstword = substr(Shrt_Desc, 1, str_locate(Shrt_Desc,",")-1)) %>% 
  filter(is.na(firstword))


# taking the median of each nutriet column will give us a good approximation of
# the general nutritional content. 
nutiet_categories <- nutiets %>% 
  mutate(Category = substr(Shrt_Desc, 1, str_locate(Shrt_Desc,",")-1)) %>% 
  select(Category, everything()) %>% 
  select(-NDB_No, -Shrt_Desc) %>%
  group_by(Category) %>% 
  select_if(function(x) !is.character(x)) %>% 
  summarise_at(vars(`Water_(g)`:Refuse_Pct), funs(median), na.rm = TRUE)


# remove some categories that we don't want
# general rules for removal:
#   - foods adults don't eat, like BABYFOOD
#   - ambiguous foods like "Snacks"
#   - some non-specific brands like "Silk"
#   - redundant categories/misspellings like CHK and CHICkN (sic)

nutiet_categories_filtered <- nutiet_categories %>% 
  filter(!is.na(Category)
         , !grepl("AUSTIN|APPLEBEE'S|BABYFOOD|ALCOHOLIC BEV|ARCHWAY|BABYFD|BABYFOO|Babyfood|BAK CHOC|Beef|
                  BEVERAGE|BEVERAGES|Beverages|BF|CAMPBELL'S CHUNKY|CAMPBELL'S|Candies|CHCKN|
                  CHEWING GUM|CHICkN|CHILD FORMULA|CKN|MORNINGSTAR|MOTHER'S|SILK|Snac|Snack|Snacks|SNACK|
                  SOUR CRM|USDA|WEND'YS|WENDYS|WENDY'S|Whale"
                  , Category, ignore.case = FALSE) # redundant category
         ) %>% 
  filter(!is.na(Category))


# convert all columns to grams (other than calories)
# remove some of the vitamin columns that are redundant
# vitamin conversion from: https://en.wikipedia.org/wiki/International_unit

mg_2_g <- function(x) x*0.001
µg_2_g <- function(x) x*1e-6
vit_A_IU_2_g <- function(x) x*3e-7 # 1 IU of vit A is 0.3 micrograms (specific to retinol)


nutiet_categories_converted <- nutiet_categories_filtered %>% 
  mutate_at(vars(contains("mg")), funs(mg_2_g)) %>% 
  mutate_at(vars(contains("µg")), funs(µg_2_g)) %>% 
  mutate("Vit_A_(g)"  = vit_A_IU_2_g(Vit_A_IU)) %>% 
  select(-Vit_A_RAE, -Vit_A_IU, -GmWt_1, -GmWt_2, -Refuse_Pct, -Vit_D_IU)

name_vector <- names(nutiet_categories_converted)
names(nutiet_categories_converted) <- str_replace_all(name_vector, "mg|µg","g")


# replace all NAs with zeros
nutiet_categories_no_nas <- nutiet_categories_converted %>% 
  mutate_all(function(x) ifelse(is.na(x),0,x))
  

# write out
nutriets_final <- nutiet_categories_no_nas

if(readline(prompt="Do you want to overwrite the nutritional file? (Y|n): ")=="Y"){
  write_csv(nutriets_final, "data/nutrient-categories.csv")
  message("...nutriet file written to data/nutrient-categories.csv")
}







