
library(tidyverse)
library(stringr)

raw_text <- readLines("data/bls-food-price-data.txt")
all_data <- as_data_frame(raw_text) %>% filter(value!="") %>% mutate(series_ind = grepl("Series Id:",value)) %>% mutate(series_num = cumsum(series_ind))
all_nums <- unique(all_data$series_num)


data_out <- data_frame()
for(i in 1:length(all_nums)){
  # i<- 1
  input_data <- all_data %>% filter(series_num==all_nums[i])
  
  top_part <- head(input_data,2) %>% select(value)
  series_id <- top_part %>% filter(grepl("Series Id",value)) %>% as.character() %>% str_replace_all("Series Id: * ","")
  series_tile <- top_part %>% filter(grepl("Series Title",value)) %>% as.character() %>% str_replace_all("Series Title: * ","")
  
  message(series_tile)
  
  bottom_part <- tail(input_data, 2) %>% select(value)
  splits <- map(bottom_part, ~str_split(.x, "\t"))
  the_names <- splits$value[[1]]
  vals <- as_data_frame(matrix(splits$value[[2]], nrow = 1))
  if(length(vals)==1){
    next
  } else {
    names(vals) <- the_names
    vals$`Series Id` <- series_id
    vals$series_title <- series_tile
    data_out <- bind_rows(data_out, vals)
  }
  
}

write_csv(data_out, "data/processed_bls_food_price_data.csv")