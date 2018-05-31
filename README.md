Diet Optimization Analysis
================

INTRO
=====

This project directory contains code and data for a Meal Plan Optimization project as part of MSDS 460.

FORMULATION
===========

Each food item in the "Nurtiet Data" file represents a binary decision variable. Over the course of any number days, any number of items can be selected for each day. Each food item has nutritional facts associated with it such as number of calories, grams of protein, carbs, etc.

Our decision variables will be foods to be selected for all meals in a day over the course of 5-7 days. Calories should be between 2000 and 2,500/day. Each food item should not be used more than once over the course of the planning period.

Our constraint will be to minimize carbohydrate intake.

DATA:
=====

-   FOOD PRICES from [BLS](https://www.bls.gov/regions/mid-atlantic/data/averageretailfoodandenergyprices_usandwest_table.htm)
-   NUTRIENT REQUIREMENT DATA form [wikipedia](https://en.wikipedia.org/wiki/Dietary_Reference_Intake)
-   COMMON NUTRIENT COUNTS IN FOODS from [USDA](https://www.ars.usda.gov/northeast-area/beltsville-md-bhnrc/beltsville-human-nutrition-research-center/nutrient-data-laboratory/docs/sr28-download-files/)
    -   *Note* that the values of the nutriets (in the columns) are per 100g of the food item listed. For example "BUTTER, WITH SALT" has 15.87 g of water per 100 g of Butter with Salt

PROGRAM
=======

``` r
suppressPackageStartupMessages({
  library(tidyverse)
  library(lpSolve)
})
```

Data
----

``` r
nutriets <- read_csv("data/nutrient-categories.csv")
constraints <- read_csv("data/nutrient-constraints.csv") %>% select(`Original Nutrient Name`:Unit)
```

Processing
----------

``` r
# keep only the cross-section of both lists
keep_names <- inner_join(data_frame(names = names(nutriets))
                         , data_frame(names = constraints$`Nutrient Name`)
                         , by = "names") %>% unlist() %>% as.character()

nutriets <- nutriets %>% select_if(names(.) %in% c("Category",keep_names))
constraints <- constraints %>% filter(`Nutrient Name` %in% keep_names)


# data cleansing -----------------------------------------------------------

# manually adding an upper caloric limit of 2,500 kcals
# 2,000 is RDI for women, 2,500 is for men
constraints <- constraints %>% 
  mutate(`Upper Bound` = ifelse(`Nutrient Name`=="Energ_Kcal", 2500,`Upper Bound`))

# if there is no upper bound, then adding UL as lower bound X2
constraints <- constraints %>% 
  mutate(`Upper Bound` = ifelse(`Upper Bound`<`Lower Bound`, `Lower Bound`*2, `Upper Bound`))


# Remove duplicate nutirents and oddities
nutriets <- nutriets %>% 
  filter(!Category %in% c("EGGS","NUTRITIONAL SUPP FOR PEOPLE W/ DIABETES"
                          , "INF FORMULA. MEAD JOHNSON"
                          ,"SHAKE", "SOY SAU", "GUMS", "SHORTENING CONFECTIONERY"
                          , "TOPPING", "MUSHROOM", "WORTHINGTON STRIPPLES"
                          , "MARGARINE-LIKE SHORTENING", "BREAKFAST BAR", "FST FOODS"
                          , "PAPA JOHN'S 14\" CHS PIZZA", "EGG MIX", "JEW'S EAR"
                          , "INF FOR","BEVERAGE", "SUNFLOWER SD BUTTER","SIDE DISHES"
                          , "CISCO", "INF FORMULA")
  ) %>% 
  filter(!grepl(c("BUTTER|OIL|MARGARINE|FAT|LARD"),Category, ignore.case = TRUE))
```

Run the LP in a loop for n number of days
-----------------------------------------

``` r
# how many days should we plan? (each day is a loop iteration)
all_days <- 7


foods_used <- c("WATER")
all_results <- list()

for(day in 1:all_days){

  # for development purposes, sample the nutrients list. Set to 1 to use full list
  sample_size <- 1
  set.seed(1)
  sample_nutriets <- sample_frac(nutriets, sample_size)
  
  # remove foods that have already been used, other than water
  foods_used <- foods_used[foods_used!="WATER"]
  sample_nutriets <- sample_nutriets %>% filter(!Category %in% foods_used)
  
  # set objective: minimize carbohydrates
  objective_function <- sample_nutriets$`Carbohydrt_(g)`
  
  # initiate LHS constraint matrix
  Left_Hand_Side <- matrix(numeric(nrow(sample_nutriets)), nrow = 1)
  
  # for each constraint, create a row in the LHS matrix
  for(i in 1:nrow(constraints)){
    
    contraint_row <- constraints[i,]
    constraint_name <- contraint_row$`Nutrient Name`
    constraint_lower <- contraint_row$`Lower Bound`
    constraint_upper <- contraint_row$`Upper Bound`
    
    nutirent_column <- sample_nutriets %>% select_at(vars(constraint_name)) %>% unlist() %>% as.numeric() %>% matrix(nrow = 1)
    rownames(nutirent_column) <- constraint_name
    Left_Hand_Side <- rbind(Left_Hand_Side, nutirent_column)
  }
  
  # remove the initialization row at the top
  Left_Hand_Side <- Left_Hand_Side[2:nrow(Left_Hand_Side),]
  
  # direction of the constraint lower and upper bound
  constraint_directions <- c(rep(">=", nrow(Left_Hand_Side))
                             ,rep("<=", nrow(Left_Hand_Side))
  )
  
  # Lower and Upper bounds for RHS
  Right_Hand_Side <- c(constraints$`Lower Bound`
                       , constraints$`Upper Bound`)
  
  # duplicate the LHS matrix since we have both upper and lower bounds
  Left_Hand_Side_all <- rbind(Left_Hand_Side, Left_Hand_Side)
  
  ## check the rows and columns match up:
  # all_equal(nrow(Left_Hand_Side_all)
  #           , length(Right_Hand_Side)
  #           , length(constraint_directions)
  # )
  # 
  # all_equal(length(objective_function)
  #           , ncol(Left_Hand_Side_all)
  # )

  lp_time_start <- Sys.time()
  (LP_Solved <- lp(direction = "min"
                   , objective.in = objective_function
                   , const.mat = Left_Hand_Side_all
                   , const.dir = constraint_directions
                   , const.rhs = Right_Hand_Side
                   , presolve=0
                   , compute.sens=TRUE
                   , all.bin = FALSE
                   #, binary.vec
                   #, all.int=FALSE
                   #, int.vec
  ))
  (lp_time <- Sys.time()-lp_time_start)
  
  
  # record results
  result_objective <- LP_Solved$objval
  
  results <- data_frame(
    Food = sample_nutriets$Category[LP_Solved$solution>0]
    ,`Amount(g)` = LP_Solved$solution[LP_Solved$solution>0]
  )
  
  results$Day <- as.integer(day)
  
  all_results[[day]] <- results
  
  # record foods used so they will be removed from subsequent loop
  foods_used <- c(foods_used, results$Food)
  
  # display info
  message("DAY ",day, ": ", length(results$Food)," items selected. "
          ,scales::percent(sample_size), " of data used. LP completed in "
          ,round(lp_time,2), units(lp_time))
}
```

    ## DAY 1: 19 items selected. 100% of data used. LP completed in 0.02secs

    ## DAY 2: 15 items selected. 100% of data used. LP completed in 0.08secs

    ## DAY 3: 16 items selected. 100% of data used. LP completed in 0.02secs

    ## DAY 4: 19 items selected. 100% of data used. LP completed in 0.02secs

    ## DAY 5: 16 items selected. 100% of data used. LP completed in 0.02secs

    ## DAY 6: 14 items selected. 100% of data used. LP completed in 0.02secs

    ## DAY 7: 15 items selected. 100% of data used. LP completed in 0.02secs

View results
------------

``` r
# print results:
max_len <- max(map_dbl(all_results, nrow))
blank_rows <- data_frame(rownum = 1:max_len)

all_results_print <- all_results %>% 
  map(~.x %>% mutate(rownum = row_number())) %>% 
  map(~full_join(.x, blank_rows, by = "rownum")) %>% 
  map(~.x %>% select(-rownum)) %>% 
  map(~{
    nms <- names(.x)
    dy <- unique(.x$Day)
    dy <- dy[!is.na(dy)]
    nm1 <- paste0("Day ",dy," food")
    nm2 <- paste0("Day ",dy," amt(g)")
    names(.x) <- c(nm1, nm2, "Day")
    .x %>% select(-Day)
  }) %>% 
  bind_cols()

knitr::kable(all_results_print[,1:6])
```

| Day 1 food                  |  Day 1 amt(g)| Day 2 food                 |  Day 2 amt(g)| Day 3 food             |  Day 3 amt(g)|
|:----------------------------|-------------:|:---------------------------|-------------:|:-----------------------|-------------:|
| ANCHOVY                     |     0.3043298| SOYBEAN                    |     0.0505939| EEL                    |     0.2059462|
| HYACINTH BNS                |     0.3599970| SALT                       |     0.0158440| KASHI BLACK BEAN MANGO |     0.2789235|
| ROSE HIPS                   |     0.0788420| MARJORAM                   |     0.0821426| GRAVY                  |     0.8278982|
| OOPAH (TUNICATE)            |     0.4911123| CREAM                      |     1.3869826| SUNFLOWER SD KRNLS     |     0.0140831|
| TODDL FORM                  |     0.6860036| FROG LEGS                  |     2.2605772| BEVER                  |     0.1693780|
| CARP                        |     0.2968224| POMPANO                    |     1.2383156| CAULIFLOWER            |     2.7554996|
| PUMPKIN LEAVES              |     0.6797342| SHORTENING FRYING HVY DUTY |     0.7173608| CASSAVA                |     1.9565861|
| JELLYFISH                   |     0.0739238| WINGED BNS                 |     0.0738863| ARUGULA                |     0.1508609|
| SHORTENING INDUSTRIAL       |     0.7069000| ASPARAGUS                  |     1.5894518| SNAIL                  |     0.4676258|
| WOCAS                       |     0.3102183| MUSHROOMS                  |     1.4422596| CHRYSANTHEMUM LEAVES   |     3.1333836|
| PATE                        |     0.1262494| VERMICELLI                 |     0.8148980| SHORTENING CAKE MIX    |     1.2502040|
| TOFU                        |     2.1707253| SISYMBRIUM SP. SEEDS       |     0.3700221| WATER                  |    16.2666469|
| CHERVIL                     |     0.4294533| WATER                      |    18.1118232| SHAD                   |     1.9169717|
| EGG                         |     0.7080428| WHALE                      |     0.2832391| AGAVE                  |     0.4107375|
| WHEY                        |     0.7475162| TARO                       |     2.3656092| CARIBOU                |     0.3151651|
| WATER                       |    22.2304946| NA                         |            NA| HEADCHEESE             |     1.1110998|
| ALMONDS                     |     0.0279348| NA                         |            NA| NA                     |            NA|
| EDAMAME                     |     0.2348668| NA                         |            NA| NA                     |            NA|
| FLATFISH (FLOUNDER&SOLE SP) |     0.2322612| NA                         |            NA| NA                     |            NA|

``` r
knitr::kable(all_results_print[,7:14])
```

| Day 4 food       |  Day 4 amt(g)| Day 5 food       |  Day 5 amt(g)| Day 6 food                   |  Day 6 amt(g)| Day 7 food            |  Day 7 amt(g)|
|:-----------------|-------------:|:-----------------|-------------:|:-----------------------------|-------------:|:----------------------|-------------:|
| HICKORYNUTS      |     0.1959211| CHEESE PRODUCT   |     0.0125131| SCRAPPLE                     |     2.5781675| ESCAROLE              |     0.4324685|
| INF FORMU        |     0.5827061| CORN PUDD        |     4.0765586| CANADIAN BACON               |     0.0658988| GAME MEAT             |     0.1593473|
| SEA BASS         |     1.6068014| SAUERKRAUT       |     0.2781897| HEALTHY REQUEST              |     0.2003501| MACADAMIA NUTS        |     0.5811086|
| FUNGI            |     0.0988975| SOY FLOUR        |     0.2003473| NOPALES                      |     3.4622969| CREAM PUFF            |     0.6095089|
| CREAM PUFF SHELL |     0.7277610| PARSNIPS         |     1.5398622| SHORTENING FRYING (HVY DUTY) |     0.8106305| SESAME SD KRNLS       |     0.2812279|
| PURSLANE         |     4.0724646| LOBSTER          |     1.1202658| SWORDFISH                    |     0.9291938| MULLET                |     1.0863113|
| STURGEON         |     0.2693793| ENDIVE           |     0.4765279| MALABAR SPINACH              |     0.5764087| SOUR CREAM            |     0.6930739|
| MUFFIN           |     0.3154898| WHITEFISH        |     0.9194460| PUMPKIN&SQUASH SEEDS         |     0.0812989| TOMATO PRODUCTS       |     1.7919978|
| INCAPARINA       |     0.0747226| BAMBOO SHOOTS    |     2.1618714| CRAYFISH                     |     1.1361865| BREAKFAST ITEMS       |     0.6773985|
| PEPPERIDGE FARM  |     0.0172388| WATER            |    16.3161109| ALFALFA SEEDS                |     3.1756455| SALMON                |     0.9627659|
| FROZ NOVLT       |     0.2139054| SESAME MEAL      |     0.0479966| WATER                        |    12.8968354| INFFORMULA            |     0.5076957|
| SHORTENING       |     1.2101485| ROE              |     0.0763003| JERUSALEM-ARTICHOKES         |     3.4490694| WATER                 |    13.2241611|
| SOYBEANS         |     1.9344143| BRATWURST        |     0.5191258| VEGETABLE JUC                |     0.0122368| PUMPKIN FLOWERS       |     3.6553506|
| CAVIAR           |     0.1126143| SAVORY           |     0.1586049| BALSAM-PEAR (BITTER GOURD)   |     1.3770464| CATTAIL               |     5.6551665|
| OLIVES           |     0.4471667| SHORTENING BREAD |     1.3589468| NA                           |            NA| HAZELNUTS OR FILBERTS |     0.3946945|
| WATER            |    17.7783711| DOCK             |     1.9370208| NA                           |            NA| NA                    |            NA|
| ACEROLA JUICE    |     0.0104963| NA               |            NA| NA                           |            NA| NA                    |            NA|
| WATERCHESTNUTS   |     1.5851081| NA               |            NA| NA                           |            NA| NA                    |            NA|
| OYSTER           |     0.0378869| NA               |            NA| NA                           |            NA| NA                    |            NA|
