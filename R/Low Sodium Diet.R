

# LOW SODIUM, CARBS AND CHOLESTEROL

library(tidyverse)
library(lpSolve)


nutriets <- read_csv("data/nutrient-categories.csv")
constraints <- read_csv("data/nutrient-constraints.csv") %>% select(`Original Nutrient Name`:Unit)

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
                          , "CISCO", "INF FORMULA","BEVERAG","TODDL FORM","SHORTENING FRYING (HVY DUTY)"
                          , "SHORTENING INDUSTRIAL")
  ) %>% 
  filter(!grepl(c("BUTTER|OIL|MARGARINE|FAT|LARD"),Category, ignore.case = TRUE))

# nutrients are in grams of nutrient per 100g of food, so reducing to nutrients per 1 gram of food
nutriets <- nutriets %>% mutate_at(vars(`Water_(g)`:`Vit_A_(g)`), funs(./100))

# Run the LP --------------------------------------------------------------

# how many days to plan? (will loop over)
all_days <- 1


foods_used <- c("WATER") # list of foods already used (water is a placeholder)
all_results <- list() # store the meal plans
keep_LPs <- list() # store the LP results
for(day in 1:all_days){
  # day <- 1
  
  # for development purposes, sample the nutrients list. 
  sample_size <- 1
  set.seed(1)
  sample_nutriets <- sample_frac(nutriets, sample_size)
  
  # remove foods that have already been used other than water
  foods_used <- foods_used[foods_used!="WATER"]
  sample_nutriets <- sample_nutriets %>% filter(!Category %in% foods_used)
  
  # set objective: minimize carbohydrates
  objective_function <- sample_nutriets$`Sodium_(g)`+sample_nutriets$`Carbohydrt_(g)`+sample_nutriets$`Cholestrl_(g)`
  
  # initiate LHS matrix
  Left_Hand_Side <- matrix(numeric(nrow(sample_nutriets)), nrow = 1)
  
  # for each constraint, create a row in the LHS matrix
  for(i in 1:nrow(constraints)){
    # i <- 1
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
  
  # there are both lower and upper bounds on the constraints
  constraint_directions <- c(rep(">=", nrow(Left_Hand_Side))
                             ,rep("<=", nrow(Left_Hand_Side))
  )
  
  # Lowe and Upper bounds for RHS
  Right_Hand_Side <- c(constraints$`Lower Bound`
                       , constraints$`Upper Bound`)
  
  # duplicate the LHS matrix since we have both upper and lower bounds
  Left_Hand_Side_Lower <- Left_Hand_Side
  rownames(Left_Hand_Side_Lower) <- paste0(rownames(Left_Hand_Side),"_Lower")
  Left_Hand_Side_Upper <- Left_Hand_Side
  rownames(Left_Hand_Side_Upper) <- paste0(rownames(Left_Hand_Side),"_Upper")
  
  Left_Hand_Side_all <- rbind(Left_Hand_Side_Lower, Left_Hand_Side_Upper)
  
  # check the rows and columns match up:
  all_equal(nrow(Left_Hand_Side_all)
            , length(Right_Hand_Side)
            , length(constraint_directions)
  )
  
  all_equal(length(objective_function)
            , ncol(Left_Hand_Side_all)
  )
  
  
  # time benchmarks:
  # non-binary:
  # 1% of nutrients: no solution
  # 10% of nutrients: <1 sec
  # 20% of nutrients: <1 sec
  # 100% in 0.03 seconds
  
  
  # binary:
  # 1% of nutrients: no solution
  # 10% of nutrients: no solution
  # 20% of nutrients: 5.03 mins
  # 25% of nutrients: 5.03 mins
  
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
  
  # compute results
  results <- data_frame(
    Food = sample_nutriets$Category[LP_Solved$solution>0]
    ,`Amount(g)` = LP_Solved$solution[LP_Solved$solution>0]
  )
  results$Day <- as.integer(day)
  
  # store results
  keep_LPs[[day]] <- LP_Solved
  all_results[[day]] <- results
  foods_used <- c(foods_used, results$Food)
  
  message("DAY ",day, ": ", length(results$Food)," items selected. ",scales::percent(sample_size), " of data used. LP completed in ",round(lp_time,2), units(lp_time))
}


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





# sensitivity analysis ----------------------------------------------------

# Question: In the event the constraints (i.e., the guidlines issued by IOM) are changed, how much 
# can the RDI's change without alterting the meal plans?


# objective functions 
keep_LPs %>% map(~.x$objval)
lp_analysis <- keep_LPs[[1]]

# The dual price gives the improvement in the objective function if the constraint is relaxed by one unit
# "Dual price", also known as Shadow Price

# In lpSolve,  the dual values for the constraints and the variables are
# combined, constraints coming first...
lp_analysis$const.count + # count of constraints plus
  lp_analysis$x.count == # count of variables equal
  length(lp_analysis$duals) # the duals

# the constrints were transposed
lp_analysis$const.count == ncol(lp_analysis$constraints)
# x count is just number of objectives (number of food items)
lp_analysis$x.count == length(lp_analysis$objective)



# The shadow price for a constraint indicates the amount by which the 
# objective function value changes given a unit increase in the RHS 
# value of the constraint, assuming all other coefficients remain constant. 
# If a shadow price is positive, a unit increase in the RHS value of the 
# associated constraint results in an increase in the optimal objective function value.



## Sensitivity Report
# 1) Objective
Objective_df <- data_frame(Name = "Minimization of Carbs, Sodium and Cholesterol"
                           , final_value = lp_analysis$objval)

# 2) variables

variable_duals <- data_frame(variable = sample_nutriets$Category
                             , final_value = lp_analysis$solution
                             , objective_coeficiet = objective_function
                             , allowable_increase = lp_analysis$sens.coef.to[(lp_analysis$const.count+1):length(lp_analysis$duals)]
                             , allowable_decrease = lp_analysis$sens.coef.from[(lp_analysis$const.count+1):length(lp_analysis$duals)]
) %>% 
  mutate(Reduced_Cost = objective_coeficiet - lp_analysis$duals[(lp_analysis$const.count+1):length(lp_analysis$duals)]) %>% 
  select(variable, final_value, Reduced_Cost, everything())

variable_duals %>% filter(final_value>0)


# 3) constraints
final_nutrients <- 
  sample_nutriets %>% 
  mutate(solution = lp_analysis$solution) %>% 
  mutate_at(vars(`Water_(g)`:`Vit_A_(g)`), funs(.*solution)) %>% 
  filter(solution>0) %>% 
  select(-solution, -Category) %>% 
  summarise_all(sum) %>% 
  gather(constraint, final_value)

all_final_values <- bind_rows(
  final_nutrients %>% mutate(constraint = paste0(constraint,"_Lower"))
  , final_nutrients %>% mutate(constraint = paste0(constraint,"_Upper"))
  )

Constraint_Sensitivity <- data_frame(constraint = colnames(lp_analysis$constraints)
                                     , shadow_price = lp_analysis$duals[1:lp_analysis$const.count]
                                     , constraint_RHS = Right_Hand_Side
                                     , allowable_increase = lp_analysis$duals.to[1:lp_analysis$const.count]
                                     , allowable_decrease = lp_analysis$duals.from[1:lp_analysis$const.count]
                                     ) %>% 
  left_join(all_final_values, by = "constraint") %>% 
  select(constraint, final_value, everything())

non_negative_sensitivities <- Constraint_Sensitivity %>% filter(shadow_price>0)












