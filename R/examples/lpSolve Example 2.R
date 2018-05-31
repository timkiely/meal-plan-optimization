
library(lpSolve)
food <- data.frame(Food=c("Corn", "2% Milk", "Wheat Bread")
                   , CostPerServing=c(.18, .23, .05)
                   , VitaminA=c(107, 500, 0)
                   , Calories=c(72, 121, 65))

mod <- lp("min",  # min/max
          food$CostPerServing,  # Objective
          rbind(food$VitaminA, food$VitaminA, food$Calories, food$Calories),  # Constraint matrix
          c(">=", "<=", ">=", "<="),  # Constraint directions
          c(5000, 50000, 2000, 2500))

mod$objval

mod$solution