
# install.packages("lpSolve")
# install.packages("lpSolveAPI")

library(lpSolve)
library(lpSolveAPI)




# example 1 ---------------------------------------------------------------
rm(lprec)
lprec <- make.lp(nrow = 4 # number of constraints
                 , ncol = 4 # number of decision variables
                 , verbose = "neutral" 
                 )

set.objfn(lprec, c(1, 3, 6.24, 0.1))
add.constraint(lprec, c(0, 78.26, 0, 2.9), ">=", 92.3)
add.constraint(lprec, c(0.24, 0, 11.31, 0), "<=", 14.8)
add.constraint(lprec, c(12.68, 0, 0.08, 0.9), ">=", 4)
set.bounds(lprec, lower = c(28.6, 18), columns = c(1, 4))
set.bounds(lprec, upper = 48.98, columns = 4)
RowNames <- c("THISROW", "THATROW","THATROW", "LASTROW","COLFIVE", "COLSIX","COLSEVEN")
ColNames <- c("COLONE", "COLTWO", "COLTHREE", "COLFOUR")
dimnames(lprec) <- list(RowNames, ColNames)
## Lets take a look at what we have done so far.
lprec  # or equivalently print(lprec)

# Now lets solve the model.
solve(lprec)
get.objective(lprec)
get.variables(lprec)
get.constraints(lprec)
