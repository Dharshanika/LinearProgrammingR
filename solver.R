# install.packages("lpSolveAPI")
library(lpSolveAPI)
library(dplyr)

data <- readr::read_delim(delim = ";", file = "calculators.csv")

model <- make.lp(ncol = 2)
lp.control(model, sense = "max")
set.type(model, c(1,2), "integer")

objective_coeffs <- data[1, 2:3] %>% as.numeric()
set.objfn(lprec = model, obj = objective_coeffs)

# add constraints
# params :
# 1. model
# 2. coefficients for variables
# 3. constraint type
# 4. constraint value
add.constraint(model, c(1, 0), ">=", data[2,2])
add.constraint(model, c(1, 0), "<=", data[3,2])

add.constraint(model, c(0, 1), ">=", data[2,3])
add.constraint(model, c(0, 1), "<=", data[3,3])

add.constraint(model, c(1,1), ">=", 200)

