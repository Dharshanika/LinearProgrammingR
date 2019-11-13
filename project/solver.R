# problem: BCC Stores

library(lpSolveAPI)
library(dplyr)

create_model <- function(n_variables) {
  return(make.lp(ncol = n_variables))
}

set_direction <- function(model) {
    lp.control(model, sense = "max")
}

set_objective <- function(model, inputs, outputs, subject_store_id) {
  # last element in vector - additional variable u_0 
  objective_coeffs <- c(as.numeric(outputs[subject_store_id, 2:3]), 0) 
  set.objfn(model, objective_coeffs, indices = c(6, 7, 8))
}

add_unity_constraint <- function(model, inputs, outputs, subject_store_id) {
  # last element in vector - additional variable u_0   
  constraint_coeffs <- c(as.numeric(inputs[subject_store_id, 2:6]), 0, 0, 0)
  add.constraint(model, constraint_coeffs, "=", 1)
}

add_other_constraints <- function(model, inputs, outputs) {
  for(i in 1:nrow(inputs)) {
    # last element in vector - additional variable u_0 
    constraint_coeffs <- c(-as.numeric(inputs[i, 2:6]),
                           as.numeric(outputs[i, 2:3]), 0)
    add.constraint(model, constraint_coeffs, "<=", 0)
  }
}

run_optimization <- function() {

  inputs <- read.csv("inputs.csv", sep = ";", header=TRUE)
  outputs <- read.csv("outputs.csv", sep = ";", header=TRUE)

  k <- 19
  m <- 5
  n <- 2

  for(store in 1:k) {
    model <- create_model(1+m+n)
    set_direction(model)
    set_objective(model, inputs, outputs, store)
    add_unity_constraint(model, inputs, outputs, store)
    add_other_constraints(model, inputs, outputs)
    solve(model)
    efficiency <- get.objective(model)
    print(paste(inputs[store, 1], ": ", efficiency, sep = ""))
  }

}

