library(lpSolveAPI)
library(dplyr)

create_model = function(n_variables) {
  return(make.lp(ncol = n_variables))
}

set_direction = function(model) {
    lp.control(model, sense = "max")
}

set_objective = function(model, inputs, outputs, subject_garage_id) {
  browser()
  objective_coeffs <- c(as.numeric(outputs[subject_garage_id, 2:3]))
  set.objfn(model, objective_coeffs, indices = c(3, 4))
}

add_unity_constraint = function(model, inputs, outputs, subject_garage_id) {
  constraint_coeffs <- c(as.numeric(inputs[subject_garage_id, 2:3]), 0, 0)
  add.constraint(model, constraint_coeffs, "=", 1)
}

add_other_constraints = function(model, inputs, outputs) {
  for(i in 1:nrow(inputs)) {
    constraint_coeffs <- c(-as.numeric(inputs[i, 2:3]),
                           as.numeric(outputs[i, 2:3]))
    add.constraint(model, constraint_coeffs, "<=", 0)

  }
}

run_optimization <- function() {

  inputs <- read.csv("inputs-ex2.csv", sep = ";")
  outputs <- read.csv("outputs-ex2.csv", sep = ";")

  for(garage in 1:4)
  {
    model <- create_model(4)
    set_direction(model)
    set_objective(model, inputs, outputs, garage)
    add_unity_constraint(model, inputs, outputs, garage)
    add_other_constraints(model, inputs, outputs)
    solve(model)
    efficiency = get.objective(model)
    print(paste(inputs[garage, 1], ": ", efficiency, sep=""))
  }

}

