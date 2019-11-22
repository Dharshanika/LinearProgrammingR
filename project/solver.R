library(lpSolveAPI)
library(dplyr)
library(purrr)
library(tibble)

create_model <- function(n_variables) {
  return(make.lp(ncol = n_variables))
}

set_direction <- function(model, direction = "max") {
    lp.control(model, sense = direction)
}

set_objective <- function(model, inputs, outputs, subject_store_id) {
  # last element in vector - additional variable u_0
  # the same element appears in all constraints
  objective_coeffs <- c(as.numeric(outputs[subject_store_id, 2:3]), 1) 
  set.objfn(model, objective_coeffs, indices = c(6, 7, 8))
}

add_unity_constraint <- function(model, inputs, outputs, subject_store_id) {
  constraint_coeffs <- c(as.numeric(inputs[subject_store_id, 2:6]), 0, 0, 0)
  add.constraint(model, constraint_coeffs, "=", 1)
}

add_other_constraints <- function(model, inputs, outputs) {
  for(i in 1:nrow(inputs)) {
    constraint_coeffs <- c(-as.numeric(inputs[i, 2:6]),
                           as.numeric(outputs[i, 2:3]), 1)
    add.constraint(model, constraint_coeffs, "<=", 0)
  }
}

run_optimization <- function() {

  inputs <- read.csv("inputs.csv", sep = ";", header = TRUE)
  outputs <- read.csv("outputs.csv", sep = ";", header = TRUE)

  stores_number <- nrow(inputs) 
  inputs_number <- ncol(inputs) - 1
  outputs_number <- ncol(outputs) - 1 
  proposed_changes <- tibble()
  
  model_size <- inputs_number + outputs_number + 1
  
  efficiency_tibble <- map_df(1:stores_number, function(store) {
    model <- create_model(model_size)
    set_direction(model, "max")
    set_objective(model, inputs, outputs, store)
    add_unity_constraint(model, inputs, outputs, store)
    add_other_constraints(model, inputs, outputs)
    solve(model)
    efficiency <- get.objective(model)

    # sometimes efficiency is extremely close to 1 but not equal to 
    # probably the reason is calculation error 
    if (round(efficiency, 5) < 1) {
      # prepare merged data frame with necessary changes for ineffective
      # and multiply all columns except name by (1 - efficiency)
      proposed_changes <<- inputs[store, ] %>% 
          mutate_at(vars(-X), ~(. * (1 - efficiency))) %>%
          bind_rows(proposed_changes)
    }
    
    return(tibble(store = inputs[store, 1], efficiency = efficiency))
  })  
  
  return(list(efficiency_tibble, proposed_changes))
}
