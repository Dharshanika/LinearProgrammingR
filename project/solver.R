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

  inputs <- read.csv("inputs.csv", sep = ";", header = TRUE)
  outputs <- read.csv("outputs.csv", sep = ";", header = TRUE)

  stores_number <- nrow(inputs) # k = 19 
  inputs_number <- ncol(inputs) - 1 # m = 5
  outputs_number <- ncol(outputs) - 1 # n = 2 
  combined_changes <- data.frame()
  combined_efficiency <- data.frame()
  
  model_size <- inputs_number + outputs_number + 1
  for(store in 1:stores_number) {
    model <- create_model(model_size)
    set_direction(model)
    set_objective(model, inputs, outputs, store)
    add_unity_constraint(model, inputs, outputs, store)
    add_other_constraints(model, inputs, outputs)
    solve(model)
    efficiency <- get.objective(model)

    combined_efficiency <- combined_efficiency %>% 
      bind_rows(
        data.frame(
          store = inputs[store, 1], 
          efficiency = efficiency
        )
      )

    print(paste(inputs[store, 1], ": ", efficiency, sep = ""))
    
    if (efficiency < 1) {
        # print changes
        for (input in (1:inputs_number) + 1) {
          print(paste("input", input, "of", inputs[store, 1], "should increase by:", (1 - efficiency) * inputs[store, input], sep = " "))
        }
        
        # prepare merged data frame with necessary changes for ineffective
        store_changes <- inputs[store, ] # get whole row for specific store
        store_changes[-1] <- store_changes[-1] * (1 - efficiency) # multiply all columns except name by (1-efficiency)
        combined_changes <- dplyr::bind_rows(combined_changes, store_changes)
      }
  }  
  
  write.csv(combined_changes, "combined_changes.csv")
  return(list(combined_efficiency, combined_changes))
}
