library(plyr); library(dplyr); source("methods.R")

# Grid of gain parameters
param_grid = list(
  A=3:7,
  B=1:50,
  G=1:30
  ) %>% 
  expand.grid()

# Starting synapse numbers
C_vals = list(
  C1 = 1.0,
  C2 = 0.8,
  C3 = 0.25,
  C4 = 0.25,
  C5 = 0.3,
  C6 = 0.1,
  C7 = 0.8
)

# Simulation function
simulation = function(C_vals, param_grid) {
  sapply(1:nrow(param_grid), function(i) {
    do.call(simulate_grid, c(param_grid[i, ], C_vals))
    }) %>%
      pracma::detrend()
}

set.seed(2342)

# Control simulation
sim_result = simulation(C_vals, param_grid)

# Simulation with increase in i to e synapses
C_vals_ie = C_vals
C_vals_ie$C4 = C_vals$C4 * 1.3 # slow i to e
C_vals_ie$C7 = C_vals$C7 * 1.3 # fast i to e

sim_result_ie = simulation(C_vals_ie, param_grid)

# Simulation with increase in both i to e and i to i synapses
C_vals_ii = C_vals_ie
C_vals_ii$C6 = C_vals$C6 * 1.3 # slow i to fast i

sim_result_ii = simulation(C_vals_ii, param_grid)
