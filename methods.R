# Sigmoid function
Sigmoid = function(v) r = 5 / (1 + exp(0.56 * (6 - v)))

# Take a step
update = function(y, interval, A, B, G, C_all) {
  a = 100
  b = 50
  g = 350
  C = 135
  
  C1 = C_all$C1 * C
  C2 = C_all$C2 * C
  C3 = C_all$C3 * C
  C4 = C_all$C4 * C
  C5 = C_all$C5 * C
  C6 = C_all$C6 * C
  C7 = C_all$C7 * C
  
  y_i_plus1 = rep(0, 10)
  y_i_plus1[1] = y[1] + y[6] * interval
  y_i_plus1[2] = y[2] + y[7] * interval
  y_i_plus1[3] = y[3] + y[8] * interval
  y_i_plus1[4] = y[4] + y[9] * interval
  y_i_plus1[5] = y[5] + y[10] * interval
  y_i_plus1[6] = y[6] + (A * a * Sigmoid(y[2]-y[3]-y[4]) - 2 * a * y[6] - a^2 * y[1]) * interval
  y_i_plus1[7] = y[7] + (A * a * (rnorm(1, mean=90, sd=30) + C2 * Sigmoid(C1* y[1])) - 2 * a * y[7] - a^2 * y[2]) * interval
  y_i_plus1[8] = y[8] + (B * b * C4 * Sigmoid(C3 * y[1]) - 2 * b * y[8] - b^2 * y[3]) * interval
  y_i_plus1[9] = y[9] + (G * g * C7 * Sigmoid(C5 * y[1] - C6 * y[5])  - 2 * g * y[9] - g^2 * y[4]) * interval
  y_i_plus1[10] = y[10] + (B * b * Sigmoid(C3 * y[1]) - 2 * b * y[10] - b^2 * y[5]) * interval
  
  return(y_i_plus1)
}

# Main simulation
simulate = function(interval, duration, A, B, G, init_time, C_all) {
  updates = duration / interval
  init_updates = init_time / interval
  
  eeg = rep(0, updates)
  y = rep(0, 10)
  
  for (i in 1:init_updates) {
    y = update(y, interval, A[1], B[1], G[1], C_all)
  }
  
  for (i in 1:updates) {
    y = update(y, interval, A[i], B[i], G[i], C_all)
    eeg[i] = y[2] - y[3] - y[4]
  }
  
  return(eeg)
}

simulate_grid = function(A, B, G, C1, C2, C3, C4, C5, C6, C7) {
  C_all = list(C1=C1, C2=C2, C3=C3, C4=C4, C5=C5, C6=C6, C7=C7)
  duration = 5 # seconds
  A = rep(A, duration * 200)
  B = rep(B, duration * 200)
  G = rep(G, duration * 200)
  eeg = simulate(0.005, duration, A, B, G, 1, C_all)
  return(eeg)
}
