
      "The aim of this excericse was to create an effiecient frontier, à la Markowitz, by randomly generating 10000 portfolios.
      The efficient frontier can be used to find the optimal combination of asstes that maximise the return of the portfolio, while minimising
      the risk (standart deviation). These efficient portfolios are located on the upwards sloping outer rim of the hyperbola, this part depicts
      the efficient frontier."

# Static vectors names

stock_names <- c("GSK", "BP", "Unilever", "Rolls Royce")
weight_names <- c("weight1", "weight2", "weight3", "weight4")
risk_and_return_names <- c("Std.", "Exp. Return")

# Data 

variance_data <- c(0.00214, 0.00079, 0.00074, 0.00108, 0.00079, 0.00523, 0.00117, 0.00204, 0.00074, 0.00117, 0.00239, 0.00125, 0.00108, 0.00204, 0.00125, 0.00486)
portfolio_weights <- c(0.25, 0.25, 0.25, 0.25, 0, 0, 1, 0, 0.5, 0.5, 0, 0)
random_weights_simulation <- c(rnorm(40000, mean = 0, sd = 1))
mean_data <- c(0.0064, 0.0046, 0.0119, 0.0124)

# Defining matrices

random_weights_matrix <- matrix(random_weights_simulation, nrow = 10000, ncol = 4, byrow = TRUE)
portfolio_weights_matrix <- matrix(portfolio_weights, nrow = 3, byrow = TRUE)
V <- matrix(variance_data, nrow = 4, byrow = TRUE, dimnames = list(stock_names, stock_names))
mean_matrix <- matrix(mean_data, nrow = 1, byrow = TRUE)
risk_and_return <- matrix(, nrow = 10000, ncol = 2, byrow = TRUE)

# Naming the columns

colnames(portfolio_weights_matrix) <- weight_names
colnames(random_weights_matrix) <- weight_names
colnames(mean_matrix) <- stock_names
colnames(risk_and_return) <- risk_and_return_names

# Standardising the weights

row_sums <- matrix(rowSums(random_weights_matrix), nrow = 10000, byrow = TRUE)

system.time(for(row in 1:nrow(random_weights_matrix)) {
  
  row_sum <- row_sums[row,]
  
    for(colu in 1:ncol(random_weights_matrix)){
      
      random_weights_matrix[row, colu] <- random_weights_matrix[row, colu]/row_sum
      
    }
}
)


# Calculating the portfolio standart deviation and expected return.

for(row in 1:nrow(risk_and_return)){
  
  risk_and_return[row, 1] <- (matrix(random_weights_matrix[row,], nrow = 1, byrow = TRUE) %*% (V %*% t(matrix(random_weights_matrix[row,], nrow = 1, byrow = TRUE))))^0.5 
  risk_and_return[row, 2] <- mean_matrix %*% t(matrix(random_weights_matrix[row,], nrow = 1, byrow = TRUE))
  
}

# Plotting

plot(risk_and_return, xlim = c(0.03, 0.15), ylim = c(0, 0.025), main = "Efficient Frontier", pch=1, col = "orange")

