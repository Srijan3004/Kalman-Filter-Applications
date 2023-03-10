library(quantmod)
library(plotly)

# Get Apple stock prices from Yahoo Finance from 2018
apple_prices_xts <- getSymbols("AAPL", auto.assign = FALSE, from = "2018-01-01")[, 1:4]

# Define Kalman filter parameters
transition_model <- 1
transition_error <- 0.5
measurement_model <- 1
measurement_error <- 0.8

# Create empty vectors to store results
filtered_state_estimates <- xts(rep(NA, nrow(apple_prices_xts)), order.by = index(apple_prices_xts))
filtered_state_errors <- rep(NA, nrow(apple_prices_xts))

# Initialize filter
filtered_state_estimate <- as.numeric(apple_prices_xts[1, "AAPL.Open"])
filtered_state_error <- 1

# Run filter over stock prices
for (i in 1:nrow(apple_prices_xts)) {
  
  # Predict next state
  predicted_state_estimate <- transition_model * filtered_state_estimate
  predicted_state_error <- sqrt(transition_model^2 * filtered_state_error^2 + transition_error^2)
  
  # Update state estimate based on measurement
  measurement <- as.numeric(apple_prices_xts[i, "AAPL.Open"])
  innovation <- measurement - measurement_model * predicted_state_estimate
  innovation_error <- sqrt(measurement_model^2 * predicted_state_error^2 + measurement_error^2)
  kalman_gain <- predicted_state_error^2 * measurement_model / (innovation_error^2 + predicted_state_error^2 * measurement_model^2)
  filtered_state_estimate <- predicted_state_estimate + kalman_gain * innovation
  filtered_state_error <- (1 - kalman_gain * measurement_model) * predicted_state_error
  
  # Store results
  filtered_state_estimates[i] <- filtered_state_estimate
  filtered_state_errors[i] <- filtered_state_error
}

# Create plotly plot
apple_prices <- as.data.frame(apple_prices_xts)
filtered_prices <- data.frame(date = index(filtered_state_estimates), price = as.numeric(filtered_state_estimates))

# Create a new plot with the x-axis and y-axis labeled
plot(index(apple_prices_xts), apple_prices_xts$AAPL.Open, type = "l", col = "black", xlab = "Date", ylab = "Price", main = "Apple stock prices and Kalman filter predictions")

# Add the filtered estimates as a blue line
lines(index(filtered_state_estimates), filtered_state_estimates, col = "blue")

# Add a legend to the plot
legend("topright", legend = c("Actual Apple price", "Kalman filter predicted"), col = c("black", "blue"), lty = 1, cex = 0.8)

library(quantmod)

# Get Apple stock price for today
getSymbols("AAPL", auto.assign = TRUE)
today_price <- AAPL$AAPL.Open[length(AAPL$AAPL.Open)]

# Define Kalman filter parameters
transition_model <- 1
transition_error <- 0.5
measurement_model <- 1
measurement_error <- 0.8

# Initialize filter
filtered_state_estimate <- as.numeric(tail(apple_prices_xts$AAPL.Open, 1))
filtered_state_error <- 1

# Predict next state for tomorrow
predicted_state_estimate <- transition_model * filtered_state_estimate
predicted_state_error <- sqrt(transition_model^2 * filtered_state_error^2 + transition_error^2)

# Predict stock price for tomorrow using Kalman filter
kalman_predicted_price <- predicted_state_estimate + predicted_state_error

# Print the predicted stock price for tomorrow
cat("The predicted stock price for tomorrow is", kalman_predicted_price)