set.seed(123)

# Define model parameters
n_obs=100
initial_price <- 100
drift <- 0.05
volatility <- 0.2
time_horizon <- 252  # Number of trading days in a year
time_steps <- 100  # Number of time steps in simulation

# Calculate the daily drift and volatility
daily_drift <- drift / time_horizon
daily_volatility <- volatility / sqrt(time_horizon)

# Simulate the log returns
log_returns <- rnorm(time_steps, mean = daily_drift, sd = daily_volatility)

# Calculate the cumulative sum of the log returns
cumulative_log_returns <- cumsum(log_returns)

# Calculate the stock prices
stock_prices <- initial_price * exp(cumulative_log_returns)

# Define the system dynamics
system_dynamics <- list(F = 1, Q = 0.05)

# Set the measurement errors and measurement models to test
measurement_errors <- c(0.9, 0.2,1.8)
measurement_models <- c(1)

# Create a 3x3 layout for the plots
par(mfrow=c(3,1))

# Loop through the different combinations of measurement errors and models
for (i in 1:length(measurement_errors)) {
  for (j in 1:length(measurement_models)) {
    
    # Define the measurement model and measurement error
    measurement_model <- measurement_models[j]
    measurement_error <- measurement_errors[i]
    
    # Initialize the Kalman Filter
    initial_state <- log(initial_price)
    initial_state_covariance <- 1
    state_estimates <- numeric(n_obs)
    state_estimate_covariances <- numeric(n_obs)
    state_estimates[1] <- initial_state
    state_estimate_covariances[1] <- initial_state_covariance
    
    # Run the Kalman Filter
    for (k in 2:n_obs) {
      # Prediction step
      predicted_state <- system_dynamics$F * state_estimates[k-1]
      predicted_state_covariance <- system_dynamics$F^2 * state_estimate_covariances[k-1] + system_dynamics$Q
      
      # Update step
      kalman_gain <- predicted_state_covariance * measurement_model / (measurement_model^2 * predicted_state_covariance + measurement_error^2)
      state_estimates[k] <- predicted_state + kalman_gain * (log(stock_prices[k]) - measurement_model * predicted_state)
      state_estimate_covariances[k] <- (1 - kalman_gain * measurement_model) * predicted_state_covariance
    }
    
    # Plot the true stock prices and the estimated log prices
    # Plot the true stock prices and the estimated log prices
    plot(stock_prices,type="l", col = "blue", ylim = c(90, max(stock_prices)))
    lines(exp(state_estimates), col = "red")
    legend("topleft", legend = c("True price", "Estimated price"), col = c("blue", "red"), lty = 1)
    title(paste0("Model: ", measurement_model, ", Error: ", measurement_error))
  }
}

# Reset the plot layout
par(mfrow=c(1,1))

# Load the animation package
library(animation)

# Define a function to update the plot
update_plot <- function(i, state_estimates, stock_prices) {
  # Plot the true stock prices and the estimated log prices up to time i
  plot(stock_prices[1:i], type = "l", col = "blue", ylim = c(90, max(stock_prices)))
  lines(exp(state_estimates[1:i]), col = "red")
  legend("topleft", legend = c("True price", "Estimated price"), col = c("blue", "red"), lty = 1)
}

# Initialize the animation
saveGIF({
  for (i in 2:n_obs) {
    # Prediction step
    predicted_state <- system_dynamics$F * state_estimates[i-1]
    predicted_state_covariance <- system_dynamics$F^2 * state_estimate_covariances[i-1] + system_dynamics$Q
    
    # Update step
    kalman_gain <- predicted_state_covariance * measurement_model / (measurement_model^2 * predicted_state_covariance + measurement_error^2)
    state_estimates[i] <- predicted_state + kalman_gain * (log(stock_prices[i]) - measurement_model * predicted_state)
    state_estimate_covariances[i] <- (1 - kalman_gain * measurement_model) * predicted_state_covariance
    
    # Update the plot
    update_plot(i, state_estimates, stock_prices)
    
    # Add the current frame to the animation
    ani.pause()
  }
}, movie.name = "stock_price_animation.gif", interval = 0.1, ani.width = 800, ani.height = 600)