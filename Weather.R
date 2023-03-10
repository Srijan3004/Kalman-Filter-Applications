library(ggplot2)


# Load weather data
data= read.csv(file.choose())
data
# Define the Kalman filter model parameters
sigma_obs <- 0.4 # Observation noise standard deviation
sigma_state <- 0.5 # State transition noise standard deviation
T <- 1 # Time step
a=runif(366,0,1/3)
b=runif(366,0,1/3)
c=runif(366,0,1/3)
d=1-a-b-c
mat=cbind(data$MinTemp,data$MaxTemp,data$Temp9am,data$Temp3pm)
weight=cbind(a,b,c,d)
newmat=mat %*% t(weight)
feel=diag(newmat)
plot(feel, color="green")
# Initialize the state estimate and covariance matrix
x_hat <- feel[1]
x_hat# Initial state estimate
P <- 1 # Initial covariance matrix

# Create vectors to store the predicted and filtered estimates
x_pred <- numeric(length = nrow(data))
x_filt <- numeric(length = nrow(data))

# Loop over the observations and perform the Kalman filter updates
for (i in 1:nrow(data)) {
  # Prediction step
  x_hat_pred <- x_hat # Predicted state estimate
  P_pred <- P + sigma_state^2 # Predicted covariance matrix
  
  # Filtering step
  y <- feel[i] # Observation
  K <- P_pred / (P_pred + sigma_obs^2) # Kalman gain
  x_hat <- x_hat_pred + K * (y - x_hat_pred) # Updated state estimate
  P <- (1 - K) * P_pred # Updated covariance matrix
  
  # Save the predicted and filtered estimates
  x_pred[i] <- x_hat_pred
  x_filt[i] <- x_hat
}

# Add the predicted and filtered estimates to the data frame
data$prediction <- x_filt
data$forecast <- x_pred

res = cbind(feel, data$prediction, data$forecast)
res
#Plot
df = data.frame(index=c(1:length(data$prediction)),
                max = data$MaxTemp, min = data$MinTemp,
                tem3=data$Temp3pm, tem9=data$Temp9am,
                pred = data$prediction,
                act = feel,
                fore=data$forecast)

p1 <- ggplot(df, aes(x=index, y=act))+geom_line(color="red")+
      geom_line(aes(y = max), color = "black")+geom_line(aes(y =min), color = "green")+
      geom_line(aes(y = tem3), color="purple")+geom_line(aes(y=tem9), color= "yellow")+
      xlab("Time Index") + ylab("Temperature")+ ggtitle("Measurements of Temperatures")
p1
########################################################################################
#Animation

library(ggplot2)
library(gganimate)

# Create a dummy data frame with random values
df <- data.frame(
  index = 1:366,
  act = feel,
  pred = data$prediction,
  fore = data$forecast
)

# Create the basic plot
p <- ggplot(df, aes(x = index, y = act))+
  geom_line(color = "green")+
  geom_line(aes(y = pred), color = "blue")+
  geom_line(aes(y = fore), color = "red")+
  xlab("Time Index") + ylab("Temperature") +
  ggtitle("Weather Prediction")

# Add animation using gganimate
p_animated <- p + transition_reveal(index)

# Preview the animation
animate(p_animated, nframes = 100)
