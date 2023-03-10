library(ggplot2)
library(kalmanfilter)
library(maxLik)

library(data.table)
library(gridExtra)
library(rgl)

dt <- 0.01 #time step
# Define the initial state variables
x <- matrix(c(0, 0, 0, 0), nrow=4, ncol=1) # initial state vector (x position, x velocity, y position, y velocity)
P <- diag(4) # initial state covariance matrix
F <- matrix(c(1, -dt, 0, 0,
              dt, 1, 0, 0,
              0, 0, 1, -dt,
              0, 0, dt, 1), nrow=4, ncol=4) # state transition matrix
Q <- diag(4)*0.0001 # process noise covariance matrix
H <- matrix(c(1, 0,
              0, 0,
              0, 1,
              0, 0), nrow=2, ncol=4) # observation matrix
R <- diag(2)*0.1 # measurement noise covariance matrix

# Define the simulation parameters

t <- seq(0, 100, by=dt) # time vector
N <- length(t) # number of time steps

# Define the true state of the object
true_pos <- sin(t) # x position
true_vel <- cos(t) # x velocity
true_ypos <- cos(t) # y position
true_yvel <- -sin(t) # y velocity
#image(true_pos, col = grey(seq(0, 1, length.out = 256)))
# Generate noisy measurements of the true state
pos_meas <- true_pos + rnorm(N, sd=0.1)
ypos_meas <- true_ypos + rnorm(N, sd=0.1)

# Perform the Kalman Filter prediction
pos_pred <- numeric(N)
ypos_pred <- numeric(N)
for (i in 1:N) {
  # Prediction step
  x_pred <- F %*% x
  P_pred <- F %*% P %*% t(F) + Q
  
  # Update step
  y <- c(pos_meas[i], ypos_meas[i]) - H %*% x_pred
  S <- H %*% P_pred %*% t(H) + R
  K <- P_pred %*% t(H) %*% solve(S)
  x <- x_pred + K %*% y
  P <- (diag(4) - K %*% H) %*% P_pred
  
  # Save the predicted state
  pos_pred[i] <- x[1]
  ypos_pred[i] <- x[3]
}

# Plot the actual and predicted positions
df <- data.frame(t=t, true_pos=true_pos, 
                 pos_meas=pos_meas, 
                 pos_pred=pos_pred, 
                 true_ypos=true_ypos, 
                 ypos_meas=ypos_meas, 
                 ypos_pred=ypos_pred)
p1 <- ggplot(df, aes(x=t, y=true_pos)) +
  geom_line(color="blue", size=4) + 
  geom_point(aes(y=pos_meas), color="red", size=1) +
  geom_point(aes(y=pos_pred), color="green", size=1) +
  xlab("Time") + ylab("Position") + ggtitle("X position")
p2 <- ggplot(df, aes(x=t, y=true_ypos)) + geom_line(color="blue", size=4) + geom_point(aes(y=ypos_meas), color="red", size=1) + geom_point(aes(y=ypos_pred), color="green", size=1) + xlab("Time")+ ylab("Position")+ggtitle("Y position")
grid.arrange(p1, p2)

plot3d(pos_pred, ypos_pred, t, col="green", xlab="X Position", ylab="Y Position", zlab="Time", lwd = 2)
plot3d(true_pos, true_ypos, t, col="blue", xlab="X Position", ylab="Y Position", zlab="Time", lwd = 2)
plot3d(pos_meas, ypos_meas, t, col="red", xlab="X Position", ylab="Y Position", zlab="Time", lwd = 2)

plot3d(pos_pred, ypos_pred, t, col="green", xlab="X Position", ylab="Y Position", zlab="Time", lwd = 2)
plot3d(true_pos, true_ypos, t, col="blue", add=TRUE)
plot3d(pos_meas, ypos_meas, t, col="red", add=TRUE)


library(animation)

# Define the function to generate the plots
plot_func <- function(i) {
  # Extract the data for the current time step
  pos_pred_i <- pos_pred[i]
  ypos_pred_i <- ypos_pred[i]
  true_pos_i <- true_pos[i]
  true_ypos_i <- true_ypos[i]
  pos_meas_i <- pos_meas[i]
  ypos_meas_i <- ypos_meas[i]
  t_i <- t[i]
  
  # Create the 3D plots
  plot3d(pos_pred[1:i], ypos_pred[1:i], t[1:i], col="green", xlab="X Position", ylab="Y Position", zlab="Time", lwd = 2)
  plot3d(true_pos[1:i], true_ypos[1:i], t[1:i], col="blue", add=TRUE)
  plot3d(pos_meas[1:i], ypos_meas[1:i], t[1:i], col="red", add=TRUE)
  
  # Create the 2D plots
  #df <- data.frame(t=t[1:i], true_pos=true_pos[1:i], pos_meas=pos_meas[1:i], pos_pred=pos_pred[1:i], true_ypos=true_ypos[1:i], ypos_meas=ypos_meas[1:i], ypos_pred=ypos_pred[1:i])
  #p1 <- ggplot(df, aes(x=t, y=true_pos)) + geom_line(color="blue", linewidth=4) + geom_point(aes(y=pos_meas), color="red", size=1) + geom_point(aes(y=pos_pred), color="green", size=1) + xlab("Time") + ylab("Position") + ggtitle("X position")
  #p2 <- ggplot(df, aes(x=t, y=true_ypos)) + geom_line(color="blue", size=4) + geom_point(aes(y=ypos_meas), color="red", size=1) + geom_point(aes(y=ypos_pred), color="green", size=1) + xlab("Time")+ ylab("Position")+ggtitle("Y position")
  #p1
}

# Generate the animation
saveGIF({
  for (i in 1:N) {
    plot_func(i)
    ani.pause(0.1)
  }
}, movie.name = "kalman_filter_animation.gif", interval = 100, ani.width = 800, ani.height = 600)
