
library(ggplot2)

#Simulating data
x = rnorm(10000, 50000, 100)
z = x + rnorm(10000, -100, 1)
t = c(1:10000)

df <- data.frame(t=t, actual = x, measured = z)
q<-  ggplot(df, aes(x=t, y=actual))+geom_point(color = "blue", size = 1)+
  xlab("Time Index") + ylab("Actual Number of Cases")+ggtitle("Simulated Actual Data")
r<- ggplot(df, aes(x=t, y=measured))+geom_point(color = "red", size = 1)+
  xlab("Time Index") + ylab("Measured Number of Cases")+ggtitle("Measured Data")
p <- ggplot(df, aes(x=t, y=actual))+geom_line(color = "blue", size = 1)+
  geom_point(aes(y=measured), color = "red")


x_hat = z[1]
x_pred = numeric(10000)
x_filt = numeric(10000)
P <- 25

for(i in 1:10000)
{
  #prediction step
  x_hat_pred <- x_hat
  P_pred <- P + 1
  
  #Filtering Step
  y <- z[i]
  K <- P_pred / (P_pred + 1)
  x_hat <- x_hat_pred + K * (y - x_hat_pred)
  P <- (1-K) * P_pred
  
  #save 
  x_pred[i] <- x_hat_pred
  x_filt[i] <- x_hat
}


pred_atday = x_filt
forecast = x_pred


df <- data.frame(t=t, actual = x, measured = z, 
                 predday = pred_atday, forecasted = forecast)

p2 <- ggplot(df, aes(x=t, y=predday))+
  geom_point(color="blue")+
  geom_point(aes(y = actual), color = "green")+
  xlab("Number of Cases")+ylab("Time Index")+ggtitle("Comparison")
      
p3 <- ggplot(df, aes(x=t, y = forecasted))+
      geom_line(color="red")


#Animation

library(ggplot2)
library(gganimate)

df1 <- data.frame(index = 1:10000,
                 actual = x,
                 predday = pred_atday, forecasted = forecast)
q1 <- ggplot(df1, aes(x=index, y=actual))+
  geom_point(color="green", linewidth=2)+
  geom_line(aes(y = forecasted), color = "red")+
  geom_line(aes(y = predday), color = "blue")+
  xlab("Number of Cases")+ylab("Time Index")+ggtitle("Comparison")
  
q_animated <- q1 + transition_reveal(index)

animate(q_animated, nframes=100)
