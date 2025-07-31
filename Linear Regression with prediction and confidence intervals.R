NEA  <- c(-94,-57,-29,135,143,151,245,355,392,473,486,535,571,580,620,690)
Fat_Gain <- c(4.2,3.0,3.7,2.7,3.2,3.6,2.4,1.3,3.8,1.7,1.6,2.2,1.0,0.4,2.3,1.1)

# a little EDA, before we get too serious --
plot(NEA,Fat_Gain, pch=17, col="dark red")

# How does the correlation coefficient look? --
My_r <- cor(NEA,Fat_Gain)
My_r

# Let's find the other needed values --
mean(NEA)
sd(NEA)

mean(Fat_Gain)
sd(Fat_Gain)

# Finding b --
My_b <- My_r * (sd(Fat_Gain)/sd(NEA))
My_b

# Finding a --
My_a <- mean(Fat_Gain) - My_b * mean(NEA)
My_a

# equation for the regression line --
### Pred_y <- My_a + My_b * some_x

# finding predicted y for a given x
# all regression lines pass trhough
# (mean_x,mean_y), so let's find y_hat
# for mean_x and check if it equals 
# mean_y.

y_hat = My_a + My_b * mean(NEA)
y_hat

y_hat == mean(Fat_Gain)

# plotting the regression line --
abline(My_a,My_b,col="yellow",lwd=4)

# using R's lm() function --
# see "Simple Graphs in R" for details

My_lm <- lm(Fat_Gain ~ NEA)  # note: y before x

coef(My_lm)      # displays slope and intercept

abline(My_lm, col="blue",lwd=1)  # adds regr. line

fitted(My_lm)   # predicted y-values for all xs

# Generating prediction intervals

My_df <- data.frame(NEA,Fat_Gain)

predict(My_lm, 
        data=My_df, 
        interval="prediction",
        conf.level=0.95)

# Generating a confidence interval for the mean 

predict(My_lm, 
        data=My_df, 
        interval="confidence",
        conf.level=0.95)

# Plotting PI and CI lines on scatterplot

#====================
#  R function to add prediction intervals or 
#  confidence intervals to an existing scatter 
#  plot. To use this function, you will need 
#  data stored in a data frame and a linear 
#  regression model.
#====================
plot.add.ci <- function(x, y, 
                        interval='prediction', 
                        level=0.95, 
                        regressionColor='red', ...) { 	
  xOrder  <- order(x) 	
  x       <- x[xOrder]   	
  y       <- y[xOrder]         
  fit     <- lm(y ~ x, data=data.frame(x=x, y=y)) 	
  newX    <- data.frame(x=jitter(x)) 	
  fitPred <- predict.lm(fit, newdata=newX, 
                        interval=interval, 
                        level=level, ...) 	
  abline(lm(y ~ x), col=regressionColor) 	
  lines(newX$x, fitPred[,2], lty=2, ...) 	
  lines(newX$x, fitPred[,3], lty=2, ...)
}

plot.add.ci(My_df$NEA,My_df$Fat_Gain,
            interval="prediction",
            regressionColor="green")

plot.add.ci(My_df$NEA,My_df$Fat_Gain,
            interval="confidence",
            level=0.95,
            regressionColor="cornflowerblue")

# Finding predicted values, using your linear model

# Note, if you have a linear model calle "My_lm"
# the y-intercept is coef(My_lm)[[1]] and the
# slope is coef(My_lm)[[2]]. 

# The equation for the linear model would be:
# y_hat=coef(My_lm)[[1]] + coef(My_lm)[[2]] * x

# If you input a vector of x values in place of
# the x variable in the above equation, R will
# generate as many y-hat values as x values that 
# are contained in the input vector.

mr_predictor <- function(x_values_of_interest,linear_model){
  y_hats <- coef(linear_model)[[1]] + coef(linear_model)[[2]] * x_values_of_interest
  return(y_hats)
}

some_xs <- c(-50,0,100,200,300,400,500,600)

mr_predictor(some_xs,My_lm)
points(some_xs,mr_predictor(some_xs,My_lm),
       pch=23,
       col="blue",
       bg="light blue",
       cex=1.5)

