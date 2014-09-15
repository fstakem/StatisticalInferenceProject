# <<<<--------------------------------------------<>-------------------------------------------->>>>
#
#       exponential_sim.R
#       By: Fredrick Stakem
#       Date: 9.14.14
#
#       Purpose:    To simulate the exponential distribution to learn basic statistics
#                   for the class Statistical Inference from John Hopkins offered
#                   through Coursera.
#
# <<<<--------------------------------------------<>-------------------------------------------->>>>

# Libraries


# <<<<-----------------------------------------< Part 1 >----------------------------------------->>>>
# Parameters
set.seed(12345)
lambda <- 0.2
num_random_samples <- 40
num_simulations <- 1000
theoretical_mean <- 1/ lambda
theoretical_sd <- theoretical_mean

# Simulate data
means <- vector()
standard_deviations <- vector()

for(i in seq(1:num_simulations))
{
    sim <- rexp(num_random_samples, lambda)
    means <- c(means, mean(sim))
    standard_deviations <- c(standard_deviations, sd(sim))
}

mean(means)
mean(standard_deviations)

# Show mean data
mean_hist <- hist(means, breaks=100)
multiplier <- mean_hist$counts / mean_hist$density
mean_density <- density(means)
mean_density$y <- mean_density$y * multiplier[1]

plot(mean_hist, col="red", main="Exponential Distribution Simulation Means", xlab="Simulation Means", xlim=c(2,8))
lines(mean_density, lwd=3)

# Show standard devaition data
sd_hist <- hist(standard_deviations, breaks=100)
multiplier <- sd_hist$counts /sd_hist$density
sd_density <- density(standard_deviations)
sd_density$y <- sd_density$y * multiplier[1]

plot(sd_hist, col="red", main="Exponential Distribution Simulation Standard Deviations", xlab="Simulation Standard Deviations", xlim=c(2,9))
lines(sd_density, lwd=3)

# Objective 1
# Show where the distribution is centered at and compare it to the theoretical center of the distribution.
actual_mean = mean(means)
mean_density <- density(means)
plot(mean_density, type = "n",  xlim=c(2,8), main='Exponential Distribution Simulation Mean Comparison', xlab='Simulation Means')
polygon(mean_density, col = "wheat")
legend('topright', c(paste('Theoretical Mean: ', theoretical_mean), paste('Actual Mean: ', format(actual_mean,digits=3))), lty=c(1,2), col=c('blue', 'darkgreen'), bty='n', cex=1.2, lwd=2)

y_at_theoretical_mean <- density(means, from=theoretical_mean, to=theoretical_mean, n=1)$y
segments(x0=theoretical_mean, y0=0, x1=theoretical_mean, y1=y_at_theoretical_mean, col="blue", lwd=2)

y_at_mean <- density(means, from=actual_mean, to=actual_mean, n=1)$y
segments(x0=actual_mean, y0= 0, x1=actual_mean, y1=y_at_mean, col="darkgreen", lwd=2, lty='dotted')

# Objective 2
# Show how variable it is and compare it to the theoretical variance of the distribution.
actual_sd = mean(standard_deviations)
sd_density <- density(standard_deviations)
plot(sd_density, type = "n",  xlim=c(2,9), main='Exponential Distribution Simulation Standard Deviation Comparison', xlab='Simulation Standard Deviations')
polygon(sd_density, col = "wheat")
legend('topright', c(paste('Theoretical Standard Deviation: ', theoretical_sd), paste('Actual Standard Deviation: ', format(actual_sd,digits=3))), lty=c(1,2), col=c('blue', 'darkgreen'), bty='n', cex=1.2, lwd=2)

y_at_theoretical_sd <- density(standard_deviations, from=theoretical_sd, to=theoretical_sd, n=1)$y
segments(x0=theoretical_sd, y0=0, x1=theoretical_sd, y1=y_at_theoretical_sd, col="blue", lwd=2)

y_at_sd <- density(standard_deviations, from=actual_sd, to=actual_sd, n=1)$y
segments(x0=actual_sd, y0= 0, x1=actual_sd, y1=y_at_sd, col="darkgreen", lwd=2, lty='dotted')

# Objective 3
# Show that the distribution is approximately normal.
mean_density <- density(means)
plot(mean_density, type = "n",  xlim=c(2,8), main='Exponential Distribution Simulation Mean ', xlab='Simulation Means')
polygon(mean_density, col = "wheat")
legend('topright', c('Normal Distribution'), lty=2, col='blue', bty='n', cex=1.2, lwd=2)

fun_x <- seq(min(means), max(means), length.out= 100)
fun_mean <- mean(means)
fun_sd <- sd(means)

normal <- dnorm(x = fun_x, mean = fun_mean, sd = fun_sd)
lines(fun_x, normal, col = "blue", lwd = 2.5, lty='dotted')

qqnorm(means, main='Normal Q-Q Plot for Exponential Simulation', cex=.8)
qqline(means,col="red", lwd = 2.5)


# Objective 4
# Evaluate the coverage of the confidence interval for 1/lambda.





