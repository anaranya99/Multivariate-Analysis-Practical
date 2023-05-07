###Multivariate Testing Related to Hotteling's T square###
##Problem 1###
#load mvtnorm package
library(mvtnorm)
# define mean vectors and covariance matrix
mean_gr1 <- c(25.80, 7.81, 10.77)
mean_gr2 <- c(28.35, 7.41, 10.75)
Sigma=cov_matrix <- matrix(c(4.7350, 0.5622, 1.4685,
                             0.5622, 0.1413, 0.2174,
                             1.4685, 0.2174, 0.5702),
                           nrow = 3, ncol = 3)
x = rbind(mean_gr1,mean_gr2)
# calculate Hotelling's T-Squared statistic
mvt=hotelling.test(x,Sigma,
                   n = c(20,72))
mvt
#########################################################
#########################################################
#########################################################
#########################################################
####Problem 2####
# Create a matrix of means for each group
group1_means <- c(58.0, 25.8, 7.8)
group2_means <- c(59.7, 28.4, 7.4)
means_matrix <- rbind(group1_means, group2_means)
# Create the pooled variance-covariance matrix
pooled_matrix <- matrix(c(13.86, 6.97, 1.08, 6.97, 4.79, 0.56, 1.08, 0.56, 0.14), nrow = 3, ncol = 3, byrow 
                        = TRUE)
solve(pooled_matrix)
# Calculate the Hotelling's T-square statistic
n1 <- 20
n2 <- 72
n <- n1 + n2
t_sq <- (n1*n2/n)*t(group1_means - group2_means)%*%solve(pooled_matrix) %*%(group1_means -
                                                                              group2_means)
t_sq
# Calculate the critical value and p-value
df1 <- 3
df2 <- n – 3
crit_val <- qf(0.95, df1, df2)
p_val <- pf(t_sq, df1, df2, lower.tail = FALSE)
# Print the results
cat("Hotelling's T-squared statistic:", round(t_sq, 2), "\n")
cat("Critical value at alpha = 0.05:", round(crit_val, 2), "\n")
cat("p-value:", round(p_val, 4), "\n")
if (t_sq > crit_val) {
  cat("Reject the null hypothesis of equal mean vectors.\n")
} else {
  cat("Fail to reject the null hypothesis of equal mean vectors.\n")
}
###########################################################
##########################################################
library(Hmisc)
# input data
Health <- c(9, 10, 8, 7, 8, 9, 12, 11, 10, 14, 7, 9, 6, 16, 14)
Stress <- c(5, 5, 7, 4, 7, 6, 8, 3, 5, 6, 7, 9, 7, 3, 8)
Coping <- c(18, 21, 17, 16, 22, 19, 25, 17, 20, 22, 18, 22, 17, 20, 26)
# combine data into a matrix
data <- cbind(Health, Stress, Coping)
# calculate the multiple correlation and test significance
result <- rcorr(as.matrix(data))
print(result$r)
print(result$P)
####################################################
####################################################
install.packages("ppcor")
library(ppcor)
library(ppcor)
Health <- c(9, 10, 8, 7, 8, 9, 12, 11, 10, 14, 7, 9, 6, 16, 14)
Stress <- c(5, 5, 7, 4, 7, 6, 8, 3, 5, 6, 7, 9, 7, 3, 8)
Coping <- c(18, 21, 17, 16, 22, 19, 25, 17, 20, 22, 18, 22, 17, 20, 26)
data <- cbind(Health, Stress, Coping)
x=data[,1:2]
y=data[,3]
result <- pcor(x,y,method = "pearson")
print(result)
########################################################



















