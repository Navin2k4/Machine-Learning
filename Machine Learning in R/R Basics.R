df <- data.frame(
  salary = c(50000, 60000, 55000, 70000, 65000, 62000, 58000, 72000, 67000, 69000),
  gender = c("Male", "Female", "Female", "Male", "Female", "Male", "Male", "Female", "Female", "Male"),
  years_experience = c(2, 5, 3, 8, 6, 7, 4, 10, 9, 8)
)
df # display the data
head(df,3) 
View(df)

df2 <- df[df$gender == 'Female',] # Based on the column name and all the columns
df2

df2 <- df[df$gender == 'Male',c(1,3)] # Based on the column name but only selected columns 
df2

# Sorting Data

df2 <- df[order(df$salary),] # Ascending Order
df2
df2 <- df[order(-df$salary),] # Descending Order
df2

min(df$salary) # Minimum value of the salary column
max(df$salary) # Maximum value of the salary column
range(df$salary) # Range (minimum and maximum values) of the salary column
mean(df$salary) # Mean (average) value of the salary column
median(df$salary) # Median (middle) value of the salary column
# Mode is not a built-in function in R, so you need to define a custom function
mode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
mode(df$salary)
max(df$salary) - min(df$salary) # Range (difference between the maximum and minimum values) of the salary column
quantile(df$salary) # Quantiles of the salary column
IQR(df$salary) # Interquartile Range (IQR) of the salary column


plot(df$salary,df$years_experience)
linearRegression <- lm(years_experience~salary,data=df)
summary(linearRegression)
abline(linearRegression,col='blue')

new_data <- data.frame(salary = 70000)
new_data
predicted_years_experience <- predict(linearRegression, new_data)
predicted_years_experience

