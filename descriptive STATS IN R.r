library(readxl)
# Read data into R
file_path <- "/home/emmanuel-roggers/Downloads/Sales and Costs(1)(1) (1).xlsx"
data <- read_excel(file_path)
print(data)

#Mean material cost
mean_material_cost <- mean(data$`Materials Costs`)
print(mean_material_cost)
# Variance of labor costs
variance_labor_cost <- var(data$`Labor Costs`,)
print(variance_labor_cost)

# Covariance of labor cost and material cost
covariance <- cov(data$`Labor Costs`, df$`Materials Costs`)
print(covariance)

#Total sales
total_sales <- sum(data$Sales)
print(total_sales)
# More descriptive statistics
#Standaerd deviation of sales.
sd_sales <- sd(df$Sales)
print(paste("Standard Deviation of Sales:", sd_sales))

# Interquartile range of Materials Costs
iqr_material_cost <- IQR(data$`Materials Costs`)
print(paste("Interquartile Range (IQR) of Material Costs:", iqr_material_cost))

# Median of Labour Costs
median_labor_cost <- median(data$`Labor Costs`)
print(median_labor_cost)


