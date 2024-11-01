# Load the testthat package
install.packages("testthat")
library(testthat)

# Load your dataset
data <- read.csv("project.csv")

# Load necessary libraries (if your code relies on specific packages)
library(dplyr)

# Question 1: Test for Linear Relationship between Height and Weight
test_that("Height-Weight Linear Model Coefficient Test", {
  height_weight_model <- lm(weight ~ height, data = data)
  coef_summary <- summary(height_weight_model)$coefficients
  p_value <- coef_summary["height", "Pr(>|t|)"]
  
  expect_lt(p_value, 0.05)  # Test if p-value is less than 0.05 (significant relationship)
  expect_gt(coef_summary["height", "Estimate"], 0)  # Check if the slope is positive
})

# Question 2: Test for Difference in Mean Height between Males and Females
test_that("Gender Difference in Height Test", {
  t_test_result <- t.test(height ~ gender, data = data)
  
  expect_lt(t_test_result$p.value, 0.05)  # Expect a p-value less than 0.05
  expect_true(t_test_result$estimate[1] < t_test_result$estimate[2])  # Expect mean height of females < males
})

# Question 3: Test for Association between Gender and Physical Activity Level
test_that("Gender and Physical Activity Independence Test", {
  activity_gender_table <- table(data$gender, data$phys)
  chi_square_result <- chisq.test(activity_gender_table)
  
  expect_gt(chi_square_result$p.value, 0.05)  # Expect a p-value greater than 0.05 (no association)
})
