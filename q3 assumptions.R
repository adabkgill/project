# HYPOTHESIS
hypothesis_chi_test <- function(var1, var2) {
  cat(glue("Hypothesis Test for Chi-Squared Test:\n\n"))
  cat(glue("Null Hypothesis (H0): There is no association between {var1} and {var2}.\n"))
  cat(glue("Alternative Hypothesis (H1): There is an association between {var1} and {var2}.\n\n"))
}

# ASSUMPTIONS
assumptions_chi_test <- function(file_name, var1, var2) {
  data <- read.csv(file_name)
  
  # Remove rows with NA values in either variable
  data <- data %>% drop_na(var1, var2)
  
  # Create a contingency table
  contingency_table <- table(data[[var1]], data[[var2]])
  
  # Display the contingency table as the assumption check
  cat(glue("Assumption Check:\n"))
  print(contingency_table)
  
  # Bar plot to visualize the distribution across categories
  plot_bar <- ggplot(data, aes_string(x = var1, fill = var2)) +
    geom_bar(position = "dodge") +
    labs(title = glue("Bar Plot of {var1} by {var2}"), x = var1, fill = var2) +
    theme_minimal()
  
  # Display the bar plot
  print(plot_bar)
}

# TEST
# Function to perform the chi-squared test
test_chi_test <- function(file_name, var1, var2) {
  data <- read.csv(file_name)
  
  # Remove rows with NA values in either variable
  data <- data %>% drop_na(var1, var2)
  
  # Create a contingency table
  contingency_table <- table(data[[var1]], data[[var2]])
  
  # Perform chi-squared test
  chi_test_result <- chisq.test(contingency_table)
  
  # Display test results
  cat(glue("Test Results:\n"))
  cat(glue("Chi-squared statistic = {round(chi_test_result$statistic, 4)}\n"))
  cat(glue("Degrees of Freedom = {chi_test_result$parameter}\n"))
  cat(glue("p-value = {round(chi_test_result$p.value, 4)}\n\n"))
  
  # Return test results for further use
  list(
    chi_square_stat = chi_test_result$statistic,
    df = chi_test_result$parameter,
    p_value = chi_test_result$p.value,
    var1 = var1,
    var2 = var2
  )
}

# DECISION
# Function to make a decision based on the p-value
decision_chi_test <- function(test_results) {
  p_value <- test_results$p_value
  decision <- ifelse(p_value < 0.05, "Reject the null hypothesis (H0)", "Do not reject the null hypothesis (H0)")
  cat(glue("Decision:\n{decision}\n\n"))
}

# CONCLUSION
# Function to provide a conclusion in plain language
conclusion_chi_test <- function(test_results) {
  var1 <- test_results$var1
  var2 <- test_results$var2
  
  if (test_results$p_value < 0.05) {
    cat(glue("Conclusion:\nThere is a statistically significant association between {var1} and {var2}.\n"))
  } else {
    cat(glue("Conclusion:\nThere is no statistically significant association between {var1} and {var2}.\n"))
  }
}

# Example usage
file_name <- "project.csv"   # Replace with your file path
var1 <- "gender"             # Replace with your first categorical variable
var2 <- "phys"      # Replace with your second categorical variable

# Run the analysis
hypothesis_chi_test(var1, var2)
assumptions_chi_test(file_name, var1, var2)
test_results <- test_chi_test(file_name, var1, var2)
decision_chi_test(test_results)
conclusion_chi_test(test_results)


CONCLUSION: There is no statistically significant association between gender and phys.