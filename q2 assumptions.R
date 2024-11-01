# HYPOTHESIS
hypothesis_ttest <- function(group_var, outcome) {
  cat(glue("Hypothesis Test for t-test:\n\n"))
  cat(glue("Null Hypothesis (H0): The mean {outcome} is the same for all levels of {group_var}.\n"))
  cat(glue("Alternative Hypothesis (H1): The mean {outcome} differs between levels of {group_var}.\n\n"))
}

# ASSUMPTIONS
assumptions_ttest <- function(file_name, group_var, outcome) {
  data <- read.csv(file_name)
  
  # Normality check using Q-Q plots for each group
  plot_qq <- ggplot(data, aes(sample = get(outcome))) + 
    stat_qq() + 
    stat_qq_line(color = "red") + 
    facet_wrap(as.formula(glue("~ {group_var}"))) +
    labs(title = glue("Q-Q Plot for {outcome} by {group_var} Levels"), x = "Theoretical Quantiles", y = "Sample Quantiles")
  
  # Variance check using boxplot for each group
  plot_box <- ggplot(data, aes_string(x = group_var, y = outcome, fill = group_var)) +
    geom_boxplot(alpha = 0.7) +
    labs(title = glue("Boxplot of {outcome} by {group_var} Levels"), x = group_var, y = outcome) +
    theme_minimal()
  
  # Display plots in a 2x1 layout
  combined_plot <- plot_qq / plot_box
  print(combined_plot)
}

# TEST
# Function to perform the t-test
test_ttest <- function(file_name, group_var, outcome, var_equal = TRUE) {
  data <- read.csv(file_name)
  
  # Perform t-test
  t_test_result <- t.test(data[[outcome]] ~ data[[group_var]], data = data, var.equal = var_equal)
  
  # Display test results
  cat(glue("Test Results:\n"))
  cat(glue("Mean of Group 1 ({levels(data[[group_var]])[1]}) = {round(t_test_result$estimate[1], 4)}\n"))
  cat(glue("Mean of Group 2 ({levels(data[[group_var]])[2]}) = {round(t_test_result$estimate[2], 4)}\n"))
  cat(glue("t-value = {round(t_test_result$statistic, 4)}\n"))
  cat(glue("Degrees of Freedom = {round(t_test_result$parameter, 4)}\n"))
  cat(glue("p-value = {round(t_test_result$p.value, 4)}\n"))
  cat(glue("95% Confidence Interval for Difference in Means = ({round(t_test_result$conf.int[1], 4)}, {round(t_test_result$conf.int[2], 4)})\n\n"))
  
  # Return test results for further use
  list(
    mean_group1 = t_test_result$estimate[1],
    mean_group2 = t_test_result$estimate[2],
    t_value = t_test_result$statistic,
    df = t_test_result$parameter,
    p_value = t_test_result$p.value,
    conf_int = t_test_result$conf.int,
    group_var = group_var,
    outcome = outcome
  )
}

# DECISION
# Function to make a decision based on the p-value
decision_ttest <- function(test_results) {
  p_value <- test_results$p_value
  decision <- ifelse(p_value < 0.05, "Reject the null hypothesis (H0)", "Do not reject the null hypothesis (H0)")
  cat(glue("Decision:\n{decision}\n\n"))
}

# CONCLUSION

# Function to provide a conclusion in plain language
conclusion_ttest <- function(test_results) {
  mean_group1 <- test_results$mean_group1
  mean_group2 <- test_results$mean_group2
  group_var <- test_results$group_var
  outcome <- test_results$outcome
  
  if (test_results$p_value < 0.05) {
    cat(glue("Conclusion:\nThere is a statistically significant difference in {outcome} between levels of {group_var}.\n"))
    cat(glue("The mean {outcome} for {group_var} level {levels(factor(group_var))[1]} is {round(mean_group1, 4)}, while for {group_var} level {levels(factor(group_var))[2]} it is {round(mean_group2, 4)}.\n\n"))
  } else {
    cat(glue("Conclusion:\nThere is no statistically significant difference in {outcome} between levels of {group_var}.\n"))
    cat(glue("The observed means are {round(mean_group1, 4)} and {round(mean_group2, 4)} for each group, which are not significantly different.\n\n"))
  }
}

file_name <- "project.csv"   # Replace with the path to your data file
group_var <- "gender"        # Replace with the actual column name for your grouping variable
outcome <- "height"          # Replace with the actual column name for the outcome variable

# Run the analysis
hypothesis_ttest(group_var, outcome)
assumptions_ttest(file_name, group_var, outcome)
test_results <- test_ttest(file_name, group_var, outcome, var_equal = TRUE)
decision_ttest(test_results)
conclusion_ttest(test_results)

# Conclusion:
There is a statistically significant difference in height between levels of gender. The mean height for gender level gender is 167.98, while for gender level NA it is 178.1299.