# HYPOTHESIS

hypothesis <- function(predictor, outcome) {
  cat(glue("Hypothesis Test:\n\n"))
  cat(glue("Null Hypothesis (H0): There is no relationship between {predictor} and {outcome} (slope = 0).\n"))
  cat(glue("Alternative Hypothesis (H1): There is a relationship between {predictor} and {outcome} (slope ≠ 0).\n\n"))
}

# ASSUMPTIONS

# Function to check linear regression assumptions with diagnostic plots
assumptions <- function(file_name, predictor, outcome){
  # Read the file and prepare the data
  data <- read.csv(file_name)
  
  # Fit the linear model
  formula <- as.formula(glue("{outcome} ~ {predictor}"))
  fit <- lm(formula, data = data)
  
  # Scatterplot of Outcome vs Predictor
  plot_scatter <- ggplot(data, aes_string(x = predictor, y = outcome)) + 
    geom_point() + 
    ggtitle(glue("Scatterplot of {outcome} vs {predictor}")) + 
    xlab(predictor) + ylab(outcome)
  
  # Obtain residuals and fitted values
  lm_resid <- augment(fit)
  
  # (a) Residuals vs Fitted values
  plot_resid_fitted <- ggplot(lm_resid, aes(x = .fitted, y = .resid)) + 
    geom_point() + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggtitle("Residuals vs Fitted") + 
    xlab("Fitted values") + ylab("Residuals")
  
  # (b) Normal Q-Q plot of residuals
  plot_qq <- ggplot(lm_resid, aes(sample = .resid)) + 
    stat_qq() +
    stat_qq_line(color = "red") +
    ggtitle("Normal Q-Q") +
    xlab("Theoretical Quantiles") + ylab("Standardized Residuals")
  
  # (c) Scale-Location plot (also known as Spread-Location)
  plot_scale_location <- ggplot(lm_resid, aes(x = .fitted, y = sqrt(abs(.resid)))) +
    geom_point() +
    geom_smooth(se = FALSE, color = "red") +
    ggtitle("Scale-Location") +
    xlab("Fitted values") + ylab("sqrt(|Residuals|)")
  
  # (d) Residuals vs Leverage
  plot_leverage <- ggplot(lm_resid, aes(x = .hat, y = .std.resid)) +
    geom_point() +
    geom_smooth(se = FALSE, color = "red") +
    geom_hline(yintercept = c(-2, 2), linetype = "dashed", color = "grey") +
    ggtitle("Residuals vs Leverage") +
    xlab("Leverage") + ylab("Standardized Residuals") +
    annotate("text", x = 0.1, y = 2, label = "Cook's distance", color = "red")
  
  # Arrange the plots in a 2x2 grid
  plot_scatter + plot_resid_fitted + plot_qq + plot_leverage
}

# TEST
# Function to perform the linear regression test and display statistics
test <- function(file_name, predictor, outcome) {
  # Read data and fit the model
  data <- read.csv(file_name)
  formula <- as.formula(glue("{outcome} ~ {predictor}"))
  fit <- lm(formula, data = data)
  
  # Get summary statistics
  summ_fit <- tidy(fit, conf.int = TRUE) %>% filter(term == predictor)
  
  # Display test results
  cat(glue("Test Results:\n"))
  cat(glue("Estimated Slope (betâ) = {round(summ_fit$estimate, 4)}\n"))
  cat(glue("95% Confidence Interval for betâ = ({round(summ_fit$conf.low, 4)}, {round(summ_fit$conf.high, 4)})\n"))
  cat(glue("t-value = {round(summ_fit$statistic, 4)}\n"))
  cat(glue("Degrees of Freedom = {fit$df.residual}\n"))
  cat(glue("p-value = {round(summ_fit$p.value, 4)}\n\n"))
  
  # Return test results for further use
  list(estimate = summ_fit$estimate, p_value = summ_fit$p.value, predictor = predictor, outcome = outcome)
}

# DECISION
# Function to make a decision based on the p-value
decision <- function(test_results) {
  p_value <- test_results$p_value
  decision <- ifelse(p_value < 0.05, "Reject the null hypothesis (H0)", "Do not reject the null hypothesis (H0)")
  cat(glue("Decision:\n{decision}\n\n"))
}

# Function to provide a conclusion in plain language
conclusion <- function(test_results) {
  estimate <- test_results$estimate
  predictor <- test_results$predictor
  outcome <- test_results$outcome
  relationship <- ifelse(estimate > 0, "increases", "decreases")
  
file_name <- "project.csv"  # Replace with the path to your file
predictor <- "height"  # Replace with the actual predictor column name
outcome <- "weight"    # Replace with the actual outcome column name
  

# CONCLUSION
  if (test_results$p_value < 0.05) {
    cat(glue("Conclusion:\nThere is a statistically significant relationship between {predictor} and {outcome}.\n"))
    cat(glue("As {predictor} increases, {outcome} {relationship} by approximately {round(abs(estimate), 4)} units per unit increase in {predictor}.\n\n"))
  } else {
    cat(glue("Conclusion:\nThere is no statistically significant relationship between {predictor} and {outcome}.\n"))
    cat(glue("The slope is close to zero, indicating no significant change in {outcome} as {predictor} varies.\n\n"))
  }
}

# Run the analysis
hypothesis(predictor, outcome)
assumptions(file_name, predictor, outcome)
test_results <- test(file_name, predictor, outcome)
decision(test_results)
conclusion(test_results)
view(assumptions)

# CONCLUSION:
There is a statistically significant relationship between height and weight. As height increases, weight increases by approximately 0.7749 units per unit increase in height.
