
# Load necessary libraries
library(dplyr)

# Import the dataset
bes_data <- read.csv("your_file_path.csv")  # Replace with the actual file path

# Data cleaning: Remove rows with missing values for key variables
selected_data <- bes_data %>%
  filter(!is.na(Age), !is.na(edlevel), !is.na(wt_vote_valid_bc))

# Compute descriptive statistics
descriptive_stats <- selected_data %>%
  summarise(
    Age_mean = mean(Age, na.rm = TRUE),        # Mean of Age
    Age_median = median(Age, na.rm = TRUE),    # Median of Age
    Voting_Weight_mean = mean(wt_vote_valid_bc, na.rm = TRUE),  # Mean of voting weight
    Voting_Weight_median = median(wt_vote_valid_bc, na.rm = TRUE)  # Median of voting weight
  )

# Print the descriptive statistics
print(descriptive_stats)

# Simple linear regression: Voting weight ~ Age
model1 <- lm(wt_vote_valid_bc ~ Age, data = selected_data)

# Display the summary of the model
summary(model1)

# Multiple regression: Voting weight ~ Age, education level, and squared age
model2 <- lm(wt_vote_valid_bc ~ Age + edlevel + I(Age^2), data = selected_data)

# Display the summary of the second model
summary(model2)

# Load necessary libraries
library(ggplot2)

# Scatterplot with a quadratic trend line: Age vs Voting Weight
ggplot(selected_data, aes(x = Age, y = wt_vote_valid_bc)) +
  geom_point(alpha = 0.3) +  # Add scatter points with transparency
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE) +  # Add a quadratic trend line
  labs(title = "Age and Voting Weight",  # Title of the plot
       x = "Age",                        # X-axis label
       y = "Voting Weight (Transformed)") +  # Y-axis label
  theme_minimal()  # Use a minimal theme for better aesthetics

# Diagnostic plots for regression model
par(mfrow = c(2, 2))  # Set layout for 2x2 plots

# Generate diagnostic plots for the second model
plot(model2)

# Save regression results to a CSV file
library(broom)  # Load broom for tidy regression output
tidy(model2) %>% write.csv("regression_results.csv", row.names = FALSE)  # Save results to CSV

# Save a visualization to a file
ggsave("age_voting_weight_plot.png", width = 8, height = 6)  # Save plot as PNG
