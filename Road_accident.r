setwd("C:\\Users\\shivb\\Desktop\\R Programming\\R Project")
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(moments)

# Load the dataset (replace 'path_to_file' with the actual path)
road_accidents <- read.csv("Roadaccedent.csv")

# View structure and summary
str(road_accidents)
summary(road_accidents)


# View the actual column names
colnames(road_accidents)

# Correct column references in your code
road_accidents <- road_accidents %>%
  rename(
    Road_Accidents_Cases = Road.Accidents.Cases,
    Road_Accidents_Injured = Road.Accidents.Injured,
    Road_Accidents_Died = Road.Accidents.Died
  )


# Ensure numerical columns are of numeric type
road_accidents <- road_accidents %>%
  mutate(
    Road_Accidents_Cases = as.numeric(Road_Accidents_Cases),
    Road_Accidents_Injured = as.numeric(Road_Accidents_Injured),
    Road_Accidents_Died = as.numeric(Road_Accidents_Died)
  )

# Check for missing values
sum(is.na(road_accidents))

# Remove rows with missing or invalid data if necessary
road_accidents <- na.omit(road_accidents)


# Function to calculate skewness and kurtosis
calculate_distribution_metrics <- function(data, column) {
  skew <- skewness(data[[column]], na.rm = TRUE)
  kurt <- kurtosis(data[[column]], na.rm = TRUE)
  return(data.frame(Skewness = skew, Kurtosis = kurt))
}

# Summary statistics for each variable
summary(road_accidents[, c("Road_Accidents_Cases", "Road_Accidents_Injured", "Road_Accidents_Died")])

# Skewness and kurtosis
metrics_cases <- calculate_distribution_metrics(road_accidents, "Road_Accidents_Cases")
metrics_injured <- calculate_distribution_metrics(road_accidents, "Road_Accidents_Injured")
metrics_died <- calculate_distribution_metrics(road_accidents, "Road_Accidents_Died")

metrics_cases
metrics_injured
metrics_died


# Correlation matrix
cor_matrix <- cor(road_accidents[, c("Road_Accidents_Cases", "Road_Accidents_Injured", "Road_Accidents_Died")], use = "complete.obs")
cor_matrix



# Linear regression: Cases vs Injuries
model_injuries <- lm(Road_Accidents_Injured ~ Road_Accidents_Cases, data = road_accidents)
summary(model_injuries)

# Linear regression: Cases vs Deaths
model_deaths <- lm(Road_Accidents_Died ~ Road_Accidents_Cases, data = road_accidents)
summary(model_deaths)



ggplot(road_accidents, aes(x = Road_Accidents_Cases)) +
  geom_histogram(binwidth = 500, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Road Accident Cases", x = "Accident Cases", y = "Frequency") +
  theme_minimal()


# Install tidyr if not already installed
install.packages("tidyr")
library(tidyr)

# Pivot data for boxplot
road_accidents_long <- road_accidents %>%
  pivot_longer(cols = c(Road_Accidents_Cases, Road_Accidents_Injured, Road_Accidents_Died),
               names_to = "Metric", values_to = "Value")

# Boxplot
ggplot(road_accidents_long, aes(x = Metric, y = Value, fill = Metric)) +
  geom_boxplot() +
  labs(title = "Boxplot of Road Accident Metrics", x = "Metric", y = "Value") +
  theme_minimal()


ggplot(road_accidents, aes(x = Road_Accidents_Cases, y = Road_Accidents_Died)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Road Accidents Cases vs Deaths", x = "Accident Cases", y = "Deaths") +
  theme_minimal()




ggplot(road_accidents, aes(x = Road_Accidents_Cases, y = Road_Accidents_Died)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Road Accidents Cases vs Deaths", x = "Accident Cases", y = "Deaths") +
  theme_minimal()




  # road_accidents <- road_accidents %>%
  # rename(
  #   Road_Accidents_Cases = Road.Accidents.Cases,
  #   Road_Accidents_Injured = Road.Accidents.Injured,
  #   Road_Accidents_Died = Road.Accidents.Died
  # )









