# Load the core tidyverse and make it available in your current R session
library(tidyverse)


# Read the CSV file into a tibble
diabetes <- read_csv("diabetes.csv")


# Print the first 10 rows of the data
diabetes %>% 
  slice_head(n = 10)
# Take a quick glance at the data
diabetes %>% 
  glimpse()
# Load the janitor package for cleaning data
library(janitor)

# Clean data a bit
diabetes_select <- diabetes %>%
  # Encode Diabetic as category
  mutate(Diabetic = factor(Diabetic, levels = c("1","0"))) %>% 
  # Drop PatientID column
  select(-PatientID) %>% 
  # Clean column names
  clean_names()


# View data set
diabetes_select %>% 
  slice_head(n = 10)
# Load the janitor package for cleaning data
library(janitor)

# Clean data a bit
diabetes_select <- diabetes %>%
  # Encode Diabetic as category
  mutate(Diabetic = factor(Diabetic, levels = c("1","0"))) %>% 
  # Drop PatientID column
  select(-PatientID) %>% 
  # Clean column names
  clean_names()

# Pivot data to a long format
diabetes_select_long <- diabetes_select %>% 
  pivot_longer(!diabetic, names_to = "features", values_to = "values")


# Print the first 10 rows
diabetes_select_long %>% 
  slice_head(n = 10)

theme_set(theme_light())
# Make a box plot for each predictor feature
diabetes_select_long %>% 
  ggplot(mapping = aes(x = diabetic, y = values, fill = features)) +
  geom_boxplot() + 
  facet_wrap(~ features, scales = "free", ncol = 4) +
  scale_color_viridis_d(option = "plasma", end = .7) +
  theme(legend.position = "none")
# Load the tidymodels packages
library(tidymodels)
# Split data into 70% for training and 30% for testing
set.seed(2056)
diabetes_split <- diabetes_select %>% 
  initial_split(prop = 0.70)
diabetes_split

# Extract the data in each split
diabetes_train <- training(diabetes_split)
diabetes_test <- testing(diabetes_split)


# Print the number of cases in each split
cat("Training cases: ", nrow(diabetes_train), "\n",
    "Test cases: ", nrow(diabetes_test), sep = "")


# Print out the first 5 rows of the training set
diabetes_train %>% 
  slice_head(n = 5)
# Make a model specifcation
logreg_spec <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")


# Print the model specification
logreg_spec
# Train a logistic regression model
logreg_fit <- logreg_spec %>% 
  fit(diabetic ~ ., data = diabetes_train)


# Print the model object
logreg_fit
# Make predictions then bind them to the test set
results <- diabetes_test %>% select(diabetic) %>% 
  bind_cols(logreg_fit %>% predict(new_data = diabetes_test))


# Compare predictions
results %>% 
  slice_head(n = 10)
# Calculate accuracy: proportion of data predicted correctly
accuracy(data = results, truth = diabetic, estimate = .pred_class)

