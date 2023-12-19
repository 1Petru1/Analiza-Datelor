library("openintro")
library("tidyverse")
library(nycflights13)
library(ggplot2)

glimpse(weather)
glimpse(flights)
glimpse(airports)
glimpse(planes)
glimpse(airlines)

?flights

filtered_flights <- flights %>%
  filter(dep_delay < 0, !is.na(carrier)) %>%
  mutate(
    dep_delay_comment = ifelse(dep_delay == -10, "Late", "On time"),
    arr_delay_comment = ifelse(arr_delay == -10, "Late", "On time"),
    season = case_when(
      month %in% c(12, 1, 2) ~ "winter",
      month %in% c(3, 4, 5) ~ "spring",
      month %in% c(6, 7, 8) ~ "summer",
      month %in% c(9, 10, 11) ~ "autumn",
      TRUE ~ "unknown"
    ),
    mot_den = month.abb[month] # Adăugăm coloana "mot_den" cu denumirea lunii
  )

filtered_flights <- filtered_flights %>%
  mutate(distance_km = distance * 1.6)


# Scatter plot cu air time și distance și trend line
ggplot(subset_data, aes(x = distance, y = air_time)) +
  geom_point(alpha = 0.7, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatter Plot: Air Time vs. Distance",
       x = "Distance",
       y = "Air Time (minute)") +
  theme_minimal()

# Creați scatter plot cu linii de trend și filtrare
ggplot(filtered_flights %>% filter(distance <= 3000), aes(x = distance, y = air_time)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Scatter Plot: Distance vs. Air Time (Filtered)",
    x = "Distance (km)",
    y = "Air Time (hours)"
  ) +
  theme_minimal()

status <- total_time_delay_season %>%
  filter(!is.na(status)) %>%
  group_by(season, status) %>%
  summarise(total_flights = n())

ggplot(status, aes(x = season, y = total_flights, fill = status)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Număr total de zboruri în funcție de sezon și statut",
    x = "Sezon",
    y = "Număr total de zboruri",
    fill = "Status"
  ) +
  theme_minimal()

subset_data <- total_time_delay_season[1:10000, ]
ggplot(subset_data, aes(x = hour, y = arr_delay)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Adăugare linie de trend
  labs(title = "Relația dintre Ora de Plecare și Întârzierea la Sosire",
       x = "Ora de Plecare",
       y = "Întârziere la Sosire (minute)")

summary_data <- flights_long %>%
  group_by(capacity, status) %>%
  summarise(total_records = n())

# Atribuirea statusului "executed" celei mai mari înregistrări din total_records
summary_data <- summary_data %>%
  mutate(status = ifelse(total_records == max(total_records), "executed", status))

# Excluderea înregistrărilor cu capacity "unknown"
summary_data <- summary_data %>%
  filter(capacity != "unknown")

# Atribuirea statusului "canceled" celei mai mici înregistrări din total_records
summary_data <- summary_data %>%
  mutate(status = ifelse(total_records == min(total_records), "canceled", status))


# Generează un plot cu ggplot2
ggplot(summary_data, aes(x = capacity, y = total_records, fill = status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Records for Cancelled and Executed Flights by Capacity",
       x = "Capacity",
       y = "Total Records",
       fill = "Status") +
  theme_minimal()

executed_flights <- flights_long %>% 
  filter(status == "executed") %>% 
  sample_n(3000)

cancelled_flights <- flights_long %>% 
  filter(status == "canceled") %>% 
  sample_n(1717)

# Combinăm cele două subseturi înapoi într-un singur data frame
final_flights <- bind_rows(executed_flights, cancelled_flights)

ggplot(final_flights, aes(x = long_flight, fill = status)) +
  geom_bar(position = "dodge") +
  labs(x = "Durata Zborului (Long/Short)", y = "Numărul de Zboruri", fill = "Status") +
  theme_minimal() +
  ggtitle("Analiza Zborurilor Executate și Anulate pe baza Duratei Zborului")

canceled_summary <- total_time_delay_season %>%
  filter(status == "canceled") %>%
  group_by(carrier) %>%
  summarise(total_canceled = n()) %>%
  arrange(desc(total_canceled))

ggplot(canceled_summary, aes(x = reorder(carrier, -total_canceled), y = total_canceled)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top Airlines with Most Canceled Flights",
       x = "Airline Carrier",
       y = "Total Canceled Flights") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(carrier_delay_freq, aes(x = "", y = total_delay, fill = carrier)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  labs(
    title = "Distribuția Frecvențelor pentru Întârzierile la Plecare (dep_delay) pe Operator de Zbor (carrier)",
    x = NULL,
    y = NULL,
    fill = "Operator de Zbor"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggplot(final_flights, aes(x = long_flight, fill = status)) +
  geom_bar(position = "dodge") +
  labs(x = "Durata Zborului (Long/Short)", y = "Numărul de Zboruri", fill = "Status") +
  theme_minimal() +
  ggtitle("Analiza Zborurilor Executate și Anulate pe baza Duratei Zborului")


library(dplyr)

# Assuming the dataset total_time_delay_season is already loaded into R

# Selecting a subset of variables
selected_data <- total_time_delay_season %>%
  select(year, month, day, dep_time, sched_dep_time, dep_delay, arr_delay, distance, status)

# Convert status to a factor since it's a categorical outcome
selected_data$status <- as.factor(selected_data$status)

# Now let's split the dataset into a training set and a test set
set.seed(123) # For reproducibility
train_indices <- sample(1:nrow(selected_data), size = 0.8 * nrow(selected_data))
train_set <- selected_data[train_indices, ]
test_set <- selected_data[-train_indices, ]

# Fit the logistic regression model on the training set
glm_model <- glm(status ~ ., family = binomial, data = train_set)

# Summary of the model to check p-values and other statistics
summary(glm_model)

# Predict on the test set
test_set$predicted_status <- predict(glm_model, newdata = test_set, type = "response")

# Convert probabilities to binary outcome based on a threshold (usually 0.5 for binary classification)
test_set$predicted_status <- ifelse(test_set$predicted_status > 0.5, "executed", "canceled")

# Evaluate the model performance
conf_mat <- table(test_set$status, test_set$predicted_status)
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
precision <- diag(conf_mat) / colSums(conf_mat)
recall <- diag(conf_mat) / rowSums(conf_mat)

# Print the performance metrics
print(paste("Accuracy:", accuracy))

# Load the randomForest package
library(randomForest)
library(ranger)

# Assuming your dataset is named total_time_delay_season and is already loaded in R

# Convert character variables to factors
char_vars <- sapply(total_time_delay_season, is.character)
total_time_delay_season[char_vars] <- lapply(total_time_delay_season[char_vars], as.factor)

# Convert the status variable to a factor
total_time_delay_season$status <- as.factor(total_time_delay_season$status)

# Split the data into training and test sets
set.seed(123) # for reproducibility
train_indices <- sample(1:nrow(total_time_delay_season), size = 0.8 * nrow(total_time_delay_season))
train_data <- total_time_delay_season[train_indices, ]
test_data <- total_time_delay_season[-train_indices, ]

# Evaluate model performance
conf_mat <- table(test_data$status, test_pred)
conf_mat

# Train the random forest model using ranger
rf_model <- ranger(status ~ ., data = train_data, num.trees = 500)

# Make predictions on the test set
test_pred <- predict(rf_model, data = test_data)$predictions

# Evaluate model performance
conf_mat <- table(test_data$status, test_pred)
conf_mat

# Train the random forest model using ranger
rf_model <- ranger(status ~ ., data = train_data, num.trees = 500)

# Make predictions on the test set
test_pred <- predict(rf_model, data = test_data)$predictions

# Convert predictions to a factor with the same levels as the actual status to ensure proper comparison
test_pred <- factor(test_pred, levels = levels(test_data$status))

# Calculate the confusion matrix
conf_mat <- table(test_data$status, test_pred)

# Calculate accuracy
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)

# Calculate precision for each class
precision <- diag(conf_mat) / colSums(conf_mat)

# Print the results
print(paste("Accuracy:", accuracy))

library(tidyverse)

# Creați un subset din setul de date original
subset_data <- total_time_delay_season[1:10000, ]

# Modelul 1: Regresie liniară pentru arr_delay în funcție de dep_delay și air_time
model1 <- lm(arr_delay ~ dep_delay + air_time, data = subset_data)

# Modelul 2: Regresie liniară pentru arr_delay în funcție de dep_delay și distance
model2 <- lm(arr_delay ~ dep_delay + distance, data = subset_data)

# Modelul 3: Regresie liniară pentru air_time în funcție de dep_delay și distance
model3 <- lm(air_time ~ dep_delay + distance, data = subset_data)

