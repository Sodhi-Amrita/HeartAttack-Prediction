df <- read.csv("C:\\Karan\\Sem 4\\Big Data\\Project\\heart_failure.csv")
print(data)


# *************** Sample *****************
library(dplyr)

# Select specific columns
selected_cols <- select(df, age, diabetes, smoking)

# Arrange the data frame based on age in ascending order
arranged_df <- arrange(df, age)

# Filter rows where age is greater than 60
filtered_df <- filter(df, age > 60)

# Select specific columns, filter rows, and arrange based on age
result <- df %>%
  select(age, smoking) %>%
  filter(smoking == 1) %>%
  arrange(age)

# Create a new column with the sum of age and time
mutated_df <- mutate(df, age_time_sum = age + time)

# Assign ranks to the serum_creatinine column
ranked_df <- mutate(df, serum_creatinine_rank = rank(serum_creatinine))


# ***************** Death per Age Group ***************
library(ggplot2)

# Count the number of deaths for each age
death_counts <- table(df$age[df$DEATH_EVENT == 1])

# Create a bar chart
bar_chart <- ggplot(data = as.data.frame(death_counts), aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Number of Deaths per Age Group", x = "Age", y = "Number of Deaths") +
  theme_minimal()

# Display the bar chart
print(bar_chart)

# ******************* Anemia *******************

library(ggplot2)

# Count the number of individuals with and without anemia, grouped by death event
anemia_death_counts <- table(df$anaemia, df$DEATH_EVENT)

# Create a data frame from the counts
anemia_death_df <- as.data.frame(anemia_death_counts)
colnames(anemia_death_df) <- c("Anaemia", "Death_Event", "Count")

# Calculate the total count for each death event
total_counts <- aggregate(Count ~ Death_Event, data = anemia_death_df, FUN = sum)

# Create a pie chart
pie_chart <- ggplot(data = total_counts, aes(x = "", y = Count, fill = Death_Event)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Anemia vs. Death Event", fill = "Death Event", x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = "right")

# Display the pie chart
print(pie_chart)


# ********************* Createnine *************

library(ggplot2)

# Calculate the average creatinine levels by death event
avg_creatinine <- aggregate(creatinine_phosphokinase ~ DEATH_EVENT, data = df, FUN = mean)

# Create a grouped bar chart
grouped_bar_chart <- ggplot(avg_creatinine, aes(x = factor(DEATH_EVENT), y = creatinine_phosphokinase, fill = factor(DEATH_EVENT))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Creatinine Levels by Death Event", x = "Death Event", y = "Average Creatinine Levels") +
  scale_fill_manual(values = c("lightblue", "lightgreen"), labels = c("Survived", "Death")) +
  theme_minimal()

# Display the grouped bar chart
print(grouped_bar_chart)


# **************** diabetes *****************


library(ggplot2)

# Calculate the counts of individuals with and without diabetes, grouped by death event
diabetes_death_counts <- table(df$diabetes, df$DEATH_EVENT)

# Create a data frame from the counts
diabetes_death_df <- as.data.frame(diabetes_death_counts)
colnames(diabetes_death_df) <- c("Diabetes", "Death_Event", "Count")

# Create a pie chart
pie_chart <- ggplot(data = diabetes_death_df, aes(x = "", y = Count, fill = factor(Death_Event))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Diabetes vs. Death Event", fill = "Death Event", x = NULL, y = NULL) +
  scale_fill_manual(values = c("green", "red"), labels = c("Survived", "Death")) +
  theme_minimal()

# Print the pie chart
print(pie_chart)


# ******************* Blood Pressure ***************
library(ggplot2)

# Calculate the counts of individuals with normal and high blood pressure, grouped by death event
bp_death_counts <- table(df$high_blood_pressure, df$DEATH_EVENT)

# Create a data frame from the counts
bp_death_df <- as.data.frame(bp_death_counts)
colnames(bp_death_df) <- c("Blood_Pressure", "Death_Event", "Count")

# Update Death_Event labels
bp_death_df$Death_Event <- ifelse(bp_death_df$Death_Event == 1, "Death", "No Death")

# Create a pie chart
pie_chart <- ggplot(data = bp_death_df, aes(x = "", y = Count, fill = factor(Death_Event))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Blood Pressure vs. Death Event", fill = "Death Event", x = NULL, y = NULL) +
  scale_fill_manual(values = c("green", "red"), labels = c("Survived", "Death")) +
  theme_minimal()

# Print the pie chart
print(pie_chart)


# ***************** Platlets **************************
library(ggplot2)

# Create a grouped bar chart
grouped_bar_chart <- ggplot(df, aes(x = factor(DEATH_EVENT), y = platelets, fill = factor(DEATH_EVENT))) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Platelets Levels vs. Death Event", x = "Death Event", y = "Mean Platelets Levels") +
  scale_fill_manual(values = c("lightblue", "lightgreen"), labels = c("Survived", "Death")) +
  theme_minimal()

# Display the grouped bar chart
print(grouped_bar_chart)

# ***************** Sex ***************************
library(ggplot2)

# Create a stacked bar chart
stacked_bar_chart <- ggplot(df, aes(x = factor(sex), fill = factor(DEATH_EVENT))) +
  geom_bar() +
  labs(title = "Sex vs. Death Event", x = "Sex", y = "Count") +
  scale_fill_manual(values = c("lightblue", "lightgreen"), labels = c("Survived", "Death")) +
  theme_minimal()

# Display the stacked bar chart
print(stacked_bar_chart)

# ****************** Smoking ************************
library(ggplot2)

# Create a bar chart
bar_chart <- ggplot(df, aes(x = factor(smoking), fill = factor(DEATH_EVENT))) +
  geom_bar() +
  labs(title = "Smoking vs. Death Event", x = "Smoking", y = "Count") +
  scale_fill_manual(values = c("lightblue", "lightgreen"), labels = c("Survived", "Death")) +
  theme_minimal()

# Display the bar chart
print(bar_chart)

# **************** Filtering Age 60 to 80 ******************
# **************** With Diabetes ***************************
library(ggplot2)

# Filter the data for age between 60 to 80
filtered_df <- df[df$age >= 60 & df$age <= 80, ]

# Calculate the count of deaths by diabetes status
death_counts <- table(filtered_df$diabetes, filtered_df$DEATH_EVENT)[2, ]

# Create a data frame for the pie chart
pie_data <- data.frame(Diabetes = c("No Diabetes", "Diabetes"), Deaths = death_counts)

# Create a pie chart
pie_chart <- ggplot(pie_data, aes(x = "", y = Deaths, fill = Diabetes)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Diabetes and Death Event", fill = "Diabetes", x = NULL, y = NULL) +
  scale_fill_manual(values = c("lightblue", "lightgreen"), labels = c("No Diabetes", "Diabetes")) +
  theme_void()

# Display the pie chart
print(pie_chart)

 # ************************ BP ***********************
library(ggplot2)

# Filter the data for age between 60 to 80
filtered_df <- df[df$age >= 60 & df$age <= 80, ]

# Calculate the count of deaths by high blood pressure status
death_counts <- table(filtered_df$high_blood_pressure, filtered_df$DEATH_EVENT)[2, ]

# Create a data frame for the pie chart
pie_data <- data.frame(BloodPressure = c("No High BP", "High BP"), Deaths = death_counts)

# Create a pie chart
pie_chart <- ggplot(pie_data, aes(x = "", y = Deaths, fill = BloodPressure)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "High Blood Pressure and Death Event", fill = "Blood Pressure", x = NULL, y = NULL) +
  scale_fill_manual(values = c("lightblue", "lightgreen"), labels = c("No High BP", "High BP")) +
  theme_void()

# Display the pie chart
print(pie_chart)

# ***********************************************************************

# Selecting Age between 60 to 80
#Including only diabetes and Death = 1

library(ggplot2)
library(dplyr)


# Step 2: DPLYR commands for visualization
# Select and subset the data for age between 60 to 80 with deaths and diabetes
subset_data <- df %>% filter(age >= 60, age <= 80) %>%
  select(age, DEATH_EVENT, diabetes)

# Arrange the data by age in ascending order
arranged_data <- subset_data %>% arrange(age)

# Filter the data for deaths and diabetes
filtered_data <- subset_data %>% filter(DEATH_EVENT == 1, diabetes == 1)

# Create a new factor variable "age_group" based on age
subset_data <- subset_data %>% mutate(age_group = ifelse(age < 70, "60-69", "70-80"))

# Step 3: Visualization using a pie chart
pie_chart <- subset_data %>%
  group_by(age_group) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = "", y = count, fill = age_group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Age Group Distribution", fill = "Age Group") +
  theme_minimal() +
  theme(legend.position = "right")

# Display the pie chart
print(pie_chart)


# **************************************************************
# With BP


# Step 2: DPLYR commands for visualization
# Select and subset the data for age between 60 to 80 with deaths and high blood pressure
subset_data <- df %>% filter(age >= 60, age <= 80) %>%
  select(age, DEATH_EVENT, high_blood_pressure)

# Arrange the data by age in ascending order
arranged_data <- subset_data %>% arrange(age)

# Filter the data for deaths and high blood pressure
filtered_data <- subset_data %>% filter(DEATH_EVENT == 1, high_blood_pressure == 1)

# Create a new factor variable "age_group" based on age
subset_data <- subset_data %>% mutate(age_group = ifelse(age < 70, "60-69", "70-80"))

# Step 3: Visualization using a pie chart
pie_chart <- subset_data %>%
  group_by(age_group, high_blood_pressure) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = "", y = count, fill = high_blood_pressure)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Age Group Distribution by High Blood Pressure", fill = "High Blood Pressure") +
  theme_minimal() +
  theme(legend.position = "right")

# Display the pie chart
print(pie_chart)

# ****************************** Matrix, Factor, and List *********************************
# Load the required libraries
library(ggplot2)

# Create a matrix from a subset of the data frame
subset_df <- df[, c("age", "time")]
matrix_data <- as.matrix(subset_df)

# Convert the "sex" column to a factor
df$sex <- factor(df$sex)

# Create a list to store the data frame and matrix
my_list <- list(data_frame = df, matrix = matrix_data)

# Visualization using the matrix
matrix_plot <- ggplot(data = as.data.frame(matrix_data), aes(x = age, y = time)) +
  geom_point() +
  labs(title = "Matrix Visualization", x = "Age", y = "Time") +
  theme_minimal()

# Visualization using the factor
factor_plot <- ggplot(data = df, aes(x = sex, fill = factor(DEATH_EVENT))) +
  geom_bar() +
  labs(title = "Factor Visualization", x = "Sex", y = "Count", fill = "Death Event") +
  theme_minimal()

# Visualization using the list (accessing the data frame)
list_plot <- ggplot(data = my_list$data_frame, aes(x = age, y = time)) +
  geom_point() +
  labs(title = "List Visualization (Data Frame)", x = "Age", y = "Time") +
  theme_minimal()

# Display the plots
print(matrix_plot)
print(factor_plot)
print(list_plot)



















