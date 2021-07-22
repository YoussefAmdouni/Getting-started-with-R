# Importing libraries 

library(tidyverse)
library(ggthemes)

# Importing data into R

employee.data <- read.csv("data/employee-data.csv",
                          skip = 23, stringsAsFactors = FALSE)

# skip = 23 tells R to skip the first 23 rows of the data set when reading the data

names(employee.data) <- c("Employee number", "First name", "Last name", "Birth date", "Gender", 
                          "Job title", "Salary", "From date", "To date")

# exporting data as a CSV
##write.csv(employee.data, file = "employee_data_2.csv", row.names = FALSE)

head(employee.data)

any(is.na(employee.data))

employee.data$Gender <- as.factor(employee.data$Gender)
employee.data$`Job title` <- as.factor(employee.data$`Job title`)

# plot the salary frequencies larger than 45000.
filter(employee.data, Salary > 45000) %>%
ggplot(aes(Salary)) + 
geom_histogram(color = "darkslateblue", fill = "dodgerblue", alpha = 0.7) +
labs(title = "Salary distribution", x = "Salary", y = "Number of employees in the salary bracket") +
theme_solarized_2(light = F, base_size = 12, base_family = "serif") + 
theme(plot.title = element_text(hjust = 0.5, size = 16))

# plot job position by gender
employee.data %>% 
  ggplot(aes(`Job title`, fill = Gender)) + geom_bar() + 
  labs(title = "Job Positions by Gender", y = "Employee count",x = "Job position") +
  theme_minimal(base_family = "serif", base_size = 12) +
  scale_fill_manual(values = c("firebrick4", "gainsboro")) + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
  
# salary distribution based on position and gender
employee.data %>%
  ggplot(aes(x = `Job title`, y = Salary)) + 
  geom_boxplot(outlier.color = "gray0", outlier.shape = 1, outlier.size = 2) +
  geom_jitter(width = 0.4, aes(color = Gender)) +
  ggtitle("Salary distribution", subtitle = "based on position and gender") +
  ylab("Salary") + xlab("Job position") + 
  theme_bw() + 
  coord_flip() + 
  scale_color_brewer(palette="Set1")


# Good earners
employee.data %>% 
  select(ends_with("name"), Gender, Salary) %>%
  filter(Salary >= 70000) %>% 
  arrange(Salary)



employee.data %>% 
  group_by(`Job title`, Gender) %>% 
  summarise(avg.salary = mean(Salary)) %>% 
  mutate(monthly = avg.salary/12) %>% 
  arrange(Gender, desc(monthly))