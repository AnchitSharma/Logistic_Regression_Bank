# https://martinctc.github.io/blog/common-statistical-tests-in-r-part-i/
library(tidyverse)
library(vivainsights)
glimpse(pq_data[, 1:10])


hrvar_count(pq_data, hrvar = "LevelDesignation")
table(pq_data$LevelDesignation)

pq_data_grouped <- pq_data %>% 
  filter(LevelDesignation %in% c("Senior Manager", "Senior IC")) %>% 
  mutate(
    ManagerIndicator = factor(LevelDesignation, levels = c("Senior Manager", "Senior IC"))
  ) %>% 
  group_by(PersonId, ManagerIndicator) %>% 
  summarise(Multitasking_hours = mean(Multitasking_hours), .groups = "drop")


t.test(
  Multitasking_hours ~ ManagerIndicator , data = pq_data_grouped,
  paired = FALSE
)

# Testing the normality
# Assumptions of t test
# independence - sample is independent
# normality - data for each group is normally distributed
# homoscedasticity - data across samples have equal variances

pq_data_grouped %>% 
  group_by(ManagerIndicator) %>% 
  summarise(
    p = shapiro.test(Multitasking_hours)$p.value,
    statistic = shapiro.test(Multitasking_hours)$statistic
  )


# multitasking hours - IC
mth_ic <- 
  pq_data_grouped %>% 
  filter(ManagerIndicator == "Senior IC") %>% 
  pull(Multitasking_hours)

qqnorm(mth_ic, pch = 1, frame = F)
qqline(mth_ic, col = "steelblue", lwd = 2)

# multitasking hours - Man
mth_man <-
  pq_data_grouped %>% 
  filter(ManagerIndicator == "Senior Manager") %>% 
  pull(Multitasking_hours)

qqnorm(mth_man, pch = 1, frame = F)
qqline(mth_man, col = "steelblue", lwd = 2)

# F test to compare two variances
var.test(
  Multitasking_hours ~ ManagerIndicator,
  data = pq_data_grouped
)

# test homoscedasticity
dotchart(
  x = pq_data_grouped$Multitasking_hours,
  groups = pq_data_grouped$ManagerIndicator
)

boxplot(
  Multitasking_hours ~ ManagerIndicator,
  data = pq_data_grouped
)

car::leveneTest(Multitasking_hours ~ ManagerIndicator,
                data = pq_data_grouped)


wilcox.test(
  Multitasking_hours ~ ManagerIndicator,
  data = pq_data_grouped,
  paired = FALSE
)

# kruskal-wallis test
pq_data_grouped_2 <- 
  pq_data %>% 
  filter(LevelDesignation %in% c(
    "Executive", "Junior IC", "Senior IC", "Senior Manager"
  )) %>% 
  mutate(ManagerIndicator = factor(LevelDesignation)) %>% 
  group_by(PersonId, ManagerIndicator) %>% 
  summarise(Multitasking_hours = mean(Multitasking_hours), .groups = "drop")

glimpse(pq_data_grouped_2)

kruskal.test(
  Multitasking_hours ~ ManagerIndicator,
  data = pq_data_grouped_2
)

# ANOVA
# Analysis of Variance - is an alternative method that generalizes the t test
# beyond two groups, so it is used to compare three or more groups
# Assumptions of ANOVA
# The data are independent.
# The responses from each factor level have a normal population distribution.
# There distribution have the same variance.



res_aov <- aov(
  Multitasking_hours ~ ManagerIndicator,
  data = pq_data_grouped_2
)
summary(res_aov)

# Next Step after ANOVA
pairwise.t.test(
  x = pq_data_grouped_2$Multitasking_hours,
  g = pq_data_grouped_2$ManagerIndicator,
  paired = FALSE,
  p.adjust.method = "bonferroni"
)




