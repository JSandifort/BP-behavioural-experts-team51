library(mediation)
library(dplyr)
library(tidyverse)
library(car)

# Load the cleaned data
combined_df <- read.csv("combined_df.csv")
surveys_cleaned <- read.csv("surveys_cleaned.csv")

# Regression Analysis
model_0 <- lm(AdLikability ~ NarrativenessScore, data=combined_df)
summary(model_0)

model_M <- lm(IdentificationAvg ~ NarrativenessScore, data=combined_df)
summary(model_M)

model_Y <- lm(AdLikability ~ NarrativenessScore + IdentificationAvg, data=combined_df)
summary(model_Y)

# Mediation analysis
results <- mediate(model_M, model_Y, treat='NarrativenessScore', mediator='IdentificationAvg', boot=TRUE, sims=5000)
summary(results)


# Create a contingency table for Video1 and Video2 categories
video1_counts <- table(surveys_cleaned$Video1)
video2_counts <- table(surveys_cleaned$Video2)

# Combine counts into a data frame
video_counts <- data.frame(VideoCategory = names(video1_counts),
                           Video1 = as.vector(video1_counts),
                           Video2 = as.vector(video2_counts))

# Print the contingency table
print(video_counts)

# Perform chi-square test
chisq_test <- chisq.test(video_counts[, -1])

# Print the result
print(chisq_test)

# Perform chi-square test for equality of counts within Video1
chisq_test_video1 <- chisq.test(video1_counts)
print(chisq_test_video1)

# Perform chi-square test for equality of counts within Video2
chisq_test_video2 <- chisq.test(video2_counts)
print(chisq_test_video2)


# Load the CSV file
data <- read.csv("combined_sex_df.csv")

# Convert the 'VideoCategory' and 'Sex' columns to factors
data$VideoCategory <- as.factor(data$VideoCategory)
data$Sex <- as.factor(data$Sex)

# Perform ANOVA for each video category to test the difference in ad likability between sexes
video_categories <- unique(data$VideoCategory)

for (category in video_categories) {
  category_data <- data %>% filter(VideoCategory == category)
  
  # Perform ANOVA
  anova_model <- aov(AdLikability ~ Sex, data = category_data)
  anova_summary <- summary(anova_model)
  
  print(paste("ANOVA results for Video Category:", category))
  print(anova_summary)
  
  # Perform post-hoc test if needed
  post_hoc <- TukeyHSD(anova_model)
  print(post_hoc)
}


