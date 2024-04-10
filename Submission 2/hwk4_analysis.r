# Meta --------------------------------------------------------------------

## Title:         Econ 470 Homework 3 
## Author:        Leila Mulveny
## Date Created:  3/25/2024
## Description:   This file renders/runs all relevant R code for the assignment


# Preliminaries -----------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, rddensity, rdd)

final.data<- read_rds("data/output/final_ma_data.rds")

# Question 1
# Remove all SNPs, 800-series plans, and prescription drug only plans (i.e., plans that do not offer Part C benefits). 
# Provide a box and whisker plot showing the distribution of plan counts by county over time. 
# Do you think that the number of plans is sufficient, too few, or too many?

# Group data by county and year, and summarize plan counts
summary_data <- final.data %>%
  group_by(county, year) %>%
  summarize(planid = n())

# Calculate upper and lower bounds for outliers
upper_bound <- quantile(summary_data$planid, 0.75) + 1.5 * IQR(summary_data$planid)
lower_bound <- quantile(summary_data$planid, 0.25) - 1.5 * IQR(summary_data$planid)

# Filter out outliers
summary_data_filtered <- summary_data %>%
  filter(planid <= upper_bound, planid >= lower_bound)

# Create a boxplot to visualize the distribution of plan counts by county over time
question1 <- ggplot(summary_data_filtered, aes(x = factor(year), y = planid)) +
  geom_boxplot(outlier.shape = NA) +  # Remove outliers from the plot
  labs(x = "Year", y = "Plan Counts", title = "Distribution of Plan Counts by County Over Time") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed

question1



#Question 2 
# Provide bar graphs showing the distribution of star ratings in 2010, 2012, and 2015. 
#How has this distribution changed over time?

library(ggplot2)

# Filter the data for the years 2010, 2012, and 2015
filtered_data <- final.data %>%
  filter(year %in% c(2010, 2012, 2015))

# Count the occurrences of each star rating for each year
rating_counts <- filtered_data %>%
  group_by(year, Star_Rating) %>%
  summarise(count = n())

# Create bar graphs for each year's distribution of star ratings
question2<- ggplot(rating_counts, aes(x = factor(Star_Rating), y = count, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~year, scales = "free") +
  labs(x = "Star Rating", y = "Count", title = "Distribution of Star Ratings Over Time") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed

question2


#Question 3
# Plot the average benchmark payment over time from 2010 through 2015. 
#How much has the average benchmark payment risen over the years?

# Filter the data for years 2010 to 2015
filtered_data <- final.data %>%
  filter(year >= 2010 & year <= 2015)

# Group the data by year and calculate the average benchmark payment for each year
average_benchmark_payments <- filtered_data %>%
  group_by(year) %>%
  summarize(average_benchmark_payment = mean(ma_rate, na.rm = TRUE))

# Print or view the result
print(average_benchmark_payments)

library(ggplot2)

# Assuming average_benchmark_payments is the data frame containing average benchmark payments for each year

# Plotting the average benchmark payments over time
question3<- ggplot(average_benchmark_payments, aes(x = year, y = average_benchmark_payment)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Benchmark Payment Over Time (2010-2015)",
       x = "Year",
       y = "Average Benchmark Payment") +
  theme_minimal()

question3

#Question 4
# Plot the average share of Medicare Advantage (relative to all Medicare eligibles) over time from 2010 through 2015. 
#Has Medicare Advantage increased or decreased in popularity? How does this share correlate with benchmark payments?

# Load the data
ma.data <- readRDS("data/output/final_ma_data.rds")

# Calculate market share data
mkt.share.data <- ma.data %>%
  group_by(fips, year) %>%
  summarize(enroll = first(avg_enrolled),
            medicare = first(avg_eligibles),
            bench = mean(ma_rate, na.rm = TRUE)) %>%
  mutate(mkt_share = enroll / medicare)

# Create ggplot object
ma.share <- ggplot(mkt.share.data, aes(x = year, y = mkt_share))

# Add stat_summary layer for summary statistics
ma.share <- ma.share +
  stat_summary(fun = mean, geom = "line") + 
  labs(x = "Year", y = "Market Share", title = "Market Share Over Years") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed

ma.share

#Question 5
# Calculate the running variable underlying the star rating. 
# Provide a table showing the number of plans that are rounded up into a 3-star, 3.5-star, 4-star, 4.5-star, and 5-star rating.


# ma.data <- read_rds("data/output/final_ma_data.rds")
# ma.data.clean <- ma.data %>%
#   filter(!is.na(avg_enrollment) & year==2010 & !is.na(partc_score)) 

# ma.data.clean <- ma.data.clean %>%
#   mutate(raw_rating=rowMeans(
#     cbind(breastcancer_screen,rectalcancer_screen,cv_cholscreen,
#           glaucoma_test,monitoring,flu_vaccine,pn_vaccine,physical_health,
#           mental_health,osteo_test,physical_monitor,primaryaccess
#           ,nodelays,carequickly,
#           overallrating_care,overallrating_plan,
#           doctor_communicate,customer_service,osteo_manage,
#           diabetes_eye,diabetes_kidney,diabetes_bloodsugar,
#           diabetes_chol,bloodpressure,ra_manage,
#           copd_test,bladder,falling,appeals_timely,
#           appeals_review),
#     na.rm=T)) %>%
#   select(contractid, planid, fips, avg_enrollment, state, county, raw_rating, partc_score,
#          avg_eligibles, avg_enrolled, premium_partc, risk_ab, Star_Rating,
#          bid, avg_ffscost, ma_rate, plan_type, partd)

# # Round the running variable to the nearest 0.5 to determine the star rating
# ma.data.clean <- ma.data.clean %>%
#   mutate(star_rating = round(raw_rating * 2) / 2)

# # Count the number of plans in each star rating category
# star_rating_counts <- ma.data.clean %>%
#   group_by(star_rating) %>%
#   summarize(number_of_plans = n())

# # Print or view the table
# print(star_rating_counts)


##UPDATED 5
#This is an updated 5… dont know if it’s correct but it looks more similar to his: 

ma.data <- read_rds("data/output/final_ma_data.rds")

ma.data.clean <- ma.data %>%
  filter(!is.na(avg_enrollment) & year==2010 & !is.na(partc_score)) 

ma.data.clean <- ma.data.clean %>%
  mutate(raw_rating=rowMeans(
    cbind(breastcancer_screen,rectalcancer_screen,cv_cholscreen,
          glaucoma_test,monitoring,flu_vaccine,pn_vaccine,physical_health,
          mental_health,osteo_test,physical_monitor,primaryaccess
          ,nodelays,carequickly,
          overallrating_care,overallrating_plan,
          doctor_communicate,customer_service,osteo_manage,
          diabetes_eye,diabetes_kidney,diabetes_bloodsugar,
          diabetes_chol,bloodpressure,ra_manage,
          copd_test,bladder,falling,appeals_timely,
          appeals_review),
    na.rm=T)) %>%
  select(contractid, planid, fips, avg_enrollment, state, county, raw_rating, partc_score,
         avg_eligibles, avg_enrolled, premium_partc, risk_ab, Star_Rating,
         bid, avg_ffscost, ma_rate, plan_type, partd)


ma.rounded <- ma.data.clean %>%
  mutate(rounded_30 = ifelse(raw_rating >= 2.75 & raw_rating < 3.00 & Star_Rating == 3.0, 1, 0),
         rounded_35 = ifelse(raw_rating >= 3.25 & raw_rating < 3.50 & Star_Rating == 3.5, 1, 0),
         rounded_40 = ifelse(raw_rating >= 3.75 & raw_rating < 4.00 & Star_Rating == 4.00, 1, 0), 
         rounded_45 = ifelse(raw_rating >= 4.25 & raw_rating < 4.50 & Star_Rating == 4.50, 1, 0), 
         rounded_50 = ifelse(raw_rating >= 4.50 & raw_rating < 5.00 & Star_Rating == 5.00, 1, 0))

rounded_summary <- ma.rounded %>%
  summarize(`3-star` = sum(rounded_30),
            `3.5-star` = sum(rounded_35),
            `4-star` = sum(rounded_40),
            `4.5-star` = sum(rounded_45),
            `5-star` = sum(rounded_50))

rounded_summary

#Question 6
#Using the RD estimator with a bandwidth of 0.125, provide an estimate of the effect of receiving a 3-star versus a 2.5 star rating on enrollments.
#Repeat to estimate the effects at 3.5 stars, and summarize your results in a table. 

# Install the 'rdrobust' package if not already installed
if (!require("rdrobust")) install.packages("rdrobust")

# Load the 'rdrobust' package
library(rdrobust)

# Estimate the effect of receiving a 3-star versus a 2.5-star rating
ma.rd3 <- ma.data.clean %>%
  filter(Star_Rating==2.5 | Star_Rating==3) %>%
  mutate(score = raw_rating - 3,
         treat = (score>=0),
         window1 = (score>=-.125 & score<=.125),
         window2 = (score>=-.125 & score<=.125),
         mkt_share = avg_enrollment/avg_eligibles,
         ln_share = log(mkt_share),
         score_treat=score*treat)

firstest3 <- rdrobust(y=ma.rd3$mkt_share, x=ma.rd3$score, c=0,
                 h=0.125, p=1, kernel="uniform", vce="hc0",
                 masspoints="off")

#summary(firstest3)

# Extract the coefficient, standard error, z-value, and p-value
coef <- firstest3$coef
std_err <- firstest3$se
z_value <- firstest3$z
p_value <- firstest3$p

# Create a data frame
results <- data.frame(
  Rating = c("3 vs 2.5"),
  Coefficient = coef,
  Std.Error = std_err,
  Z.Value = z_value,
  P.Value = p_value
)

# Print the results
print(results)

#The coefficient (Coef.) is -0.014. This means that, on average, receiving a 3-star rating instead of a 2.5-star rating decreases enrollments by 0.014.
#This is an average effect and the actual effect may vary depending on other factors. Also, the p-value (P>|z|) is less than 0.05, which indicates that this effect is statistically significant.


# Estimate the effect of receiving a 3.5-star rating
ma.rd35 <- ma.data.clean %>%
  filter(Star_Rating==3 | Star_Rating==3.5) %>%
  mutate(score = raw_rating - 3.5,
         treat = (score>=0),
         window1 = (score>=-.125 & score<=.125),
         window2 = (score>=-.125 & score<=.125),
         mkt_share = avg_enrollment/avg_eligibles,
         ln_share = log(mkt_share),
         score_treat=score*treat)

firstest35 <- rdrobust(y=ma.rd35$mkt_share, x=ma.rd35$score, c=0,
                  h=0.125, p=1, kernel="uniform", vce="hc0",
                  masspoints="off")

#summary(firstest35)

# Extract the coefficient, standard error, z-value, and p-value
coef <- firstest35$coef
std_err <- firstest35$se
z_value <- firstest35$z
p_value <- firstest35$p

# Create a data frame
results2 <- data.frame(
  Rating = c("3 vs 3.5"),
  Coefficient = coef,
  Std.Error = std_err,
  Z.Value = z_value,
  P.Value = p_value
)

# Print the results
print(results2)

# Conventional: The estimated effect is -0.005. This suggests that, on average, receiving a 3-star rating instead of a 3.5-star rating decreases the outcome by 0.005. The standard error of this estimate is 0.0026, and the z-value is -1.944. The p-value is 1, which suggests that this effect is not statistically significant at conventional levels (e.g., p < 0.05).
# Bias-Corrected: The estimated effect is -0.062. This suggests a larger decrease in the outcome when receiving a 3-star rating instead of a 3.5-star rating. The standard error is the same as in the Conventional method, but the z-value is much larger (-23.991), indicating a stronger effect. However, the p-value is still 1, suggesting that this effect is not statistically significant.
# Robust: The estimated effect is the same as in the Bias-Corrected method (-0.062), but the standard error is larger (0.0083). The z-value is -7.444, which is smaller than in the Bias-Corrected method but still indicates a strong effect. The p-value is 1, suggesting that this effect is not statistically significant.

#Question 7
#Repeat your results for bandwidths of 0.1, 0.12, 0.13, 0.14, and 0.15 (again for 3 and 3.5 stars).
#Show all results in a graph. 
#How sensitive are your findings to the choice of bandwidth?
# Install and load necessary packages
if (!require("rdrobust")) install.packages("rdrobust")
if (!require("ggplot2")) install.packages("ggplot2")
library(rdrobust)
library(ggplot2)

# Define the bandwidths
bandwidths <- c(0.1, 0.12, 0.13, 0.14, 0.15)

# Initialize an empty data frame to store the results
resultsband <- data.frame()

# Loop over the bandwidths
for (h in bandwidths) {
  # Estimate the effect for 3-star rating
  est3 <- rdrobust(y=ma.rd3$mkt_share, x=ma.rd3$score, c=0, h=h, p=1, kernel="uniform", vce="hc0", masspoints="off")
  resultsband <- rbind(resultsband, data.frame(Bandwidth=h, Star_Rating=3, Estimate=est3$coef[1]))

  # Estimate the effect for 3.5-star rating
  est35 <- rdrobust(y=ma.rd35$mkt_share, x=ma.rd35$score, c=0, h=h, p=1, kernel="uniform", vce="hc0", masspoints="off")
  resultsband <- rbind(resultsband, data.frame(Bandwidth=h, Star_Rating=3.5, Estimate=est35$coef[1]))
}

# Plot the results
Q7 <- ggplot(resultsband, aes(x=Bandwidth, y=Estimate, color=factor(Star_Rating))) +
  geom_line() +
  labs(x="Bandwidth", y="Estimate", color="Star Rating") +
  theme_minimal()

#print(Q7)

#Question 8
#Examine (graphically) whether contracts appear to manipulate the running variable. 
#Basically look at the distribution of the running variable before and after the relevant threshold values.
#What do you find?
# Install and load necessary packages
if (!require("rddensity")) install.packages("rddensity")
library(rddensity)
# Install and load necessary packages
if (!require("rdd")) install.packages("rdd")
library(rdd)

# Create density plots for the scores around the threshold of 3 stars
dens3 <- rddensity(ma.rd3$score, c=0)
rdplotdensity(dens3, ma.rd3$score)

# Create density plots for the scores around the threshold of 3.5 stars
dens35 <- rddensity(ma.rd35$score, c=0)
rdplotdensity(dens35, ma.rd35$score)

# #Question 9
# #Similar to Question 8, examine whether plans just above the threshold values have different characteristics than contracts just below the threshold values. 
# #Use HMO and Part D status as your plan characteristics. 
#The threshold is 3.25 if you are looking at 3 v 3.5. Are these plans similar to eachother above and below?
#The threshold is 2.75 if you are looking at 2.5 v

# Load necessary libraries
library(dplyr)
library(cobalt)

# Create lp.vars (Threshold is 2.75 and bandwidth is 0.125)
lp_vars <- ma.data.clean %>% 
  ungroup() %>%
  filter((raw_rating >= 2.75 - .125 & Star_Rating == 2.5) | 
           (raw_rating <= 2.75 + .125 & Star_Rating == 3) & 
           (plan_type == "HMO/HMOPOS")) %>%
  mutate(rounded = (Star_Rating == 3)) %>%
  select(plan_type, partd, rounded) %>%
  filter(complete.cases(.))


# Create lp.covs
lp_covs <- lp_vars %>% select(plan_type, partd)

# Create plot.30
plot.30 <- love.plot(bal.tab(lp_covs, treat = lp_vars$rounded), 
                     colors = "black", 
                     shapes = "circle") +
  theme_bw() + 
  theme(legend.position = "none")

# Create lp.vars (Threshold is 3.25 and bandwidth is 0.125)
lp_vars <- ma.data.clean %>% 
  ungroup() %>%
  filter((raw_rating >= 3.25 - .125 & Star_Rating == 3) | 
           (raw_rating <= 3.25 + .125 & Star_Rating == 3.5) & 
           (plan_type == "HMO/HMOPOS")) %>%
  mutate(rounded = (Star_Rating == 3.5)) %>%
  select(plan_type, partd, rounded) %>%
  filter(complete.cases(.))

# Create lp.covs
lp_covs <- lp_vars %>% select(plan_type, partd)

# Create plot.35
plot.35 <- love.plot(bal.tab(lp_covs, treat = lp_vars$rounded), 
                     colors = "black", 
                     shapes = "circle") +
  theme_bw() + 
  theme(legend.position = "none")

save.image("submission 1/Hwk4_workspace.Rdata")