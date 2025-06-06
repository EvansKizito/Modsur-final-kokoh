# Chapter 2: Preprocessing and Exploratory Data Analysis for the AIDS clinical trials dataset

# Load required packages
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

# Function to plot the elbow method for a numeric vector
elbow_plot <- function(data, var_name = "variable", max_k = 10) {
  wss <- sapply(1:max_k, function(k) {
    kmeans(data, centers = k, nstart = 25)$tot.withinss
  })
  plot(1:max_k, wss, type = "b", pch = 19,
       xlab = "Number of clusters K",
       ylab = "Total within-clusters sum of squares",
       main = paste("Elbow Method -", var_name))
}

# Load the dataset from the CSV file
# The file should be located in the same directory as this script

aids <- read.csv("aids_clinical_trials_data.csv")

# Preview the data and summary statistics
head(aids)
summary(aids)

# Check for missing values
sum(is.na(aids))

# Convert selected categorical variables to factor
categorical_vars <- c("trt", "hemo", "homo", "drugs", "karnof", "oprior",
                      "z30", "zprior", "race", "gender", "str2", "strat",
                      "symptom", "treat", "offtrt", "cid")
aids[categorical_vars] <- lapply(aids[categorical_vars], as.factor)

# K-means clustering to create categorical versions of age, cd40 and wtkg
set.seed(123)

# Determine k using elbow plot (optional)
elbow_plot(aids$age, var_name = "age")
elbow_plot(aids$cd40, var_name = "cd40")
elbow_plot(aids$wtkg, var_name = "wtkg")

# age_cluster
k <- 2
age_data <- aids$age
kmeans_result <- kmeans(age_data, centers = k, nstart = 25)
print(kmeans_result)
aids$age_cluster <- as.factor(kmeans_result$cluster)

# cd40_cluster
cd40_data <- aids$cd40
kmeans_result <- kmeans(cd40_data, centers = k, nstart = 25)
print(kmeans_result)
aids$cd40_cluster <- as.factor(kmeans_result$cluster)

# wtkg_cluster
wtkg_data <- aids$wtkg
kmeans_result <- kmeans(wtkg_data, centers = k, nstart = 25)
print(kmeans_result)
aids$wtkg_cluster <- as.factor(kmeans_result$cluster)

# Inspect the structure after preprocessing
str(aids)

#------------------------------------
# Exploratory Data Analysis
#------------------------------------

do_km_analysis <- function(var_name) {
  formula <- as.formula(paste0("Surv(time, cid) ~ ", var_name))
  fit <- survfit(formula, data = aids)
  ggsurv <- ggsurvplot(
    fit, data = aids, risk.table = TRUE,
    pval = FALSE, conf.int = FALSE,
    ggtheme = theme_minimal(),
    xlab = "Time", ylab = "Survival probability",
    legend.title = var_name
  )
  print(ggsurv)
  print(boxplot(aids$time ~ aids[[var_name]], xlab = var_name, ylab = "Time"))
  print(table(aids$cid, aids[[var_name]]))
  test <- survdiff(formula, data = aids)
  print(test)
}

# 1) Treatment
km_trt <- do_km_analysis("trt")

# 2) Gender
km_gender <- do_km_analysis("gender")

# 3) Age cluster
km_age <- do_km_analysis("age_cluster")

# 4) cd40 cluster
km_cd40 <- do_km_analysis("cd40_cluster")

# 5) wtkg cluster
km_wtkg <- do_km_analysis("wtkg_cluster")

# Cox PH model with selected covariates
cox_model <- coxph(
  Surv(time, cid) ~ trt + gender + age_cluster + cd40_cluster + wtkg_cluster,
  data = aids,
  method = "breslow"
)

# Visualize hazard ratios
ggforest(cox_model)

# Example additional plot
ggplot(aids, aes(trt, time, fill = age_cluster)) +
  geom_boxplot() +
  theme_minimal()
