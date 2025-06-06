# Load Packages ----
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(survminer)
library(cluster)
library(forestmodel)
library(psych)
library(MASS)
library(ADGofTest)
library(rms)
library(broom)

# Load Data ----
data(cancer, package="survival")
head(veteran)

# Deskripsi Data ----
str(veteran) # Observasi: 137. Variabel: 8. 
# time, status
# Ada 3 variabel prediktor tipe kategorik: trt, celltype, prior
# Ada 3 variabel prediktor tipe numerik: age, diagtime, karno
summary(veteran) # Mean status: 0.9343 (Persentase data lengkap: 93,43%)

# Preprocessing ----

## Cek Missing Value ----
sum(is.na(veteran)) # Tidak ada missing value

## Ubah tipe variabel kategorik menjadi factor ----
str(veteran) # trt, prior belum menjadi factor
veteran$trt <- as.factor(veteran$trt)
veteran$prior <- as.factor(veteran$prior)
str(veteran) # Lihat hasil

## Ubah variabel numerik "age" menjadi kategorik ----
age_data <- veteran$age
age_data <- as.matrix(age_data)
# Determine the optimal number of clusters using the Elbow Method
wss <- numeric(10) # Vector to store the within-cluster sum of squares
for (i in 1:10) {
  wss[i] <- sum(kmeans(age_data, centers=i, nstart=25)$tot.withinss)
}
# Plot the Elbow Method
plot(1:10, wss, type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main="Elbow Method for Determining Optimal Number of Clusters")
# Based on the plot, choose the number of clusters
k <- 2
# Perform k-means clustering
set.seed(123) # Setting seed for reproducibility
kmeans_result <- kmeans(age_data, centers = k, nstart = 25)
# Examine the clustering result
print(kmeans_result)
# Add cluster assignments to the original dataset
veteran$age_cluster <- as.factor(kmeans_result$cluster)




## Ubah variabel numerik "diagtime" menjadi kategorik ----
diagtime_data <- veteran$diagtime
diagtime_data <- as.matrix(diagtime_data)
# Determine the optimal number of clusters using the Elbow Method
wss <- numeric(10) # Vector to store the within-cluster sum of squares
for (i in 1:10) {
  wss[i] <- sum(kmeans(diagtime_data, centers=i, nstart=25)$tot.withinss)
}
# Plot the Elbow Method
plot(1:10, wss, type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main="Elbow Method for Determining Optimal Number of Clusters")
# Based on the plot, choose the number of clusters
k <- 2
# Perform k-means clustering
set.seed(123) # Setting seed for reproducibility
kmeans_result <- kmeans(diagtime_data, centers = k, nstart = 25)
# Examine the clustering result
print(kmeans_result)
# Add cluster assignments to the original dataset
veteran$diagtime_cluster <- as.factor(kmeans_result$cluster)



## Ubah variabel numerik "karno" menjadi kategorik ----
karno_data <- veteran$karno
karno_data <- as.matrix(karno_data)
# Determine the optimal number of clusters using the Elbow Method
wss <- numeric(10) # Vector to store the within-cluster sum of squares
for (i in 1:10) {
  wss[i] <- sum(kmeans(karno_data, centers=i, nstart=25)$tot.withinss)
}
# Plot the Elbow Method
plot(1:10, wss, type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main="Elbow Method for Determining Optimal Number of Clusters")
# Based on the plot, choose the number of clusters
k <- 2
# Perform k-means clustering
set.seed(123) # Setting seed for reproducibility
kmeans_result <- kmeans(karno_data, centers = k, nstart = 25)
# Examine the clustering result
print(kmeans_result)
# Add cluster assignments to the original dataset
veteran$karno_cluster <- as.factor(kmeans_result$cluster)

# Hasil Perubahan Variabel Numerik menjadi Kategorik:
str(veteran)



# EDA ----
# Ada 6 variabel prediktor tipe kategorik: trt, celltype, prior, age_cluster, diagtime_cluster, karno_cluster


## Statistika Deskriptif ----
describe(veteran)
summary(veteran)
str(veteran)


## Variabel "trt" ----
## Boxplot "trt"
ggplot(data = veteran) +
  geom_boxplot(aes(x = factor(trt),
                   y = time),
               fill = "coral",
               alpha = .6) +
  labs(x = "Type of Treatment",
       y = "Survival Time") +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 18)) +
  theme_light()

## Tabel Distribusi
table(veteran$status, veteran$trt)

## Grafik Fungsi Survival "trt"
treatment <- survfit(Surv(time, status) ~ trt, data=veteran)
ggsurvplot(treatment)
ggsurvplot(treatment, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c("1 = standard", "2 = test"), legend.title="Treatment",  
           palette=c("coral", "mediumturquoise"), 
           title="", 
           risk.table.height=.3)

## Uji Log Rank
survdiff(Surv(time, status) ~ trt, data=veteran)



## Variabel "celltype" ----
## Boxplot "celltype"
ggplot(data = veteran) +
  geom_boxplot(aes(x = factor(celltype),
                   y = time),
               fill = "coral",
               alpha = .6) +
  labs(x = "Cell Type",
       y = "Survival Time") +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 18)) +
  theme_light()

## Tabel Distribusi
table(veteran$status, veteran$celltype)

## Grafik Fungsi Survival "celltype"
celltype <- survfit(Surv(time, status) ~ celltype, data=veteran)
ggsurvplot(celltype)
ggsurvplot(celltype, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c('1 = squamous','2 = smallcell','3 = adeno','4 = large'), legend.title="Cell Type",  
           palette=c("coral", "lightgoldenrod1", "mediumturquoise", "mediumpurple1"), 
           title="", 
           risk.table.height=.3)

## Uji Log Rank
survdiff(Surv(time, status) ~ celltype, data=veteran)



## Variabel "prior" ----
## Boxplot "prior"
ggplot(data = veteran) +
  geom_boxplot(aes(x = factor(prior),
                   y = time),
               fill = "coral",
               alpha = .6) +
  labs(x = "Prior Therapy",
       y = "Survival Time") +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 18)) +
  theme_light()

## Tabel Distribusi
table(veteran$status, veteran$prior)

## Grafik Fungsi Survival "prior"
prior <- survfit(Surv(time, status) ~ prior, data=veteran)
ggsurvplot(prior)
ggsurvplot(prior, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c('0 = no','10 = yes'), legend.title="Prior Therapy",  
           palette=c("coral", "mediumturquoise"), 
           title="", 
           risk.table.height=.3)

## Uji Log Rank
survdiff(Surv(time, status) ~ prior, data=veteran)



## Variabel "age_cluster" ----
## Boxplot "age_cluster"
ggplot(data = veteran) +
  geom_boxplot(aes(x = factor(age_cluster),
                   y = time),
               fill = "coral",
               alpha = .6) +
  labs(x = "Age Cluster",
       y = "Survival Time") +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 18)) +
  theme_light()


## Tabel Distribusi
table(veteran$status, veteran$age_cluster)

## Grafik Fungsi Survival "age_cluster"
age_cluster <- survfit(Surv(time, status) ~ age_cluster, data=veteran)
ggsurvplot(age_cluster)
ggsurvplot(age_cluster, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c('1 = Cluster 1','2 = Cluster 2'), legend.title="Age",  
           palette=c("coral", "mediumturquoise"), 
           title="", 
           risk.table.height=.3)

## Uji Log Rank
survdiff(Surv(time, status) ~ age_cluster, data=veteran)




## Variabel "diagtime_cluster" ----
## Boxplot "diagtime_cluster"
ggplot(data = veteran) +
  geom_boxplot(aes(x = factor(diagtime_cluster),
                   y = time),
               fill = "coral",
               alpha = .6) +
  labs(x = "Diagnosis Cluster",
       y = "Survival Time") +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 18)) +
  theme_light()

## Tabel Distribusi
table(veteran$status, veteran$diagtime_cluster)

## Grafik Fungsi Survival "diagtime_cluster"
diagtime_cluster <- survfit(Surv(time, status) ~ diagtime_cluster, data=veteran)
ggsurvplot(diagtime_cluster)
ggsurvplot(diagtime_cluster, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c('1 = Cluster 1','2 = Cluster 2'), legend.title="Diagnostic Time",  
           palette=c("coral", "lightgoldenrod1"), 
           title="", 
           risk.table.height=.3)

## Uji Log Rank
survdiff(Surv(time, status) ~ diagtime_cluster, data=veteran)


## Variabel "karno_cluster" ----
## Boxplot "karno_cluster"
ggplot(data = veteran) +
  geom_boxplot(aes(x = factor(karno_cluster),
                   y = time),
               fill = "coral",
               alpha = .6) +
  labs(x = "Karnofsky Performance Score Cluster",
       y = "Survival Time") +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 18)) +
  theme_light()


## Tabel Distribusi
table(veteran$status, veteran$karno_cluster)

## Grafik Fungsi Survival "karno_cluster"
karno_cluster <- survfit(Surv(time, status) ~ karno_cluster, data=veteran)
ggsurvplot(karno_cluster)
ggsurvplot(karno_cluster, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c('1 = Cluster 1','2 = Cluster 2'), legend.title="Karnofsky Performance Score",  
           palette=c("coral", "mediumturquoise"), 
           title="", 
           risk.table.height=.3)

## Uji Log Rank
survdiff(Surv(time, status) ~ karno_cluster, data=veteran)

## Stratified Test dengan ggforest ----
fit.coxph <- coxph(Surv(time, status) ~ trt + celltype + age_cluster + diagtime_cluster + karno_cluster + prior, 
                   data = veteran)
ggforest(fit.coxph, data = veteran)

## Kesimpulan: Variabel celltype dan karno_cluster signifikan ----
# artinya celltype dan karno_cluster merupakan variabel penjelas yang potensial untuk fungsi survival dari kejadian pasien yang tidak survive.


## Boxplot antara "celltype" dan "karno_cluster" ----
ggplot(veteran) +
  geom_boxplot(aes(x = factor(celltype),
                   y = time,
                   fill = factor(karno_cluster)),
               alpha = .6) +
  labs(x = "Cell Type",
       y = "Patient's Survival Time") +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 18)) +
  theme_light()

# Uji Asumsi Cox PH ----
## Uji Asumsi Cox PH secara Grafis ----
# Sumber: Modul Ibu Sarini

### Variabel "trt" ----
legend_labels <- levels(veteran$trt)
plot( survfit( Surv(time, status ) ~ trt, data=veteran),
      fun="cloglog", lty=1:2, mark.time=FALSE,
      xlab="Waktu survival, T", ylab="log(H(t))",
      main="Treatment" )
legend( 1, 1, lty=1:2, legend=legend_labels,
        bty="n" )

### Variabel "celltype" ----
legend_labels <- levels(veteran$celltype)
plot( survfit( Surv(time, status ) ~ celltype, data=veteran),
      fun="cloglog", lty=1:2, mark.time=FALSE,
      xlab="Waktu survival, T", ylab="log(H(t))",
      main="Cell Type" )
legend( 1, 1, lty=1:2, legend=legend_labels,
        bty="n" )

### Variabel "prior" ----
legend_labels <- levels(veteran$prior)
plot( survfit( Surv(time, status ) ~ prior, data=veteran),
      fun="cloglog", lty=1:2, mark.time=FALSE,
      xlab="Waktu survival, T", ylab="log(H(t))",
      main="Prior Therapy" )
legend( 1, 1, lty=1:2, legend=legend_labels,
        bty="n" )

### Variabel "age_cluster" ----
legend_labels <- levels(veteran$age_cluster)
plot( survfit( Surv(time, status ) ~ age_cluster, data=veteran),
      fun="cloglog", lty=1:2, mark.time=FALSE,
      xlab="Waktu survival, T", ylab="log(H(t))",
      main="Age" )
legend( 1, 1, lty=1:2, legend=legend_labels,
        bty="n" )

### Variabel "diagtime_cluster" ----
legend_labels <- levels(veteran$diagtime_cluster)
plot( survfit( Surv(time, status ) ~ diagtime_cluster, data=veteran),
      fun="cloglog", lty=1:2, mark.time=FALSE,
      xlab="Waktu survival, T", ylab="log(H(t))",
      main="Diagnosis Time" )
legend( 1, 1, lty=1:2, legend=legend_labels,
        bty="n" )

### Variabel "karno_cluster" ----
legend_labels <- levels(veteran$karno_cluster)
plot( survfit( Surv(time, status ) ~ karno_cluster, data=veteran),
      fun="cloglog", lty=1:2, mark.time=FALSE,
      xlab="Waktu survival, T", ylab="log(H(t))",
      main="Karnofsky Performance Score" )
legend( 1, 1, lty=1:2, legend=legend_labels,
        bty="n" )

# Model ----
# Model 1: Use all covariate ----
model1 <- coxph(Surv(time, status) ~ trt + celltype + age_cluster + diagtime_cluster + karno_cluster + prior, data = veteran, 
                method='breslow')
summary(model1)
# Asumsi Cox PH
cox.zph(model1)

# Model 2: Use all covariate, Backward AIC ----
model2 <- stepAIC(object=model1, data=veteran_data, direction="backward")
summary(model2)
# Asumsi Cox PH
cox.zph(model2)

# Model 3: Use all covariate, strata:karno_cluster ----
model3 <- coxph(Surv(time, status) ~ trt + celltype + age_cluster + diagtime_cluster + strata(karno_cluster) + prior, data = veteran, 
                      method='breslow')
summary(model3)
# Asumsi Cox PH
cox.zph(model3)

# Model 3.5: Use all covariate, except prior, strata:karno_cluster ----
model3.5 <- coxph(Surv(time, status) ~ trt + celltype + age_cluster + diagtime_cluster + strata(karno_cluster), data = veteran, 
                  method='breslow')
summary(model3.5)
# Asumsi Cox PH
cox.zph(model3.5)

# Model 4: Use all covariate, strata:celltype ----
model4 <- coxph(Surv(time, status) ~ trt + strata(celltype) + age_cluster + diagtime_cluster + karno_cluster + prior, data = veteran, 
                      method='breslow')
summary(model4)
# Asumsi Cox PH
cox.zph(model4)

# Model 5: Use all covariate, strata:celltype and karno_cluster ----
model5 <- coxph(Surv(time, status) ~ trt + strata(celltype) + age_cluster + diagtime_cluster + strata(karno_cluster) + prior, data = veteran, 
                      method='breslow')
summary(model5)
# Asumsi Cox PH
cox.zph(model5)

# Model 6: Use all covariate, strata:karno_cluster, Backward AIC ----
model6_draft <- coxph(Surv(time, status) ~ trt + celltype + age_cluster + diagtime_cluster + strata(karno_cluster) + prior, data = veteran, 
                method='breslow')
model6 <- stepAIC(object=model6_draft, data=veteran_data, direction="backward")
summary(model6)
# Asumsi Cox PH
cox.zph(model6)

# Model 7: Use all covariate, strata:celltype, Backward AIC ----
model7_draft <- coxph(Surv(time, status) ~ trt + strata(celltype) + age_cluster + diagtime_cluster + karno_cluster + prior, data = veteran, 
                      method='breslow')
model7 <- stepAIC(object=model7_draft, data=veteran_data, direction="backward")
summary(model7)
# Asumsi Cox PH
cox.zph(model7)

# Model 8: Use all covariate, strata:celltype and karno_cluster, Backward AIC ----
model8_draft <- coxph(Surv(time, status) ~ trt + strata(celltype) + age_cluster + diagtime_cluster + strata(karno_cluster) + prior, data = veteran, 
                      method='breslow')
# model8 <- stepAIC(object=model8_draft, data=veteran_data, direction="backward")
# Tidak ada yang signifikan


# Informasi Setiap Model ----
allmodel <- list(model1,model2,model3,model4,model5, model6, model7)

## Variabel yang signifikan ----
for (i in 1:length(allmodel)) {
  model <- allmodel[[i]]
  tidy_model <- tidy(model)
  significant_vars <- tidy_model %>%
    filter(p.value < 0.05)
  cat("Model", i, ":", deparse(substitute(model)), "\n")
  cat("Variabel signifikan:\n")
  print(significant_vars)
  cat("\n")
}

## AIC ----
AIC(model1,model2,model3,model3.5,model4,model5,model6, model7) #model 8 tidak ada

# Uji Lanjutan untuk Model 6 ----
## Cox-Snell residual plot ----
veteran$resid_mart = residuals(model6, type="martingale")

veteran$resid_coxsnell = -(veteran$resid_mart-veteran$status)

fit_coxsnell = coxph(Surv(resid_coxsnell, status)~1, data=veteran)
df_base_haz = basehaz(fit_coxsnell, centered=FALSE)
ggplot(data=df_base_haz, mapping=aes(x=time, y=hazard)) +
  geom_point() +
  labs(x="Cox-Snell residuals as pseudo observed times",
       y="Estimated cumulative hazard at pseudo observed times") +
  geom_abline(intercept=0, slope=1) +
  theme_bw() + theme(legend.key=element_blank())

## Martingale Residual Plot ----
ggplot(data=veteran, mapping=aes(x=unclass(celltype), y=resid_mart)) +
  geom_point() +
  geom_smooth() +
  theme_bw() + theme(legend.key=element_blank())
