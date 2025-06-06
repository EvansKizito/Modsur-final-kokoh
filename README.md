# Model survival final

Ini adalah project akhir mata kuliah model survival dengan tema regresi Cox PH.

1. `aids_clinical_trials_data.csv` merupakan file utama dalam pengerjaan project ini.
2. `FINAL PROJECT_MODSUR (G).pdf`dan Regresi Cox Ph Kelompok G.R merupakan referensi pengerjaan project-nya (tujuan sama).
3. Pengerjaan utamanya memakai bahasa pemrograman R.
4. Cleaning boleh pakai Python (jika ada).
5. Fokus tugas saya dalam tim ini adalah BAB 2 yang ada pada referensi.
6. Script `chapter2_analysis.R` berisi kode R untuk melakukan preprocessing dan
   exploratory data analysis pada data `aids_clinical_trials_data.csv` sesuai
   langkah-langkah di BAB 2. Script ini juga membuat plot metode elbow untuk
   membantu menentukan berapa jumlah klaster (k) yang digunakan pada k-means.

Untuk menjalankan analisis BAB 2, buka R kemudian jalankan:

```R
source("chapter2_analysis.R")
```
Variable Name	Role	Type	Demographic	Description	Units	Missing Values
pidnum	ID	Integer		Patient ID		no
cid	Target	Binary		censoring indicator (1 = failure, 0 = censoring)		no
time	Feature	Integer		time to failure or censoring		no
trt	Feature	Integer		treatment indicator (0 = ZDV only; 1 = ZDV + ddI, 2 = ZDV + Zal, 3 = ddI only)		no
age	Feature	Integer	Age	age (yrs) at baseline		no
wtkg	Feature	Continuous		weight (kg) at baseline		no
hemo	Feature	Binary		hemophilia (0=no, 1=yes)		no
homo	Feature	Binary	Sexual Orientation	homosexual activity (0=no, 1=yes)		no
drugs	Feature	Binary		history of IV drug use (0=no, 1=yes)		no
karnof	Feature	Integer		Karnofsky score (on a scale of 0-100)		no
oprior	Feature	Binary		Non-ZDV antiretroviral therapy pre-175 (0=no, 1=yes)		no
z30	Feature	Binary		ZDV in the 30 days prior to 175 (0=no, 1=yes)		no
zprior	Feature	Binary		ZDV prior to 175 (0=no, 1=yes)		no
preanti	Feature	Integer		# days pre-175 anti-retroviral therapy		no
race	Feature	Integer	Race	race (0=White, 1=non-white)		no
gender	Feature	Binary	Gender	gender (0=F, 1=M)		no
str2	Feature	Binary		antiretroviral history (0=naive, 1=experienced)		no
strat	Feature	Integer		antiretroviral history stratification (1='Antiretroviral Naive',2='> 1 but <= 52 weeks of prior antiretroviral therapy',3='> 52 weeks)		no
symptom	Feature	Binary		symptomatic indicator (0=asymp, 1=symp)		no
treat	Feature	Binary		treatment indicator (0=ZDV only, 1=others)		no
offtrt	Feature	Binary		indicator of off-trt before 96+/-5 weeks (0=no,1=yes)		no
cd40	Feature	Integer		CD4 at baseline		no
cd420	Feature	Integer		CD4 at 20+/-5 weeks		no
cd80	Feature	Integer		CD8 at baseline		no
cd820	Feature	Integer		CD8 at 20+/-5 weeks		no
