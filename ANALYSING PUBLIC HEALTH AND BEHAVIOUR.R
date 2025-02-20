
# Call required packages.
library('tidyverse')
library('lubridate')
library('ggplot2')
library('dplyr')
library('readxl')
library('tidyr')
library('janitor')
library('skimr')
library('sqldf')
library('plotrix')
library('knitr')
library('reshape2')


#=====================
# STEP 1: COLLECT DATA
#=====================
# Display your working directory.
getwd() 

# Set your working directory to simplify calls to data.
setwd("C:\\Users\\rupali shrivastava\\OneDrive\\Desktop\\QuickShare_2502031806") 

# Read CSV files.
dp <- read.csv("prevalence-of-depression-males-vs-females.csv")
ax <- read.csv("prevalence-of-anxiety-disorders-males-vs-females.csv")
bp <- read.csv("prevalence-of-bipolar-disorder-in-males-vs-females.csv")
ed <- read.csv("prevalence-of-eating-disorders-in-males-vs-females.csv")
sz <- read.csv("prevalence-of-schizophrenia-in-males-vs-females.csv")
sr <- read.csv("suicide_rates_2019.csv")
ab <- read.csv("share-with-alcohol-and-substance-use-disorders 1990-2016.csv")
health <- read.csv("data.csv")
rd <- read.csv("road_death_2019.csv")


#====================================================
# STEP 2:CLEAN DATA ON R
#====================================================
# Remove all NA values & empty cells.
ax <- na.omit(ax)
ax <-ax[,-8]
bp <- na.omit(bp)
bp <-bp[,-8]
dp <- na.omit(dp)
dp <-dp[,-8]
ed <- na.omit(ed)
ed <-ed[,-8]
sz <- na.omit(sz)
sz <- sz[,-8]
ab <- na.omit(ab)
sr <- na.omit(sr)
health <- health[,-6:-8]
rd <- na.omit(rd)

# Using the clean names function in Janitor to streamline column names.
ax <- clean_names(ax)
bp <- clean_names(bp)
dp <- clean_names(dp)
ed <- clean_names(ed)
sz <- clean_names(sz)
ab <- clean_names(ab)
sr <- clean_names(sr)
health <- clean_names(health)
rd <- clean_names(rd)

# Take a look at the structure of data frames.
str(ax)
str(sr)


#Join relevant data frames.
# note that merge does not accept more than two data frames.
agg <- merge(ax, bp, by = c("index","year", "entity", "code"
                            , "population_historical_estimates"))
agg <- merge(agg, dp, by = c("index","year", "entity", "code"
                             , "population_historical_estimates"))
agg <- merge(agg, ed, by = c("index","year", "entity", "code"
                             , "population_historical_estimates"))
agg <- merge(agg, sz, by = c("index","year", "entity", "code"
                             , "population_historical_estimates"))

remove(ax,bp,dp,ed,sz)

# Renaming columns to make analysis easier.
agg <- agg %>% 
  rename(country = entity)

ab <- ab %>% 
  rename(country = entity)

# Summarizing suicide data.
su.r <- sr %>% 
  group_by(continent, code, country, year, sex) %>% 
  summarise(suicide_rate = sum(suicide_rate)) %>% 
  arrange(continent, code, country, year, sex)

# Transforming some data
agg <- agg %>% 
  mutate(prevalence_anxiety_disorders_sex_both = (prevalence_anxiety_disorders_sex_male_age_age_standardized_percent + prevalence_anxiety_disorders_sex_female_age_age_standardized_percent)
         , prevalence_bipolar_disorder_both = (prevalence_bipolar_disorder_sex_male_age_age_standardized_percent + prevalence_bipolar_disorder_sex_female_age_age_standardized_percent)
         , prevalence_depressive_disorders_both = (prevalence_depressive_disorders_sex_male_age_age_standardized_percent + prevalence_depressive_disorders_sex_female_age_age_standardized_percent)
         , prevalence_eating_disorders_both = (prevalence_eating_disorders_sex_male_age_age_standardized_percent + prevalence_eating_disorders_sex_female_age_age_standardized_percent)
         , prevalence_schizophrenia_both = (prevalence_schizophrenia_sex_male_age_age_standardized_percent + prevalence_schizophrenia_sex_female_age_age_standardized_percent)) %>% 
  relocate(prevalence_anxiety_disorders_sex_both, .after = prevalence_anxiety_disorders_sex_female_age_age_standardized_percent) %>% 
  relocate(prevalence_bipolar_disorder_both, .after = prevalence_bipolar_disorder_sex_female_age_age_standardized_percent) %>% 
  relocate(prevalence_depressive_disorders_both, .after = prevalence_depressive_disorders_sex_female_age_age_standardized_percent) %>% 
  relocate(prevalence_eating_disorders_both, .after = prevalence_eating_disorders_sex_female_age_age_standardized_percent) %>% 
  relocate(prevalence_schizophrenia_both, .after = prevalence_schizophrenia_sex_female_age_age_standardized_percent)


#====================================================
# STEP 3:Descriptive Statistics
#====================================================
# Descriptive analysis in data frames.
## Structure & composition of summarized data.
str(agg)
str(su.r)
str(rd)
str(health)

skim(agg)
skim(su.r)
skim(rd)
skim(health)

## Find out number of unique country entries.
n_distinct(agg$code) 
n_distinct(su.r$country)
n_distinct(health$countries)
n_distinct(rd$country)

## Conducting summary statistics.
summary(agg)
summary(su.r)
summary(rd)
summary(health)

#====================================================
# STEP 4:Visualizing the data
#====================================================
# Population stats.
## Determining the Global population.
agg %>% 
  group_by(year) %>% 
  summarize(pop = mean(population_historical_estimates)/1000000) %>% 
  arrange(year) %>% 
  ggplot(mapping = aes(x = year, y = pop)) + 
  geom_line(color = "maroon") + 
  labs(x="Year",y="Population",
       title = "Global Average Population")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

## Determining the Kenyan Average Population.
agg %>% 
  filter(country == "Kenya") %>% 
  group_by(year) %>% 
  summarize(pop = mean(population_historical_estimates)/1000000) %>% 
  arrange(year) %>% 
  ggplot(mapping = aes(x = year, y = pop)) + 
  geom_line(color = "blue") + 
  labs(x="Year",y="Population",
       title = "Kenyan Average Population")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

# Suicide stats.
## A look into suicide rates Globally in 2019.
su.r %>% 
  filter(year == 2019) %>% 
  group_by(sex) %>% 
  summarize(sr = mean(suicide_rate)) %>% 
  arrange(sex) %>% 
  ggplot() + 
  geom_col(mapping = aes(x = sex, y = sr), color = "magenta", fill = "blue") + 
  labs(x="Biological Gender",y="Suicide Rates",
       title = "Average suicide rates in 2019")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

## Determining Global Vs Kenyan Average Suicide Rates From 2000-2019.
su.kenya <- su.r %>% 
  filter(year == 2019, country == "Kenya") %>% 
  group_by(sex) %>% 
  summarize(sr.k = mean(suicide_rate)) %>% 
  arrange(sex)

x <- su.r %>% 
  filter(year == 2019) %>% 
  group_by(sex) %>% 
  summarize(sr = mean(suicide_rate)) %>% 
  arrange(sex)

su.kenya <- merge(su.kenya, x, by = "sex")

ggplot(su.kenya, aes(x = sex)) + 
  geom_bar(mapping = aes(y=sr.k), stat="identity"
           , position="stack", alpha=.8, fill='red'
           , color='red') + 
  geom_bar(mapping = aes(y=sr), stat="identity"
           , position ="stack", alpha=.8, fill="green"
           , color='green') +
  labs(x="Biological Gender",y="Suicide Rates"
       , title = "Average Suicide Rates Globally Vs. Kenya in 2019") +
  theme(plot.title = element_text(hjust = 0.5, face="bold")) 

## Determining Global Average Suicide Rates From 2000-2019.
### Both sexes.
su.r %>% 
  group_by(year, sex) %>% 
  filter(sex == "Both sexes") %>% 
  summarize(sr = mean(suicide_rate)) %>% 
  arrange(year, sex) %>% 
  ggplot(mapping = aes(x = year, y = sr)) + 
  geom_line(aes(color = sex)) + 
  labs(x="Year",y="Suicide Rates",
       title = "Global Average Suicide Rates From 2000-2019")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Males Vs. Females.
su.r %>% 
  group_by(year, sex) %>% 
  filter(sex != "Both sexes") %>% 
  summarize(sr = mean(suicide_rate)) %>% 
  arrange(year, sex) %>% 
  ggplot(mapping = aes(x = year, y = sr)) + 
  geom_line(aes(color = sex)) + 
  labs(x="Year",y="Suicide Rates",
       title = "Global Average Suicide Rates From 2000-2019")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

## Determining Kenyan Average Suicide Rates From 2000-2019.
### Both sexes.
su.r %>% 
  filter(country == "Kenya", sex == "Both sexes") %>% 
  group_by(year, sex) %>% 
  summarize(sr = mean(suicide_rate)) %>% 
  arrange(year, sex) %>% 
  ggplot(mapping = aes(x = year, y = sr)) + 
  geom_line(aes(color = sex)) + 
  labs(x="Year",y="Suicide Rates",
       title = "Kenyan Average Suicide Rates From 2000-2019")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Males Vs. Females.
su.r %>% 
  filter(country == "Kenya", sex != "Both sexes") %>% 
  group_by(year, sex) %>% 
  summarize(sr = mean(suicide_rate)) %>% 
  arrange(year, sex) %>% 
  ggplot(mapping = aes(x = year, y = sr)) + 
  geom_line(aes(color = sex)) + 
  labs(x="Year",y="Suicide Rates",
       title = "Kenyan Average Suicide Rates From 2000-2019")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

# Death Stats
## Determining the global probability of dying between age 30 and exact age 70 from any of cardiovascular disease, cancer, diabetes or chronic respiratory disease
### Aggregated.
health %>% 
  group_by(year) %>% 
  summarize(prob = mean(probability_of_dying_between_age_30_and_exact_age_70_from_any_of_cardiovascular_disease_cancer_diabetes_or_chronic_respiratory_disease_both)) %>% 
  arrange(year) %>% 
  ggplot(mapping = aes(x = year, y = prob)) + 
  geom_line(color = "red") + 
  labs(x="Year",y="Probability Of Dying",
       title = "Global Average Probability Of Dying Between 30-70 Years")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Males Vs. Females.
health %>% 
  group_by(year) %>% 
  summarize(probm = mean(probability_of_dying_between_age_30_and_exact_age_70_from_any_of_cardiovascular_disease_cancer_diabetes_or_chronic_respiratory_disease_male)
            , probf = mean(probability_of_dying_between_age_30_and_exact_age_70_from_any_of_cardiovascular_disease_cancer_diabetes_or_chronic_respiratory_disease_female)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = probm), color = "blue") + 
  geom_line(mapping = aes(x = year, y = probf), color = "green") +
  labs(x="Year",y="Probability Of Dying",
       title = "Global Average Probability Of Dying Between 30-70 Years")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

## Determining the Kenyan probability of dying between age 30 and exact age 70 from any of cardiovascular disease, cancer, diabetes or chronic respiratory disease.
### Aggregated.
health %>% 
  filter(countries == "Kenya") %>% 
  group_by(year) %>% 
  summarize(prob = mean(probability_of_dying_between_age_30_and_exact_age_70_from_any_of_cardiovascular_disease_cancer_diabetes_or_chronic_respiratory_disease_both)) %>% 
  arrange(year) %>% 
  ggplot(mapping = aes(x = year, y = prob)) + 
  geom_line(color = "blue") + 
  labs(x="Year",y="Probability Of Dying",
       title = "Kenyan Average Probability Of Dying Between 30-70 Yrs")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Males Vs. Females.
health %>% 
  filter(countries == "Kenya") %>%
  group_by(year) %>% 
  summarize(probm = mean(probability_of_dying_between_age_30_and_exact_age_70_from_any_of_cardiovascular_disease_cancer_diabetes_or_chronic_respiratory_disease_male)
            , probf = mean(probability_of_dying_between_age_30_and_exact_age_70_from_any_of_cardiovascular_disease_cancer_diabetes_or_chronic_respiratory_disease_female)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = probm), color = "blue") + 
  geom_line(mapping = aes(x = year, y = probf), color = "green") +
  labs(x="Year",y="Probability Of Dying",
       title = "Kenyan Average Probability Of Dying Between 30-70 Years")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

# Alcohol prevalence stats.
## Determining the global prevalence alcohol and substance use disorder.
ab %>% 
  group_by(year) %>% 
  summarize(prev = mean(prevalence_alcohol_and_substance_use_disorders_both_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot(mapping = aes(x = year, y = prev)) + 
  geom_line(color = "red") + 
  labs(x="Year",y="Prevalence",
       title = "Global Average Prevalence Of Alcohol Disorder")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

## Determining the Kenyan prevalence alcohol and substance use disorder.
ab %>% 
  filter(country == "Kenya") %>% 
  group_by(year) %>% 
  summarize(prev = mean(prevalence_alcohol_and_substance_use_disorders_both_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot(mapping = aes(x = year, y = prev)) + 
  geom_line(color = "blue") + 
  labs(x="Year",y="Prevalence",
       title = "Kenyan Average Prevalence Of Alcohol Disorder")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

# Anxiety stats.
## Determining the Global prevalence of anxiety disorder.
agg %>% 
  group_by(year) %>% 
  summarize(ad = mean(prevalence_anxiety_disorders_sex_both)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = ad), color = "red") + 
  labs(x="Year",y="Prevalence",
       title = "Global Average Prevalence Of Anxiety Disorder")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Males.
agg %>% 
  group_by(year) %>% 
  summarize(adm = mean(prevalence_anxiety_disorders_sex_male_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = adm), color = "blue")+
  labs(x="Year",y="Prevalence",
       title = "Global Average Prevalence Of Anxiety Disorder (males)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Females.
agg %>% 
  group_by(year) %>% 
  summarize(adf = mean(prevalence_anxiety_disorders_sex_female_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = adf), color = "green")+
  labs(x="Year",y="Prevalence",
       title = "Global Average Prevalence Of Anxiety Disorder in (females)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Males Vs. Females.
agg %>% 
  group_by(year) %>% 
  summarize(adm = mean(prevalence_anxiety_disorders_sex_male_age_age_standardized_percent)
            ,adf = mean(prevalence_anxiety_disorders_sex_female_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = adm), color = "blue")+
  geom_line(mapping = aes(x = year, y = adf), color = "green")+
  labs(x="Year",y="Prevalence",
       title = "Global Average Prevalence Of Anxiety Disorder")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

## Determining the Kenyan prevalence of anxiety disorder.
agg %>% 
  group_by(year) %>% 
  filter(country == "Kenya") %>% 
  summarize(ad = mean(prevalence_anxiety_disorders_sex_both)) %>% 
  arrange(year) %>% 
  ggplot(mapping = aes(x = year, y = ad)) + 
  geom_line(color = "green") + 
  labs(x="Year",y="Prevalence",
       title = "Kenyan Average Prevalence Of Anxiety Disorder")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Males.
agg %>% 
  group_by(year) %>% 
  filter(country == "Kenya") %>% 
  summarize(adm = mean(prevalence_anxiety_disorders_sex_male_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = adm), color = "blue")+
  labs(x="Year",y="Prevalence",
       title = "Kenyan Mean Prevalence Of Anxiety Disorder (males)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Females.
agg %>% 
  group_by(year) %>% 
  filter(country == "Kenya") %>% 
  summarize(adf = mean(prevalence_anxiety_disorders_sex_female_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = adf), color = "green")+
  labs(x="Year",y="Prevalence",
       title = "Kenyan Mean Prevalence Of Anxiety Disorders (females)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Males Vs. Females.
agg %>% 
  group_by(year) %>% 
  filter(country == "Kenya") %>% 
  summarize(adm = mean(prevalence_anxiety_disorders_sex_male_age_age_standardized_percent)
            ,adf = mean(prevalence_anxiety_disorders_sex_female_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = adm), color = "blue")+
  geom_line(mapping = aes(x = year, y = adf), color = "green")+
  labs(x="Year",y="Prevalence",
       title = "Kenyan Average Prevalence Of Anxiety Disorders (M Vs. F)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

## Determining the Global prevalence of bipolar disorder.
### Aggregated.
agg %>% 
  group_by(year) %>% 
  summarize(bp = mean(prevalence_bipolar_disorder_both)) %>% 
  arrange(year) %>% 
  ggplot(mapping = aes(x = year, y = bp)) + 
  geom_line(color = "red") + 
  labs(x="Year",y="Prevalence",
       title = "Global Average Prevalence Of Bipolar Disorder")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Males.
agg %>% 
  group_by(year) %>% 
  summarize(bpm = mean(prevalence_bipolar_disorder_sex_male_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = bpm), color = "blue")+
  labs(x="Year",y="Prevalence",
       title = "Global Average Prevalence Of Bipolar Disorder (males)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Females.
agg %>% 
  group_by(year) %>% 
  summarize(bpf = mean(prevalence_bipolar_disorder_sex_female_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = bpf), color = "green")+
  labs(x="Year",y="Prevalence",
       title = "Global Average Prevalence Of Bipolar Disorder (females)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Males Vs. Females.
agg %>% 
  group_by(year) %>% 
  summarize(bpm = mean(prevalence_bipolar_disorder_sex_male_age_age_standardized_percent)
            ,bpf = mean(prevalence_bipolar_disorder_sex_female_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = bpm), color = "blue")+
  geom_line(mapping = aes(x = year, y = bpf), color = "green")+
  labs(x="Year",y="Prevalence",
       title = "Global Average Prevalence Of Bipolar Disorder (M Vs. F)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

## Determining the Kenyan prevalence of bipolar disorder.
### Aggregated.
agg %>% 
  group_by(year) %>% 
  filter(country == "Kenya") %>% 
  summarize(ad = mean(prevalence_bipolar_disorder_both)) %>% 
  arrange(year) %>% 
  ggplot(mapping = aes(x = year, y = ad)) + 
  geom_line(color = "green") + 
  labs(x="Year",y="Prevalence",
       title = "Kenyan Average Prevalence Of Bipolar Disorder")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Males.
agg %>% 
  group_by(year) %>% 
  filter(country == "Kenya") %>% 
  summarize(bpm = mean(prevalence_bipolar_disorder_sex_male_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = bpm), color = "blue")+
  labs(x="Year",y="Prevalence",
       title = "Kenyan Average Prevalence Of Bipolar Disorder (males)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Females.
agg %>% 
  group_by(year) %>% 
  filter(country == "Kenya") %>% 
  summarize(bpf = mean(prevalence_bipolar_disorder_sex_female_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = bpf), color = "green")+
  labs(x="Year",y="Prevalence",
       title = "Kenyan Mean Prevalence Of Bipolar Disorder (females)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Males Vs. Females.
agg %>% 
  group_by(year) %>% 
  filter(country == "Kenya") %>% 
  summarize(bpm = mean(prevalence_bipolar_disorder_sex_male_age_age_standardized_percent)
            ,bpf = mean(prevalence_bipolar_disorder_sex_female_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = bpm), color = "blue")+
  geom_line(mapping = aes(x = year, y = bpf), color = "green")+
  labs(x="Year",y="Prevalence",
       title = "Kenyan Mean Prevalence Of Bipolar Disorder (M Vs. F)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

## Determining the Global prevalence of depressive disorder.
### Aggregated.
agg %>% 
  group_by(year) %>% 
  summarize(dep = mean(prevalence_depressive_disorders_both)) %>% 
  arrange(year) %>% 
  ggplot(mapping = aes(x = year, y = dep)) + 
  geom_line(color = "red") + 
  labs(x="Year",y="Prevalence",
       title = "Global Average Prevalence Of Depressive Disorders")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Males.
agg %>% 
  group_by(year) %>% 
  summarize(depm = mean(prevalence_depressive_disorders_sex_male_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = depm), color = "blue")+
  labs(x="Year",y="Prevalence",
       title = "Global Average Prevalence Of Depressive Disorders (Males)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Females.
agg %>% 
  group_by(year) %>% 
  summarise(depf = mean(prevalence_depressive_disorders_sex_female_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = depf), color = "green")+
  labs(x="Year",y="Prevalence",
       title = "Global Mean Prevalence Of Depressive Disorders (Females)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Males Vs. Females
agg %>% 
  group_by(year) %>% 
  summarize(depm = mean(prevalence_depressive_disorders_sex_male_age_age_standardized_percent)
            ,depf = mean(prevalence_depressive_disorders_sex_female_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = depm), color = "blue")+
  geom_line(mapping = aes(x = year, y = depf), color = "green")+
  labs(x="Year",y="Prevalence",
       title = "Global Average Prevalence Of Depressive Disorders (M Vs. F)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

## Determining the Kenyan prevalence of depressive disorder.
### Aggregated.
agg %>% 
  group_by(year) %>% 
  filter(country == "Kenya") %>% 
  summarize(ad = mean(prevalence_depressive_disorders_both)) %>% 
  arrange(year) %>% 
  ggplot(mapping = aes(x = year, y = ad)) + 
  geom_line(color = "green") + 
  labs(x="Year",y="Prevalence",
       title = "Kenyan Average Prevalence Of Depressive Disorders")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Males.
agg %>% 
  group_by(year) %>% 
  filter(country == "Kenya") %>% 
  summarize(depm = mean(prevalence_depressive_disorders_sex_male_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = depm), color = "blue")+
  labs(x="Year",y="Prevalence",
       title = "Kenyan Average Prevalence Of Depressive Disorders (Males)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Females.
agg %>% 
  group_by(year) %>% 
  filter(country == "Kenya") %>% 
  summarise(depf = mean(prevalence_depressive_disorders_sex_female_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = depf), color = "green")+
  labs(x="Year",y="Prevalence",
       title = "Kenyan Mean Prevalence Of Depressive Disorders (Females)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Males Vs. Females
agg %>% 
  group_by(year) %>% 
  filter(country == "Kenya") %>% 
  summarize(depm = mean(prevalence_depressive_disorders_sex_male_age_age_standardized_percent)
            ,depf = mean(prevalence_depressive_disorders_sex_female_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = depm), color = "blue")+
  geom_line(mapping = aes(x = year, y = depf), color = "green")+
  labs(x="Year",y="Prevalence",
       title = "Kenyan Mean Prevalence Of Depressive Disorders (M Vs. F)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

## Determining the Global prevalence of eating disorder.
### Aggregated.
agg %>% 
  group_by(year) %>% 
  summarize(ad = mean(prevalence_eating_disorders_both)) %>% 
  arrange(year) %>% 
  ggplot(mapping = aes(x = year, y = ad)) + 
  geom_line(color = "red") + 
  labs(x="Year",y="Prevalence",
       title = "Global Average Prevalence Of Eating Disorders")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Males.
agg %>% 
  group_by(year) %>% 
  summarize(edm = mean(prevalence_eating_disorders_sex_male_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = edm), color = "blue")+
  labs(x="Year",y="Prevalence",
       title = "Global Average Prevalence Of Eating Disorders (Male)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Females.
agg %>% 
  group_by(year) %>% 
  summarize(edf = mean(prevalence_eating_disorders_sex_female_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = edf), color = "green")+
  labs(x="Year",y="Prevalence",
       title = "Global Mean Prevalence Of Eating Disorders (Female)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Males Vs. Females.
agg %>% 
  group_by(year) %>% 
  summarize(edm = mean(prevalence_eating_disorders_sex_male_age_age_standardized_percent)
            ,edf = mean(prevalence_eating_disorders_sex_female_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = edm), color = "blue")+
  geom_line(mapping = aes(x = year, y = edf), color = "green")+
  labs(x="Year",y="Prevalence",
       title = "Global Mean Prevalence Of Eating Disorders (M Vs. F)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

## Determining the Kenyan prevalence of eating disorder
### Aggregated.
agg %>% 
  group_by(year) %>% 
  filter(country == "Kenya") %>% 
  summarize(ed = mean(prevalence_eating_disorders_both)) %>% 
  arrange(year) %>% 
  ggplot(mapping = aes(x = year, y = ed)) + 
  geom_line(color = "green") + 
  labs(x="Year",y="Prevalence",
       title = "Kenyan Average Prevalence Of Eating Disorders")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Males.
agg %>% 
  group_by(year) %>% 
  filter(country == "Kenya") %>% 
  summarize(edm = mean(prevalence_eating_disorders_sex_male_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = edm), color = "blue")+
  labs(x="Year",y="Prevalence",
       title = "Kenyan Average Prevalence Of Eating Disorders (Male)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Females.
agg %>% 
  group_by(year) %>% 
  filter(country == "Kenya") %>% 
  summarize(edf = mean(prevalence_eating_disorders_sex_female_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = edf), color = "green")+
  labs(x="Year",y="Prevalence",
       title = "Kenyan Mean Prevalence Of Eating Disorders (Female)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Males Vs. Females.
agg %>% 
  group_by(year) %>% 
  filter(country == "Kenya") %>% 
  summarize(edm = mean(prevalence_eating_disorders_sex_male_age_age_standardized_percent)
            ,edf = mean(prevalence_eating_disorders_sex_female_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = edm), color = "blue")+
  geom_line(mapping = aes(x = year, y = edf), color = "green")+
  labs(x="Year",y="Prevalence",
       title = "Kenyan Mean Prevalence Of Eating Disorders (M Vs. F)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

## Determining the Global prevalence of Schizophrenia.
### Aggregated.
agg %>% 
  group_by(year) %>% 
  summarize(sch = mean(prevalence_schizophrenia_both)) %>% 
  arrange(year) %>% 
  ggplot(mapping = aes(x = year, y = sch)) + 
  geom_line(color = "red") + 
  labs(x="Year",y="Prevalence",
       title = "Global Average Prevalence Of Schizophrenia")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Males.
agg %>% 
  group_by(year) %>% 
  summarize(schm = mean(prevalence_schizophrenia_sex_male_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = schm), color = "blue")+
  labs(x="Year",y="Prevalence",
       title = "Global Average Prevalence Of Schizophrenia (Male)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Females.
agg %>% 
  group_by(year) %>% 
  summarize(schf = mean(prevalence_schizophrenia_sex_female_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = schf), color = "green")+
  labs(x="Year",y="Prevalence",
       title = "Global Mean Prevalence Of Schizophrenia (Female)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Males Vs. Females.
agg %>% 
  group_by(year) %>% 
  summarize(schm = mean(prevalence_schizophrenia_sex_male_age_age_standardized_percent)
            ,schf = mean(prevalence_schizophrenia_sex_female_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = schm), color = "blue")+
  geom_line(mapping = aes(x = year, y = schf), color = "green")+
  labs(x="Year",y="Prevalence",
       title = "Global Mean Prevalence Of Schizophrenia (M Vs. F)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

## Determining the Kenyan prevalence of Schizophrenia disorders.
### Aggregated.
agg %>% 
  group_by(year) %>% 
  filter(country == "Kenya") %>% 
  summarize(ad = mean(prevalence_schizophrenia_both)) %>% 
  arrange(year) %>% 
  ggplot(mapping = aes(x = year, y = ad)) + 
  geom_line(color = "green") + 
  labs(x="Year",y="Prevalence",
       title = "Kenyan Average Prevalence Of Schizophrenia")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Males.
agg %>% 
  group_by(year) %>% 
  filter(country == "Kenya") %>% 
  summarize(schm = mean(prevalence_schizophrenia_sex_male_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = schm), color = "blue")+
  labs(x="Year",y="Prevalence",
       title = "Kenyan Average Prevalence Of Schizophrenia (Male)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Females.
agg %>% 
  group_by(year) %>% 
  summarize(schf = mean(prevalence_schizophrenia_sex_female_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = schf), color = "green")+
  labs(x="Year",y="Prevalence",
       title = "Kenyan Mean Prevalence Of Schizophrenia (Female)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

### Males Vs. Females.
agg %>% 
  group_by(year) %>% 
  summarize(schm = mean(prevalence_schizophrenia_sex_male_age_age_standardized_percent)
            ,schf = mean(prevalence_schizophrenia_sex_female_age_age_standardized_percent)) %>% 
  arrange(year) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = schm), color = "blue")+
  geom_line(mapping = aes(x = year, y = schf), color = "green")+
  labs(x="Year",y="Prevalence",
       title = "Kenyan Mean Prevalence Of Schizophrenia (M Vs. F)")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

## Determining the Global Average Road Accident Death Rate.
rd %>% 
  group_by(year) %>% 
  summarize(dr = mean(road_traffic_death_rate)) %>% 
  arrange(year) %>% 
  ggplot(mapping = aes(x = year, y = dr)) + 
  geom_line(color = "red") + 
  labs(x="Year",y="Death Rate",
       title = "Global Average Road Accident Death Rate")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

## Determining the Kenyan Average Road Accident Death Rate.
rd %>% 
  filter(country == "Kenya") %>% 
  group_by(year) %>% 
  summarize(dr = mean(road_traffic_death_rate)) %>% 
  arrange(year) %>% 
  ggplot(mapping = aes(x = year, y = dr)) + 
  geom_line(color = "blue") + 
  labs(x="Year",y="Prevalence",
       title = "Kenyan Average Road Accident Death Rate")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

# Aggregating health data
hagg <- merge(ab, agg, by = c("country", "year", "code"))

health <- health %>% 
  rename(country = countries)

hagg <- merge(hagg, health, by = c("country", "year"))

hagg <- merge(hagg, su.r, by = c("country", "year", "code"))

hagg <- merge(hagg, rd, by = c("country", "year", "code"))

## Removing unnecessary variables from data frame.
hagg <- hagg[,-28]
hagg <- hagg[,-25:-26]
hagg <- hagg [,-5]

## Removing excessive variables including non-numerical variables.
nhagg <- hagg[,-1:-3]
nhagg <- nhagg[,-2:-4]
nhagg <- nhagg[,-3:-4]
nhagg <- nhagg[,-4:-5]
nhagg <- nhagg[,-5:-6]
nhagg <- nhagg[,-6:-7]
nhagg <- nhagg[,-8:-9]

## Renaming the variables to short forms.
nhagg <- nhagg %>% 
  rename(aldis = prevalence_alcohol_and_substance_use_disorders_both_age_standardized_percent) %>% 
  rename(andis = prevalence_anxiety_disorders_sex_both) %>% 
  rename(bidis = prevalence_bipolar_disorder_both) %>% 
  rename(depdis = prevalence_depressive_disorders_both) %>% 
  rename(eddis = prevalence_eating_disorders_both) %>% 
  rename(schdis = prevalence_schizophrenia_both) %>% 
  rename(dydis = probability_of_dying_between_age_30_and_exact_age_70_from_any_of_cardiovascular_disease_cancer_diabetes_or_chronic_respiratory_disease_both) %>% 
  rename(sui = suicide_rate) %>% 
  rename(rdth = road_traffic_death_rate)

## Finding the correlations between variables and rounding off to the nearest 2 dp.
corhagg <- round(cor(nhagg),2)
head(corhagg)


## Create the correlation heat matrix by melting the correlations.
melted_corhagg <- melt(corhagg)
head(melted_corhagg)

# Create a heat map.
ggplot(melted_corhagg, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed() +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) 

# Get lower triangle of the correlation matrix
get_lower_tri<-function(corhagg){
  corhagg[upper.tri(corhagg)] <- NA
  return(corhagg)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(corhagg){
  corhagg[lower.tri(corhagg)]<- NA
  return(corhagg)
}

upper_tri <- get_upper_tri(corhagg)
upper_tri

# Melt the correlation matrix
melted_corhagg_u <- melt(upper_tri, na.rm = TRUE)

# Create a correlation Heat map
ggplot(melted_corhagg_u, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  labs(title = "Global Correlation Heat Map Of Health Variables")+
  theme(plot.title = element_text(hjust = 0.5, face="bold")) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

khagg <- hagg %>% 
  filter(country == "Kenya")

## Removing excessive variables including non-numerical variables.
knhagg <- khagg[,-1:-3]
knhagg <- knhagg[,-2:-4]
knhagg <- knhagg[,-3:-4]
knhagg <- knhagg[,-4:-5]
knhagg <- knhagg[,-5:-6]
knhagg <- knhagg[,-6:-7]
knhagg <- knhagg[,-8:-9]

## Renaming the variables to short forms.
knhagg <- knhagg %>% 
  rename(aldis = prevalence_alcohol_and_substance_use_disorders_both_age_standardized_percent) %>% 
  rename(andis = prevalence_anxiety_disorders_sex_both) %>% 
  rename(bidis = prevalence_bipolar_disorder_both) %>% 
  rename(depdis = prevalence_depressive_disorders_both) %>% 
  rename(eddis = prevalence_eating_disorders_both) %>% 
  rename(schdis = prevalence_schizophrenia_both) %>% 
  rename(dydis = probability_of_dying_between_age_30_and_exact_age_70_from_any_of_cardiovascular_disease_cancer_diabetes_or_chronic_respiratory_disease_both) %>% 
  rename(sui = suicide_rate) %>% 
  rename(rdth = road_traffic_death_rate)

## Finding the correlations between variables and rounding off to the nearest 2 dp.
corkhagg <- round(cor(knhagg),2)
head(corkhagg)


## Create the correlation heat matrix by melting the correlations.
melted_corkhagg <- melt(corkhagg)
head(melted_corkhagg)

# Create a heat map.
ggplot(melted_corkhagg, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed() +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) 

# Get lower triangle of the correlation matrix
get_lower_tri_k <-function(corkhagg){
  corkhagg[upper.tri(corkhagg)] <- NA
  return(corkhagg)
}

# Get upper triangle of the correlation matrix
get_upper_tri_k <- function(corkhagg){
  corkhagg[lower.tri(corkhagg)]<- NA
  return(corkhagg)
}

upper_tri_k <- get_upper_tri_k(corkhagg)
upper_tri_k

# Melt the correlation matrix
melted_corhagg_u_k <- melt(upper_tri_k, na.rm = TRUE)

# Create a correlation Heat map
ggplot(melted_corhagg_u_k, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  labs(title = "Correlation Heat Map Of Health Variables In Kenya")+
  theme(plot.title = element_text(hjust = 0.5, face="bold")) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
