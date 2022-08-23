# Loading required library and defined required function
library(tidyverse)
library(knitr)
if (!require("kableExtra"))
{
  install.packages("kableExtra")
  library(kableExtra)
}

library(lme4)
library(gtsummary)
library(readxl)
library(ggpubr)
library(patchwork)
library(data.table)
library(naniar)
library(JointAI)
library(arm)

prevalence_plot <- function(dat,...){
  # Function to display bar plot prevalence based on category
  # Input: dat = data, ... = variable to categorize the prevalence
  # Output: bar plot of prevalence by category
  
  # Compute prevalence based on input category
  df <- dat %>%  group_by(...) %>% 
    dplyr::summarise(Prevalence = 100*sum(result=="Positive")/n()) %>%
    na.omit()
  df <- data.table(df)
  
  # Plot prevalence based on input category
  p <- ggplot(data=df, aes(x=..., y=Prevalence))
  p <- p+geom_bar(stat="identity")
  p <- p+ylab("Leptospirosis Prevalence (%)")+theme_classic()
  
  return(p)
}

# Read data
leptospirosis.dt <- read_excel("Leptospirosis data for MSc SWDS.xlsx")
leptospirosis.dt <- data.table(leptospirosis.dt)

# Remove duplicated rows
leptospirosis.dt <- leptospirosis.dt %>% distinct()

# Find occupations above or 20 worker
rare_job <- names(which(table(leptospirosis.dt$occupation)<20))
job_index <- which(!leptospirosis.dt$occupation %in% rare_job)

# Find locations above or 20 residents
rare_loc <- names(which(table(leptospirosis.dt$location)<20))
loc_index <- which(!leptospirosis.dt$location %in% rare_loc)

# Find villages above or 20 residents
rare_vil <- names(which(table(leptospirosis.dt$village)<20))
vil_index <- which(!leptospirosis.dt$village %in% rare_vil)

# Plot percentage of missing data in each variable and missing data pattern
p1 <- gg_miss_var(leptospirosis.dt,show_pct=TRUE)+theme_classic()

p2 <- md_pattern(leptospirosis.dt, pattern = FALSE, color = c('#34111b', '#e30f41'))

p1/p2

# Create age categories factor variable
leptospirosis.dt <- leptospirosis.dt %>%  mutate(
  age_group = dplyr::case_when(
    age <= 9            ~ "0-9",
    age > 9 & age <= 19 ~ "10-19",
    age > 19 & age <= 29 ~ "20-29",
    age > 29 & age <= 39 ~ "30-39",
    age > 39 & age <= 49 ~ "40-49",
    age > 49 & age <= 59 ~ "50-59",
    age > 59 & age <= 69 ~ "60-69",
    age > 69 & age <= 79 ~ "70-79",
    age >= 80            ~ "80+",
  ))
leptospirosis.dt$age_group <- factor(leptospirosis.dt$age_group)

# ELISA test result distribution
ggplot(leptospirosis.dt, aes(x=result,fill=result))+
  geom_bar(aes(y = 100*(..count..)/sum(..count..)))+ xlab("ELISA Test Result")+
  ylab("Proportion of Leptospirosis Cases (%)")+ theme_classic()+
  theme(legend.position="none",plot.title = element_text(hjust = 0.5))+
  ggtitle("Proportion of Leptospirosis Cases Based on ELISA Test Results")

# Occupation distribution
leptospirosis.dt %>% filter(!is.na(occupation)) %>% 
  ggplot(aes(x=occupation,fill=occupation))+geom_bar(aes(y = (..count..)))+
  geom_text(stat='count', aes(label=..count..),vjust=-1)+ 
  xlab("Occupation")+
  ylab("Number of Employees")+theme_classic()+
  theme(legend.position="none",plot.title=element_text(hjust=0.5))+
  ggtitle("Kenyan ELISA Test Participant Employment Status")+
  guides(x = guide_axis(angle = 90)) 

# Plot prevalence by age, gender and occupation
p3 <- prevalence_plot(leptospirosis.dt,age_group)+xlab("Age")

p4 <- prevalence_plot(leptospirosis.dt,gender)+xlab("Sex")

p5 <- prevalence_plot(leptospirosis.dt[job_index,],occupation)+xlab("Occupation")

(p3+p4)/p5

# Plot prevalence by livestock existence at home and land use
p7 <- prevalence_plot(leptospirosis.dt,livestk_home)+xlab("Livestock at Home")

p8 <- prevalence_plot(leptospirosis.dt,landuse)+xlab("Land Use")

p7/p8

# Create altitude categories factor variable
leptospirosis.dt <- leptospirosis.dt %>%  mutate(
  altitude_group = dplyr::case_when(
    altitude <= 49            ~ "0-50",
    altitude > 49 & altitude <= 99 ~ "51-101",
    altitude > 99                  ~ "102+",
  ))

leptospirosis.dt$altitude_group <- factor(leptospirosis.dt$altitude_group)

levels(leptospirosis.dt$altitude_group) <- c("0-50","51-101","102+")

# Create hospital distance categories factor variable
leptospirosis.dt <- leptospirosis.dt %>%  mutate(
  disthosp_group = dplyr::case_when(
    disthosp <= 10            ~ "0-10",
    disthosp > 10 & disthosp <= 21 ~ "11-21",
    disthosp > 21 & disthosp <= 32 ~ "22-32",
    disthosp > 32                  ~ "33+"
  ))

leptospirosis.dt$disthosp_group <- factor(leptospirosis.dt$disthosp_group)

levels(leptospirosis.dt$disthosp_group) <- c("0-10","11-21","22-32","33+")

# Plot prevalence by constituency, altitude and hospital distance
p11 <- prevalence_plot(leptospirosis.dt,constituency)+xlab("Constituency")

p12 <- prevalence_plot(leptospirosis.dt,altitude_group)
p12 <- p12+xlab("Village Altitude (metres)")

p13 <- prevalence_plot(leptospirosis.dt,disthosp_group)
p13 <- p13+xlab("Hospital Distance (kilometres)")
(p12+p13)/p11

# Village and location distribution
p14 <- leptospirosis.dt %>% filter(!is.na(village)) %>% 
  ggplot(aes(x=village,fill=village))+geom_bar(aes(y = (..count..)))+
  geom_text(stat='count', aes(label=..count..),vjust=-1)+ 
  xlab("")+
  ylab("Number of Participants")+theme_classic()+
  theme(legend.position="none",plot.title=element_text(hjust=0.5))+
  ggtitle("Kenyan ELISA Test Participants Based on Villages")+
  guides(x = guide_axis(angle = 90)) 

p15 <- leptospirosis.dt %>% filter(!is.na(location)) %>% 
  ggplot(aes(x=location,fill=location))+geom_bar(aes(y = (..count..)))+
  geom_text(stat='count', aes(label=..count..),vjust=-1)+ 
  xlab("")+
  ylab("Number of Participants")+theme_classic()+
  theme(legend.position="none",plot.title=element_text(hjust=0.5))+
  ggtitle("Kenyan ELISA Test Participants Based on Locations")+
  guides(x = guide_axis(angle = 90)) 

p14/p15

# Plot prevalence by location and village
p9 <- prevalence_plot(leptospirosis.dt[loc_index,],location)+xlab("")+guides(x = guide_axis(angle = 90)) 

p10 <- prevalence_plot(leptospirosis.dt[vil_index,],village)+xlab("")+guides(x = guide_axis(angle = 90)) 

p9/p10

# Household head occupation distribution
leptospirosis.dt %>% filter(!is.na(hhoccup)) %>% 
  ggplot(aes(x=hhoccup,fill=hhoccup))+geom_bar(aes(y = (..count..)))+
  geom_text(stat='count', aes(label=..count..),vjust=-1)+ 
  xlab("Household Head Occupation")+
  ylab("Number of Household Head")+theme_classic()+
  theme(legend.position="none",plot.title=element_text(hjust=0.5))+
  ggtitle("Kenyan Household Head Employment Status")+
  guides(x = guide_axis(angle = 90))

# Find household head occupation with 20 worker or above
rare_hhjob <- names(which(table(leptospirosis.dt$hhoccup)<20))
hhjob_index <- which(!leptospirosis.dt$hhoccup %in% rare_hhjob)

# Plot prevalence based on household head occupation
prevalence_plot(leptospirosis.dt[hhjob_index,],hhoccup)+xlab("Household Head Occupation")

# Convert test result columns to factor
leptospirosis.dt$result <- factor(leptospirosis.dt$result)

# Fit Binomial GLMM model
model1 <- glmer(result ~ age + altitude + gender +famsize+hhgender+
                  hhage+hhoccup+landuse+(1|genhhid)+(1|village)+
                  (1|constituency),
                family=binomial,leptospirosis.dt[hhjob_index,])

model2 <- glmer(result ~ age + altitude + gender +hhage+famsize+hhoccup+
                  landuse+(1|genhhid)+(1|village),
                family=binomial,leptospirosis.dt[hhjob_index,])

# Model fit comparison table
model_list <- list(model1, model2)
aic_vec <- sapply(model_list, AIC)
bic_vec <- sapply(model_list, BIC)
model_name <- c("I","II")
summary.dt <- data.table(Model = model_name, AIC = aic_vec, BIC = bic_vec)

knitr::kable(summary.dt,"latex",digits=2,
             caption="Results of model comparisons.",booktabs=TRUE) %>%
  kable_styling(position="center",latex_options = "hold_position") 

# Extract random intercept for best model
ranef_param <- ranef(model2,condVar=TRUE)

for (i in 1:length(ranef_param)){
  colnames(ranef_param[[i]]) <- "Intercept"
}

# Diagnostic plot for best model
layout( matrix(c(1,1,2,3,4,5), nrow=3, byrow=TRUE) )
binnedplot(fitted(model2),residuals(model2,type="response"),
           xlab="Expected probability of testing positive for leptospirosis")

hist(ranef_param$genhhid$Intercept,prob=TRUE,xlab= "Random effect of household",
     main="Histogram for Random Effect of Household")

lines(density(ranef_param$genhhid$Intercept),col="red")

qqnorm(ranef_param$genhhid$Intercept,
       main = "Normal Q-Q Plot for Random Effect of Household")

qqline(ranef_param$genhhid$Intercept,col="red")

hist(ranef_param$village$Intercept,prob=TRUE,xlab= "Random effect of village",
     main="Histogram for Random Effect of Village")

lines(density(ranef_param$village$Intercept),col="red")

qqnorm(ranef_param$village$Intercept,
       main = "Normal Q-Q Plot for Random Effect of Village")

qqline(ranef_param$village$Intercept,col="red")

# Binomial GLMM estimated Odds Ratio table
list_label <- list(age ~ "Age of the sampled person", altitude ~ "Village altitude",
                   gender ~ "Gender of the sampled person", famsize ~ "Family size",
                   hhage ~ "Age of the household head", landuse ~ "Land use",
                   hhoccup ~"Household head occupation")
or.tbl <- tbl_regression(model2,exponentiate=TRUE,
                         estimate_fun = partial(style_ratio, digits = 4), label=list_label) 
or.tbl <- modify_header(or.tbl,label = "**Variable**") 
or.tbl <- as_flex_table(or.tbl)
or.tbl