# DiversityFirst Quantitative Data Analysis

Paula Adams
July 31st 2025

- [RQ1: Models and Results](#rq1-models-and-results)
  - [Feel Included](#feel-included)
  - [Feel Uncomfortable](#feel-uncomfortable)
- [RQ2: Models and Results](#rq2-models-and-results)
  - [Impact on Experience in Course](#impact-on-experience-in-course)
  - [Connection to course content](#connection-to-course-content)
  - [Sense of Belonging](#sense-of-belonging)
  - [Likert plot: Impact, Connection, Sense of
    Belonging](#likert-plot-impact-connection-sense-of-belonging)
- [RQ4: Models and Results](#rq4-models-and-results)
  - [Naturalistic Fallacy](#naturalistic-fallacy)


##Set-Up and Data

Included Packages:

tidyverse, ggplot2, scales (has percent scale for graphs), emmeans,
ggstats (for gglikert plots), cowplot (a clean theme for ggplot, also
has the function plot_grid to combine plots), paletteer (for color
palettes), and superb (to add significance to figures)

```{r}
options(contrasts = c("contr.treatment", "contr.poly"))

#upload data
QuantData <- read.csv("DivFirst_quantdata.csv")
```

## RQ1: Models and Results

### Feel Included

Students were asked “Are there ways sex and gender have been taught or
discussed that made you feel included in this class?”(Y/N). We
calculated the percentage of students who said yes or no by Section,
Gender, and LGBTQ Identity.

```{r}
QuantData %>% subset(!is.na(Q100_text)) -> QuantData_Q100subset

#Count Students
Q100_StudentCounts <- QuantData_Q100subset %>% distinct(ResponseId,.keep_all = TRUE) %>% count(Class_text)
Q100_StudentCounts_gender <- QuantData_Q100subset %>% subset(!is.na(Gender)) %>% distinct(ResponseId,.keep_all = TRUE) %>% group_by(Gender) %>% count(Class_text)
Q100_StudentCounts_gendercombo <- QuantData_Q100subset %>% subset(!is.na(Gender_Combo)) %>% distinct(ResponseId,.keep_all = TRUE) %>% group_by(Gender_Combo) %>% count(Class_text)
Q100_StudentCounts_lgbt <- QuantData_Q100subset %>% subset(!is.na(Q109_text)) %>% distinct(ResponseId,.keep_all = TRUE) %>% group_by(Q109_text) %>% count(Class_text)

#Make Summary Tables
Q100_Summary <- QuantData_Q100subset %>% count(Q100_text) %>% 
  mutate(Percent=100*(n/sum(n))) 
Q100_Summary_class <- QuantData_Q100subset %>% group_by(Class_text) %>% count(Q100_text) %>% 
  mutate(Percent=case_when(Class_text=="Traditional" ~ 100*(n/196), Class_text=="Treatment" ~ 100*(n/169))) 

Q100_Summary_class_gender <- QuantData_Q100subset %>% subset(!is.na(Gender)) %>% group_by(Class_text,Gender) %>% count(Q100_text) %>% 
  mutate(Percent=case_when(Class_text=="Traditional" & Gender=="Cis Man"   ~ 100*(n/57), 
                           Class_text=="Traditional" & Gender=="Cis Woman" ~ 100*(n/117), 
                           Class_text=="Traditional" & Gender=="TGNC"  ~ 100*(n/3), 
                           Class_text=="Treatment"   & Gender=="Cis Man"   ~ 100*(n/49), 
                           Class_text=="Treatment"   & Gender=="Cis Woman" ~ 100*(n/104), 
                           Class_text=="Treatment"   & Gender=="TGNC"  ~ 100*(n/5))) 

Q100_Summary_class_gendercombo <- QuantData_Q100subset %>% subset(!is.na(Gender_Combo)) %>% group_by(Class_text,Gender_Combo) %>% count(Q100_text) %>% 
  mutate(Percent=case_when(Class_text=="Traditional" & Gender_Combo=="Cis Man"        ~ 100*(n/57), 
                           Class_text=="Traditional" & Gender_Combo=="Woman_TGNC" ~ 100*(n/120), 
                           Class_text=="Treatment"   & Gender_Combo=="Woman_TGNC" ~ 100*(n/109), 
                           Class_text=="Treatment"   & Gender_Combo=="Cis Man"        ~ 100*(n/49))) 

Q100_Summary_class_lgbtq <- QuantData_Q100subset %>% subset(!is.na(Q109_text)) %>% group_by(Class_text,Q109_text) %>% count(Q100_text) %>% 
  mutate(Percent=case_when(Class_text=="Traditional" & Q109_text=="Not LGBTQIA+" ~ 100*(n/144), 
                           Class_text=="Traditional" & Q109_text=="LGBTQIA+"     ~ 100*(n/30), 
                           Class_text=="Treatment"   & Q109_text=="Not LGBTQIA+" ~ 100*(n/133), 
                           Class_text=="Treatment"   & Q109_text=="LGBTQIA+"     ~ 100*(n/19))) 


#Create Combined table for bar graph
Q100_Summary_class_lgbtq  %>% gather("Demographic","Value", Q109_text) -> Q100_Summary_class_lgbtq_tocombine
Q100_Summary_class_gender %>% gather("Demographic","Value", Gender) -> Q100_Summary_class_gender_tocombine
Q100_Summary_class$Demographic <- c("Class","Class","Class","Class")
Q100_Summary_class$Value <- c("Class Total","Class Total","Class Total","Class Total")

bind_rows(Q100_Summary_class,Q100_Summary_class_gender_tocombine,Q100_Summary_class_lgbtq_tocombine) -> Q100_combinedsummary

Q100_combinedsummary$Value <- factor(Q100_combinedsummary$Value,levels=c("Cis Man","Cis Woman","TGNC","Not LGBTQIA+","LGBTQIA+","Class Total"))
Q100_combinedsummary$Demographic <- factor(Q100_combinedsummary$Demographic,levels=c("Class","Q109_text","Gender"))

Q100_combinedsummary_wider <- Q100_combinedsummary %>% pivot_wider(names_from="Class_text",values_from=c("n","Percent"),values_fill=0) %>% data.frame  

Q100_combinedsummary <- Q100_combinedsummary %>% unite("Class_DemCat", c(Class_text,Demographic), na.rm = TRUE, remove = FALSE) 
Q100_combinedsummary$Class_DemCat <- factor(Q100_combinedsummary$Class_DemCat,levels=c("Treatment_Class","Treatment_Q109_text","Treatment_Gender","Traditional_Class","Traditional_Q109_text","Traditional_Gender"))

subset(Q100_combinedsummary,Q100_text == "Yes") -> Q100_combined_summary_forbar
```


The results were then tested for significant differences using the
generalized linear model:

```{r}
# Model used in Model selection
    # includedGLM_fullinteraction <- glm((Q100-1) ~ Class_text*Q109_text+Class_text*Gender_Combo, data=QuantData_Q100subset, family=binomial,na.action=na.exclude)

# Best Model: Q100 ~ Class_text*Q109_text
includedGLM <- glm((Q100-1) ~ Class_text*Q109_text, data=QuantData_Q100subset, family=binomial,na.action=na.exclude)
summary(includedGLM)
```

We then calculated the odds ratio for the total compared to the Traditional Section, and for each demographic group to the same group in the Traditional section. 
We then compared demographic groups within each section (for example, Women+TGNC to cisgender men). 

```{r}
Q100_odds_Treatmenteffect_class <- glm(as.factor(Q100_text) ~ Class_text, data=QuantData_Q100subset, family=binomial, na.action=na.exclude)
Q100_odds_Treatmenteffect_LGBTQ <- glm(as.factor(Q100_text) ~ Class_text, data=QuantData_Q100subset,subset=Q109_text=="LGBTQIA+", family=binomial, na.action=na.exclude)
Q100_odds_Treatmenteffect_notLGBTQ <- glm(as.factor(Q100_text) ~ Class_text, data=QuantData_Q100subset,subset=Q109_text=="Not LGBTQIA+", family=binomial, na.action=na.exclude)
Q100_odds_Treatmenteffect_Men <- glm(as.factor(Q100_text) ~ Class_text, data=QuantData_Q100subset,subset=Gender_Combo=="Cis Man", family=binomial, na.action=na.exclude)
Q100_odds_Treatmenteffect_WTGNC <- glm(as.factor(Q100_text) ~ Class_text, data=QuantData_Q100subset,subset=Gender_Combo=="Woman_TGNC", family=binomial, na.action=na.exclude)

Q100_odds_Demographiceffect_Gender <- glm(as.factor(Q100_text) ~ Gender_Combo, data=QuantData_Q100subset,subset=Class_text=="Treatment", family=binomial, na.action=na.exclude)
Q100_odds_Demographiceffect_LGBTQ <- glm(as.factor(Q100_text) ~ Q109_text, data=QuantData_Q100subset,subset=Class_text=="Treatment", family=binomial, na.action=na.exclude)
Q100_odds_Demographiceffect_Gender_Traditional <- glm(as.factor(Q100_text) ~ Gender_Combo, data=QuantData_Q100subset,subset=Class_text=="Traditional", family=binomial, na.action=na.exclude)
Q100_odds_Demographiceffect_LGBTQ_Traditional <- glm(as.factor(Q100_text) ~ Q109_text, data=QuantData_Q100subset,subset=Class_text=="Traditional", family=binomial, na.action=na.exclude)

class_oddsratios_95CI <- exp(confint(Q100_odds_Treatmenteffect_class))
LGBTQ_oddsratios_95CI <- exp(confint(Q100_odds_Treatmenteffect_LGBTQ))
notLGBTQ_oddsratios_95CI <- exp(confint(Q100_odds_Treatmenteffect_notLGBTQ))
Men_oddsratios_95CI <- exp(confint(Q100_odds_Treatmenteffect_Men))
WTGNC_oddsratios_95CI <- exp(confint(Q100_odds_Treatmenteffect_WTGNC))
demographic_Gender_oddsratios_95CI <- exp(confint(Q100_odds_Demographiceffect_Gender))
demographic_LGBTQ_oddsratios_95CI <- exp(confint(Q100_odds_Demographiceffect_LGBTQ))
Traditionaldemographic_Gender_oddsratios_95CI <- exp(confint(Q100_odds_Demographiceffect_Gender_Traditional))
Traditionaldemographic_LGBTQ_oddsratios_95CI <- exp(confint(Q100_odds_Demographiceffect_LGBTQ_Traditional))

class_oddsratios <- exp(coef(Q100_odds_Treatmenteffect_class))
LGBTQ_oddsratios <- exp(coef(Q100_odds_Treatmenteffect_LGBTQ))
notLGBTQ_oddsratios <- exp(coef(Q100_odds_Treatmenteffect_notLGBTQ))
Men_oddsratios <- exp(coef(Q100_odds_Treatmenteffect_Men))
WTGNC_oddsratios <- exp(coef(Q100_odds_Treatmenteffect_WTGNC))
demographic_Gender_oddsratios <- exp(coef(Q100_odds_Demographiceffect_Gender))
demographic_LGBTQ_oddsratios <- exp(coef(Q100_odds_Demographiceffect_LGBTQ))
Traditionaldemographic_Gender_oddsratios <- exp(coef(Q100_odds_Demographiceffect_Gender_Traditional))
Traditionaldemographic_LGBTQ_oddsratios <- exp(coef(Q100_odds_Demographiceffect_LGBTQ_Traditional))

class_oddsratios_coefs <- summary(Q100_odds_Treatmenteffect_class)$coefficients
LGBTQ_oddsratios_coefs <- summary(Q100_odds_Treatmenteffect_LGBTQ)$coefficients
notLGBTQ_oddsratios_coefs <- summary(Q100_odds_Treatmenteffect_notLGBTQ)$coefficients
Men_oddsratios_coefs <- summary(Q100_odds_Treatmenteffect_Men)$coefficients
WTGNC_oddsratios_coefs <- summary(Q100_odds_Treatmenteffect_WTGNC)$coefficients
demographic_Gender_oddsratios_coefs <- summary(Q100_odds_Demographiceffect_Gender)$coefficients
demographic_LGBTQ_oddsratios_coefs <- summary(Q100_odds_Demographiceffect_LGBTQ)$coefficients
Traditionaldemographic_Gender_oddsratios_coefs <- summary(Q100_odds_Demographiceffect_Gender_Traditional)$coefficients
Traditionaldemographic_LGBTQ_oddsratios_coefs <- summary(Q100_odds_Demographiceffect_LGBTQ_Traditional)$coefficients

oddsratio_table1 <- cbind(class_oddsratios_coefs[ ,c("Estimate", "Pr(>|z|)")],
                          odds_ratio = class_oddsratios,
                          class_oddsratios_95CI,question="Q100") %>% as.data.frame.matrix() 
oddsratio_table1 <- tibble::rownames_to_column(oddsratio_table1, "Fixed.Effect") 
oddsratio_table1$odds_ratio <- as.numeric(oddsratio_table1$odds_ratio)
oddsratio_table1$CI_2.5 <- as.numeric(oddsratio_table1$`2.5 %`)
oddsratio_table1$CI97.5 <- as.numeric(oddsratio_table1$`97.5 %`)
oddsratio_table1 %>% subset (Fixed.Effect != "(Intercept)") -> oddsratio_table1
oddsratio_table1$Demographic <- "Class Total"

oddsratio_table2 <- cbind(LGBTQ_oddsratios_coefs[ ,c("Estimate", "Pr(>|z|)")],
                          odds_ratio = LGBTQ_oddsratios,
                          LGBTQ_oddsratios_95CI,question="Q100") %>% as.data.frame.matrix() 
oddsratio_table2 <- tibble::rownames_to_column(oddsratio_table2, "Fixed.Effect") 
oddsratio_table2$odds_ratio <- as.numeric(oddsratio_table2$odds_ratio)
oddsratio_table2$CI_2.5 <- as.numeric(oddsratio_table2$`2.5 %`)
oddsratio_table2$CI97.5 <- as.numeric(oddsratio_table2$`97.5 %`)
oddsratio_table2 %>% subset (Fixed.Effect != "(Intercept)") -> oddsratio_table2
oddsratio_table2$Demographic <- "LGBTQIA+"

oddsratio_table3 <- cbind(notLGBTQ_oddsratios_coefs[ ,c("Estimate", "Pr(>|z|)")],
                          odds_ratio = notLGBTQ_oddsratios,
                          notLGBTQ_oddsratios_95CI,question="Q100") %>% as.data.frame.matrix() 
oddsratio_table3 <- tibble::rownames_to_column(oddsratio_table3, "Fixed.Effect") 
oddsratio_table3$odds_ratio <- as.numeric(oddsratio_table3$odds_ratio)
oddsratio_table3$CI_2.5 <- as.numeric(oddsratio_table3$`2.5 %`)
oddsratio_table3$CI97.5 <- as.numeric(oddsratio_table3$`97.5 %`)
oddsratio_table3 %>% subset (Fixed.Effect != "(Intercept)") -> oddsratio_table3
oddsratio_table3$Demographic <- "Not LGBTQIA+"

oddsratio_table4 <- cbind(Men_oddsratios_coefs[ ,c("Estimate", "Pr(>|z|)")],
                          odds_ratio = Men_oddsratios,
                          Men_oddsratios_95CI,question="Q100") %>% as.data.frame.matrix() 
oddsratio_table4 <- tibble::rownames_to_column(oddsratio_table4, "Fixed.Effect") 
oddsratio_table4$odds_ratio <- as.numeric(oddsratio_table4$odds_ratio)
oddsratio_table4$CI_2.5 <- as.numeric(oddsratio_table4$`2.5 %`)
oddsratio_table4$CI97.5 <- as.numeric(oddsratio_table4$`97.5 %`)
oddsratio_table4 %>% subset (Fixed.Effect != "(Intercept)") -> oddsratio_table4
oddsratio_table4$Demographic <- "Cis Man"


oddsratio_table5 <- cbind(WTGNC_oddsratios_coefs[ ,c("Estimate", "Pr(>|z|)")],
                          odds_ratio = WTGNC_oddsratios,
                          WTGNC_oddsratios_95CI,question="Q100") %>% as.data.frame.matrix() 
oddsratio_table5 <- tibble::rownames_to_column(oddsratio_table5, "Fixed.Effect") 
oddsratio_table5$odds_ratio <- as.numeric(oddsratio_table5$odds_ratio)
oddsratio_table5$CI_2.5 <- as.numeric(oddsratio_table5$`2.5 %`)
oddsratio_table5$CI97.5 <- as.numeric(oddsratio_table5$`97.5 %`)
oddsratio_table5 %>% subset (Fixed.Effect != "(Intercept)") -> oddsratio_table5
oddsratio_table5$Demographic <- "Woman & TGNC"

oddsratio_table6 <- cbind(demographic_Gender_oddsratios_coefs[ ,c("Estimate", "Pr(>|z|)")],
                          odds_ratio = demographic_Gender_oddsratios,
                          demographic_Gender_oddsratios_95CI,question="Q100") %>% as.data.frame.matrix() 
oddsratio_table6 <- tibble::rownames_to_column(oddsratio_table6, "Fixed.Effect") 
oddsratio_table6$odds_ratio <- as.numeric(oddsratio_table6$odds_ratio)
oddsratio_table6$CI_2.5 <- as.numeric(oddsratio_table6$`2.5 %`)
oddsratio_table6$CI97.5 <- as.numeric(oddsratio_table6$`97.5 %`)
oddsratio_table6 %>% subset (Fixed.Effect != "(Intercept)") -> oddsratio_table6
oddsratio_table6$Demographic <- "Treatment Woman_TGNC vs Cis Man"

oddsratio_table7 <- cbind(demographic_LGBTQ_oddsratios_coefs[ ,c("Estimate", "Pr(>|z|)")],
                          odds_ratio = demographic_LGBTQ_oddsratios,
                          demographic_LGBTQ_oddsratios_95CI,question="Q100") %>% as.data.frame.matrix() 
oddsratio_table7 <- tibble::rownames_to_column(oddsratio_table7, "Fixed.Effect") 
oddsratio_table7$odds_ratio <- as.numeric(oddsratio_table7$odds_ratio)
oddsratio_table7$CI_2.5 <- as.numeric(oddsratio_table7$`2.5 %`)
oddsratio_table7$CI97.5 <- as.numeric(oddsratio_table7$`97.5 %`)
oddsratio_table7 %>% subset (Fixed.Effect != "(Intercept)") -> oddsratio_table7
oddsratio_table7$Demographic <- "Treatment LGBTQIA+ vs Not LGBTQIA+"

oddsratio_table8 <- cbind(Traditionaldemographic_Gender_oddsratios_coefs[ ,c("Estimate", "Pr(>|z|)")],
                          odds_ratio = Traditionaldemographic_Gender_oddsratios,
                          Traditionaldemographic_Gender_oddsratios_95CI,question="Q100") %>% as.data.frame.matrix() 
oddsratio_table8 <- tibble::rownames_to_column(oddsratio_table8, "Fixed.Effect") 
oddsratio_table8$odds_ratio <- as.numeric(oddsratio_table8$odds_ratio)
oddsratio_table8$CI_2.5 <- as.numeric(oddsratio_table8$`2.5 %`)
oddsratio_table8$CI97.5 <- as.numeric(oddsratio_table8$`97.5 %`)
oddsratio_table8 %>% subset (Fixed.Effect != "(Intercept)") -> oddsratio_table8
oddsratio_table8$Demographic <- "Traditional Woman_TGNC vs Cis Man"

oddsratio_table9 <- cbind(Traditionaldemographic_LGBTQ_oddsratios_coefs[ ,c("Estimate", "Pr(>|z|)")],
                          odds_ratio = Traditionaldemographic_LGBTQ_oddsratios,
                          Traditionaldemographic_LGBTQ_oddsratios_95CI,question="Q100") %>% as.data.frame.matrix() 
oddsratio_table9 <- tibble::rownames_to_column(oddsratio_table9, "Fixed.Effect") 
oddsratio_table9$odds_ratio <- as.numeric(oddsratio_table9$odds_ratio)
oddsratio_table9$CI_2.5 <- as.numeric(oddsratio_table9$`2.5 %`)
oddsratio_table9$CI97.5 <- as.numeric(oddsratio_table9$`97.5 %`)
oddsratio_table9 %>% subset (Fixed.Effect != "(Intercept)") -> oddsratio_table9
oddsratio_table9$Demographic <- "Traditional LGBTQIA+ vs Not LGBTQIA+"

TreatmentEffect_oddsratiotable <- bind_rows(oddsratio_table1,oddsratio_table2,oddsratio_table3,oddsratio_table5,oddsratio_table4)
DemographicLGBTQ_oddsratiotable <- bind_rows(oddsratio_table7,oddsratio_table9)
DemographicGender_oddsratiotable <- bind_rows(oddsratio_table6,oddsratio_table8)

TreatmentEffect_oddsratiotable$Demographic   <- factor(TreatmentEffect_oddsratiotable$Demographic, levels=c("Class Total","LGBTQIA+","Not LGBTQIA+","Woman & TGNC","Cis Man"))
DemographicLGBTQ_oddsratiotable$Demographic <- factor(DemographicLGBTQ_oddsratiotable$Demographic, levels=c("Treatment LGBTQIA+ vs Not LGBTQIA+","Traditional LGBTQIA+ vs Not LGBTQIA+"))
DemographicGender_oddsratiotable$Demographic <- factor(DemographicGender_oddsratiotable$Demographic, levels=c("Treatment Woman_TGNC vs Cis Man","Traditional Woman_TGNC vs Cis Man"))
```

## Feel Uncomfortable

Students were asked “Are there ways sex and gender have been taught or
discussed in this class that made you feel uncomfortable?” Y/N). We
calculated the percentage of students who said yes or no by Section,
Gender, and LGBTQ Identity.

```{r}
QuantData %>% subset(!is.na(Q98_text)) -> Q98_subset

Q98_StudentCounts <- Q98_subset %>% count(Class_text)
Q98_StudentCounts_gender <- Q98_subset %>% subset(!is.na(Gender)) %>% group_by(Gender) %>% count(Class_text)
Q98_StudentCounts_gendercombo <- Q98_subset %>% subset(!is.na(Gender_Combo)) %>% group_by(Gender_Combo) %>% count(Class_text)
Q98_StudentCounts_lgbtq <- Q98_subset %>% subset(!is.na(Q109_text)) %>% group_by(Q109_text) %>% count(Class_text)

#Make summary tables with percents
Q98_summary <- Q98_subset %>% count(Q98_text)
Q98_class_summary <- Q98_subset %>% group_by(Class_text) %>% count(Q98_text) %>% mutate(Percent=case_when(Class_text=="Traditional" ~ 100*(n/198), Class_text=="Treatment" ~ 100*(n/169))) 
Q98_class_gender_summary <- Q98_subset %>% subset(!is.na(Gender)) %>% group_by(Class_text,Gender) %>% count(Q98_text) %>% 
  mutate(Percent = case_when(Class_text=="Traditional" & Gender=="Man"   ~ 100*(n/57), 
                             Class_text=="Traditional" & Gender=="Woman" ~ 100*(n/117), 
                             Class_text=="Traditional" & Gender=="TGNC"  ~ 100*(n/3), 
                             Class_text=="Treatment"   & Gender=="Man"   ~ 100*(n/49), 
                             Class_text=="Treatment"   & Gender=="Woman" ~ 100*(n/104), 
                             Class_text=="Treatment"   & Gender=="TGNC"  ~ 100*(n/5))) 

Q98_class_gendercombo_summary <- Q98_subset %>% subset(!is.na(Gender_Combo)) %>% group_by(Class_text,Gender_Combo) %>% count(Q98_text) %>%
  mutate(Percent = case_when(Class_text=="Traditional" & Gender_Combo=="Man"        ~ 100*(n/57), 
                             Class_text=="Traditional" & Gender_Combo=="Woman_TGNC" ~ 100*(n/120), 
                             Class_text=="Treatment"   & Gender_Combo=="Man"        ~ 100*(n/49), 
                             Class_text=="Treatment"   & Gender_Combo=="Woman_TGNC" ~ 100*(n/109))) 

Q98_class_lgbtq_summary <- Q98_subset %>% subset(!is.na(Q109_text)) %>% group_by(Class_text,Q109_text) %>% count(Q98_text) %>%
  mutate(Percent = case_when(Class_text=="Traditional" & Q109_text=="Not LGBTQ" ~ 100*(n/144),
                             Class_text=="Traditional" & Q109_text=="LGBTQ"     ~ 100*(n/30), 
                             Class_text=="Treatment"   & Q109_text=="Not LGBTQ" ~ 100*(n/133), 
                             Class_text=="Treatment"   & Q109_text=="LGBTQ"     ~ 100*(n/19))) 

#Create Combined table for bar graph
Q98_class_lgbtq_summary  %>% gather("Demographic","Value", Q109_text) -> Q98_Summary_class_lgbtq_tocombine
Q98_class_gender_summary %>% gather("Demographic","Value", Gender) -> Q98_Summary_class_gender_tocombine
Q98_class_summary$Demographic <- c("Class","Class","Class")
Q98_class_summary$Value <- c("Class Total","Class Total","Class Total")

bind_rows(Q98_class_summary,Q98_Summary_class_gender_tocombine,Q98_Summary_class_lgbtq_tocombine) -> Q98_combinedsummary

Q98_combinedsummary$Value <- factor(Q98_combinedsummary$Value,levels=c("Man","Woman","TGNC","Not LGBTQ","LGBTQ","Class Total"))
Q98_combinedsummary$Demographic <- factor(Q98_combinedsummary$Demographic,levels=c("Class","Q109_text","Gender"))

Q98_combinedsummary_wider <- Q98_combinedsummary %>% pivot_wider(names_from="Class_text",values_from=c("n","Percent"),values_fill=0) %>% data.frame  

Q98_combinedsummary <- Q98_combinedsummary %>% unite("Class_DemCat", c(Class_text,Demographic), na.rm = TRUE, remove = FALSE) 
Q98_combinedsummary$Class_DemCat <- factor(Q98_combinedsummary$Class_DemCat,levels=c("Treatment_Class","Treatment_Q109_text","Treatment_Gender","Traditional_Class","Traditional_Q109_text","Traditional_Gender"))

subset(Q98_combinedsummary,Q98_text == "No") -> Q98_combined_summary_forbar
```

The results were then tested for significant differences using the
generalized linear model:

```{r}
uncomfortableGLM_fullinteraction <- glm((Q98-1) ~ Class_text*Q109_text+Class_text*Gender_Combo, data=Q98_subset, family=binomial, na.action=na.exclude)
summary(uncomfortableGLM_fullinteraction)
```


# RQ2: Models and Results

## Impact on Experience in Course

Students in the Treatment section were asked “The lecture on sexual
diversity in the beginning of the course had \_\_\_\_\_\_\_ impact on my
experience in the course:” with options (1) “a very negative”, (2) “a
moderately negative”, (3) “a slightly negative”, (4) “no impact”, (5) “a
slightly positive”, (6) “a moderately positive”, (7) “a very positive”.
We calculated the percentage of students who said yes or no by Gender
and LGBTQ Identity.

```{r}
Q110_subset <- QuantData %>% subset(!is.na(Q110))
#Student Counts
Q110_StudentCount <- QuantData %>% subset(!is.na(Q110)) %>% count(Class_text)
Q110_StudentCount_Gender <- QuantData %>% subset(!is.na(Q110) & !is.na(Gender)) %>% count(Gender) 
Q110_StudentCount_GenderCombo <- QuantData %>% subset(!is.na(Q110) & !is.na(Gender)) %>% count(Gender_Combo) 
Q110_StudentCount_LGBT <-  QuantData %>% subset(!is.na(Q110) & !is.na(Q109_text)) %>% count(Q109_text)

#Make Summary Tables
Q110_Summary <- QuantData %>% subset(!is.na(Q110)) %>% count(Q110_Group_text) %>% mutate(Percent=100*(n/sum(n)))
Q110_Summary_Simple <- QuantData %>% subset(!is.na(Q110)) %>% count(Q110_Group_Simple) %>% mutate(Percent=100*(n/sum(n)))

Q110_Summary_Gender <-QuantData %>% subset(!is.na(Q110) & !is.na(Gender)) %>% group_by(Gender) %>%count(Q110_Group_text) %>% 
  mutate(Percent =case_when(Gender=="Cis Man"~100*(n/49), 
                            Gender=="Cis Woman"~100*(n/104), 
                            Gender=="TGNC"~100*(n/5)))

Q110_Summary_Gender_Simple <- QuantData %>% subset(!is.na(Q110) & !is.na(Gender)) %>% group_by(Gender) %>%count(Q110_Group_Simple) %>% 
  mutate(Percent =case_when(Gender=="Cis Man"~100*(n/49), 
                            Gender=="Cis Woman"~100*(n/104), 
                            Gender=="TGNC"~100*(n/5)))

Q110_Summary_GenderCombo <- QuantData %>% subset(!is.na(Q110) & !is.na(Gender)) %>% group_by(Gender_Combo) %>%count(Q110_Group_text) %>% 
  mutate(Percent =case_when(Gender_Combo=="Cis Man"~100*(n/49), 
                            Gender_Combo=="Woman_TGNC"~100*(n/109)))

Q110_Summary_GenderCombo_Simple <-QuantData %>% subset(!is.na(Q110) & !is.na(Gender)) %>% group_by(Gender_Combo) %>%count(Q110_Group_Simple) %>% 
  mutate(Percent =case_when(Gender_Combo=="Cis Man"~100*(n/49), 
                            Gender_Combo=="Woman_TGNC"~100*(n/109)))

Q110_Summary_LGBTQ <-  QuantData %>% subset(!is.na(Q110) & !is.na(Q109_text)) %>% group_by(Q109_text) %>%count(Q110_Group_text) %>% 
  mutate(Percent =case_when(Q109_text=="Not LGBTQIA+"~100*(n/133), 
                            Q109_text=="LGBTQIA+"~100*(n/19)))

Q110_Summary_LGBTQ_Simple <- QuantData %>% subset(!is.na(Q110) & !is.na(Q109_text)) %>% group_by(Q109_text) %>%count(Q110_Group_Simple) %>% 
  mutate(Percent =case_when(Q109_text=="Not LGBTQIA+"~100*(n/133), 
                            Q109_text=="LGBTQIA+"~100*(n/19)))
```

The results were then tested for significant differences using the
generalized linear model:

```{r}
CourseExperienceGLM <- glm(as.integer(Q110) ~ Gender_Combo+Q109_text, data=Q110_subset, na.action=na.exclude) 
summary(CourseExperienceGLM )
coef(summary(CourseExperienceGLM))
confint.default(CourseExperienceGLM)
```


## Connection to course content

Students in the Treatment section were asked “The lecture on sexual
diversity in the beginning of the course made me feel more connected to
the biology course content” with the option of selection (1) “Strongly
disagree” to (7) “Strongly Agree.” We calculated the percentage of
students who said yes or no by Gender and LGBTQ Identity.

```{r}
Q112_subset <- QuantData %>% subset(!is.na(Q112))
#Student Counts
Q112_StudentCount <- QuantData %>% subset(!is.na(Q112)) %>% count(Class_text)
Q112_StudentCount_Gender <- QuantData %>% subset(!is.na(Q112) & !is.na(Gender)) %>% count(Gender) 
Q112_StudentCount_GenderCombo <- QuantData %>% subset(!is.na(Q112) & !is.na(Gender)) %>% count(Gender_Combo) 
Q112_StudentCount_LGBT <- QuantData %>% subset(!is.na(Q112) & !is.na(Q109_text)) %>% count(Q109_text)

#Make Summary Tables
Q112_Summary <- QuantData %>% subset(!is.na(Q112)) %>% count(Q112_text) %>% mutate(Percent=100*(n/sum(n)))
Q112_Summary_Simple <- QuantData %>% subset(!is.na(Q112)) %>% count(Q112_Simple) %>% mutate(Percent=100*(n/sum(n)))

Q112_Summary_Gender <- QuantData %>% subset(!is.na(Q112) & !is.na(Gender)) %>% group_by(Gender) %>%count(Q112_text) %>% 
  mutate(Percent =case_when(Gender=="Cis Man"  ~100*(n/49), 
                            Gender=="Cis Woman"~100*(n/103), 
                            Gender=="TGNC" ~100*(n/5)))

Q112_Summary_Gender_Simple <- QuantData %>% subset(!is.na(Q112) & !is.na(Gender)) %>% group_by(Gender) %>% count(Q112_Simple) %>% 
  mutate(Percent =case_when(Gender=="Cis Man"  ~100*(n/49), 
                            Gender=="Cis Woman"~100*(n/103), 
                            Gender=="TGNC" ~100*(n/5)))

Q112_Summary_GenderCombo <- QuantData %>% subset(!is.na(Q112) & !is.na(Gender)) %>% group_by(Gender_Combo) %>%count(Q112_text) %>% 
  mutate(Percent =case_when(Gender_Combo=="Cis Man"       ~100*(n/49), 
                            Gender_Combo=="Woman_TGNC"~100*(n/108)))

Q112_Summary_GenderCombo_Simple <- QuantData %>% subset(!is.na(Q112) & !is.na(Gender))  %>% group_by(Gender_Combo) %>% count(Q112_Simple) %>% 
  mutate(Percent=case_when(Gender_Combo=="Cis Man"       ~100*(n/49), 
                           Gender_Combo=="Woman_TGNC"~100*(n/108)))

Q112_Summary_LGBTQ <- QuantData %>% subset(!is.na(Q112) & !is.na(Q109_text)) %>% group_by(Q109_text) %>%count(Q112_text) %>% 
  mutate(Percent =case_when(Q109_text=="Not LGBTQIA+"~100*(n/132), 
                            Q109_text=="LGBTQIA+"    ~100*(n/19)))

Q112_Summary_LGBTQ_Simple <- QuantData %>% subset(!is.na(Q112) & !is.na(Q109_text)) %>% group_by(Q109_text) %>% count(Q112_Simple) %>% 
  mutate(Percent=case_when(Q109_text=="Not LGBTQIA+"~100*(n/132), 
                           Q109_text=="LGBTQIA+"    ~100*(n/19)))
```

The results were then tested for significant differences using the
generalized linear model:

```{r}
MoreConnectedGLM <- glm(as.integer(Q112) ~ Gender_Combo+Q109_text, data=Q112_subset, na.action=na.exclude) 
summary(MoreConnectedGLM )
coef(summary(MoreConnectedGLM))
   
confint.default(MoreConnectedGLM)
```

## Sense of Belonging

Students were asked “The lecture on sexual diversity in the beginning of
the course increased my sense of belonging in the classroom” with the
option of selection (1) “Strongly disagree” to (7) “Strongly Agree.” We
calculated the percentage of students who said yes or no by Gender and
LGBTQ Identity.
```{r}
Q114_subset <- QuantData %>% subset(!is.na(Q114))
#Student Counts
Q114_StudentCount <- QuantData %>% subset(!is.na(Q114)) %>% count(Class_text)
Q114_StudentCount_Gender <- QuantData %>% subset(!is.na(Q114) & !is.na(Gender)) %>% count(Gender) 
Q114_StudentCount_GenderCombo <- QuantData %>% subset(!is.na(Q114) & !is.na(Gender)) %>% count(Gender_Combo) 
Q114_StudentCount_LGBT <- QuantData %>% subset(!is.na(Q114) & !is.na(Q109_text)) %>% count(Q109_text)

#Make Summary Tables
Q114_Summary <- QuantData %>% subset(!is.na(Q114)) %>% count(Q114_text) %>% mutate(Percent=100*(n/sum(n)))
Q114_Summary_Simple <- QuantData %>% subset(!is.na(Q114)) %>% count(Q114_Simple) %>% mutate(Percent=100*(n/sum(n)))

Q114_Summary_Gender <- QuantData %>% subset(!is.na(Q114) & !is.na(Gender))%>% group_by(Gender) %>%count(Q114_text) %>% 
  mutate(Percent =case_when(Gender=="Cis Man"  ~100*(n/49), 
                            Gender=="Cis Woman"~100*(n/103), 
                            Gender=="TGNC" ~100*(n/5)))

Q114_Summary_Gender_Simple <- QuantData %>% subset(!is.na(Q114) & !is.na(Gender)) %>% group_by(Gender) %>% count(Q114_Simple) %>% 
  mutate(Percent =case_when(Gender=="Cis Man"  ~100*(n/49), 
                            Gender=="Cis Woman"~100*(n/103), 
                            Gender=="TGNC" ~100*(n/5)))

Q114_Summary_GenderCombo <- QuantData %>% subset(!is.na(Q114) & !is.na(Gender)) %>% group_by(Gender_Combo) %>%count(Q114_text) %>% 
  mutate(Percent =case_when(Gender_Combo=="Cis Man"       ~100*(n/49), 
                            Gender_Combo=="Woman_TGNC"~100*(n/108)))

Q114_Summary_GenderCombo_Simple <- QuantData %>% subset(!is.na(Q114) & !is.na(Gender)) %>% group_by(Gender_Combo) %>% count(Q114_Simple) %>% 
  mutate(Percent=case_when(Gender_Combo=="Cis Man"         ~100*(n/49), 
                           Gender_Combo == "Woman_TGNC"~100*(n/108)))

Q114_Summary_LGBTQ <- QuantData %>% subset(!is.na(Q114) & !is.na(Q109_text))  %>% group_by(Q109_text) %>%count(Q114_text) %>% 
  mutate(Percent =case_when(Q109_text=="Not LGBTQIA+"~100*(n/132), 
                            Q109_text=="LGBTQIA+"    ~100*(n/19)))

Q114_Summary_LGBTQ_Simple <- QuantData %>% subset(!is.na(Q114) & !is.na(Q109_text))  %>% group_by(Q109_text) %>% count(Q114_Simple) %>% 
  mutate(Percent=case_when(Q109_text=="Not LGBTQIA+"~100*(n/132), 
                           Q109_text == "LGBTQIA+"  ~100*(n/19)))
```

The results were then tested for significant differences using the
generalized linear model:
```{r}
IncreaseBelongingGLM <- glm(as.integer(Q114) ~ Gender_Combo+Q109_text, data=Q114_subset, na.action=na.exclude) 
summary(IncreaseBelongingGLM )
coef(summary(IncreaseBelongingGLM))
  
confint.default(IncreaseBelongingGLM)
```

# RQ4: Models and Results

## Naturalistic Fallacy

Students in both sections were asked “If non-human animals act a
particular way, does that mean that it is ethical for humans to act that
way?” to which they could respond Yes/No. We tested for differences
between sections with the model:

```{r}
QuantData %>% subset(!is.na(Q105_text)) %>% count(Class_text) -> Q105_StudentCounts
QuantData %>% subset(!is.na(Q105_text)) %>% group_by(Class_text) %>% count(Q105_text) %>% 
  mutate(Percent=case_when(Class_text=="Traditional" ~ 100*(n/180), Class_text=="Treatment" ~ 100*(n/160))) -> Q105_Summary_class

nonhumanethicalGLM <- glm(as.factor(Q105) ~ Class_text, data=QuantData, family=binomial, na.action=na.exclude) 
summary(nonhumanethicalGLM) #notsignifigant
```


We then asked students if they agree/disagree with the statement
“Behaviors are morally/ethically ‘good’ if they are found in nature”
with the options (1) “Strongly disagree” to (7) “Strongly agree”, and
tested for differences between sections with the model:

```{r}
QuantData %>% subset(!is.na(Q106)) %>% count(Class_text) -> Q106_StudentCount
QuantData %>% subset(!is.na(Q106)) %>% group_by(Class_text) %>% count(Q106_text)  %>% 
  mutate(Percent=case_when(Class_text=="Traditional" ~ 100*(n/180), Class_text=="Treatment" ~ 100*(n/160))) -> Q106_Summary_class

QuantData %>% subset(!is.na(Q106)) %>% group_by(Class_text) %>% count(Q106_Simple)  %>% 
  mutate(Percent=case_when(Class_text=="Traditional" ~ 100*(n/180), Class_text=="Treatment" ~ 100*(n/160))) -> Q106_Summary_class_simple

Q106_Summary_class_wider <- Q106_Summary_class %>% pivot_wider(names_from="Class_text",values_from=c("n","Percent"),values_fill=0) %>% data.frame  
Q106_Summary_class_simple_wider <- Q106_Summary_class_simple %>% pivot_wider(names_from="Class_text",values_from=c("n","Percent"),values_fill=0) %>% data.frame  

NatureEthicsGLM <- glm(as.integer(Q106) ~ Class_text, data=QuantData, na.action=na.exclude) 
summary(NatureEthicsGLM) #notsignifigant
```
