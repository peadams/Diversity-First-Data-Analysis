DiversityFirst Quantitative Data Analysis
================
Paula Adams
20230420

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

Date rendered: Thu Mar 6 18:19:49 2025

\#Set-Up and Data

Included Packages:

tidyverse, ggplot2, scales (has percent scale for graphs), emmeans,
ggstats (for gglikert plots), cowplot (a clean theme for ggplot, also
has the function plot_grid to combine plots), paletteer (for color
palettes), and superb (to add significance to figures)

``` r
options(contrasts = c("contr.treatment", "contr.poly"))

#upload data
QuantData <- read.csv("DivFirst_quantdata_20250226_.csv")
```

# RQ1: Models and Results

## Feel Included

Students were asked “Are there ways sex and gender have been taught or
discussed that made you feel included in this class?”(Y/N). We
calculated the percentage of students who said yes or no by Section,
Gender, and LGBTQ Identity.

``` r
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
  mutate(Percent=case_when(Class_text=="Traditional" & Gender=="Man"   ~ 100*(n/57), 
                           Class_text=="Traditional" & Gender=="Woman" ~ 100*(n/117), 
                           Class_text=="Traditional" & Gender=="TGNC"  ~ 100*(n/3), 
                           Class_text=="Treatment"   & Gender=="Man"   ~ 100*(n/49), 
                           Class_text=="Treatment"   & Gender=="Woman" ~ 100*(n/104), 
                           Class_text=="Treatment"   & Gender=="TGNC"  ~ 100*(n/5))) 

Q100_Summary_class_gendercombo <- QuantData_Q100subset %>% subset(!is.na(Gender_Combo)) %>% group_by(Class_text,Gender_Combo) %>% count(Q100_text) %>% 
  mutate(Percent=case_when(Class_text=="Traditional" & Gender_Combo=="Man"        ~ 100*(n/57), 
                           Class_text=="Traditional" & Gender_Combo=="Woman_TGNC" ~ 100*(n/120), 
                           Class_text=="Treatment"   & Gender_Combo=="Woman_TGNC" ~ 100*(n/109), 
                           Class_text=="Treatment"   & Gender_Combo=="Man"        ~ 100*(n/49))) 

Q100_Summary_class_lgbtq <- QuantData_Q100subset %>% subset(!is.na(Q109_text)) %>% group_by(Class_text,Q109_text) %>% count(Q100_text) %>% 
  mutate(Percent=case_when(Class_text=="Traditional" & Q109_text=="Not LGBTQ" ~ 100*(n/144), 
                           Class_text=="Traditional" & Q109_text=="LGBTQ"     ~ 100*(n/30), 
                           Class_text=="Treatment"   & Q109_text=="Not LGBTQ" ~ 100*(n/133), 
                           Class_text=="Treatment"   & Q109_text=="LGBTQ"     ~ 100*(n/19))) 


#Create Combined table for bar graph
Q100_Summary_class_lgbtq  %>% gather("Demographic","Value", Q109_text) -> Q100_Summary_class_lgbtq_tocombine
Q100_Summary_class_gender %>% gather("Demographic","Value", Gender) -> Q100_Summary_class_gender_tocombine
Q100_Summary_class$Demographic <- c("Class","Class","Class","Class")
Q100_Summary_class$Value <- c("Class Total","Class Total","Class Total","Class Total")

bind_rows(Q100_Summary_class,Q100_Summary_class_gender_tocombine,Q100_Summary_class_lgbtq_tocombine) -> Q100_combinedsummary

Q100_combinedsummary$Value <- factor(Q100_combinedsummary$Value,levels=c("Man","Woman","TGNC","Not LGBTQ","LGBTQ","Class Total"))
Q100_combinedsummary$Demographic <- factor(Q100_combinedsummary$Demographic,levels=c("Class","Q109_text","Gender"))

Q100_combinedsummary_wider <- Q100_combinedsummary %>% pivot_wider(names_from="Class_text",values_from=c("n","Percent"),values_fill=0) %>% data.frame  

Q100_combinedsummary <- Q100_combinedsummary %>% unite("Class_DemCat", c(Class_text,Demographic), na.rm = TRUE, remove = FALSE) 
Q100_combinedsummary$Class_DemCat <- factor(Q100_combinedsummary$Class_DemCat,levels=c("Treatment_Class","Treatment_Q109_text","Treatment_Gender","Traditional_Class","Traditional_Q109_text","Traditional_Gender"))

subset(Q100_combinedsummary,Q100_text == "Yes") -> Q100_combined_summary_forbar
```

![](Clean_Github_DivFirst_QuantitativeDataAnalysis_Paula_files/figure-gfm/unnamed-chunk-5-1.jpeg)<!-- -->

The results were then tested for significant differences using the
generalized linear model:

``` r
includedGLM_fullinteraction <- glm((Q100-1) ~ Class_text*Q109_text+Class_text*Gender_Combo, data=QuantData_Q100subset, family=binomial, na.action=na.exclude)
summary(includedGLM_fullinteraction)
```

    ## 
    ## Call:
    ## glm(formula = (Q100 - 1) ~ Class_text * Q109_text + Class_text * 
    ##     Gender_Combo, family = binomial, data = QuantData_Q100subset, 
    ##     na.action = na.exclude)
    ## 
    ## Coefficients:
    ##                                            Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)                                 0.52778    0.27878   1.893   0.0583
    ## Class_textTreatment                        -0.21412    0.41087  -0.521   0.6023
    ## Q109_textLGBTQ                             -0.18629    0.41494  -0.449   0.6535
    ## Gender_ComboWoman_TGNC                     -0.16147    0.33727  -0.479   0.6321
    ## Class_textTreatment:Q109_textLGBTQ         -1.34311    0.73165  -1.836   0.0664
    ## Class_textTreatment:Gender_ComboWoman_TGNC  0.05545    0.50132   0.111   0.9119
    ##                                             
    ## (Intercept)                                .
    ## Class_textTreatment                         
    ## Q109_textLGBTQ                              
    ## Gender_ComboWoman_TGNC                      
    ## Class_textTreatment:Q109_textLGBTQ         .
    ## Class_textTreatment:Gender_ComboWoman_TGNC  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 444.69  on 323  degrees of freedom
    ## Residual deviance: 433.51  on 318  degrees of freedom
    ##   (41 observations deleted due to missingness)
    ## AIC: 445.51
    ## 
    ## Number of Fisher Scoring iterations: 4

We then calculated estimated marginal means and pair-wise comparison for
the variables Section and LGBTQ. The model is:

``` r
emmeans(includedGLM_fullinteraction, pairwise ~ Class_text*Q109_text)
```

    ## Warning: Model has 365 prior weights, but we recovered 324 rows of data.
    ## So prior weights were ignored.

    ## $emmeans
    ##  Class_text  Q109_text emmean    SE  df asymp.LCL asymp.UCL
    ##  Traditional Not LGBTQ  0.447 0.178 Inf    0.0977     0.796
    ##  Treatment   Not LGBTQ  0.261 0.185 Inf   -0.1028     0.624
    ##  Traditional LGBTQ      0.261 0.390 Inf   -0.5033     1.025
    ##  Treatment   LGBTQ     -1.269 0.593 Inf   -2.4300    -0.107
    ## 
    ## Results are averaged over the levels of: Gender_Combo 
    ## Results are given on the logit (not the response) scale. 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ##  contrast                                    estimate    SE  df z.ratio p.value
    ##  Traditional Not LGBTQ - Treatment Not LGBTQ  0.18640 0.257 Inf   0.725  0.8873
    ##  Traditional Not LGBTQ - Traditional LGBTQ    0.18629 0.415 Inf   0.449  0.9698
    ##  Traditional Not LGBTQ - Treatment LGBTQ      1.71579 0.619 Inf   2.773  0.0284
    ##  Treatment Not LGBTQ - Traditional LGBTQ     -0.00011 0.432 Inf   0.000  1.0000
    ##  Treatment Not LGBTQ - Treatment LGBTQ        1.52940 0.603 Inf   2.538  0.0543
    ##  Traditional LGBTQ - Treatment LGBTQ          1.52951 0.709 Inf   2.157  0.1356
    ## 
    ## Results are averaged over the levels of: Gender_Combo 
    ## Results are given on the log odds ratio (not the response) scale. 
    ## P value adjustment: tukey method for comparing a family of 4 estimates

We then calculated the odds ratio for each group compared to the control
of Traditional-Not LGBTQ students with the model where “Section_LGBTQ”
is a combined variable of Section and LGBTQ identity:

``` r
includedLGBTQ_Class_glm <- glm(as.factor(Q100_text) ~ Class_LGBTQ, data=(QuantData), family=binomial, na.action=na.exclude) #Yes is signifigant
summary(includedLGBTQ_Class_glm)
```

    ## 
    ## Call:
    ## glm(formula = as.factor(Q100_text) ~ Class_LGBTQ, family = binomial, 
    ##     data = (QuantData), na.action = na.exclude)
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)                     -0.4229     0.1704  -2.481  0.01308 * 
    ## Class_LGBTQTraditional_LGBTQ     0.2893     0.4037   0.717  0.47356   
    ## Class_LGBTQTreatment_Not LGBTQ   0.1658     0.2442   0.679  0.49706   
    ## Class_LGBTQTreatment_LGBTQ       1.7446     0.5880   2.967  0.00301 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 447.49  on 325  degrees of freedom
    ## Residual deviance: 436.54  on 322  degrees of freedom
    ##   (43 observations deleted due to missingness)
    ## AIC: 444.54
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
summary(includedLGBTQ_Class_glm)$coefficients
```

    ##                                  Estimate Std. Error    z value    Pr(>|z|)
    ## (Intercept)                    -0.4228569  0.1704057 -2.4814709 0.013084140
    ## Class_LGBTQTraditional_LGBTQ    0.2893255  0.4036913  0.7166997 0.473559398
    ## Class_LGBTQTreatment_Not LGBTQ  0.1658117  0.2441574  0.6791183 0.497062895
    ## Class_LGBTQTreatment_LGBTQ      1.7446127  0.5879664  2.9671979 0.003005275

``` r
includedLGBTQ_Class_glm_oddsratios_95CI <- exp(confint(includedLGBTQ_Class_glm))
```

    ## Waiting for profiling to be done...

``` r
includedLGBTQ_Class_glm_oddsratios <- exp(coef(includedLGBTQ_Class_glm))

coefs <- summary(includedLGBTQ_Class_glm)$coefficients
oddsratio_table <- cbind(coefs[ ,c("Estimate", "Pr(>|z|)")],
                          odds_ratio = includedLGBTQ_Class_glm_oddsratios,
                          includedLGBTQ_Class_glm_oddsratios_95CI,question="Q100") 
oddsratio_table <- as.data.frame.matrix(oddsratio_table)

oddsratio_table <- tibble::rownames_to_column(oddsratio_table, "Fixed.Effect") 
oddsratio_table$odds_ratio <- as.numeric(oddsratio_table$odds_ratio)
oddsratio_table$CI_2.5 <- as.numeric(oddsratio_table$`2.5 %`)
oddsratio_table$CI97.5 <- as.numeric(oddsratio_table$`97.5 %`)
oddsratio_table %>% subset (Fixed.Effect != "(Intercept)") -> oddsratio_table_plot
```

![](Clean_Github_DivFirst_QuantitativeDataAnalysis_Paula_files/figure-gfm/unnamed-chunk-9-1.jpeg)<!-- -->

## Feel Uncomfortable

Students were asked “Are there ways sex and gender have been taught or
discussed in this class that made you feel uncomfortable?” Y/N). We
calculated the percentage of students who said yes or no by Section,
Gender, and LGBTQ Identity.

``` r
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

``` r
uncomfortableGLM_fullinteraction <- glm((Q98-1) ~ Class_text*Q109_text+Class_text*Gender_Combo, data=Q98_subset, family=binomial, na.action=na.exclude)
summary(uncomfortableGLM_fullinteraction)
```

    ## 
    ## Call:
    ## glm(formula = (Q98 - 1) ~ Class_text * Q109_text + Class_text * 
    ##     Gender_Combo, family = binomial, data = Q98_subset, na.action = na.exclude)
    ## 
    ## Coefficients:
    ##                                              Estimate Std. Error z value
    ## (Intercept)                                 2.057e+01  2.392e+03   0.009
    ## Class_textTreatment                        -1.918e+01  2.392e+03  -0.008
    ## Q109_textLGBTQ                             -3.339e-09  3.647e+03   0.000
    ## Gender_ComboWoman_TGNC                     -2.584e-08  2.912e+03   0.000
    ## Class_textTreatment:Q109_textLGBTQ          7.309e-01  3.647e+03   0.000
    ## Class_textTreatment:Gender_ComboWoman_TGNC  7.732e-01  2.912e+03   0.000
    ##                                            Pr(>|z|)
    ## (Intercept)                                   0.993
    ## Class_textTreatment                           0.994
    ## Q109_textLGBTQ                                1.000
    ## Gender_ComboWoman_TGNC                        1.000
    ## Class_textTreatment:Q109_textLGBTQ            1.000
    ## Class_textTreatment:Gender_ComboWoman_TGNC    1.000
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 144.64  on 323  degrees of freedom
    ## Residual deviance: 110.74  on 318  degrees of freedom
    ##   (43 observations deleted due to missingness)
    ## AIC: 122.74
    ## 
    ## Number of Fisher Scoring iterations: 19

# RQ2: Models and Results

## Impact on Experience in Course

Students in the Treatment section were asked “The lecture on sexual
diversity in the beginning of the course had \_\_\_\_\_\_\_ impact on my
experience in the course:” with options (1) “a very negative”, (2) “a
moderately negative”, (3) “a slightly negative”, (4) “no impact”, (5) “a
slightly positive”, (6) “a moderately positive”, (7) “a very positive”.
We calculated the percentage of students who said yes or no by Gender
and LGBTQ Identity.

``` r
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
  mutate(Percent =case_when(Gender=="Man"~100*(n/49), 
                            Gender=="Woman"~100*(n/104), 
                            Gender=="TGNC"~100*(n/5)))

Q110_Summary_Gender_Simple <- QuantData %>% subset(!is.na(Q110) & !is.na(Gender)) %>% group_by(Gender) %>%count(Q110_Group_Simple) %>% 
  mutate(Percent =case_when(Gender=="Man"~100*(n/49), 
                            Gender=="Woman"~100*(n/104), 
                            Gender=="TGNC"~100*(n/5)))

Q110_Summary_GenderCombo <- QuantData %>% subset(!is.na(Q110) & !is.na(Gender)) %>% group_by(Gender_Combo) %>%count(Q110_Group_text) %>% 
  mutate(Percent =case_when(Gender_Combo=="Man"~100*(n/49), 
                            Gender_Combo=="Woman_TGNC"~100*(n/109)))

Q110_Summary_GenderCombo_Simple <-QuantData %>% subset(!is.na(Q110) & !is.na(Gender)) %>% group_by(Gender_Combo) %>%count(Q110_Group_Simple) %>% 
  mutate(Percent =case_when(Gender_Combo=="Man"~100*(n/49), 
                            Gender_Combo=="Woman_TGNC"~100*(n/109)))

Q110_Summary_LGBTQ <-  QuantData %>% subset(!is.na(Q110) & !is.na(Q109_text)) %>% group_by(Q109_text) %>%count(Q110_Group_text) %>% 
  mutate(Percent =case_when(Q109_text=="Not LGBTQ"~100*(n/133), 
                            Q109_text=="LGBTQ"~100*(n/19)))

Q110_Summary_LGBTQ_Simple <- QuantData %>% subset(!is.na(Q110) & !is.na(Q109_text)) %>% group_by(Q109_text) %>%count(Q110_Group_Simple) %>% 
  mutate(Percent =case_when(Q109_text=="Not LGBTQ"~100*(n/133), 
                            Q109_text=="LGBTQ"~100*(n/19)))
```

The results were then tested for significant differences using the
generalized linear model:

``` r
CourseExperienceGLM <- glm(as.integer(Q110) ~ Gender_Combo+Q109_text, data=Q110_subset, na.action=na.exclude) 
summary(CourseExperienceGLM )
```

    ## 
    ## Call:
    ## glm(formula = as.integer(Q110) ~ Gender_Combo + Q109_text, data = Q110_subset, 
    ##     na.action = na.exclude)
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              4.0000     0.1766  22.655  < 2e-16 ***
    ## Gender_ComboWoman_TGNC   0.5517     0.2175   2.537  0.01222 *  
    ## Q109_textLGBTQ           1.1325     0.2999   3.776  0.00023 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 1.402855)
    ## 
    ##     Null deviance: 245.62  on 150  degrees of freedom
    ## Residual deviance: 207.62  on 148  degrees of freedom
    ##   (17 observations deleted due to missingness)
    ## AIC: 484.6
    ## 
    ## Number of Fisher Scoring iterations: 2

``` r
coef(summary(CourseExperienceGLM))
```

    ##                         Estimate Std. Error   t value     Pr(>|t|)
    ## (Intercept)            4.0000000  0.1765632 22.654782 5.795582e-50
    ## Gender_ComboWoman_TGNC 0.5517241  0.2174841  2.536849 1.222159e-02
    ## Q109_textLGBTQ         1.1324864  0.2999320  3.775810 2.303620e-04

``` r
confint.default(CourseExperienceGLM)
```

    ##                            2.5 %    97.5 %
    ## (Intercept)            3.6539426 4.3460574
    ## Gender_ComboWoman_TGNC 0.1254632 0.9779851
    ## Q109_textLGBTQ         0.5446304 1.7203424

## Connection to course content

Students in the Treatment section were asked “The lecture on sexual
diversity in the beginning of the course made me feel more connected to
the biology course content” with the option of selection (1) “Strongly
disagree” to (7) “Strongly Agree.” We calculated the percentage of
students who said yes or no by Gender and LGBTQ Identity.

``` r
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
  mutate(Percent =case_when(Gender=="Man"  ~100*(n/49), 
                            Gender=="Woman"~100*(n/103), 
                            Gender=="TGNC" ~100*(n/5)))

Q112_Summary_Gender_Simple <- QuantData %>% subset(!is.na(Q112) & !is.na(Gender)) %>% group_by(Gender) %>% count(Q112_Simple) %>% 
  mutate(Percent =case_when(Gender=="Man"  ~100*(n/49), 
                            Gender=="Woman"~100*(n/103), 
                            Gender=="TGNC" ~100*(n/5)))

Q112_Summary_GenderCombo <- QuantData %>% subset(!is.na(Q112) & !is.na(Gender)) %>% group_by(Gender_Combo) %>%count(Q112_text) %>% 
  mutate(Percent =case_when(Gender_Combo=="Man"       ~100*(n/49), 
                            Gender_Combo=="Woman_TGNC"~100*(n/108)))

Q112_Summary_GenderCombo_Simple <- QuantData %>% subset(!is.na(Q112) & !is.na(Gender))  %>% group_by(Gender_Combo) %>% count(Q112_Simple) %>% 
  mutate(Percent=case_when(Gender_Combo=="Man"       ~100*(n/49), 
                           Gender_Combo=="Woman_TGNC"~100*(n/108)))

Q112_Summary_LGBTQ <- QuantData %>% subset(!is.na(Q112) & !is.na(Q109_text)) %>% group_by(Q109_text) %>%count(Q112_text) %>% 
  mutate(Percent =case_when(Q109_text=="Not LGBTQ"~100*(n/132), 
                            Q109_text=="LGBTQ"    ~100*(n/19)))

Q112_Summary_LGBTQ_Simple <- QuantData %>% subset(!is.na(Q112) & !is.na(Q109_text)) %>% group_by(Q109_text) %>% count(Q112_Simple) %>% 
  mutate(Percent=case_when(Q109_text=="Not LGBTQ"~100*(n/132), 
                           Q109_text=="LGBTQ"    ~100*(n/19)))
```

The results were then tested for significant differences using the
generalized linear model:

``` r
MoreConnectedGLM <- glm(as.integer(Q112) ~ Gender_Combo+Q109_text, data=Q112_subset, na.action=na.exclude) 
summary(MoreConnectedGLM )
```

    ## 
    ## Call:
    ## glm(formula = as.integer(Q112) ~ Gender_Combo + Q109_text, data = Q112_subset, 
    ##     na.action = na.exclude)
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              3.4222     0.2416  14.163  < 2e-16 ***
    ## Gender_ComboWoman_TGNC   0.5196     0.2982   1.742  0.08352 .  
    ## Q109_textLGBTQ           1.2160     0.4109   2.960  0.00359 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 2.627302)
    ## 
    ##     Null deviance: 426.46  on 149  degrees of freedom
    ## Residual deviance: 386.21  on 147  degrees of freedom
    ##   (17 observations deleted due to missingness)
    ## AIC: 575.54
    ## 
    ## Number of Fisher Scoring iterations: 2

``` r
coef(summary(MoreConnectedGLM))
```

    ##                         Estimate Std. Error   t value     Pr(>|t|)
    ## (Intercept)            3.4222222  0.2416288 14.163137 2.909238e-29
    ## Gender_ComboWoman_TGNC 0.5196382  0.2982189  1.742473 8.351713e-02
    ## Q109_textLGBTQ         1.2160343  0.4108882  2.959526 3.592638e-03

``` r
confint.default(MoreConnectedGLM)
```

    ##                              2.5 %   97.5 %
    ## (Intercept)             2.94863843 3.895806
    ## Gender_ComboWoman_TGNC -0.06486006 1.104137
    ## Q109_textLGBTQ          0.41070828 2.021360

## Sense of Belonging

Students were asked “The lecture on sexual diversity in the beginning of
the course increased my sense of belonging in the classroom” with the
option of selection (1) “Strongly disagree” to (7) “Strongly Agree.” We
calculated the percentage of students who said yes or no by Gender and
LGBTQ Identity.

``` r
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
  mutate(Percent =case_when(Gender=="Man"  ~100*(n/49), 
                            Gender=="Woman"~100*(n/103), 
                            Gender=="TGNC" ~100*(n/5)))

Q114_Summary_Gender_Simple <- QuantData %>% subset(!is.na(Q114) & !is.na(Gender)) %>% group_by(Gender) %>% count(Q114_Simple) %>% 
  mutate(Percent =case_when(Gender=="Man"  ~100*(n/49), 
                            Gender=="Woman"~100*(n/103), 
                            Gender=="TGNC" ~100*(n/5)))

Q114_Summary_GenderCombo <- QuantData %>% subset(!is.na(Q114) & !is.na(Gender)) %>% group_by(Gender_Combo) %>%count(Q114_text) %>% 
  mutate(Percent =case_when(Gender_Combo=="Man"       ~100*(n/49), 
                            Gender_Combo=="Woman_TGNC"~100*(n/108)))

Q114_Summary_GenderCombo_Simple <- QuantData %>% subset(!is.na(Q114) & !is.na(Gender)) %>% group_by(Gender_Combo) %>% count(Q114_Simple) %>% 
  mutate(Percent=case_when(Gender_Combo=="Man"         ~100*(n/49), 
                           Gender_Combo == "Woman_TGNC"~100*(n/108)))

Q114_Summary_LGBTQ <- QuantData %>% subset(!is.na(Q114) & !is.na(Q109_text))  %>% group_by(Q109_text) %>%count(Q114_text) %>% 
  mutate(Percent =case_when(Q109_text=="Not LGBTQ"~100*(n/132), 
                            Q109_text=="LGBTQ"    ~100*(n/19)))

Q114_Summary_LGBTQ_Simple <- QuantData %>% subset(!is.na(Q114) & !is.na(Q109_text))  %>% group_by(Q109_text) %>% count(Q114_Simple) %>% 
  mutate(Percent=case_when(Q109_text=="Not LGBTQ"~100*(n/132), 
                           Q109_text == "LGBTQ"  ~100*(n/19)))
```

The results were then tested for significant differences using the
generalized linear model:

``` r
IncreaseBelongingGLM <- glm(as.integer(Q114) ~ Gender_Combo+Q109_text, data=Q114_subset, na.action=na.exclude) 
summary(IncreaseBelongingGLM )
```

    ## 
    ## Call:
    ## glm(formula = as.integer(Q114) ~ Gender_Combo + Q109_text, data = Q114_subset, 
    ##     na.action = na.exclude)
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              3.3111     0.2548  12.994  < 2e-16 ***
    ## Gender_ComboWoman_TGNC   0.4563     0.3145   1.451  0.14891    
    ## Q109_textLGBTQ           1.3905     0.4333   3.209  0.00164 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 2.921902)
    ## 
    ##     Null deviance: 475.39  on 149  degrees of freedom
    ## Residual deviance: 429.52  on 147  degrees of freedom
    ##   (17 observations deleted due to missingness)
    ## AIC: 591.49
    ## 
    ## Number of Fisher Scoring iterations: 2

``` r
coef(summary(IncreaseBelongingGLM))
```

    ##                         Estimate Std. Error   t value     Pr(>|t|)
    ## (Intercept)            3.3111111  0.2548159 12.994129 3.445676e-26
    ## Gender_ComboWoman_TGNC 0.4563307  0.3144945  1.450998 1.489111e-01
    ## Q109_textLGBTQ         1.3904529  0.4333127  3.208890 1.636046e-03

``` r
confint.default(IncreaseBelongingGLM)
```

    ##                             2.5 %   97.5 %
    ## (Intercept)             2.8116811 3.810541
    ## Gender_ComboWoman_TGNC -0.1600671 1.072729
    ## Q109_textLGBTQ          0.5411755 2.239730

## Likert plot: Impact, Connection, Sense of Belonging

    ## Scale for x is already present.
    ## Adding another scale for x, which will replace the existing scale.
    ## Scale for fill is already present.
    ## Adding another scale for fill, which will replace the existing scale.
    ## Scale for x is already present.
    ## Adding another scale for x, which will replace the existing scale.
    ## Scale for fill is already present.
    ## Adding another scale for fill, which will replace the existing scale.
    ## Scale for x is already present.
    ## Adding another scale for x, which will replace the existing scale.
    ## Scale for fill is already present.
    ## Adding another scale for fill, which will replace the existing scale.

![](Clean_Github_DivFirst_QuantitativeDataAnalysis_Paula_files/figure-gfm/unnamed-chunk-18-1.jpeg)<!-- -->

# RQ4: Models and Results

## Naturalistic Fallacy

Students in both sections were asked “If non-human animals act a
particular way, does that mean that it is ethical for humans to act that
way?” to which they could respond Yes/No. We tested for differences
between sections with the model:

``` r
QuantData %>% subset(!is.na(Q105_text)) %>% count(Class_text) -> Q105_StudentCounts
QuantData %>% subset(!is.na(Q105_text)) %>% group_by(Class_text) %>% count(Q105_text) %>% 
  mutate(Percent=case_when(Class_text=="Traditional" ~ 100*(n/180), Class_text=="Treatment" ~ 100*(n/160))) -> Q105_Summary_class

nonhumanethicalGLM <- glm(as.factor(Q105) ~ Class_text, data=QuantData, family=binomial, na.action=na.exclude) 
summary(nonhumanethicalGLM) #notsignifigant
```

    ## 
    ## Call:
    ## glm(formula = as.factor(Q105) ~ Class_text, family = binomial, 
    ##     data = QuantData, na.action = na.exclude)
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           3.0681     0.3617   8.483   <2e-16 ***
    ## Class_textTreatment   0.3659     0.5807   0.630    0.529    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 110.36  on 339  degrees of freedom
    ## Residual deviance: 109.95  on 338  degrees of freedom
    ##   (29 observations deleted due to missingness)
    ## AIC: 113.95
    ## 
    ## Number of Fisher Scoring iterations: 6

We then asked students if they agree/disagree with the statement
“Behaviors are morally/ethically ‘good’ if they are found in nature”
with the options (1) “Strongly disagree” to (7) “Strongly agree”, and
tested for differences between sections with the model:

``` r
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

    ## 
    ## Call:
    ## glm(formula = as.integer(Q106) ~ Class_text, data = QuantData, 
    ##     na.action = na.exclude)
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           2.9171     0.1023  28.516   <2e-16 ***
    ## Class_textTreatment  -0.1984     0.1493  -1.328    0.185    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 1.894102)
    ## 
    ##     Null deviance: 645.44  on 340  degrees of freedom
    ## Residual deviance: 642.10  on 339  degrees of freedom
    ##   (28 observations deleted due to missingness)
    ## AIC: 1189.5
    ## 
    ## Number of Fisher Scoring iterations: 2

    ## Scale for x is already present.
    ## Adding another scale for x, which will replace the existing scale.
    ## Scale for fill is already present.
    ## Adding another scale for fill, which will replace the existing scale.

![](Clean_Github_DivFirst_QuantitativeDataAnalysis_Paula_files/figure-gfm/unnamed-chunk-21-1.jpeg)<!-- -->
