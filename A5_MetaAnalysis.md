Building on the shoulders of giants: meta-analysis
==================================================

Questions to be answered
------------------------

1.  What is the current evidence for distinctive vocal patterns in
    schizophrenia? Report how many papers report quantitative estimates,
    comment on what percentage of the overall studies reviewed they
    represent (see PRISMA chart) your method to analyze them, the
    estimated effect size of the difference (mean effect size and
    standard error) and forest plots representing it. N.B. Only measures
    of pitch mean and pitch sd are required for the assignment. Feel
    free to ignore the rest (although pause behavior looks interesting,
    if you check my article).

2.  Do the results match your own analysis from Assignment 3? If you add
    your results to the meta-analysis, do the estimated effect sizes
    change? Report the new estimates and the new forest plots.

3.  Assess the quality of the literature: report and comment on
    heterogeneity of the studies (tau, I2), on publication bias (funnel
    plot), and on influential studies.

Tips on the process to follow:
------------------------------

-   Download the data on all published articles analyzing voice in
    schizophrenia and the prisma chart as reference of all articles
    found and reviewed

-   Look through the dataset to find out which columns to use, and if
    there is any additional information written as comments (real world
    data is always messy!).

    -   Hint: PITCH\_F0M and PITCH\_F0SD group of variables are what you
        need
    -   Hint: Make sure you read the comments in the columns:
        `pitch_f0_variability`, `frequency`, `Title`,
        `ACOUST_ANA_DESCR`, `DESCRIPTION`, and `COMMENTS`

-   Following the procedure in the slides calculate effect size and
    standard error of the effect size per each study. N.B. we focus on
    pitch mean and pitch standard deviation. . first try using lmer (to
    connect to what you know of mixed effects models) . then use rma()
    (to get some juicy additional statistics)

-   Build a forest plot of the results (forest(model))

-   Go back to Assignment 3, add your own study to the data table, and
    re-run meta-analysis. Do the results change?

-   Now look at the output of rma() and check tau and I2

<!-- -->

    #n1: sample_size_SC
    #n2: sample_size_HC

    #m1: pitch_F0_SZ_M
    #m2: pitch_F0_HC_M

    #sd1: pitch_F0_SZ_SD
    #sd2: pitch_F0_HC_SD

    #add sd like: pitch_F0_SD_HC_M to every type (hogy miért az jó kérdés)

    #f1 and f2 to be as far as possible
    #because if they are close, that means they are hard to separate
    #in Danish: they are close
    #in English: they are far

    ############

    #so many NAs, each study has 1 row, és nem biztos hogy mindegyik measure-t tartalmazta amit keresünk

    ##################
    #hogy számoljuk ki az effect sizeot?
    # (m1-m2)/sqr(pooled_sd) -> package that implemented: metafor package, function: escalc (effect size calculation)

    # what is i? ni1, mi1, sdi1 -> calculation is on study level

    #yi, vi to output dataframe

    #install.packages("metafor")
    library(metafor)

    ## Loading required package: Matrix

    ## Loading 'metafor' package (version 2.1-0). For an overview 
    ## and introduction to the package please type: help(metafor).

    pacman::p_load(lme4, lmerTest, ggplot2, tidyverse)

    metadata <- readxl::read_xlsx("Matrix_MetaAnalysis_Diagnosis_updated290719.xlsx")

    ## New names:
    ## * frequency -> frequency...68
    ## * frequency -> frequency...73
    ## * frequency -> frequency...78
    ## * frequency -> frequency...83
    ## * frequency -> frequency...88
    ## * ... and 7 more problems

    #smd: standardized mean difference: 
    pitchm_es <- escalc(measure = "SMD", n1i = SAMPLE_SIZE_SZ, n2i = SAMPLE_SIZE_HC, m1i = PITCH_F0_SZ_M, m2i = PITCH_F0_HC_M, sd1i = PITCH_F0_SZ_SD, sd2i = PITCH_F0_HC_SD, data = metadata)

    pitchsd_es <- escalc(measure = "SMD", n1i = SAMPLE_SIZE_SZ, n2i = SAMPLE_SIZE_HC, m1i = PITCH_F0SD_SZ_M, m2i = PITCH_F0SD_HC_M, sd1i = PITCH_F0SD_SZ_SD, sd2i = PITCH_F0SD_HC_SD, data = metadata)

    #for each study:
    #n1i: sample size of SCZ
    #n2i: sample size of HC
    #m1i: mean oitch for SCZ
    #m2i: mean pitch HC
    #sd1i: standard deviation of pitch of SCZ
    #sd2i: standard deviation of pitch of HC
        #think of it as a variability of pitch

    #smd = cohen's d = effect size

    #we get: standardized measure of the difference between the mean of the two groups in each study
    #calculated: mean estimated eff size within each study(: within because in studies might done things differently) + variance
    #new columns: yi and vi
        #yi: vector to specify the observed effect size or outcomes
        #vi: vector to specify the corresponding sampling variances (sd in each study, measure of mean variability) (sd of smd) UNCERTAINTY

    #randomized measures: you should expect these measures

    #we are never certain -> we want to estimate a measure of uncertainty
    #take random pair: calc diff, take other pair, calc diff, etc.....
    #what is the average difference? the average error when I use the mean expected variance (SD: mean error, variance is that just squared)

    #high variance = low weight
    #weights = 1/vi

    #merge the new variables with the original data
    data <- metadata %>% mutate(esmean = pitchm_es$yi, varmean = pitchm_es$vi)
    data <- data %>% mutate(essd = pitchsd_es$yi, varsd = pitchsd_es$vi)

    #model <- lmer(1 + (1 | Study, weights = 1/vi))

    model1 <- lmer(esmean ~ 1 + (1|StudyID), data, weights = 1/varmean, REML=F, control = lmerControl(check.nobs.vs.nlev = "ignore", check.nobs.vs.nRE = "ignore"))
    summary(model1)

    ## Linear mixed model fit by maximum likelihood . t-tests use
    ##   Satterthwaite's method [lmerModLmerTest]
    ## Formula: esmean ~ 1 + (1 | StudyID)
    ##    Data: data
    ## Weights: 1/varmean
    ## Control: 
    ## lmerControl(check.nobs.vs.nlev = "ignore", check.nobs.vs.nRE = "ignore")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##      9.7      9.0     -1.8      3.7        3 
    ## 
    ## Scaled residuals: 
    ##       Min        1Q    Median        3Q       Max 
    ## -0.006661 -0.002930  0.001998  0.003651  0.007026 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance  Std.Dev.
    ##  StudyID  (Intercept) 0.1078829 0.328455
    ##  Residual             0.0000362 0.006017
    ## Number of obs: 6, groups:  StudyID, 6
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error     df t value Pr(>|t|)
    ## (Intercept)   0.2065     0.1341 6.0000    1.54    0.174

    model2 <- lmer(essd ~ 1 + (1|StudyID), data, weights = 1/varsd, REML=F, control = lmerControl(check.nobs.vs.nlev = "ignore", check.nobs.vs.nRE = "ignore"))

    ## boundary (singular) fit: see ?isSingular

    summary(model2)

    ## Linear mixed model fit by maximum likelihood . t-tests use
    ##   Satterthwaite's method [lmerModLmerTest]
    ## Formula: essd ~ 1 + (1 | StudyID)
    ##    Data: data
    ## Weights: 1/varsd
    ## Control: 
    ## lmerControl(check.nobs.vs.nlev = "ignore", check.nobs.vs.nRE = "ignore")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##     44.8     46.9    -19.4     38.8       12 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.35018 -0.53708 -0.09548  0.35894  3.02323 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  StudyID  (Intercept)  0.00    0.000   
    ##  Residual             11.01    3.318   
    ## Number of obs: 15, groups:  StudyID, 12
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error      df t value Pr(>|t|)
    ## (Intercept)  -0.2333     0.2115 15.0000  -1.103    0.287
    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

    #forest plot: if studies on both sides: they do not go in the same direction -> they do not find significantly similar results (p value is also not significant)

    #rma (regression meta analysis): same as lmer, BUT is wrapping the output in a way that lets you use the forest things

    #on mean model
    model3 <-  rma(esmean, varmean, data = data, slab=StudyID)

    ## Warning in rma(esmean, varmean, data = data, slab = StudyID): Studies with
    ## NAs omitted from model fitting.

    summary(model3) #sample from same population, is the difference between studies reducible to that?

    ## 
    ## Random-Effects Model (k = 6; tau^2 estimator: REML)
    ## 
    ##   logLik  deviance       AIC       BIC      AICc 
    ##  -2.1749    4.3497    8.3497    7.5686   14.3497   
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.0712 (SE = 0.0908)
    ## tau (square root of estimated tau^2 value):      0.2668
    ## I^2 (total heterogeneity / total variability):   50.29%
    ## H^2 (total variability / sampling variability):  2.01
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 5) = 9.8472, p-val = 0.0797
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval    ci.lb   ci.ub 
    ##   0.1628  0.1554  1.0476  0.2948  -0.1417  0.4672    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    forest(model3) #size of the square: size of sample size (numbers: standard deviation, numbersS in parentheses are confidence intervals)

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-2-1.png)

        #best would be if they were on the same side on zero and kind of on top of each other
    funnel(model3) #the smaller the sample size, the bigger the effect size

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-2-2.png)

        #each dot is a different study, y-axis: larger studies with higher power towards top, lower powered studies          towards the bottom
    infm <- influence(model3) #shows influential data points (that give big variance (aka outliers))
    print(infm) #influential data point from study 11 (not 13 as in plot shows)

    ## 
    ##    rstudent  dffits cook.d  cov.r tau2.del QE.del    hat  weight    dfbs 
    ## 1    0.5262  0.1826 0.0361 1.2793   0.0866 9.1799 0.1286 12.8591  0.1803 
    ## 5   -1.8551 -0.8370 0.4538 0.8656   0.0280 4.9417 0.1969 19.6867 -0.8137 
    ## 11   1.6002  0.7739 0.4550 0.8674   0.0346 5.8661 0.1586 15.8640  0.7871 
    ## 18   0.2243  0.0623 0.0050 1.5403   0.1052 9.4274 0.1992 19.9247  0.0632 
    ## 28   0.5115  0.1697 0.0309 1.2612   0.0860 9.2530 0.1190 11.9019  0.1672 
    ## 50  -0.8629 -0.4438 0.2178 1.3578   0.0841 8.3757 0.1976 19.7636 -0.4464 
    ##    inf 
    ## 1 
    ## 5 
    ## 11   * 
    ## 18 
    ## 28 
    ## 50

    plot(infm)

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-2-3.png)

    #to see if there are any publication bias (assymmetry in the funnel plot)
    regtest(model3)

    ## 
    ## Regression Test for Funnel Plot Asymmetry
    ## 
    ## model:     mixed-effects meta-regression model
    ## predictor: standard error
    ## 
    ## test for funnel plot asymmetry: z = 1.5798, p = 0.1142

    ranktest(model3)

    ## 
    ## Rank Correlation Test for Funnel Plot Asymmetry
    ## 
    ## Kendall's tau = 0.2000, p = 0.7194

    #Taskm <-  rma(esmean, varmean, mods = cbind(Language), data = data, slab=StudyID)
    #forest(Taskm)


    #on sd model
    model4 <-  rma(essd, varsd, data = data, slab=StudyID)

    ## Warning in rma(essd, varsd, data = data, slab = StudyID): Studies with NAs
    ## omitted from model fitting.

    summary(model4)

    ## 
    ## Random-Effects Model (k = 15; tau^2 estimator: REML)
    ## 
    ##   logLik  deviance       AIC       BIC      AICc 
    ## -21.7894   43.5789   47.5789   48.8570   48.6698   
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 1.1965 (SE = 0.4817)
    ## tau (square root of estimated tau^2 value):      1.0938
    ## I^2 (total heterogeneity / total variability):   95.01%
    ## H^2 (total variability / sampling variability):  20.05
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 14) = 165.0993, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se     zval    pval    ci.lb   ci.ub 
    ##  -0.2425  0.2915  -0.8320  0.4054  -0.8139  0.3288    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    forest(model4)

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-2-4.png)

    funnel(model4)

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-2-5.png)

    infsd <- influence(model4)
    print(infsd)

    ## 
    ##      rstudent  dffits cook.d  cov.r tau2.del   QE.del    hat weight 
    ## 5      0.0149  0.0033 0.0000 1.1628   1.3031 165.0981 0.0681 6.8120 
    ## 6      0.3912  0.1012 0.0109 1.1369   1.2794 163.7776 0.0630 6.2964 
    ## 8     -0.0719 -0.0199 0.0004 1.1584   1.3002 164.9937 0.0665 6.6468 
    ## 9.1   -0.1232 -0.0336 0.0012 1.1585   1.2997 164.7844 0.0669 6.6889 
    ## 14    -0.2650 -0.0717 0.0055 1.1537   1.2938 163.6838 0.0670 6.7043 
    ## 15     5.6462  1.3862 0.6131 0.3109   0.2969  61.1099 0.0643 6.4328 
    ## 22    -0.5929 -0.1589 0.0266 1.1272   1.2628 158.9801 0.0667 6.6686 
    ## 42.1   0.1693  0.0455 0.0023 1.1639   1.3021 163.1599 0.0698 6.9831 
    ## 42.2   0.0901  0.0238 0.0006 1.1646   1.3037 164.7881 0.0692 6.9241 
    ## 46    -1.5066 -0.3906 0.1404 0.9785   1.0900 145.1838 0.0638 6.3755 
    ## 47.1  -1.2786 -0.3386 0.1096 1.0222   1.1392 144.1403 0.0658 6.5793 
    ## 47.2  -0.7259 -0.1939 0.0390 1.1113   1.2442 156.6876 0.0664 6.6398 
    ## 48.1   0.3912  0.1044 0.0116 1.1460   1.2844 162.2075 0.0672 6.7205 
    ## 48.2   0.4107  0.1096 0.0128 1.1445   1.2827 161.9134 0.0672 6.7202 
    ## 50    -0.1675 -0.0459 0.0023 1.1600   1.2999 164.2886 0.0681 6.8075 
    ##         dfbs inf 
    ## 5     0.0033     
    ## 6     0.1010     
    ## 8    -0.0199     
    ## 9.1  -0.0336     
    ## 14   -0.0717     
    ## 15    1.4477   * 
    ## 22   -0.1589     
    ## 42.1  0.0456     
    ## 42.2  0.0238     
    ## 46   -0.3914     
    ## 47.1 -0.3387     
    ## 47.2 -0.1939     
    ## 48.1  0.1044     
    ## 48.2  0.1096     
    ## 50   -0.0459

    plot(infsd) #influential data point is study 15 not 17, R just tries to make the plot pretty 

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-2-6.png)

    #to see if there are any publication bias (assymmetry in the funnel plot)
    regtest(model4)

    ## 
    ## Regression Test for Funnel Plot Asymmetry
    ## 
    ## model:     mixed-effects meta-regression model
    ## predictor: standard error
    ## 
    ## test for funnel plot asymmetry: z = 0.2515, p = 0.8014

    ranktest(model4)

    ## 
    ## Rank Correlation Test for Funnel Plot Asymmetry
    ## 
    ## Kendall's tau = -0.2762, p = 0.1686

    #Tasksd <-  rma(essd, varsd, mods = cbind(Language), data = data, slab=StudyID)
    #forest(Tasksd)

    #the higher the SE, the smaller the sample size

    #last task: take own data and add them to meta analysis
    #from assignment3, how do we add that model to the meta analysis?
    #yi, vi for my analysis (up to us how we consider using it)
    #calculate yi(cohen's d: standardized mean diff: diff between 2 groups in which the output measure has been scaled) and vi
    #yi: pitch mean~diagnosis (1|...) beta for diagnosis is yi
    #vi: squared sd, so we want the sd: mindkettő okés
        # 1: beta comes with SE -> take SE as SD
        # 2: SD: average error in our prediction, lmer model spit out residual, average residual is the SD -> we square it, we get to vi
            #taking a datapoint, I am predicting it is the mean: what is the error -> do it it with all datapoints -> average the error(difference) -> average error         when making a pred from the model
            #average residual is SD, then square it to get vi
    #SD for weights
    #big sds are getting even bigger, penalizing studies with small sample sizes or with big errors -> if you are bad, double punishment on you! haha

    #we can use our models in multiple ways: meta analysis, extract cohen's d

    #if we used more than 1 studies: 1+Diagnosis | STudy how each study is deviating from the main effect, beta and 7 values (if 7 studies extact with: ranef(model))

    #load Danish SCZ data from Assignment 3
    danish <- read.csv("danishdata.csv")

    sum <- danish %>% group_by(Diagnosis) %>% 
      summarise("SampleSize" = nlevels(as.factor(uID)),
                "MeanofMean" = mean(mean),
                "SDofMean" = sd(mean), 
                "MeanofSD" = mean(sd), 
                "SDofSD" = sd(sd))

    new <- data.frame("StudyID" = 60, 
                      "SAMPLE_SIZE_SZ" = sum$SampleSize[sum$Diagnosis == 1], 
                      "SAMPLE_SIZE_HC" = sum$SampleSize[sum$Diagnosis == 0],
                      "PITCH_F0_SZ_M" = sum$MeanofMean[sum$Diagnosis == 1],
                      "PITCH_F0_HC_M" = sum$MeanofMean[sum$Diagnosis == 0],
                      "PITCH_F0_SZ_SD" = sum$SDofMean[sum$Diagnosis == 1], 
                      "PITCH_F0_HC_SD" = sum$SDofMean[sum$Diagnosis == 0],
                      "PITCH_F0SD_SZ_M" = sum$MeanofSD[sum$Diagnosis == 1],
                      "PITCH_F0SD_HC_M" = sum$MeanofSD[sum$Diagnosis == 0],
                      "PITCH_F0SD_SZ_SD" = sum$SDofSD[sum$Diagnosis == 1], 
                      "PITCH_F0SD_HC_SD" = sum$SDofSD[sum$Diagnosis == 0])

    pitchm_es2 <- escalc(measure = "SMD", n1i = SAMPLE_SIZE_SZ, n2i = SAMPLE_SIZE_HC, m1i = PITCH_F0_SZ_M, m2i = PITCH_F0_HC_M, sd1i = PITCH_F0_SZ_SD, sd2i = PITCH_F0_HC_SD, data = new)

    pitchsd_es2 <- escalc(measure = "SMD", n1i = SAMPLE_SIZE_SZ, n2i = SAMPLE_SIZE_HC, m1i = PITCH_F0SD_SZ_M, m2i = PITCH_F0SD_HC_M, sd1i = PITCH_F0SD_SZ_SD, sd2i = PITCH_F0SD_HC_SD, data = new)


    new2 <- new %>% mutate(esmean = pitchm_es2$yi, varmean = pitchm_es2$vi)
    new22 <- new2 %>% mutate(essd = pitchsd_es2$yi, varsd = pitchsd_es2$vi)

    data2 <- bind_rows(data, new22)

    write.csv(data2, "all_merged.csv")


    #yi: beta for diagnosis
    #vi: average residuals is SD, then square it to get vi


    model1_all <- lmer(esmean ~ 1 + (1|StudyID), data2, weights = 1/varmean, REML=F, 
                       control = lmerControl(check.nobs.vs.nlev = "ignore", check.nobs.vs.nRE = "ignore"))

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
    ## control$checkConv, : Hessian is numerically singular: parameters are not
    ## uniquely determined

    summary(model1_all)

    ## Linear mixed model fit by maximum likelihood . t-tests use
    ##   Satterthwaite's method [lmerModLmerTest]
    ## Formula: esmean ~ 1 + (1 | StudyID)
    ##    Data: data2
    ## Weights: 1/varmean
    ## Control: 
    ## lmerControl(check.nobs.vs.nlev = "ignore", check.nobs.vs.nRE = "ignore")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##     10.1      9.9     -2.0      4.1        4 
    ## 
    ## Scaled residuals: 
    ##       Min        1Q    Median        3Q       Max 
    ## -0.011124 -0.005087  0.002031  0.008183  0.014160 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance  Std.Dev.
    ##  StudyID  (Intercept) 0.1048571 0.32382 
    ##  Residual             0.0001143 0.01069 
    ## Number of obs: 7, groups:  StudyID, 7
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error     df t value Pr(>|t|)
    ## (Intercept)   0.1610     0.1224 7.0000   1.316     0.23
    ## convergence code: 0
    ##  Hessian is numerically singular: parameters are not uniquely determined

    model2_all <- lmer(essd ~ 1 + (1|StudyID), data2, weights = 1/varsd, REML=F, 
                       control = lmerControl(check.nobs.vs.nlev = "ignore", check.nobs.vs.nRE = "ignore"))

    ## boundary (singular) fit: see ?isSingular

    summary(model2_all)

    ## Linear mixed model fit by maximum likelihood . t-tests use
    ##   Satterthwaite's method [lmerModLmerTest]
    ## Formula: essd ~ 1 + (1 | StudyID)
    ##    Data: data2
    ## Weights: 1/varsd
    ## Control: 
    ## lmerControl(check.nobs.vs.nlev = "ignore", check.nobs.vs.nRE = "ignore")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##     45.0     47.3    -19.5     39.0       13 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.38511 -0.44684 -0.09025  0.37185  3.12916 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  StudyID  (Intercept)  0.00    0.000   
    ##  Residual             10.33    3.213   
    ## Number of obs: 16, groups:  StudyID, 13
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error      df t value Pr(>|t|)
    ## (Intercept)  -0.2421     0.1853 16.0000  -1.306     0.21
    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

    ### on mean model
    model3_all <-  rma(esmean, varmean, data = data2, slab=StudyID)

    ## Warning in rma(esmean, varmean, data = data2, slab = StudyID): Studies with
    ## NAs omitted from model fitting.

    summary(model3_all) #sample from same population, is the difference between studies reducible to that?

    ## 
    ## Random-Effects Model (k = 7; tau^2 estimator: REML)
    ## 
    ##   logLik  deviance       AIC       BIC      AICc 
    ##  -2.2403    4.4805    8.4805    8.0640   12.4805   
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.0560 (SE = 0.0660)
    ## tau (square root of estimated tau^2 value):      0.2367
    ## I^2 (total heterogeneity / total variability):   50.95%
    ## H^2 (total variability / sampling variability):  2.04
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 6) = 11.7069, p-val = 0.0688
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval    ci.lb   ci.ub 
    ##   0.0973  0.1291  0.7536  0.4511  -0.1558  0.3504    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    forest(model3_all) #size of the square: size of sample size (numbers: standard deviation, numbersS in parentheses are confidence intervals)

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    funnel(model3_all) #the smaller the sample size, the bigger the effect size

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-3-2.png)

        #each dot is a different study, y-axis: larger studies with higher power towards top, lower powered studies          towards the bottom
    infm_all <- influence(model3_all) #shows influential data points (that give big variance (aka outliers))
    print(infm_all) #influential data point from study 11 (not 13 as in plot shows)

    ## 
    ##    rstudent  dffits cook.d  cov.r tau2.del  QE.del    hat  weight    dfbs 
    ## 1    0.7236  0.2252 0.0518 1.1469   0.0598 10.5540 0.0966  9.6608  0.2237 
    ## 5   -1.4279 -0.5705 0.2989 1.0860   0.0473  8.9085 0.1551 15.5127 -0.5707 
    ## 11   2.0762  1.0296 0.7048 0.6025   0.0101  6.5643 0.1217 12.1677  1.1396 
    ## 18   0.4508  0.1381 0.0221 1.3705   0.0729 10.6034 0.1573 15.7270  0.1383 
    ## 28   0.6962  0.2048 0.0429 1.1415   0.0602 10.6798 0.0888  8.8837  0.2032 
    ## 50  -0.6874 -0.3563 0.1494 1.3912   0.0750 11.1900 0.1558 15.5818 -0.3566 
    ## 60  -0.8145 -0.4620 0.2569 1.4478   0.0712  9.8472 0.2247 22.4664 -0.4748 
    ##    inf 
    ## 1 
    ## 5 
    ## 11   * 
    ## 18 
    ## 28 
    ## 50 
    ## 60

    plot(infm_all)

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-3-3.png)

    #to see if there are any publication bias (assymmetry in the funnel plot)
    regtest(model3_all)

    ## 
    ## Regression Test for Funnel Plot Asymmetry
    ## 
    ## model:     mixed-effects meta-regression model
    ## predictor: standard error
    ## 
    ## test for funnel plot asymmetry: z = 1.8577, p = 0.0632

    ranktest(model3_all)

    ## 
    ## Rank Correlation Test for Funnel Plot Asymmetry
    ## 
    ## Kendall's tau = 0.2381, p = 0.5619

    ### on sd model
    model4_all <-  rma(essd, varsd, data = data2, slab=StudyID)

    ## Warning in rma(essd, varsd, data = data2, slab = StudyID): Studies with NAs
    ## omitted from model fitting.

    summary(model4_all) #sample from same population, is the difference between studies reducible to that?

    ## 
    ## Random-Effects Model (k = 16; tau^2 estimator: REML)
    ## 
    ##   logLik  deviance       AIC       BIC      AICc 
    ## -22.7874   45.5748   49.5748   50.9909   50.5748   
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 1.1018 (SE = 0.4293)
    ## tau (square root of estimated tau^2 value):      1.0497
    ## I^2 (total heterogeneity / total variability):   95.23%
    ## H^2 (total variability / sampling variability):  20.96
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 15) = 165.2049, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se     zval    pval    ci.lb   ci.ub 
    ##  -0.2453  0.2711  -0.9049  0.3655  -0.7767  0.2861    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    forest(model4_all) #size of the square: size of sample size (numbers: standard deviation, numbersS in parentheses are confidence intervals)

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-3-4.png)

    funnel(model4_all) #the smaller the sample size, the bigger the effect size

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-3-5.png)

        #each dot is a different study, y-axis: larger studies with higher power towards top, lower powered studies          towards the bottom
    infsd_all <- influence(model4_all) #shows influential data points (that give big variance (aka outliers))
    print(infsd_all) #influential data point from study 11 (not 13 as in plot shows)

    ## 
    ##      rstudent  dffits cook.d  cov.r tau2.del   QE.del    hat weight 
    ## 5      0.0180  0.0040 0.0000 1.1519   1.1941 165.1993 0.0638 6.3758 
    ## 6      0.4083  0.1016 0.0109 1.1259   1.1720 163.8370 0.0586 5.8567 
    ## 8     -0.0722 -0.0193 0.0004 1.1476   1.1914 165.1197 0.0621 6.2088 
    ## 9.1   -0.1255 -0.0331 0.0012 1.1477   1.1910 164.9292 0.0625 6.2512 
    ## 14    -0.2730 -0.0713 0.0054 1.1429   1.1856 163.8838 0.0627 6.2668 
    ## 15     5.8636  1.3659 0.5924 0.3022   0.2648  61.3317 0.0599 5.9934 
    ## 22    -0.6138 -0.1587 0.0264 1.1165   1.1569 159.3042 0.0623 6.2307 
    ## 42.1   0.1790  0.0465 0.0023 1.1530   1.1931 163.1608 0.0655 6.5494 
    ## 42.2   0.0964  0.0246 0.0007 1.1537   1.1947 164.8407 0.0649 6.4895 
    ## 46    -1.5600 -0.3889 0.1386 0.9690   0.9981 145.6143 0.0594 5.9360 
    ## 47.1  -1.3258 -0.3382 0.1087 1.0117   1.0427 144.6787 0.0614 6.1407 
    ## 47.2  -0.7519 -0.1937 0.0387 1.1006   1.1398 157.0497 0.0620 6.2017 
    ## 48.1   0.4094  0.1054 0.0118 1.1349   1.1765 162.2276 0.0628 6.2832 
    ## 48.2   0.4297  0.1106 0.0130 1.1334   1.1749 161.9311 0.0628 6.2828 
    ## 50    -0.1716 -0.0454 0.0022 1.1493   1.1912 164.4764 0.0637 6.3712 
    ## 60    -0.0345 -0.0099 0.0001 1.1562   1.1965 165.0993 0.0656 6.5624 
    ##         dfbs inf 
    ## 5     0.0040     
    ## 6     0.1015     
    ## 8    -0.0193     
    ## 9.1  -0.0331     
    ## 14   -0.0713     
    ## 15    1.4394   * 
    ## 22   -0.1587     
    ## 42.1  0.0466     
    ## 42.2  0.0246     
    ## 46   -0.3898     
    ## 47.1 -0.3384     
    ## 47.2 -0.1937     
    ## 48.1  0.1054     
    ## 48.2  0.1107     
    ## 50   -0.0455     
    ## 60   -0.0099

    plot(infsd_all)

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-3-6.png)

    #to see if there are any publication bias (assymmetry in the funnel plot)
    regtest(model4_all)

    ## 
    ## Regression Test for Funnel Plot Asymmetry
    ## 
    ## model:     mixed-effects meta-regression model
    ## predictor: standard error
    ## 
    ## test for funnel plot asymmetry: z = 0.2450, p = 0.8065

    ranktest(model4_all)

    ## 
    ## Rank Correlation Test for Funnel Plot Asymmetry
    ## 
    ## Kendall's tau = -0.2333, p = 0.2281

    #sd: pitch variability between groups
