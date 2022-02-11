# Biomechanics manuscript

This repository accompanies the manuscript preprint "Estimating knee movement patterns of recreational runners across training sessions using multilevel functional regression model".

It contains both data and the R code needed to reproduce all analyses and results included in the manuscript. 


# Repository navigation

For practitioners, potentially most useful R code scripts and result files are referenced below.

### Methods implementation

- R implementation of multilevel function-on-scalar regression with covariates using the proposed 3-step approach proposed and implemented in Cui et al. (2021). Follows closely the code attached to the original work, with minor edits (non-parallel handling, message printing, other) introduced by MK and MM. --  [R/lfos3s.R](https://github.com/martakarass/biomechanics-manuscript/blob/main/R/lfos3s.R)

- R implementation of three-way nested multilevel function-on-scalar regression, including estimation of the coefficient of intra-class correlation (ICC). Based on the code attached to Shou et al. (2015), with minor fixes introduced by MK and MM. --  [R/threenest.R](https://github.com/martakarass/biomechanics-manuscript/blob/main/R/threenest.R)

### Methods application

- Fit three-way nested multilevel function-on-scalar regression and estimate the coefficient of intra-class correlation (ICC). -- [R/get_ICC_N3.R](https://github.com/martakarass/biomechanics-manuscript/blob/main/R/get_ICC_N3.R) 

- Fit multilevel function-on-scalar regression with covariates. -- [R/get_fosr_with_covariates.R](https://github.com/martakarass/biomechanics-manuscript/blob/main/R/get_fosr_with_covariates.R) 

### Results visualization 

- Plot raw subsecond-level data of knee location trajectories. --  [/R/plot_data_raw.R](https://github.com/martakarass/biomechanics-manuscript/blob/main/R/plot_data_raw.R) 

- Plot scatterplots of functional scores 1 and 2 of level 1, 2 and 3, calculated with
three-way nested multilevel function-on-scalar regression models. --  [/R/plot_N3_scores.R](https://github.com/martakarass/biomechanics-manuscript/blob/main/R/plot_N3_scores.R) 

- Plot fixed effects estimates and confidence intervals estimated with multilevel function-on-scalar regression with covariates. -- [/R/plot_fosr_with_covariates.R](https://github.com/martakarass/biomechanics-manuscript/blob/main/R/plot_fosr_with_covariates.R) 
