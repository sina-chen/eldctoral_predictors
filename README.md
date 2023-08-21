# Replication materials for "Electoral predictors of polling errors"
This repository contains supplementary and replication materials for the paper "[Electoral predictors of polling errors]"(https://preprints.apsanet.org/engage/apsa/article-details/64639021f2112b41e9a85d2e) by Sina Chen, John Körtner, Peter Selb, and Jens Wiederspohn.


## Abstract 

Case studies of polling failures focus on within-election differences in poll accuracy. The crucial question of why polls fail in one election but not in others often remains a matter of speculation. To develop a contextual understanding, we review and unify theories of election features suspected of encouraging polling errors, including mobilization, candidacies, polarization, and electoral conduct. We extend a Bayesian hierarchical modeling approach that separates poll bias and variance at the election level and links error components to election features. Investigating 9,298 pre-election polls across 367 U.S. Senate elections, 1990-2022, we find an overall trend toward smaller but more uniform errors. Poll variance is negatively associated with mobilization and polarization. Until 2004, frontrunners and incumbents were overestimated. We find little evidence that polls are biased for female or minority candidates. Republicans in states with lower levels of state democracy are underestimated in recent years.


## Description of files
- The [code](https://github.com/sina-chen/eldctoral_predictors/tree/main/code) folder contains all code required for replication.

  - The [preparation](https://github.com/sina-chen/eldctoral_predictors/tree/main/code/preparation) contains all code for prepare polling data and merge relevant covariates.
  - The [fit_stan](https://github.com/sina-chen/eldctoral_predictors/tree/main/code/fit_stan) contains all code for fitting the single models.
      
      - In the subfolder [stan_ml](https://github.com/sina-chen/eldctoral_predictors/tree/main/code/fit_stan/stan_ml) the stan models can be found
  
  - The [results_vis](https://github.com/sina-chen/eldctoral_predictors/tree/main/code/results_vis) folder contains all code necessary for visualizing the results.

- The [data] https://github.com/sina-chen/eldctoral_predictors/tree/main/data) folder contains polling data for 2022 and all data on covariates. Polling data from 1990-2020 may be obtained from 538 on request.


## Contact information

Sina Chen

Graduate School of the Social and Behavioural Sciences

University of Konstanz

Email: sina.chen@uni-konstanz.de

  
