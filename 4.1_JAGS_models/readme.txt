4.1_JAGS_models

R files containing Bayesian models in JAGS format to run in rjags

Refer to Table XX for descriptions of model structure and covariates included in each model.

- All models using informed observation error prior from 2007 & 2008 data from all 4 sites to estimate a (shape) and r (rate)
- All models using informed initial conditions prior from ME lakes x_ic = -5, tau_ic = 100

*1 site models: currently using Herrick Cove data

Random Walk:

- RW_obs_1site = random walk model with informed observation error prior

- DLM_1site = null dynamic linear model with autocorrelation (slope) term and bias/offset (y-intercept) - best model from Lofton @ 1 week horizon and best of RW models @ 4 week horizon

2 Covariates: min water temp & GDD on sampling day

- wtrtemp_min_and_GDD_1site = MinWaterTemp & GDD model

- wtrtemp_min_and_GDD_1site_RY = MinWaterTemp & GDD model with Random Year Effect

Heteroskedastic 1 covariate: min water temp


***3 site models: currently leaving out Fichter data

Random Walk:

RW_obs_3sites = random walk model with informed observation error prior

DLM_3sites = null dynamic linear model with autocorrelation (slope) term and bias/offset (y-intercept) - best model from Lofton @ 1 week horizon and best of RW models @ 4 week horizon

2 Covariates: min water temp & GDD

wtrtemp_min_and_GDD_3sites = MinWaterTemp & GDD model

wtrtemp_min_and_GDD_3sites_RY = MinWaterTemp & GDD model with Random Year Effect

wtrtemp_min_and_GDD_3sites_RY_RS = MinWaterTemp & GDD model with Random Year Effect & Random Site Effect

