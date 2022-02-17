# Design Document for RoboCar

## Purpose

This package will integrate two existing packages for covariate adjustment in RCTs: RobinCAR, which does adjustment using linear models, and __ which does adjustment for survival data.

The code will be written in R using the object-oriented S4 classes.

## User API

### Main Functions for Inference

There will be three main functions that implement robust inference. Each of the functions will share the following arguments:

- `data`: a dataset that contains all of the columns passed in as arguments
- `treat_col`: column name for treatment group
- `response_col`: column name for outcome
- `strata_cols`: column names for stratification variables
- `covariate_cols`: column names for covariates to use in adjustment
- `car_scheme`: name of the type of covariate-adaptive randomization scheme, must be one of
	+ `"simple"` (default)
	+ `"permuted-block"`
	+ `"pocock-simon"`

#### Linear Models

The linear function will be named `robincar_linear`, and it will have the following arguments in addition to those shared above:

- `adj_method`: adjustment method to use, must be one of "ANOVA" (default), "ANCOVA", or "ANHECOVA"
- `vcovHC`: type of heteroskedasticity-consistent variance estimates, one of "HC0", "HC1", "HC3"
- `conf_level`: 95 would correspond to a 95\% confidence interval
- `contrast`: optional function to use for the contrasts, can be either one of `diff`, `ratio`, or a function taking in a vector with dimension $k$ and outputting a vector with dimension $d$ where $k$ is the number of arms in the dataset and $d$ is the number of contrasts desired.

It will return an `Estimator` object and potentially a `Contrast` object based on whether or not a contrast was requested. These objects have `show` and `summarize` methods that will display and summarize the results.

#### Generalized Linear Models

The GLM function will be named: `robincar_glm`

#### Time-to-Event Data

There will be two time-to-event functions, and they will be named `robtestcar_tte_logrank` and `robtestcar_tte_score` and it will have the following arguments in addition to those shared above:

- `event_col`: name of event indicator (1: had event, 0: censored)

### Data Simulation Functions

TODO

## Backend Structure

The following is an outline of the structure of the S4 classes that will be used in the functions. The main classes are (1) Data, (2) Estimator, and (3) Contrast. For TTE data, we could have a Tester, since testing is the primary purpose in that case.

### Data

The Data class keeps track of all data attributes including storing the data, knowing column names, and functions for data retrieval as a vector or a matrix.

`Data`: Data class with attributes for column names and dataset

- Slots
	+ `data`: dataset with these columns
	+ `treat_col`: name of treatment column
	+ `response_col`: name of response column
	+ `strata_cols`: column names for stratification variables
	+ `covariate_cols`: column names for covariates to use in adjustment
	+ TODO: Other attributes about the data that will be used later, like sample size

- Methods:
	+ `get_X(Data)`: return a covariate matrix
	+ `get_Y(Data)`: return a vector of outcomes
	+ `get_Z(Data)`: return a stratification variable matrix
	+ `get_XZ(Data)`: returns X and joint levels of Z, used for ANOVA and ANHECOVA
	+ `check_independence(Data)`: checks for linearly independent columns of X, Z, and X and Z combined; will give a warning and say which columns will be dropped from the linear model fitting

#### Data Sub-Classes

`DataTTE`: Data class for time-to-event data

- Additional Slots
	+ `event_col`: name of event indicator (1: had event, 0: censored) (`response_col` has the time to event variable)
	
- Additional Methods:
	+ `get_E(Data)`: return a vector of censoring indicators

### Estimator

Estimator is the class that performs the estimation based on some data. Right now, we would just have linear models in this category, but in the future, we could have an `Estimator` base class and a `LinearEstimator` (with `ANOVA`, `ANCOVA`, and `ANHECOVA` as sub-classes) and `NonlinearEstimator` as sub-classes to `Estimator`.

`Estimator`: Base object for model fitting

- Slots
	+ `adj_method`: adjustment method
	+ `car_scheme`: covariate-adaptive randomization scheme

- Methods
	+ `fit(Estimator, Data)`: (BLANK METHOD) fits the model procedure for `Estimator` to a `Data` object
	+ `se(Estimator, Data)`: Gets the standard error for the procedure `Estimator` using a `Data` object. Adjusts the standard error according to the `car_scheme`.
	+ `show(Estimator)`: summarizes the output and shows the results

#### Estimator Sub-Classes

`ANOVA(Estimator)`:

- Method Overrides
	+ `fit`: does not adjust for any covariates.

`ANCOVA(Estimator)`:

- Method Overrides
	+ `fit`: adjusts for X and the joint levels of Z.
	
`ANHECOVA(Estimator)`:

- Method Overrides
	+ `fit`: adjusts for X and the joint levels of Z.
	+ `se`: ignores the `car_scheme` and just computes the standard error under simple randomization.

### Contrast

Creates contrasts based on the output of `Estimator` and a contrast function.

`Contrast`: Base object for contrast estimation

- Slots
	+ `h_func`: a contrast function to use
	+ `k`: dimension of input (will check `h_func` for quality control)
	+ `d`: dimension of output (will check `h_func` for quality control)

- Methods
	+ `estimate(Contrast, Estimator, Data)`: applies the `h_func` to the estimates that are passed in by the Estimator
	+ `jacobian(Contrast, Data)`: computes the jacobian using numerical derivatives
	+ `varcov(Contrast, Data)`: computes the variance-covariance matrix
	+ `show(Contrast)`: 
	
#### Contrast Sub-Classes

`DiffContrast(Contrast)`:

- Slot Overrides:
	+ `h_func`: difference function

- Method Overrides:
	+ `jacobian`: uses closed form expression for jacobian of difference

`RatioContrast(Contrast)`:

- Slot Overrides:
	+ `h_func`: ratio function

- Method Overrides:

	`jacobian`: uses closed form expression for jacobian of ratio


