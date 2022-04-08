.return.error <- function(err){
  if(length(err) > 0) stop(paste0(err, sep="\n"))
}

.x.exist.warn <- function() warning("Covariates specified, but no adjustment desired. Ignoring them.")
.z.exist.warn <- function() warning("Strata specified, but no adjustment desired. Ignoring them.")
.z.exist.warn.sr <- function() warning("Strata specified, but simple randomization chosen. Ignoring Z in adjustment.")

.x.miss.warn <- function() warning("Covariates not specified, but adjustment desired. Changing method to ANOVA.")
.z.miss.err <- function() stop("No strata specified, but covariate-adaptive randomization desired. Please provide strata.")

.car.min.err <- function() stop("Minimization is not compatible with desired adjustment method. Please use ANHECOVA instead.")
