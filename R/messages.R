#' List of functions and strings used to signal error messages and warnings
messages <- list()

messages$errorFoldDistanceMustBeGreaterThanOne <- function(folds) {
  paste0(
    "Parameter 'folds' must be >1! Following values have been passed: '",
    paste(folds, collapse = ", "),
    "'."
  )
}

messages$warningPlotAsFrequencyOverwritesY <- function() {
  "plotAsFrequency = TRUE will overwrite mapping of y"
}

messages$warningCannotFitDistributionCategorical <- function() {
  "It is not possible to fit a distribution for categorical data within a bar plot"
}

messages$warningCannotCalculateMeanCategorical <- function() {
  "It is not possible to calculate a mean for categorical data within a bar plot"
}

messages$errorInvalidDistribution <- function(distribution) {
  paste("distribution", distribution, "is an invalid function")
}

messages$errorBinWidthNoBinwidthFound <- function() {
  "error within bin width determination. No binwidth found. Is the plot empty?"
}

messages$errorBinWidthBinsNotUnique <- function() {
  "error within bin width determination. Are the bins not unique?"
}

messages$warningTablesOnlyWithoutFaceting <- function() {
  "Tables will be only added if there is no faceting vs x (xFacetColumn is NULL)"
}

messages$errorFilenameContainsPath <- function() {
  "filename should not contain a path, use input variable filepath"
}

messages$warningDeviceMayNotBeSupported <- function(device) {
  paste0("Device '", device, "' may not be supported by ggsave")
}

messages$warningNoMetaDataForXAxis <- function() {
  "No metaData available for x-axis"
}

messages$errorContinuousXScaleNotPossibleForFactors <- function(discreteScale) {
  paste0('continuous x scale is not possible for factors, please select "', discreteScale, '"')
}

messages$errorDiscreteXScaleNotPossibleForContinuous <- function(linearScale, logScale) {
  paste0(
    'discrete x scale is not possible for continuous data. Select "',
    linearScale, '" or "', logScale, '" or convert data to factor'
  )
}

messages$errorEvaluationOfAestheticFailed <- function(aesthetic, cond) {
  paste("evaluation of aesthetic", aesthetic, "failed:", cond)
}

messages$errorObservedDataMultipleErrorDefinitions <- function(errorTypes) {
  paste(
    "observed data mapping contains more then one error definition:",
    paste0(errorTypes, collapse = ", ")
  )
}

messages$errorGroupAestheticNeeded <- function() {
  'for mapping of observed to simulated aesthetic "group" or "groupby" is needed'
}

messages$errorInvalidColor <- function(color, varName) {
  paste("Invalid color:", color, "in", varName)
}

messages$errorDataOrObservedDataRequired <- function() {
  "At least 'data' or 'observedData' is required."
}

messages$warningCouldNotDeriveDataWithMapping <- function() {
  "It was not possible to derive the data with the mapping"
}

messages$warningInvalidShapeCode <- function(shape) {
  paste0("Invalid shape code: ", shape, ". Using default square.")
}
