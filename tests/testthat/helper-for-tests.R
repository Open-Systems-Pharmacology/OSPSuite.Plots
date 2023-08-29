# Helper for tests
getTestDataFilePath <- function(fileName) {
  dataPath <- file.path(getwd(), "..", "data", fsep = .Platform$file.sep)
  file.path(dataPath, fileName, fsep = .Platform$file.sep)
}

executeWithTestFile <- function(actionWithFile) {
  newFile <- tempfile()
  actionWithFile(newFile)
  file.remove(newFile)
}

comparisonTolerance <- function(tolerance = 1e-4) {
  tolerance
}
