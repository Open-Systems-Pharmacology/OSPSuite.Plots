#' `Linter`  for Adjacent Empty Lines
#'
#' This `linter` checks for  consecutive blank lines in R source code.
#' If found, it generates a lint warning indicating that the blank lines are superfluous.
#'
#' @param maxNEmptyLines maximal allowed number of empty lines
#'
#' @return A list of linting messages for any lines that contain three or more consecutive blank lines.
#' @export
adjacentEmptyLines_linter <- function(maxNEmptyLines = 2) { # nolint: object_name_linter  use lintr convention for function names
  lintr::Linter(function(sourceExpression) {
    blanks <- rex::re_matches(
      sourceExpression$file_lines,
      rex::rex(
        rex::shortcuts$start,
        rex::shortcuts$any_spaces,
        rex::shortcuts$end
      )
    )
    lineNumber <- length(sourceExpression$file_lines)
    lints <- list()
    consecutiveBlankLines <- 0

    if (lineNumber > 1L) {
      for (i in lineNumber:1) {
        if (!is.na(blanks[[i]]) && isTRUE(blanks[[i]])) {
          consecutiveBlankLines <- consecutiveBlankLines + 1
        } else {
          consecutiveBlankLines <- 0
        }

        if (consecutiveBlankLines > maxNEmptyLines) {
          lints[[length(lints) + 1L]] <- lintr::Lint(
            filename = sourceExpression$filename,
            line_number = i + 2,
            column_number = 1L,
            type = "style",
            message = "Too many consecutive blank lines are superfluous.",
            line = sourceExpression$file_lines[[i + 2]]
          )
        }
      }
    }
    lints
  })
}
