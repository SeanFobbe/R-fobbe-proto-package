#+
#'## f.randomID: Add random ID to Data Tables
#' Adds a randomID for each row to a Data Table object via in-place modification.


#' @param x A data.table.

f.randomID <- function(x){

    x[, randomID := {
        randomID <- sample(.N, .N)
        list(randomID)
        }]
    }
