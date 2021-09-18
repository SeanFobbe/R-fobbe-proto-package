#'# f.show.values: Show example values of data.table as kable
#' This function functions much like the standard data.table print behaviour, only that the result is a kable table. This is is useful when Listings throws an error for unknown symbols, as kable tables are processed with regular LaTeX, not listings.


#'@param x A data.table for which example values are to be shown.

f.show.values <- function(x){
    length <- length(x)
    rows.final <- seq((x[,.N] - 4), x[,.N], 1)

    for (index.begin in seq(1, length, 4)){
        index.end <- index.begin + 3

        if(index.end > length){
            index.end <- length
        }

        print(kable(x[c(1:5,
                        rows.final),
                      index.begin:index.end],
              format = "latex",
              align = c("p{4cm}"),
              booktabs = TRUE,
              longtable = TRUE))
    }
}
