#'## f.empty.vector.NA: Replaces empty elements in vector with "NA"

f.vec.empty.NA <- function(x) gsub("^$", "NA", x)
