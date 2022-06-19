
#' @param command.name The name of the LaTeX command.
#' @param command.body The body of the LaTeX command.
#'
#' @return A functional LaTeX command.


f.latexcommand <- function(command.name,
                           command.body){
    
    newcommand <- paste0("\\newcommand{\\",
                         command.name,
                         "}{",
                         command.body,
                         "}")
    
    return(newcommand)
    
}
