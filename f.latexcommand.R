
#' @param command.name The name of the new LaTeX command.
#' @param command.body The body of the new LaTeX command.
#'
#' @return A new LaTeX command.


f.latexcommand <- function(command.name,
                           command.body){
    
    newcommand <- paste0("\\newcommand{\\",
                         command.name,
                         "}{",
                         command.body,
                         "}")
    
    return(newcommand)
    
}
