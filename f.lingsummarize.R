#'# f.lingsummarize
#' Parallel computation of tokens, types and sentences for each document of a given Quanteda corpus object.


f.lingsummarize <- function(df,
                        threads = detectCores()){
 
    print(paste("Parallel processing using", threads, "threads."))

    
    corpus <- corpus(df)

    cl <- makeForkCluster(threads)
    registerDoParallel(cl)
    
    result <- foreach(i = seq_len(ndoc(corpus)),
                      .errorhandling = 'pass') %dopar% {
                          temp <- summary(corpus[i])
                          return(temp)
                      }
    stopCluster(cl)
    
    summary.corpus <- rbindlist(result)
    return(summary.corpus)
}
