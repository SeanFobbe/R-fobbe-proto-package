#'# f.dopar.spacyparse
#' Iterated parallel computation of linguistic annotations via spacy.


#' @param x A data table. Must have variables "doc_id" and "text".



f.future_spacyparse <- function(x,
                               threads = detectCores(),
                               chunksize = 1,
                               model = "en_core_web_sm",
                               pos = TRUE,
                               tag = FALSE,
                               lemma = FALSE,
                               entity = FALSE,
                               dependency = FALSE,
                               nounphrase = FALSE){

    begin.dopar <- Sys.time()

    spacy_initialize(model = model)

    
    print(paste0("Parallel processing using ",
                 threads,
                 " threads. Begin at ",
                 begin.dopar,
                 ". Processing ",
                 x[,.N],
                 " documents"))

    
    cl <- makeForkCluster(threads)
    registerDoParallel(cl)

    itx <- iter(x,
                by = "row",
                chunksize = chunksize)



    result <- foreach(document = itx,
                      .errorhandling = 'pass') %dopar% {
                          
                          out <- spacy_parse(document,
                                             pos = pos,
                                             tag = tag,
                                             lemma = lemma,
                                             entity = entity,
                                             dependency = dependency,
                                             nounphrase = nounphrase,
                                             multithread = FALSE)

                          return(out)}

    stopCluster(cl)
    
    txt.parsed <- rbindlist(result)
    

    end.dopar <- Sys.time()
    duration.dopar <- end.dopar - begin.dopar

    print(paste0("Runtime was ",
                 round(duration.dopar,
                       digits = 2),
                 " ",
                 attributes(duration.dopar)$units,
                 ". Ended at ",
                 end.dopar, "."))

    spacy_finalize()
    
    return(txt.parsed)


}
