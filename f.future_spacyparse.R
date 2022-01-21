#'# f.dopar.spacyparse
#' Iterated parallel computation of linguistic annotations via spacy.


#' @param x A data table. Must have variables "doc_id" and "text".



f.future_spacyparse <- function(x,
                               chunksize = 1,
                               model = "en_core_web_sm",
                               pos = TRUE,
                               tag = FALSE,
                               lemma = FALSE,
                               entity = FALSE,
                               dependency = FALSE,
                               nounphrase = FALSE){

    begin <- Sys.time()

    spacy_initialize(model = model)

    
    print(paste0("Begin at ",
                 begin,
                 ". Processing ",
                 x[,.N],
                 " documents"))


    
    raw.list <- split(x, seq(nrow(x)))
    
    result.list <- future_lapply(raw.list,
                                 f.lingsummarize,
                                 future.seed = TRUE,
                                 future.scheduling = chunksperworker,
                                 future.chunk.size = chunksize)
    
    result.dt <- rbindlist(result.list)
    


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
    

    end <- Sys.time()
    duration <- end - begin

    print(paste0("Runtime was ",
                 round(duration,
                       digits = 2),
                 " ",
                 attributes(duration)$units,
                 ". Ended at ",
                 end.dopar, "."))

    spacy_finalize()
    
    return(txt.parsed)


}
