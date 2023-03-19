
#'## f.zenodo.stats

#' @param url URL of Zenodo entry, e.g. "https://zenodo.org/record/7051929". Works on DOIs returned by f.repo.content (see below).
#'
#' @return A data frame of views and downloads for the specified URL, including the date the statistics were acquired.




f.zenodo.stats <- function(url){
    
    html <- rvest::read_html(url)
    nodes <- rvest::html_nodes(html, "[class='stats-data']")
    text <- rvest::html_text(nodes, trim = TRUE)
    
    temp <- gsub(",", "", text)
    temp <- as.numeric(temp)
    view <- temp[1]
    dl <- temp[2]
    date <- Sys.Date()
    
    df <- data.frame(view,
                     dl,
                     date,
                     url)

    Sys.sleep(1)
    
    return(df)
}







#'## f.repo.content

#' @param reponame Name of Zenodo repository, e.g. "sean-fobbe-data".
#'
#' @return A data frame of titles and concept DOIs.


f.repo.content <- function(reponame){
    
    url <- paste0("https://zenodo.org/oai2d?verb=ListRecords&set=user-",
                  reponame,
                  "&metadataPrefix=dcat")
    

    ## Read XML
    xml <- rvest::read_html(url)

    ## DCAT: Extract Concept DOI Link
    nodes <- rvest::html_nodes(xml, "isversionof")
    url <- rvest::html_attr(nodes, "rdf:resource")

    ## DCAT: Extract Title
    nodes <- rvest::html_nodes(xml, "title")
    title <- rvest::html_text(nodes)

    ## Create Data Frame
    df <- data.frame(title, url)

    ## Deduplicate via Concept DOIs
    df <- df[-which(duplicated(df$url)), ]
    rownames(df) <- 1:nrow(df)

    
    return(df)

    
}




