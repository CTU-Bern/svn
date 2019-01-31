#* ******************************************** */ 
#* Study: GENERIC CODE                        */ 
#* Author: Alan Haynes                          */
#* Purpose: Function to prepare report SVN statement   */  
#* Date created: 30.01.2019                     */ 
#* Last update:                       */ 
#* External routines: None                      */ 
#* ******************************************** */ 

svn_details <- function(path_to_repo = NULL){
  if(is.null(path_to_repo)) path_to_repo <- ""
  
  res <- sapply(path_to_repo, function(x) {
    stat <- paste("svn status", x)
    info <- paste("svn info", x)
    stat <- system(stat, intern = TRUE)
    info <- system(info, intern = TRUE)
    if(x == "") x <- getwd()
    
    rel <- info[grepl("Relative URL", info)]
    rel <- gsub("Relative URL: ", "", rel)
    
    # identify repo
    ss <- unlist(strsplit(info[grepl("Repository Root", info)], "/"))
    repo_name <- trimws(ss[length(ss)])
    
    # identify revision
    ss <- unlist(strsplit(info[grepl("^Revision", info)], ":"))
    rev_no <- trimws(ss[length(ss)])
    
    # changes?
    status <- substring(stat, 1, 1)
    changes <- any(c("A", "M", "D") %in% status)
    changetxt <- ifelse(changes, "CAUTION: REPO HAS CHANGED SINCE LAST COMMIT", "")
    txt <- sprintf("Folder %s is at revision %s from the %s SVN repository. %s", x, rev_no, repo_name, changetxt)
    txt
  })
  
  paste(unlist(res), collapse = "\n")
  
  
}


svn_details()
svn_details("ext/r")
svn_details("ext/ado")
svn_details("ext/helper")

svn_details(c("", "ext/r", "ext/ado"))

path_to_repo <- c("", "ext/r", "ext/ado", "ext/helper")
svn_details(path_to_repo)

path_to_repo <- "ext/ado"






