

#' get details on the status of an svn repo
#'
#' @param path_to_repo path to the folder whose status to query (if NULL it uses the working directory)
#'
#' @return character
#' @export
#'
#' @examples
#' svn_details()
#'
svn_details <- function(path_to_repo = NULL){
  if(!.svn_exists()) stop("SVN doen't appear to be installed")
  if(is.null(path_to_repo)) path_to_repo <- ""

  control <- sapply(path_to_repo, .svn_control)
  if(!any(control)) stop("'path_to_repo' not under SVN control")

  res <- sapply(path_to_repo, function(x) {
    control <- .svn_control(x)
    if(control){
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
    } else {
      if(x == "") x <- getwd()
      txt <- sprintf("Folder %s is not under SVN control", x)
    }
    txt
  })

  paste(unlist(res), collapse = "\n")


}


# svn_details()
# svn_details("ext/r")
# svn_details("ext/ado")
# svn_details("ext/helper")
#
# svn_details(c("", "ext/r", "ext/ado"))
#
# path_to_repo <- c("", "ext/r", "ext/ado", "ext/helper")
# svn_details(c(path_to_repo, ""))
#
# path_to_repo <- "ext/ado"




#' Check whether a path is under SVN control
#'
#' @param path_to_repo
#'
#' @return
#'
.svn_control <- function(path_to_repo = NULL){
  if(is.null(path_to_repo)) path_to_repo <- ""
  return <- suppressWarnings(tryCatch(system(paste("svn info", path_to_repo), intern = TRUE)))
  if(!is.null(attributes(return)$status)){
    x <- FALSE
  } else {
    x <- TRUE
  }
  x
}
# path_to_repo <- "C:\\Users\\haynes\\Documents\\ClinicalStudies/690_PreferenceSensitiveCare_NRP74/21_Statistics_690/stataR_hyst"
# .svn_control(path_to_repo)

.svn_exists <- function(){
  return <- suppressWarnings(try(system("svn", intern = TRUE)))
  class(return) != "try-error"
}


#' Find conflicts in repo
#'
#' @param path_to_repo
#'
#' @return character vector of conflicts (tree or otherwise)
#' @export
#'
#' @examples
svn_conflict <- function(path_to_repo = NULL){
  if(!.svn_exists()) stop("SVN doen't appear to be installed")
  if(is.null(path_to_repo)) path_to_repo <- ""

  control <- sapply(path_to_repo, .svn_control)
  if(!any(control)) stop("'path_to_repo' not under SVN control")

  res <- sapply(path_to_repo, function(x) {
    control <- .svn_control(x)
    if(control){
      stat <- paste("svn status", x)
      stat <- system(stat, intern = TRUE)
      # changes?
      status <- substring(stat, 1, 8)
      conf <- status[grepl("C", status)]
      txt <- stat[grepl("C", status)]


    } else {
      if(x == "") x <- getwd()
      txt <- sprintf("Folder %s is not under SVN control", x)
    }
    txt
  })

  return(res)

}
# svn_conflict()

#' States of files in repo
#'
#' @param path_to_repo
#'
#' @return list containing lists of each type of file status
#' @export
#'
#' @examples
svn_state <- function(path_to_repo = NULL, ignored = FALSE){
  if(!.svn_exists()) stop("SVN doen't appear to be installed")
  if(is.null(path_to_repo)) path_to_repo <- ""

  control <- sapply(path_to_repo, .svn_control)
  if(!any(control)) stop("'path_to_repo' not under SVN control")

  res <- lapply(path_to_repo, function(x){
    control <- .svn_control(x)
    if(control){
      stat <- paste("svn status", x)
      stat <- system(stat, intern = TRUE)

      status <- substr(stat, 1, 8)
      txt <- list(add = stat[grepl("A", status)],
                  delete = stat[grepl("D", status)],
                  modified = stat[grepl("M", status)],
                  conflict = stat[grepl("C", status)],
                  external = stat[grepl("X", status)],
                  missing = stat[grepl("!", status)],
                  replaced = stat[grepl("R", status)]
                  )
      if(ignored){
        txt$ignored <- stat[grepl("?", status)]
      }

    } else {
      if(x == "") x <- getwd()
      txt <- sprintf("Folder %s is not under SVN control", x)
    }
    txt
  })

  res
}
