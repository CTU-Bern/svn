#' get details on the status of an svn repo
#'
#' @param path_to_repo path(s) to the folder(s) whose status to query (if NULL it uses the working directory). Can be a vector.
#'
#' @return character
#' @export
#'
#' @examples
#' # svn_details()
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
#' @param path_to_repo path(s) to the folder(s) whose status to query (if NULL it uses the working directory). Can be a vector.
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

# internal function to check whether SVN is installed
.svn_exists <- function(){
  return <- suppressWarnings(try(system("svn", intern = TRUE)))
  class(return) != "try-error"
}


#' Find conflicts in repo
#'
#' @param path_to_repo path(s) to the folder(s) whose status to query (if NULL it uses the working directory). Can be a vector.
#'
#' @return character vector of conflicts (tree or otherwise)
#' @export
#'
#' @examples
#' # svn_conflict(path_to_some_repo)
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
#' @param path_to_repo path(s) to the folder(s) whose status to query (if NULL it uses the working directory). Can be a vector.
#' @param ignored logical. Return ignored/unversioned files only?
#'
#' @return list containing lists of each type of file status (e.g. added (\code{add}), deleted (\code{delete}), ...)
#' @export
#'
#' @examples
#' # svn_state(path_to_some_repo)
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


# svn_log
#' Log of commits in repository/repositories
#'
#' @param path_to_repo path(s) to the folder(s) whose status to query (if NULL it uses the working directory). Can be a vector.
#'
#' @return list of dataframes containing repo, revision, user, date, number of lines and commit message
#' @export
#'
#' @examples
#' # svn_log(path_to_some_repo)
svn_log <- function(path_to_repo = NULL){
  if(!.svn_exists()) stop("SVN doen't appear to be installed")
  if(is.null(path_to_repo)) path_to_repo <- ""

  control <- sapply(path_to_repo, .svn_control)
  if(!any(control)) stop("'path_to_repo' not under SVN control")

  res <- lapply(path_to_repo, function(x){
    control <- .svn_control(x)
    if(control){
      stat <- paste("svn log", x)
      stat <- system(stat, intern = TRUE)
      stat <- stat[-length(stat)]
      w <- grep("--------", stat)
      info <- stat[w+1]
      s <- strsplit(info, " | ", fixed = TRUE)
      rev <- sapply(s, "[", 1)
      user <- sapply(s, "[", 2)
      date <- sapply(s, "[", 3)
      date <- as.POSIXct(substr(date, 1, 19), format = "%Y-%m-%d %H:%M:%S")
      lines <- sapply(s, "[", 4)
      txt <- lapply(w, function(x){
        y <- min(min(w[w > x])-1, length(stat), na.rm = TRUE)
        stat[(x+3):y]
      })
      txt <- sapply(txt, paste, collapse = "\n")
      out <- data.frame(repo = x,
                 rev = rev,
                 user = user,
                 date = date,
                 lines = lines,
                 txt = txt)

      } else {
      if(x == "") x <- getwd()
      txt <- sprintf("Folder %s is not under SVN control", x)
      out <- data.frame(repo = x,
                        rev = NA,
                        user = NA,
                        date = NA,
                        txt = txt)
    }
    out
  })

  res
}

#' Get SVN revision number
#'
#' @param path_to_repo path to repo(s)
#'
#' @return number or list of numbers
#' @export
#'
#' @examples
#' # svn_revnum(path_to_some_repo)
svn_revnum <- function(path_to_repo = NULL, update = TRUE){
  if(!.svn_exists()) stop("SVN doen't appear to be installed")
  if(is.null(path_to_repo)) path_to_repo <- ""

  control <- sapply(path_to_repo, .svn_control)
  if(!any(control)) stop("'path_to_repo' not under SVN control")

  if(length(path_to_repo) > 1){


    res <- lapply(path_to_repo, function(x){
      control <- .svn_control(x)
      if(control){
        if(update){
          update_cmd <- paste("svn update", x)
          system(update_cmd, intern = TRUE)
        }
        .revnum(x)

      } else {
        rev_no <- NA
      }
      rev_no
    })
  } else {

    if(update){
      update_cmd <- paste("svn update", path_to_repo)
      system(update_cmd, intern = TRUE)
    }
    res <- .revnum(path_to_repo)
  }
  res
}

.revnum <- function(x = ""){
  info <- paste("svn info", x)
  info <- system(info, intern = TRUE)
  if(x == "") x <- getwd()

  # identify revision
  ss <- unlist(strsplit(info[grepl("^Revision", info)], ":"))
  rev_no <- as.numeric(trimws(ss[length(ss)]))
  rev_no
}
