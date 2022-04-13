# svn
Interface between R and SVN to get info on repo status.

## Installation
``` R
# if you do not have devtools installed
# install.packages("devtools", dependencies=TRUE)
# install svn package
devtools::install_github("CTU-Bern/svn")
```

## Testing/Development 
``` R
devtools::document(".")
# devtools::test(".") # tests not really possible as not transferrable to other machines
devtools::install()
```
*NOTE: due to external software requirements (SVN and a suitable repo), examples do not work and the package is not fully tested*

## Usage

``` R
# return details of current wirking directory in a format that could be used in a report
svn_details()
# return lists of file status (added, modified, etc)
svn_state()
# same as above, but with a list of the ignored files too
svn_state(ignored = TRUE)
# return a list of conflicted files
svn_conflict()
# the log
svn_log()
# revision number
svn_revnum()
```

The functions also allow multiple repositories to be queried simultaneously (useful for e.g. externals).

``` R
repos <- c("path_to_repo1", "path_to_repo2", "path_to_repo3")
# return details of current wirking directory
svn_details(repos)
# return lists of file status (added, modified, etc)
svn_state(repos)
# same as above, but with a list of the ignored files too
svn_state(repos, ignored = TRUE)
# return a list of conflicted files
svn_conflict(repos)
# the log
svn_log(repos)
# revision number
svn_revnum(repos)
```

