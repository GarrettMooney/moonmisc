##' Update installed github packages
##'
##' Update installed github packages
##' @title update_github
##' @return none
##' @author https://stackoverflow.com/questions/32538052/update-all-packages-from-github
##' @export
update_github <- function() {
  # check/load necessary packages
  # remotes package
  if (!("package:remotes" %in% search())) {
    tryCatch(require(remotes), error = function(x) {warning(x); cat("Cannot load remotes package \n")})
    on.exit(detach("package:remotes", unload=TRUE))
  }

  pkgs <- installed.packages(fields = "RemoteType")
  github_pkgs <- pkgs[pkgs[, "RemoteType"] %in% "github", "Package"]

  print(github_pkgs)
  lapply(github_pkgs, function(pac) {
    message("Updating ", pac, " from GitHub...")

    repo = packageDescription(pac, fields = "GithubRepo")
    username = packageDescription(pac, fields = "GithubUsername")

    try(install_github(repo = paste0(username, "/", repo)))
  })
}
