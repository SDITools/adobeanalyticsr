#' Setting credentials to the environment
#'
#
#' @export
#'
aa_creds <- function()
  {
    clientid <- tolower(as.character(readline(prompt="Do you know your client id? (y/n) ")))
    if(clientid == 'y'){
      ci <-as.character(readline(prompt="Enter your client id --> "))
      Sys.setenv("AA_CLIENT_ID" = ci)
    } else {
      readline("Go here and find out how to get it. (press return to continue)")
    }
    clientsecret <- tolower(as.character(readline(prompt="Do you know your client secret key? (y/n) ")))
    if(clientsecret == 'y'){
      ci <-as.character(readline(prompt="Enter your client secret key --> "))
      Sys.setenv("AA_CLIENT_SECRET" = ci)
    } else {
      readline("You can find your client secret by going here.  (press return to continue)")
    }
    rsid <- tolower(as.character(readline(prompt="Do you know your report suite id? (y/n) ")))
    if(rsid == 'y'){
      ci <-as.character(readline(prompt="Enter your report suite id --> "))
      Sys.setenv("AA_REPORTSUITE_ID" = ci)
    } else {
      readline("You can find your Report Suite Id by going here [link]. (press return to continue)")
    }
    if(clientid == 'y' & clientsecret == 'y' & rsid == 'y') {
      "You're all set! Nicely done!"
    }
}

