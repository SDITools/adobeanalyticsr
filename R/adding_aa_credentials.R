#' Setting credentials to the environment
#'
#
#' @export
#'
aa_creds <- function()
  {
    a1 <-as.character(readline(prompt="Do you know your client_id? (y/n) "))
    if(a1 == 'y'){
      ci <-as.character(readline(prompt="Enter your client_id --> "))
      Sys.setenv("TEST_ID" = ci)
    } else {
      readline("go here and find out how to get it. (press enter to continue)")
    }
    a2 <-as.character(readline(prompt="Do you know your client_secret? (y/n) "))
    if(a2 == 'y'){
      ci <-as.character(readline(prompt="Enter your client_secret --> "))
      Sys.setenv("TEST_SECRET" = ci)
    } else {
      readline("you can find your client secret by going here [link]...now!")
    }
    if(a1 == 'y' & a2 == 'y') {
      readline("your all set! Nicely done! Press enter to continue")
    }
}

aa_creds()
Sys.getenv("TEST_SECRET")
Sys.getenv("TEST_CLIENT")
