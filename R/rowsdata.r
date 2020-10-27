#' Used to join previous rows of data
#'
#' @param it joining the itemIds and corresponding data for each new row of data
#' @param i the dimension number the function is working on now
#'
#' @export
#'

rowsdata <- function(it, i) {
  if(i == 2) {
    tf <- res[[it]]$rows %>% mutate(!!prefinalnames[[1]][[1]] := dat[[1]][[it]],
                                    !!prefinalnames[[1]][[2]] := dat[[2]][[it]])
    return(tf)
  }
  if(i == 3) {
    tf <- resn[[it]]$rows %>% mutate(!!prefinalnames[[2]][[1]] := dat[[1]][it],
                                     !!prefinalnames[[2]][[2]] := dat[[2]][it],
                                     !!prefinalnames[[1]][[1]] := dat[[3]][it],
                                     !!prefinalnames[[1]][[2]] := dat[[4]][it])
    return(tf)
  }
  if(i == 4) {
    tf <- resn[[it]]$rows %>% mutate(!!prefinalnames[[3]][[1]] := dat[[1]][it],
                                     !!prefinalnames[[3]][[2]] := dat[[2]][it],
                                     !!prefinalnames[[2]][[1]] := dat[[3]][it],
                                     !!prefinalnames[[2]][[2]] := dat[[4]][it],
                                     !!prefinalnames[[1]][[1]] := dat[[5]][it],
                                     !!prefinalnames[[1]][[2]] := dat[[6]][it])
    return(tf)
  }
  if(i == 5) {
    tf <- resn[[it]]$rows %>% mutate(!!prefinalnames[[4]][[1]] := dat[[1]][it],
                                     !!prefinalnames[[4]][[2]] := dat[[2]][it],
                                     !!prefinalnames[[3]][[1]] := dat[[3]][it],
                                     !!prefinalnames[[3]][[2]] := dat[[4]][it],
                                     !!prefinalnames[[2]][[1]] := dat[[5]][it],
                                     !!prefinalnames[[2]][[2]] := dat[[6]][it],
                                     !!prefinalnames[[1]][[1]] := dat[[7]][it],
                                     !!prefinalnames[[1]][[2]] := dat[[8]][it])
    return(tf)
  }
  if(i == 6) {
    tf <-  resn[[it]]$rows %>% mutate(!!prefinalnames[[5]][[1]] := dat[[1]][it],
                                      !!prefinalnames[[5]][[2]] := dat[[2]][it],
                                      !!prefinalnames[[4]][[1]] := dat[[3]][it],
                                      !!prefinalnames[[4]][[2]] := dat[[4]][it],
                                      !!prefinalnames[[3]][[1]] := dat[[5]][it],
                                      !!prefinalnames[[3]][[2]] := dat[[6]][it],
                                      !!prefinalnames[[2]][[1]] := dat[[7]][it],
                                      !!prefinalnames[[2]][[2]] := dat[[8]][it],
                                      !!prefinalnames[[1]][[1]] := dat[[9]][it],
                                      !!prefinalnames[[1]][[2]] := dat[[10]][it])
    return(tf)
  }
  if(i == 7) {
    tf <- resn[[it]]$rows %>% mutate(!!prefinalnames[[6]][[1]] := dat[[1]][it],
                                     !!prefinalnames[[6]][[2]] := dat[[2]][it],
                                     !!prefinalnames[[5]][[1]] := dat[[3]][it],
                                     !!prefinalnames[[5]][[2]] := dat[[4]][it],
                                     !!prefinalnames[[4]][[1]] := dat[[5]][it],
                                     !!prefinalnames[[4]][[2]] := dat[[6]][it],
                                     !!prefinalnames[[3]][[1]] := dat[[7]][it],
                                     !!prefinalnames[[3]][[2]] := dat[[8]][it],
                                     !!prefinalnames[[2]][[1]] := dat[[9]][it],
                                     !!prefinalnames[[2]][[2]] := dat[[10]][it],
                                     !!prefinalnames[[1]][[1]] := dat[[11]][it],
                                     !!prefinalnames[[1]][[2]] := dat[[12]][it])
    return(tf)
  }
  if(i == 8) {
    tf <- resn[[it]]$rows %>% mutate(!!prefinalnames[[7]][[1]] := dat[[1]][it],
                                     !!prefinalnames[[7]][[2]] := dat[[2]][it],
                                     !!prefinalnames[[6]][[1]] := dat[[3]][it],
                                     !!prefinalnames[[6]][[2]] := dat[[4]][it],
                                     !!prefinalnames[[5]][[1]] := dat[[5]][it],
                                     !!prefinalnames[[5]][[2]] := dat[[6]][it],
                                     !!prefinalnames[[4]][[1]] := dat[[7]][it],
                                     !!prefinalnames[[4]][[2]] := dat[[8]][it],
                                     !!prefinalnames[[3]][[1]] := dat[[9]][it],
                                     !!prefinalnames[[3]][[2]] := dat[[10]][it],
                                     !!prefinalnames[[2]][[1]] := dat[[11]][it],
                                     !!prefinalnames[[2]][[2]] := dat[[12]][it],
                                     !!prefinalnames[[1]][[1]] := dat[[13]][it],
                                     !!prefinalnames[[1]][[2]] := dat[[14]][it])
    return(tf)
  }
  if(i == 9) {
    tf <- resn[[it]]$rows %>% mutate(!!prefinalnames[[8]][[1]] := dat[[1]][it],
                                     !!prefinalnames[[8]][[2]] := dat[[2]][it],
                                     !!prefinalnames[[7]][[1]] := dat[[3]][it],
                                     !!prefinalnames[[7]][[2]] := dat[[4]][it],
                                     !!prefinalnames[[6]][[1]] := dat[[5]][it],
                                     !!prefinalnames[[6]][[2]] := dat[[6]][it],
                                     !!prefinalnames[[5]][[1]] := dat[[7]][it],
                                     !!prefinalnames[[5]][[2]] := dat[[8]][it],
                                     !!prefinalnames[[4]][[1]] := dat[[9]][it],
                                     !!prefinalnames[[4]][[2]] := dat[[10]][it],
                                     !!prefinalnames[[3]][[1]] := dat[[11]][it],
                                     !!prefinalnames[[3]][[2]] := dat[[12]][it],
                                     !!prefinalnames[[2]][[1]] := dat[[13]][it],
                                     !!prefinalnames[[2]][[2]] := dat[[14]][it],
                                     !!prefinalnames[[1]][[1]] := dat[[15]][it],
                                     !!prefinalnames[[1]][[2]] := dat[[16]][it])
    return(tf)
  }
  if(i == 10) {
    tf <- resn[[it]]$rows %>% mutate(!!prefinalnames[[9]][[1]] := dat[[1]][it],
                                     !!prefinalnames[[9]][[2]] := dat[[2]][it],
                                     !!prefinalnames[[8]][[1]] := dat[[3]][it],
                                     !!prefinalnames[[8]][[2]] := dat[[4]][it],
                                     !!prefinalnames[[7]][[1]] := dat[[5]][it],
                                     !!prefinalnames[[7]][[2]] := dat[[6]][it],
                                     !!prefinalnames[[6]][[1]] := dat[[7]][it],
                                     !!prefinalnames[[6]][[2]] := dat[[8]][it],
                                     !!prefinalnames[[5]][[1]] := dat[[9]][it],
                                     !!prefinalnames[[5]][[2]] := dat[[10]][it],
                                     !!prefinalnames[[4]][[1]] := dat[[11]][it],
                                     !!prefinalnames[[4]][[2]] := dat[[12]][it],
                                     !!prefinalnames[[3]][[1]] := dat[[13]][it],
                                     !!prefinalnames[[3]][[2]] := dat[[14]][it],
                                     !!prefinalnames[[2]][[1]] := dat[[15]][it],
                                     !!prefinalnames[[2]][[2]] := dat[[16]][it],
                                     !!prefinalnames[[1]][[1]] := dat[[17]][it],
                                     !!prefinalnames[[1]][[2]] := dat[[18]][it])
    return(tf)
  }
}
