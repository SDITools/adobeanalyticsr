#' Apply previously rund rows of data
#' @noRd
#'
apply_rowsdata <- function(i, resn, prefinalnames, dat)  {
  rowsdata <- function(it, i) {
    if(i == 3) {
      if(resn[[it]]$numberOfElements != 0) {
        tf <- resn[[it]]$rows %>% dplyr::mutate(!!prefinalnames[[2]][[1]] := dat[[1]][it],
                                                !!prefinalnames[[2]][[2]] := dat[[2]][it],
                                                !!prefinalnames[[1]][[1]] := dat[[3]][it],
                                                !!prefinalnames[[1]][[2]] := dat[[4]][it])

        return(tf)
      }
    }
    if(i == 4) {
      if(resn[[it]]$numberOfElements != 0) {
        tf <- resn[[it]]$rows %>% dplyr::mutate(!!prefinalnames[[3]][[1]] := dat[[1]][it],
                                                !!prefinalnames[[3]][[2]] := dat[[2]][it],
                                                !!prefinalnames[[2]][[1]] := dat[[3]][it],
                                                !!prefinalnames[[2]][[2]] := dat[[4]][it],
                                                !!prefinalnames[[1]][[1]] := dat[[5]][it],
                                                !!prefinalnames[[1]][[2]] := dat[[6]][it])
        return(tf)
      }
    }
    if(i == 5) {
      if(resn[[it]]$numberOfElements != 0) {
        tf <- resn[[it]]$rows %>% dplyr::mutate(!!prefinalnames[[4]][[1]] := dat[[1]][it],
                                                !!prefinalnames[[4]][[2]] := dat[[2]][it],
                                                !!prefinalnames[[3]][[1]] := dat[[3]][it],
                                                !!prefinalnames[[3]][[2]] := dat[[4]][it],
                                                !!prefinalnames[[2]][[1]] := dat[[5]][it],
                                                !!prefinalnames[[2]][[2]] := dat[[6]][it],
                                                !!prefinalnames[[1]][[1]] := dat[[7]][it],
                                                !!prefinalnames[[1]][[2]] := dat[[8]][it])
        return(tf)
      }
    }
    if(i == 6) {
      if(resn[[it]]$numberOfElements != 0) {
        tf <-  resn[[it]]$rows %>% dplyr::mutate(!!prefinalnames[[5]][[1]] := dat[[1]][it],
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
    }
    if(i == 7) {
      if(resn[[it]]$numberOfElements != 0) {
        tf <- resn[[it]]$rows %>% dplyr::mutate(!!prefinalnames[[6]][[1]] := dat[[1]][it],
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
    }
    if(i == 8) {
      if(resn[[it]]$numberOfElements != 0) {
        tf <- resn[[it]]$rows %>% dplyr::mutate(!!prefinalnames[[7]][[1]] := dat[[1]][it],
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
    }
    if(i == 9) {
      if(resn[[it]]$numberOfElements != 0) {
        tf <- resn[[it]]$rows %>% dplyr::mutate(!!prefinalnames[[8]][[1]] := dat[[1]][it],
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
    }
    if(i == 10) {
      if(resn[[it]]$numberOfElements != 0) {
        tf <- resn[[it]]$rows %>% dplyr::mutate(!!prefinalnames[[9]][[1]] := dat[[1]][it],
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
    if(i == 11) {
      if(resn[[it]]$numberOfElements != 0) {
        tf <- resn[[it]]$rows %>% dplyr::mutate(!!prefinalnames[[10]][[1]] := dat[[1]][it],
                                                !!prefinalnames[[10]][[2]] := dat[[2]][it],
                                                !!prefinalnames[[9]][[1]] := dat[[3]][it],
                                                !!prefinalnames[[9]][[2]] := dat[[4]][it],
                                                !!prefinalnames[[8]][[1]] := dat[[5]][it],
                                                !!prefinalnames[[8]][[2]] := dat[[6]][it],
                                                !!prefinalnames[[7]][[1]] := dat[[7]][it],
                                                !!prefinalnames[[7]][[2]] := dat[[8]][it],
                                                !!prefinalnames[[6]][[1]] := dat[[9]][it],
                                                !!prefinalnames[[6]][[2]] := dat[[10]][it],
                                                !!prefinalnames[[5]][[1]] := dat[[11]][it],
                                                !!prefinalnames[[5]][[2]] := dat[[12]][it],
                                                !!prefinalnames[[4]][[1]] := dat[[13]][it],
                                                !!prefinalnames[[4]][[2]] := dat[[14]][it],
                                                !!prefinalnames[[3]][[1]] := dat[[15]][it],
                                                !!prefinalnames[[3]][[2]] := dat[[16]][it],
                                                !!prefinalnames[[2]][[1]] := dat[[17]][it],
                                                !!prefinalnames[[2]][[2]] := dat[[18]][it],
                                                !!prefinalnames[[1]][[1]] := dat[[19]][it],
                                                !!prefinalnames[[1]][[2]] := dat[[20]][it])
        return(tf)
      }
    }
    if(i == 12) {
      if(resn[[it]]$numberOfElements != 0) {
        tf <- resn[[it]]$rows %>% dplyr::mutate(!!prefinalnames[[11]][[1]] := dat[[1]][it],
                                                !!prefinalnames[[11]][[2]] := dat[[2]][it],
                                                !!prefinalnames[[10]][[1]] := dat[[3]][it],
                                                !!prefinalnames[[10]][[2]] := dat[[4]][it],
                                                !!prefinalnames[[9]][[1]] := dat[[5]][it],
                                                !!prefinalnames[[9]][[2]] := dat[[6]][it],
                                                !!prefinalnames[[8]][[1]] := dat[[7]][it],
                                                !!prefinalnames[[8]][[2]] := dat[[8]][it],
                                                !!prefinalnames[[7]][[1]] := dat[[9]][it],
                                                !!prefinalnames[[7]][[2]] := dat[[10]][it],
                                                !!prefinalnames[[6]][[1]] := dat[[11]][it],
                                                !!prefinalnames[[6]][[2]] := dat[[12]][it],
                                                !!prefinalnames[[5]][[1]] := dat[[13]][it],
                                                !!prefinalnames[[5]][[2]] := dat[[14]][it],
                                                !!prefinalnames[[4]][[1]] := dat[[15]][it],
                                                !!prefinalnames[[4]][[2]] := dat[[16]][it],
                                                !!prefinalnames[[3]][[1]] := dat[[17]][it],
                                                !!prefinalnames[[3]][[2]] := dat[[18]][it],
                                                !!prefinalnames[[2]][[1]] := dat[[19]][it],
                                                !!prefinalnames[[2]][[2]] := dat[[20]][it],
                                                !!prefinalnames[[1]][[1]] := dat[[21]][it],
                                                !!prefinalnames[[1]][[2]] := dat[[22]][it])
        return(tf)
      }
    }
    if(i == 13) {
      if(resn[[it]]$numberOfElements != 0) {
        tf <- resn[[it]]$rows %>% dplyr::mutate(!!prefinalnames[[12]][[1]] := dat[[1]][it],
                                                !!prefinalnames[[12]][[2]] := dat[[2]][it],
                                                !!prefinalnames[[11]][[1]] := dat[[3]][it],
                                                !!prefinalnames[[11]][[2]] := dat[[4]][it],
                                                !!prefinalnames[[10]][[1]] := dat[[5]][it],
                                                !!prefinalnames[[10]][[2]] := dat[[6]][it],
                                                !!prefinalnames[[9]][[1]] := dat[[7]][it],
                                                !!prefinalnames[[9]][[2]] := dat[[8]][it],
                                                !!prefinalnames[[8]][[1]] := dat[[9]][it],
                                                !!prefinalnames[[8]][[2]] := dat[[10]][it],
                                                !!prefinalnames[[7]][[1]] := dat[[11]][it],
                                                !!prefinalnames[[7]][[2]] := dat[[12]][it],
                                                !!prefinalnames[[6]][[1]] := dat[[13]][it],
                                                !!prefinalnames[[6]][[2]] := dat[[14]][it],
                                                !!prefinalnames[[5]][[1]] := dat[[15]][it],
                                                !!prefinalnames[[5]][[2]] := dat[[16]][it],
                                                !!prefinalnames[[4]][[1]] := dat[[17]][it],
                                                !!prefinalnames[[4]][[2]] := dat[[18]][it],
                                                !!prefinalnames[[3]][[1]] := dat[[19]][it],
                                                !!prefinalnames[[3]][[2]] := dat[[20]][it],
                                                !!prefinalnames[[2]][[1]] := dat[[21]][it],
                                                !!prefinalnames[[2]][[2]] := dat[[22]][it],
                                                !!prefinalnames[[1]][[1]] := dat[[23]][it],
                                                !!prefinalnames[[1]][[2]] := dat[[24]][it])
        return(tf)
      }
    }
    if(i == 14) {
      if(resn[[it]]$numberOfElements != 0) {
        tf <- resn[[it]]$rows %>% dplyr::mutate(!!prefinalnames[[13]][[1]] := dat[[1]][it],
                                                !!prefinalnames[[13]][[2]] := dat[[2]][it],
                                                !!prefinalnames[[12]][[1]] := dat[[3]][it],
                                                !!prefinalnames[[12]][[2]] := dat[[4]][it],
                                                !!prefinalnames[[11]][[1]] := dat[[5]][it],
                                                !!prefinalnames[[11]][[2]] := dat[[6]][it],
                                                !!prefinalnames[[10]][[1]] := dat[[7]][it],
                                                !!prefinalnames[[10]][[2]] := dat[[8]][it],
                                                !!prefinalnames[[9]][[1]] := dat[[9]][it],
                                                !!prefinalnames[[9]][[2]] := dat[[10]][it],
                                                !!prefinalnames[[8]][[1]] := dat[[11]][it],
                                                !!prefinalnames[[8]][[2]] := dat[[12]][it],
                                                !!prefinalnames[[7]][[1]] := dat[[13]][it],
                                                !!prefinalnames[[7]][[2]] := dat[[14]][it],
                                                !!prefinalnames[[6]][[1]] := dat[[15]][it],
                                                !!prefinalnames[[6]][[2]] := dat[[16]][it],
                                                !!prefinalnames[[5]][[1]] := dat[[17]][it],
                                                !!prefinalnames[[5]][[2]] := dat[[18]][it],
                                                !!prefinalnames[[4]][[1]] := dat[[19]][it],
                                                !!prefinalnames[[4]][[2]] := dat[[20]][it],
                                                !!prefinalnames[[3]][[1]] := dat[[21]][it],
                                                !!prefinalnames[[3]][[2]] := dat[[22]][it],
                                                !!prefinalnames[[2]][[1]] := dat[[23]][it],
                                                !!prefinalnames[[2]][[2]] := dat[[24]][it],
                                                !!prefinalnames[[1]][[1]] := dat[[25]][it],
                                                !!prefinalnames[[1]][[2]] := dat[[26]][it])
        return(tf)
      }
    }
    if(i == 15) {
      if(resn[[it]]$numberOfElements != 0) {
        tf <- resn[[it]]$rows %>% dplyr::mutate(!!prefinalnames[[14]][[1]] := dat[[1]][it],
                                                !!prefinalnames[[14]][[2]] := dat[[2]][it],
                                                !!prefinalnames[[13]][[1]] := dat[[3]][it],
                                                !!prefinalnames[[13]][[2]] := dat[[4]][it],
                                                !!prefinalnames[[12]][[1]] := dat[[5]][it],
                                                !!prefinalnames[[12]][[2]] := dat[[6]][it],
                                                !!prefinalnames[[11]][[1]] := dat[[7]][it],
                                                !!prefinalnames[[11]][[2]] := dat[[8]][it],
                                                !!prefinalnames[[10]][[1]] := dat[[9]][it],
                                                !!prefinalnames[[10]][[2]] := dat[[10]][it],
                                                !!prefinalnames[[9]][[1]] := dat[[11]][it],
                                                !!prefinalnames[[9]][[2]] := dat[[12]][it],
                                                !!prefinalnames[[8]][[1]] := dat[[13]][it],
                                                !!prefinalnames[[8]][[2]] := dat[[14]][it],
                                                !!prefinalnames[[7]][[1]] := dat[[15]][it],
                                                !!prefinalnames[[7]][[2]] := dat[[16]][it],
                                                !!prefinalnames[[6]][[1]] := dat[[17]][it],
                                                !!prefinalnames[[6]][[2]] := dat[[18]][it],
                                                !!prefinalnames[[5]][[1]] := dat[[19]][it],
                                                !!prefinalnames[[5]][[2]] := dat[[20]][it],
                                                !!prefinalnames[[4]][[1]] := dat[[21]][it],
                                                !!prefinalnames[[4]][[2]] := dat[[22]][it],
                                                !!prefinalnames[[3]][[1]] := dat[[23]][it],
                                                !!prefinalnames[[3]][[2]] := dat[[24]][it],
                                                !!prefinalnames[[2]][[1]] := dat[[25]][it],
                                                !!prefinalnames[[2]][[2]] := dat[[26]][it],
                                                !!prefinalnames[[1]][[1]] := dat[[27]][it],
                                                !!prefinalnames[[1]][[2]] := dat[[28]][it])
        return(tf)
      }
    }
  }

resrows <- purrr::map2_dfr(seq(length(resn)), i, rowsdata)
return(resrows)
}
