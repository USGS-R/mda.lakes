#' @title Summary stats for Notaro
#' 
#' @param nc.file path to nc file
#' 
#' @return data.frame with summary statistics
#' 
#' 
#' @import tidyr
#' @import lubridate
#' 
#' @export 
summarize_notaro = function(nc.file){
  library(glmtools)
  exclude.vars <- c("extc_coef", "salt", "precip", "wind")
  var.names <- as.character(sim_vars(nc.file)$name)
  
  lake.data = lapply(var.names[!var.names %in% exclude.vars], function(x) summarize_var_notaro(nc.file, x))
  df.data <- data.frame()
  for (i in 1:length(lake.data)){
    df.data <- rbind(df.data, lake.data[[i]])
  }
  return(df.data)
}


# for the 1D vars, get depths and summarize, for annual scale
summarize_var_notaro <- function(nc.file, var.name){
  library(dplyr)
  library(tidyr)
  library(lubridate)
  
  unit <- sim_var_units(nc.file, var.name)
  is.1D <- glmtools:::.is_heatmap(nc.file, var.name)
  value.name <- sprintf('%s%s', var.name, ifelse(unit=='', '',paste0(' (',unit,')')))
  if (is.1D){
    rename_depths <- function(depths){
      unlist(lapply(strsplit(depths, '[_]'), function(x) sprintf('%s_%s', round(eval(parse(text=head(tail(x,2),1))), digits = 2), tail(x,1))))
    }
    get_depth <- function(names){
      as.numeric(unname(unlist(sapply(names, function(x) strsplit(x,'[_]')[[1]][1]))))
    }
    get_stat <- function(names){
      unname(unlist(sapply(names, function(x) strsplit(x,'[_]')[[1]][2])))
    }
    var <- get_var(nc.file, var.name, reference='surface') %>% 
      mutate(base.date=as.POSIXct(paste0(lubridate::year(DateTime),'-01-01')), tz='UTC') %>% 
      mutate(doy=as.numeric(DateTime-base.date)/86400+1) %>% 
      select(-DateTime, -tz, -base.date) %>% select(doy, everything()) %>% group_by(doy) %>%  
      summarize_each(c('mean','sd')) %>% 
      setNames(c('doy',rename_depths(names(.)[-1L]))) %>% 
      as.data.frame %>% gather(key = doy) %>% 
      setNames(c('doy','depth_stat','value')) %>% 
      mutate(depth=get_depth(depth_stat), statistic=get_stat(depth_stat), variable=value.name) %>% 
      select(doy, depth, statistic, value, variable) %>% 
      filter(doy < 366)
  } else {
    var <- get_var(nc.file, var.name)%>% 
      mutate(base.date=as.POSIXct(paste0(lubridate::year(DateTime),'-01-01')), tz='UTC') %>% 
      mutate(doy=as.numeric(DateTime-base.date)/86400+1) %>% select_('doy', var.name) %>% group_by(doy) %>% 
      summarize_each(c('mean','sd')) %>% as.data.frame %>% gather(doy) %>% 
      setNames(c('doy','statistic','value')) %>% 
      mutate(depth=NA, variable=value.name) %>% 
      select(doy, depth, statistic, value, variable) %>% 
      filter(doy < 366)
  }
  
  return(var)
}

