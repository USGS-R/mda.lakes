#'@title
#'fits LTER data to NLDAS, uses NLDAS to gap-fill
#'@description see title
#'@export
LTER_NLDAS_gapper <- function(file_out = '../inst/extdata/LTER_met_gapped.csv'){
  
  LTER <- read.csv('../inst/extdata/NTL_LTER_met.csv')
  
  # Sparkling Lake data used as the NLDAS comparison here
  NLDAS <- read.csv(get_driver_path('WBIC_1881900.csv'))
  lt_dt <- as.Date(LTER[, 1])
  nl_dt <- as.Date(NLDAS[, 1])
  # subset the times to only include overlapping
  st_dt <- max(c(min(lt_dt),min(nl_dt)))
  en_dt <- min(c(max(lt_dt),max(nl_dt)))
  fin_dates <- seq(st_dt,en_dt, by = 'days')
  
  val_out <- val_out <- data.frame(matrix(ncol = 7, nrow = length(fin_dates)))
  val_out[,1] <- fin_dates
  names(val_out) <- names(LTER)[1:7]
  for (col in 2:7){
    lt_dt <- as.Date(LTER[!is.na(LTER[,col]),1])
    nl_dt <- as.Date(NLDAS[, 1])
    
    # Assumption: NLDAS is complete, LTER is a subset.
    lt <- LTER[!is.na(LTER[,col]),col]
    
    lt <- lt[lt_dt >= st_dt & lt_dt <= en_dt]
    lt_dt <- lt_dt[lt_dt >= st_dt & lt_dt <= en_dt]
    
    nl <- NLDAS[nl_dt >= st_dt & nl_dt <= en_dt, col]
    nl_dt <- nl_dt[nl_dt >= st_dt & nl_dt <= en_dt]
    nl_u_i <- nl_dt %in% lt_dt
  
    
    lt_u_i <- lt_dt %in% nl_dt[nl_u_i]
    lt_dt = lt_dt[lt_u_i]
    lt <- lt[lt_u_i]
    if(!all(lt_dt== nl_dt[nl_u_i])){
      stop('data failed to match assumptions')
    }
    
    
    lm <- lm(lt~nl[nl_u_i])
    m <- lm$coefficients[[2]]
    b <- lm$coefficients[[1]]
    for (j in 1:length(fin_dates)){
      u_i <-  lt_dt == fin_dates[j]
      if (any(u_i)){
        val_in <- mean(lt[u_i])
        val_out[j, col] <- val_in
      } else {
        u_i <-  nl_dt == fin_dates[j]
        val_in <- mean(nl[u_i]*m+b)
        if (is.na(val_in)){
          u_i <- nl_dt %in% fin_dates[c(j-1, j+1)] # to catch 2012-12-31 missing from NLDAS
          val_in <- approx(x= fin_dates[c(j-1, j+1)], y = nl[u_i]*m+b, fin_dates[j])$y
        }
        val_out[j,col] <- val_in
      }
      
    }
    val_out
  }
  
  val_out[, 7] <- val_out[, 7]*0.001 # mm/day to m/day
  write.table(x = val_out, file = file_out,quote = F, sep = ',', row.names = F, col.names = T)
}