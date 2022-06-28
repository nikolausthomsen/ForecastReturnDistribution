# functions for DataPrep.R




##### stationary transformation suggested by FRED #####

# see https://research.stlouisfed.org/econ/mccracken/fred-databases/

# 1. no transformation
# t1 <- function(x) x

# 2. diff x_t
t2 <- function(x) c(NA_real_,diff(x))

# 3. diff^2 x_t
t3 <- function(x) c(NA_real_,NA_real_,diff(diff(x)))

# 4. log x_t
t4 <- function(x) log(x)

# 5. diff log x_t
t5 <- function(x){
  tmp <- c(NA_real_,diff(log(x)))
  tmp[is.infinite(tmp)] <- NA_real_ 
  tmp
}

# 6. diff^2 log x_t
t6 <- function(x) c(NA_real_,NA_real_,diff(diff(log(x))))

# 7. diff (x_t / x_t-1 -1)
t7 <- function(x) c(NA_real_,diff(x/lag(x,1)-1))



##### other functions #####

# get the starting and ending dates of each time series
TSnaInfo <- function(dat,dateCol){
  nam_data <- names(dat)
  dat <- dat %>% arrange(.[,dateCol])
  d    <- dat %>% select(-all_of(dateCol))
  date <- dat %>% pull(all_of(dateCol)) %>% as.Date()
  df <- data.frame(
    beginTS = date[apply(d,2,function(x)min(which(!is.na(x))))],
    endTS   = date[apply(d,2,function(x)max(which(!is.na(x))))],
    numNAtotal = colSums(is.na(d))
  )
  # initialize two columns
  df <- df %>% mutate(numNAbtwTSstartend     = NA_real_,
                      numConsNAbtwTSstartend = NA_real_)
  rownames(df) <- nam <- nam_data[nam_data!=ifelse(is.numeric(dateCol),nam_data[dateCol],dateCol)]
  for(n in nam){
    NAbtwTSstartend <- ifelse(date>=df[n,"beginTS"] & date<=df[n,"endTS"],
                              is.na(d[,n]),FALSE)
    df[n,"numNAbtwTSstartend"]     <-  sum(NAbtwTSstartend)
    r <- rle(NAbtwTSstartend)
    df[n,"numConsNAbtwTSstartend"] <- max(r$lengths[r$values])
    
  }
  df <- df %>% mutate(numConsNAbtwTSstartend = if_else(numConsNAbtwTSstartend==-Inf,
                                                       0,
                                                       numConsNAbtwTSstartend))
  return(df)
}