library(data.table)
library(tidyverse)
library(parallel)

f <- function(s){
  d <- d %>% filter(siten1 == s)
  as_list <- as_list %>% filter(siten1 == s)
  
  ans <- ashas(d,as_list);
  ans <- ans %>% t()
  colnames(ans) <- c("s2","tr_2","tr_5","customer","kansen")
  ans <- ans %>% as_tibble()
  
  ans
}

cl <- makeCluster(2)

clusterEvalQ(cl, {
  library(tidyverse)
  library(data.table)
  library(dken)
  d <- read_csv("data3.csv")
  as_list <- d %>% filter(SH_ON_OFF_COND_C ==2,SH_KIND_C==1,SH_HDNSEN_1_I != SH_HDNSEN_2_I,
                          SH_KACHI_KBN_C ==1)
  d <- d %>% filter(SH_ON_OFF_COND_C != 2 | is.na(SH_ON_OFF_COND_C)) %>% 
    select(-SH_HDNSEN_1_I,-SH_HDNSEN_2_I,-SH_JIKO_EIGYOSHO_C,-SH_JIKO_DENCHU_NO_C,-SH_JIKO_DANI_C,
           -SH_AITE_EIGYOSHO_C,-SH_AITE_DENCHU_NO_C,-SH_AITE_DANI_C,-SH_KIKI_NO_C,-SH_SHIYO_KBN_C,-SH_KIND_C,-SH_ON_OFF_COND_C)
  d <- d %>% mutate(ms2 = 0)
  })

microbenchmark::microbenchmark(
  output <- parLapply(cl,c(1:6),f),times=1
)

stopCluster(cl)