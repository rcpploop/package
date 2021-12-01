 library(tidyverse)
 library(parallel)
 # install.packages("dken_1.4.zip", repos = NULL, type="win.binary")
 mat <- read_csv("teiden.csv")
     f <- function(feder){
         d <- mat %>% filter(fed1 == feder | fed2 == feder)
         d <- d %>% select(-1,-2)
         cpp_s2has2(d,feder)
       }
    
       fed_list <- unique(mat$fed1)
       fed_list <- fed_list[fed_list > 0]
      
         cl <- makeCluster(8)
        
           clusterEvalQ(cl, {
               library(tidyverse)
               library(dken)
               mat <- read_csv("teiden.csv")
               fed_list <- unique(mat$fed1)
             })

        
           microbenchmark::microbenchmark(
               output <- parLapply(cl,fed_list,f),times=1
             )
           
           stopCluster(cl)

           
           
           
to_df <- function(x){
  i <- x$id %>% as_tibble()
  d <- x$df %>% t() %>% as_tibble()
  
  cbind(i,d)
}

output2 <- output %>% map(to_df)
output2 <- reduce(output2,bind_rows)
# write.csv(output3,"origin3.csv",row.names = FALSE)
# origin3 <- read_csv("origin3.csv")
# origin3 <- origin3 %>% arrange(value)
# 
# origin <- read_csv("origin.csv")
# colnames(origin) <- c("value","V11","V21","V31","V41")
# 
# identical(origin,origin3)
# 
# o <- origin3 %>% left_join(origin,by="value")
