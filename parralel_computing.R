library(foreach)
library(doParallel)
library(ggplot2)

#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(cores-1) #not to overload your computer
registerDoParallel(cl)

#set up calculations and run in foreach loop
n = 1000

big_list <- foreach(i=1:n) %dopar% {
  big_list[i] = i^2
}

print(big_list)

#stop cluster
stopCluster(cl)


#Now I am going to put this in a function and run some different variations
par_core_tester <- function(n_cores, n_vec) {
  
  time1 <- Sys.time()
  
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  big_list <- list()
  
  big_list <- foreach(i=1:n_vec) %dopar% {
    big_list[i] = i^2
  } 
  
  stopCluster(cl)
  
  run_time <- difftime(Sys.time(), time1, units = ,'secs')
  
  times= as.data.frame(cbind(n_vec,n_cores,run_time))
  
  return(times)
}

results = data.frame()

time_a <- Sys.time()

for (calcs in seq(10000,1000000,10000)) {
  for (cores in c(1,3,5,7)) {
    temp <- par_core_tester(cores,calcs)
    results = rbind(results, temp)
    print('-----------------------------------------------------------------')
    print('-----------------------------------------------------------------')
    print(paste0(100-(calcs)/10000,' calcs remaining. Last completed core: ',cores,'.'))
  }
}

total_time <- Sys.time() - time_a
print(paste0('Total time for loop: ',round(total_time,3)))


results$run_time[results$n_vec<90000] = results$run_time[results$n_vec<90000]/60
results$run_time[results$n_vec<125000 & results$run_time>10] <- results$run_time[results$n_vec<125000 & results$run_time>10]/60


write.csv(results,'parallel_comp_results.csv')


ggplot(results[results$n_vec <= 50000,], 
       aes(x = n_vec, 
           y = run_time, 
           group = n_cores, 
           color = as.factor(n_cores))) + 
  geom_line() +
  labs(x = "Number of Calculations", y = "Run Time", color = "Cores") +
  theme_light()

ggplot(results, aes(x = n_vec, 
                    y = run_time, 
                    group = n_cores, 
                    color = as.factor(n_cores))) + 
 