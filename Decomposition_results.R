##########----------------------------------------------------------------------------------------------------##########
#         Decomposition data. May take several hours 
##########----------------------------------------------------------------------------------------------------##########

source("svyMVdecomp_nocluster.R")

Groupings <- c("Low_SES","Parent_edu","Location_RR","Indigenous")
Cohorts <- c(2003,2006,2009)

if (file.exists("MV_decomp_22.RData")) { 
  cat("Results for decomp age 22 were stored in a previous run. Loading results from RData file ... \n")
  load(file="MV_decomp_22.RData") } else {
    ptm <- proc.time()
    start_time <- Sys.time()
    no_cores <- max(1,detectCores() - 2) # use all cores except 2
    cat("Setting up clusters for parallel computation. Process will use",paste(no_cores),"CPU cores ... \n")
    cl <- makeCluster(no_cores)
    clusterExport(cl, list("svyMVdecomp","Groupings","Cohorts",
                           "Inference_model_2003","Inference_model_2006","Inference_model_2009"))
    cat("Running mean value decompositions (2003, 2006 & 2009 cohorts). This may take an hour or longer ... \n") 
    MV_decomp_22 <- lapply(Cohorts, function(z) {  
      parLapply(cl,Groupings, function(x) {
        svyMVdecomp(get(paste0("Inference_model_",z)), x) })})
    stopCluster(cl)
    gc()
    cat("Complete. Now saving results to RData file. \n")
    end_time <- Sys.time()
    cat("Total CPU time taken: ")
    cat(seconds_to_period((proc.time() - ptm)["elapsed"]) ) 
    cat("\n")
    cat("Total system time taken: ")
    print(end_time - start_time)
    
    save(MV_decomp_22,file="MV_decomp_22.RData") 
  } 

for (z in 1:length(Cohorts)) {
  for (x in 1:length(Groupings)) {
    assign(paste0("MV_",Cohorts[z],"_22_",Groupings[x]),
           MV_decomp_22[[z]][[x]]) }}
  

### Go8

Go8_models <- c("2009_Go8","2009_non_Go8")


if (file.exists("MV_decomp_Go8.RData")) { 
  cat("Results for decomp Go8 universities were stored in a previous run. Loading results from RData file ... \n")
  load(file="MV_decomp_Go8.RData") } else {
    ptm <- proc.time()
    start_time <- Sys.time()
    no_cores <- max(1,detectCores() - 2) # use all cores except 2
    cat("Setting up clusters for parallel computation. Process will use",paste(no_cores),"CPU cores ... \n")
    cl <- makeCluster(no_cores)
    clusterExport(cl, list("svyMVdecomp","Groupings","Go8_models","Inference_model_2009_Go8","Inference_model_2009_non_Go8"))
    cat("Running mean value decompositions (2009 cohort: Go8 vs non-Go8 universities). This may take 45 minutes or longer ... \n")
    MV_decomp_Go8 <- lapply(Go8_models, function(z) {  
      parLapply(cl,Groupings, function(x) {
        svyMVdecomp(get(paste0("Inference_model_",z)), x) })})
    stopCluster(cl)
    gc()
    cat("Complete. Now saving results to RData file. \n")
    end_time <- Sys.time()
    cat("Total CPU time taken: ")
    cat(seconds_to_period((proc.time() - ptm)["elapsed"]) ) 
    cat("\n")
    cat("Total system time taken: ")
    print(end_time - start_time)
    
    save(MV_decomp_Go8,file="MV_decomp_Go8.RData") 
  } 


for (z in 1:length(Go8_models)) {
  for (x in 1:length(Groupings)) {
    assign(paste0("MV_",Go8_models[z],"_",Groupings[x]),
           MV_decomp_Go8[[z]][[x]]) }}



rm(MV_decomp_22,MV_decomp_Go8)


MV_data_2003 <- Decomp_table(Cohort="2003",Model=Inference_model_2003,Groupings=Groupings,Age="22")
MV_data_2006 <- Decomp_table(Cohort="2006",Model=Inference_model_2006,Groupings=Groupings,Age="22")
MV_data_2009 <- Decomp_table(Cohort="2009",Model=Inference_model_2009,Groupings=Groupings,Age="22")

