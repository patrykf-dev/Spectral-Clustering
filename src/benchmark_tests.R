source("spectral.R")

###################################################################
####  BENCHMARKING PART  ##########################################
###################################################################

Process_method <- function(testMethod, testFolder, testSubFolder = "") {
    setwd("D:/Studia/_PrzetwarzanieDanych/praca_domowa2/zbiory-benchmarkowe");
    files <- list.files();
    
    dataFiles <- files[grepl(pattern = ".data.gz", files)];
    labelFiles <- files[grepl(pattern = ".labels0.gz", files)];
    for (i in 1:length(dataFiles)){
        fileName <- substr(dataFiles[i], 1, nchar(dataFiles[i]) - 8);
        data <- read.table(paste0(dataFiles[i]));
        labels <- as.integer(read.table(paste0(labelFiles[i]))[,1]);
        
        print(paste0("PROCESSING FILE: ", fileName, " ", i, " / ", length(dataFiles)));
        
        if(testSubFolder == ""){
            results <- testMethod(data, labels);
            path <- paste0("results/", toString(testFolder), "/", fileName, ".csv");
        } else {
            results <- testMethod(data, labels, testSubFolder);
            path <- paste0("results/", toString(testFolder), "_", testSubFolder, "/", fileName, ".csv");
        }
        
        write.table(t(results), path, row.names = FALSE, col.names = FALSE);
    }
}


######### 1. GENIE
Genie_method <- function(data, labels) {
    k <- max(labels);
    genieResults <- cutree(genie::hclust2(objects = as.matrix(data), thresholdGini = 0.5), k);
    return(genieResults);
}

Process_method(Genie_method, "genie")


######### 2. HCLUST
Hclust_method <- function(data, labels, method) {
    k <- max(labels);
    hclustResults <- cutree(hclust(dist(data), method = method), k);
    return(hclustResults);
}

hclustMethods <- c("complete", "single", "average", "ward.D2", "mcquitty", "median", "centroid");
for(i in 1:length(hclustMethods)) {
    Process_method(Hclust_method, "hclust", hclustMethods[i])
}

######### 3. CUSTOM
Custom_method <- function(data, labels, M) {
    k <- max(labels);
    customResults<- Spectral_clustering(data, k, M, FALSE);
}

set.seed(100);
mValues <- c(5, 20, 100, 200, 500);
for(i in 1:length(mValues)) {
    Process_method(Custom_method, "custom", mValues[i])
}


######### 4. PROTOCLUST
Protoclust_method <- function(data, labels) {
    k <- max(labels);
    protoclustResults <- cutree(protoclust::protoclust(dist(data)), k);
    return(protoclustResults);
}

Process_method(Protoclust_method, "protoclust")

###################################################################
####  END OF BENCHMARKING PART  ###################################
###################################################################


Benchmark_kernlab_method <- function(fileName) {
    setwd(paste0("D:/Studia/_PrzetwarzanieDanych/praca_domowa2/zbiory-benchmarkowe"));
    data <- read.table(paste0(fileName, ".data.gz"));
    labels <- as.integer(read.table(paste0(fileName, ".labels0.gz"))[,1]);
    k <- max(labels)
    
    start_time <- Sys.time()
    kernlabResult <- kernlab::specc(as.matrix(data), k);
    end_time <- Sys.time()
    print(paste("SPECC", (end_time - start_time)))
    
    start_time <- Sys.time()
    unlistedVals <- unclass(kernlabResult)
    dt <- data.frame("Set name" = fileName, "Index" = "FM", "Value" = FM_index(unlistedVals, labels)[1], stringsAsFactors = FALSE);
    dt[nrow(dt) + 1,] = list(fileName, "AR", adjustedRandIndex(unlistedVals, labels));
    end_time <- Sys.time()
    print(paste("INDICES", (end_time - start_time)))
    
    return(dt)
}

benchmarkSets <- c("cross", "dense", "lsun", "spiral", "wingnut", "zigzag")
kernlabResult <- lapply(benchmarkSets, Benchmark_kernlab_method)
 

ggplot(df, aes(factor(Set.name), Value, fill = Index)) + 
    geom_bar(stat="identity", position = "dodge") + 
    scale_fill_brewer(palette = "Set1") + 
    geom_text(aes(label = round(Value, digits = 2)), position=position_dodge(width=0.9), vjust=-0.25) +
    xlab("Zbiór") + ylab("Wartość wskaźnika")



Compare_methods <- function(fileName) {
    setwd(paste0("D:/Studia/_PrzetwarzanieDanych/praca_domowa2/zbiory-benchmarkowe"));
    data <- read.table(paste0(fileName, ".data.gz"));
    labels <- as.integer(read.table(paste0(fileName, ".labels0.gz"))[,1]);
    k <- max(labels);
    
    # Process algorithms
    # TODO: add finding best k for custom algorithm
    myResult4 <- Spectral_clustering(data, k, 4, FALSE);
    myResult20 <- Spectral_clustering(data, k, 20, FALSE);
    myResult200 <- Spectral_clustering(data, k, 200, FALSE);
    
    # Find similarity indices
    dt <- data.frame("Method" = "Custom M=4", "Index" = "FM", "Value" = FM_index(myResult4, labels)[1], stringsAsFactors = FALSE);
    dt[nrow(dt) + 1,] = list("Custom M=4", "AR", adjustedRandIndex(myResult4, labels));
    dt[nrow(dt) + 1,] = list("Custom M=20", "FM", FM_index(myResult20, labels)[1]);
    dt[nrow(dt) + 1,] = list("Custom M=20", "AR", adjustedRandIndex(myResult20, labels));
    dt[nrow(dt) + 1,] = list("Custom M=200", "FM", FM_index(myResult200, labels)[1]);
    dt[nrow(dt) + 1,] = list("Custom M=200", "AR", adjustedRandIndex(myResult200, labels));
    return(dt);
}
