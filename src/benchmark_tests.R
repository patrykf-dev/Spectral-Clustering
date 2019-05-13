source("spectral.R")

Compare_hclust_methods <- function(fileName) {
    setwd(paste0("D:/Studia/_PrzetwarzanieDanych/praca_domowa2/zbiory-benchmarkowe"));
    data <- read.table(paste0(fileName, ".data.gz"));
    labels <- as.integer(read.table(paste0(fileName, ".labels0.gz"))[,1]);
    k <- max(labels);
    
    # Process algorithms
    hCompleteResult <- cutree(hclust(dist(data), method = "complete"), k);
    hSingleResult <- cutree(hclust(dist(data), method = "single"), k);
    hAverageResult <- cutree(hclust(dist(data), method = "average"), k);
    hWardResult <- cutree(hclust(dist(data), method = "ward.D2"), k);
    hMcquittyResult <- cutree(hclust(dist(data), method = "mcquitty"), k);
    hMedianResult <- cutree(hclust(dist(data), method = "median"), k);
    hCentroidResult <- cutree(hclust(dist(data), method = "centroid"), k);
    
    # Find similarity indices
    dt <- data.frame("Method" = "hclust Complete", "Index" = "FM", "Value" = FM_index(hCompleteResult, labels)[1], stringsAsFactors = FALSE);
    dt[nrow(dt) + 1,] = list("hclust Complete", "AR", adjustedRandIndex(hCompleteResult, labels));
    
    dt[nrow(dt) + 1,] = list("hclust Single", "FM", FM_index(hSingleResult, labels)[1]);
    dt[nrow(dt) + 1,] = list("hclust Single", "AR", adjustedRandIndex(hSingleResult, labels));
    
    dt[nrow(dt) + 1,] = list("hclust Average", "FM", FM_index(hAverageResult, labels)[1]);
    dt[nrow(dt) + 1,] = list("hclust Average", "AR", adjustedRandIndex(hAverageResult, labels));
    
    dt[nrow(dt) + 1,] = list("hclust ward.D2", "FM", FM_index(hWardResult, labels)[1]);
    dt[nrow(dt) + 1,] = list("hclust ward.D2", "AR", adjustedRandIndex(hWardResult, labels));
    
    dt[nrow(dt) + 1,] = list("hclust mcquitty", "FM", FM_index(hMcquittyResult, labels)[1]);
    dt[nrow(dt) + 1,] = list("hclust mcquitty", "AR", adjustedRandIndex(hMcquittyResult, labels));
    
    dt[nrow(dt) + 1,] = list("hclust median", "FM", FM_index(hMedianResult, labels)[1]);
    dt[nrow(dt) + 1,] = list("hclust median", "AR", adjustedRandIndex(hMedianResult, labels));
    
    dt[nrow(dt) + 1,] = list("hclust centroid", "FM", FM_index(hCentroidResult, labels)[1]);
    dt[nrow(dt) + 1,] = list("hclust centroid", "AR", adjustedRandIndex(hCentroidResult, labels));
    
    return(dt);
}


benchmarkSets <- c("cross", "dense", "lsun", "spiral", "wingnut", "zigzag")
hclustResult <- lapply(benchmarkSets, Compare_hclust_methods)
df <- Reduce(rbind, hclustResult)



compared <- Compare_hclust_methods("wingnut")

ggplot(compared, aes(factor(Method), Value, fill = Index)) + 
    geom_bar(stat="identity", position = "dodge") + 
    scale_fill_brewer(palette = "Set1") + 
    geom_text(aes(label = round(Value, digits = 2)), position=position_dodge(width=0.9), vjust=-0.25, angle = 90, hjust = -0.1)


###################################################################
####  BENCHMARKING PART  ##########################################
###################################################################

Benchmark_method <- function(testMethod, testFolder, testSubFolder = "") {
    setwd(paste0("D:/Studia/_PrzetwarzanieDanych/praca_domowa2/zbiory-benchmarkowe"));
    files <- list.files();
    
    dataFiles <- files[grepl(pattern = ".data.gz", files)];
    labelFiles <- files[grepl(pattern = ".labels0.gz", files)];
    for (i in 1:length(dataFiles)){
        fileName <- substr(dataFiles[i], 1, nchar(dataFiles[i]) - 8);
        data <- read.table(paste0(dataFiles[i]));
        labels <- as.integer(read.table(paste0(labelFiles[i]))[,1]);
        
        print(paste0("PROCESSING FILE: ", i, " / ", length(dataFiles), " -> ", (i * 100 / length(dataFiles)), "%"))
        
        results <- testMethod(data, labels);
        if(testSubFolder == ""){
            path <- paste0("results/", toString(testFolder), "/", fileName, ".csv");
        } else {
            path <- paste0("results/", toString(testFolder), "_", testSubFolder, "/", fileName, ".csv");
        }
        
        write.table(t(results), path, row.names = FALSE, col.names = FALSE);
    }
}

Benchmark_hclust_method <- function(data, labels, method) {
    k <- max(labels);
    hclustResults <- cutree(hclust(dist(data), method = method), k);
    return(hclustResults);
}

Benchmark_genie_method <- function(data, labels) {
    k <- max(labels);
    genieResults <- cutree(genie::hclust2(objects = as.matrix(data), thresholdGini = 0.5), k);
    return(genieResults);
}

Benchmark_custom_method <- function(data, labels, M) {
    k <- max(labels);
    customResults<- Spectral_clustering(data, k, M, FALSE);
}


# CALLS
Benchmark_method(Benchmark_genie_method, "genie")

hclustMethods <- c("complete", "single", "average", "ward.D2", "mcquitty", "median", "centroid");
for(i in 1:length(hclustMethods)) {
    Benchmark_method(Benchmark_hclust_method, "hclust", hclustMethods[i])
}


set.seed(100);
mValues <- c(5);
for(i in 1:length(mValues)) {
    Benchmark_method(Benchmark_custom_method, "custom", mValues[i])
}


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




# testing genie...
Compare_genie_method <- function(fileName) {
    setsPath <- "D:/Studia/_PrzetwarzanieDanych/praca_domowa2/zbiory-benchmarkowe";
    setwd(setsPath);
    labels <- as.integer(read.table(paste0(fileName, ".labels0.gz"))[,1]);
    
    genieResult <- read.table(paste0(setsPath, "/results/genie/", fileName, ".csv"));
    
    dt <- data.frame("Set name" = fileName, "Index" = "FM", "Value" = FM_index(genieResult, labels)[1], stringsAsFactors = FALSE);
    dt[nrow(dt) + 1,] = list(fileName, "AR", adjustedRandIndex(genieResult, labels));
    
    return(dt)
}

benchmarkSets <- c("cross", "dense", "lsun", "spiral", "wingnut", "zigzag")
genieResults <- lapply(benchmarkSets, Compare_genie_method)
