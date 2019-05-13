source("spectral.R")

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