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









##################################################



setsPath <- "D:/Studia/_PrzetwarzanieDanych/praca_domowa2/zbiory-benchmarkowe";

Compare_method <- function(fileName, methodName) {
    setwd(setsPath);
    labels <- as.integer(read.table(paste0(fileName, ".labels0.gz"))[,1]);
    
    genieResult <- read.table(paste0(setsPath, "/results/", methodName, "/", fileName, ".csv"));
    
    dt <- data.frame("Set name" = fileName, "Index" = "FM", "Value" = FM_index(genieResult, labels)[1], stringsAsFactors = FALSE);
    dt[nrow(dt) + 1,] = list(fileName, "AR", abs(adjustedRandIndex(genieResult, labels)));
    
    return(dt)
}

fileNames <- list.files()[grepl(pattern = ".data.gz", list.files())];
for(i in 1:length(fileNames)) {
    fileNames[i] <- substr(fileNames[i], 1, nchar(fileNames[i]) - 8);
}


customResults <- lapply(fileNames, Compare_method, methodName="custom_20");
df <- Reduce(rbind, customResults);

