
Compare_methods <- function(fileName) {
    k <- 3;
    setwd(paste0("D:/Studia/_PrzetwarzanieDanych/praca_domowa2/zbiory-benchmarkowe"));
    data <- read.table(paste0(fileName, ".data.gz"));
    labels <- as.integer(read.table(paste0(fileName, ".labels0.gz"))[,1]);
    
    # Process algorithms
    # TODO: add finding best k for custom algorithm
    myResult4 <- Spectral_clustering(data, k, 4, FALSE);
    myResult20 <- Spectral_clustering(data, k, 20, FALSE);
    myResult200 <- Spectral_clustering(data, k, 200, FALSE);
    hCompleteResult <- cutree(hclust(dist(data), method = "complete"), k);
    hSingleResult <- cutree(hclust(dist(data), method = "single"), k);
    hAverageResult <- cutree(hclust(dist(data), method = "average"), k);
    
    # Find similarity indices
    dt <- data.frame("Method" = "Custom M=4", "Index" = "FM", "Value" = FM_index(myResult4, labels)[1], stringsAsFactors = FALSE);
    dt[nrow(dt) + 1,] = list("Custom M=4", "AR", adjustedRandIndex(myResult4, labels));
    
    dt[nrow(dt) + 1,] = list("Custom M=20", "FM", FM_index(myResult20, labels)[1]);
    dt[nrow(dt) + 1,] = list("Custom M=20", "AR", adjustedRandIndex(myResult20, labels));
    
    dt[nrow(dt) + 1,] = list("Custom M=200", "FM", FM_index(myResult200, labels)[1]);
    dt[nrow(dt) + 1,] = list("Custom M=200", "AR", adjustedRandIndex(myResult200, labels));
    
    dt[nrow(dt) + 1,] = list("hclust Complete", "FM", FM_index(hCompleteResult, labels)[1]);
    dt[nrow(dt) + 1,] = list("hclust Complete", "AR", adjustedRandIndex(hCompleteResult, labels));
    
    dt[nrow(dt) + 1,] = list("hclust Single", "FM", FM_index(hSingleResult, labels)[1]);
    dt[nrow(dt) + 1,] = list("hclust Single", "AR", adjustedRandIndex(hSingleResult, labels));
    
    dt[nrow(dt) + 1,] = list("hclust Average", "FM", FM_index(hAverageResult, labels)[1]);
    dt[nrow(dt) + 1,] = list("hclust Average", "AR", adjustedRandIndex(hAverageResult, labels));
    
    return(dt);
}

compared <- Compare_methods("wingnut")

ggplot(compared, aes(factor(Method), Value, fill = Index)) + 
    geom_bar(stat="identity", position = "dodge") + 
    scale_fill_brewer(palette = "Set1")



# CHECKING SETS DIMENSIONS
Test_set_dimension <- function(name) {
    data <- as.matrix(read.table(paste0(name, ".data.gz")));
    labels <- as.matrix(read.table(paste0(name, ".labels0.gz"))[,1]);
    print(paste("Data ", name, " is ", ncol(data), "x", nrow(data), " and labels is ", ncol(labels), "x", nrow(labels)))
}
names <- c("atom", "chainlink", "engytime", "lsun", "twodiamonds", "wingnut")
asd <- lapply(names, Test_set_dimension)
