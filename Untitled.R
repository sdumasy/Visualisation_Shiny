library(tidyr)

    birth_data <- read.csv(skip = 3, "Data/birth.csv")
    death_data <- read.csv(skip = 3, "Data/death.csv")
    birth_data <- subset(birth_data, select = -c(X, X2017, X2016))
    death_data <- subset(death_data, select = -c(X, X2017, X2016))   
    
    # colnames(birth_data)[which(grepl(names(birth_data), "X"))] <- "newColumnName"
    # colnames(birth_data)[grepl('X',colnames(birth_data))] <- 'variable'
    names(birth_data) <- gsub(x = names(birth_data), pattern = "X", replacement = "")  
    names(death_data) <- gsub(x = names(death_data), pattern = "X", replacement = "")  
    
    # lol = birth_data - death_data
    lol = cbind(birth_data[,c(0:2)], birth_data[,c(5:ncol(birth_data))] - death_data[,c(5:ncol(death_data))])
    
    data <- birth_data[1:2 , c(5: length(birth_data))]
    
    data3 <- birth_data[birth_data[, "Country.Code"] == "ABW" , c(5: length(birth_data))]
    data4 <- birth_data[birth_data[, "Country.Code"] == "ATG" , c(5: length(birth_data))]
    
    
    
     data_long3 <- gather(data3, year, value, '1960':'2015')
      data_long4 <- gather(data4, year, value, '1960':'2015')
      
      lalal <- merge(data_long3, data_long4, by = "year", sort = FALSE, all.x = TRUE)
      
     veccie <- c("ABW", "ATG", "ALB", "BFA", "BEN")
    datalol <- birth_data[birth_data[, "Country.Code"] %in% veccie , c(5: length(birth_data))]
    datalol_long <- gather(datalol[1,], year, value1, '1960':'2015')
    for(i in 2:length(veccie)) {
      print(i)
      data_new <- gather(datalol[i,], year, value, "1960":"2015")
      colnames(data_new) <- c("year", paste("value", i, sep = ""))
      datalol_long <<- merge(datalol_long, data_new, by = "year", sort = FALSE, all.x = TRUE)
    }
    print(datalol_long)
      
    plot(data)
    
    selected_countries = c("BEL", "ARG")
    
    selected <- filter(birth_data, !Country.Code %in% selected_countries)
    