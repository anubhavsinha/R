#complete.R
normalized_filename = function(index){
        if(nchar(index)==1){
                filename = paste('00', as.character(index), sep = '')
                #print(filename)
        }
        else if (nchar(index)==2)
        {
                filename = paste('0', as.character(index), sep = '') 
                #print(filename)
        }
        else{
                filename = as.character(index)
        }
        
        normalized = paste(filename,'.csv', sep = '')
        normalized
}


complete <- function(directory, ID=1:332)
{
        d <- NULL
        for (index in ID){
                filename <- normalized_filename(index)
                table <- read.csv(paste(directory, '/', filename, sep = ''))
                good <- complete.cases(table)
                good_table <- table[good,]
                good_rows <- nrow(good_table)
                if(index==ID[1]){
                        d <- data.frame(id=index, nobs=good_rows)     
                }
                else{
                        d <- rbind(d, c(index, good_rows))
                }
                
                
                
        }
        d
}