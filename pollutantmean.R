# pollutantmean
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

pollutantmean <- function(directory, pollutant, id=1:332)
{       
        items <- c()
        
        for (i in id)
        {
                filename = normalized_filename(i)
                frame = read.csv(paste(directory,'/',filename, sep = ''))
                if (pollutant=='sulfate'){
                        sulfate = frame$sulfate
                        missing = is.na(sulfate)
                        items <- c(items, sulfate[!missing])
                }
                if (pollutant=='nitrate'){
                        nitrate = frame$nitrate
                        missing = is.na(nitrate)
                        items <- c(items, nitrate[!missing])
                }
                
        }

actual_mean <- mean(items)
actual_mean
}
        
