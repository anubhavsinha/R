#corr.R
corr <- function(directory, threshold=0)
{
        correlation <- vector(mode="numeric", length=0)
       for(i in 1:332){
               filename = normalized_filename(i)
               table <- read.csv(paste(directory, '/', filename, sep = ''))
               good <- complete.cases(table)
               good_table <- table[good,]
               rows<- nrow(good_table)
               #print(rows)
               
               if (rows>threshold)
                {
                       
                       x = good_table$sulfate
                       y = good_table$nitrate
                       correlation<-c(correlation, cor(x,y))
                       
               }   
               
       }
        correlation
        
}