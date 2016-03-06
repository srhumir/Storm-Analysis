relable <- function(oldfactor, convert){
        newvalue <- as.character(oldfactor)
        for (i in unique(newvalue)){
                #print(i)
                #print(length(newvalue[newvalue == i]))
                #print(length(convert$value[convert$exp == i]))
                newvalue[newvalue == i] <- convert$value[convert$exp == i]
        }
        as.numeric(newvalue)
}