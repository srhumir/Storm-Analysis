relable <- function(oldfactor, convert){
        newvalue <- as.character(oldfactor)
        for (i in unique(newvalue)){
                newvalue[newvalue == i] <- convert$value[convert$exp == i]
        }
        as.numeric(newvalue)
}