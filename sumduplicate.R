sumduplicate <- function(pattern, expensetable){
       index <- grep(tolower(pattern), tolower(expensetable[,1]))
       sum <- sum(expensetable[index,2])
       new <- data.frame(pattern, sum)
       names(new) <- names(expensetable)
       rbind(expensetable[-index,],new)
}