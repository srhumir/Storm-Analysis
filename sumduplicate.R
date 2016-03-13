sumduplicate <- function(pattern, expensetable){
       index <- grep(tolower(pattern), tolower(expensetable[,1]))
       sum <- apply(expensetable[index,c(names(expensetable)[2:4])],2,sum)
       new <- data.frame(pattern, t(sum))
       names(new) <- names(expensetable)
       rbind(expensetable[-index,],new)
}