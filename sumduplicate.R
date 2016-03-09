sumduplicate <- function(pattern, expensetable){
       index <- grep(tolower(pattern), tolower(expensetable[,1]))
       sum <- apply(expensetable[index,c(names(expensetable)[2:4])],2,sum)
       #sum <- sum(expensetable[index,c(names(expensetable)[2:4])])
       #sum3 <- sum(expensetable[index,3])
       #sum4 <- sum(expensetable[index,4])
       new <- data.frame(pattern, t(sum))
       names(new) <- names(expensetable)
       rbind(expensetable[-index,],new)
}