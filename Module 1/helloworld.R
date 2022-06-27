text <- "hello World"
rand <- runif(1000000)
text <- mean(rand)
rand <- rnorm(1000000,0,1)
rand10 <- rnorm(1000000,0,10)
max.rand <- max(rand)
max.rand10 <- max(rand10)
text <- abs(max.rand - max.rand10)
print(text)