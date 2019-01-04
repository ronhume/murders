library(dplyr)
library(dslabs)
set.seed(1986)
n <- round(2^rnorm(1000, 8, 1))

set.seed(1)
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:100),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))


schools %>% top_n(10, quality) %>% arrange(desc(quality))

set.seed(1)
scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

schools %>% top_n(10, score) %>% arrange(desc(score)) %>% select(id, size, score)

median(schools$size)

top_10 <- schools %>% top_n(10, score) %>% arrange(desc(score)) %>% select(id, size, score)
median(top_10$size)

bottom_10 <- schools %>% top_n(-10, score) %>% arrange(desc(score)) %>% select(id, size, score)
median(bottom_10$size)

schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank<=10), col = 2) 

overall <- mean(sapply(scores, mean))

#alpha <- 25
#score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
#schools %>% mutate(score_reg = score_reg) %>%
#  top_n(10, score_reg) %>% arrange(desc(score_reg))

#alphas <- seq(10,250)
#rmse <- sapply(alphas, function(alpha){
#  score_reg <- sapply(scores, function(x) overall+sum(x-overall)/(length(x)+alpha))
#  mean((score_reg - schools$quality)^2)
#})
#plot(alphas, rmse)
#alphas[which.min(rmse)]  

#alpha <- 128
#score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
#schools %>% mutate(score_reg = score_reg) %>%
#  top_n(10, score_reg) %>% arrange(desc(score_reg))


#alpha <- 128
#score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
#schools %>% mutate(score_reg = score_reg) %>%
#  top_n(10, score_reg) %>% arrange(desc(score_reg))

alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  mean((score_reg - schools$quality)^2)
})
plot(alphas, rmse)
alphas[which.min(rmse)]  


