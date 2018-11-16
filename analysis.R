library(tidyverse)
load("rda/murders.rda")

murders %>% mutate(abb=reorder(abb,rate)) %>%
  ggplot(aes(abb,rate)) +
  geom_bar(width=0.5,stat="identity",color="black") +
  coord_flip()
ggsave("figs/barplot.png")


install.packages("HistData")
library(HistData)
data("GaltonFamilies")
galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1, rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)

rss <- function(beta0, beta1, data) {
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}