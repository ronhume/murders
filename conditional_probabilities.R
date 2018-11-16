set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

# prob test is positive
mean(test)

# prob disease | test negative
mean(disease[test==0])

# prob disease | test positive
mean(disease[test==1]==1)

# relative risk
mean(disease[test==1]==1) / mean(disease==1)

# heights questions
library(dslabs)
data("heights")
heights %>% mutate(height = round(height)) %>% group_by(height) %>% summarize(p=mean(sex=="Male")) %>%
qplot(height, p, data =.)