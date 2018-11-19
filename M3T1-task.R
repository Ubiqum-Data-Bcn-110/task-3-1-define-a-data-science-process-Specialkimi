df<- iris
#install.packages("dplyr")
library(dplyr)
df %>%
  select(Species)
df %>%
  filter(Species=="versicolor")
df %>%
  group_by(Species) %>%
summarize(Sepal.Length=sum(Sepal.Length))

df %>%
  group_by(date) %>%
  summarise

df %>%
  group_by(Species) %>%
  summarize(xpro=sum(Sepal.Length))



