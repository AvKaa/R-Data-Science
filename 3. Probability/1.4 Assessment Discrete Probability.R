library(gtools)
library(tidyverse)

# Q1.
# a)
str(permutations(8,3))
# b)
str(permutations(3,3))
# c)
str(combinations(8,3))
# d)
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")

B <- 10000
set.seed(1)
all_jamaicans <- replicate(B,{
  win <- sample(runners,3, replace = F)
  (win[1] %in% "Jamaica" & win[2] %in% "Jamaica" & win[3] %in% "Jamaica")
})
mean(all_jamaicans)

# Q2.
# a)
nrow(combinations(6,1))*nrow(combinations(6,2))*nrow(combinations(2,1))
# b)
nrow(combinations(6,1))*nrow(combinations(6,2))*nrow(combinations(3,1))
# c)
nrow(combinations(6,1))*nrow(combinations(6,3))*nrow(combinations(3,1))
# d)
sides <- nrow(combinations(6,2))
drinks <- nrow(combinations(3,1))
nm <- function(x){
  x*sides*drinks
}
x <- seq(1:12)
sapply(x,nm)
plot(x,meals)
# e)
entree <- nrow(combinations(6,1))
drinks <- nrow(combinations(3,1))
nm <- function(x){
  sides <- nrow(combinations(x,2))
  entree*sides*drinks
}
x <- seq(from = 2, to = 12, by = 1)
sapply(x,nm)
plot(x,meals)

# Q3.
# a)
nrow(esoph)
# b)
esoph %>% select(ncases) %>% sum
# c)
esoph %>% select(ncontrols) %>% sum

# Q4.
# a)
signif(3)
esoph %>%
  filter(alcgp == "120+") %>%
  summarize(cancer = sum(ncases), total = sum(ncases)+sum(ncontrols), prob = cancer/total)
# b)
esoph %>%
  filter(alcgp == "0-39g/day") %>%
  summarize(cancer = sum(ncases), total = sum(ncases)+sum(ncontrols), prob = cancer/total)
# c)
teng <- esoph %>%
  filter(!tobgp == "0-9g/day") %>%
  summarize(sum(ncases))
tot <- esoph %>% select(ncases) %>% sum
teng/tot
# d)
teng <- esoph %>%
  filter(!tobgp == "0-9g/day") %>%
  summarize(sum(ncontrols))
tot <- esoph %>% select(ncontrols) %>% sum
teng/tot

# Q5.
# a)
head(esoph)
halc <- esoph %>%
  filter(alcgp == "120+") %>%
  summarize(sum(ncases))
halc
tot <- esoph %>% summarize(sum(ncases))
alcp <- halc/tot
# b)
head(esoph)
htob <- esoph %>%
  filter(tobgp == "30+") %>%
  summarize(sum(ncases))
htob
tobp <- htob/tot
# c)
hat <- esoph %>%
  filter(alcgp == "120+" & tobgp == "30+") %>%
  summarize(sum(ncases))
hat
atp <- hat/tot
atp
# d)
aot <- esoph %>%
  filter(alcgp == "120+" | tobgp == "30+") %>%
  summarize(sum(ncases))
aot
aotp <- aot/tot
aotp

# Q6.
# a)
head(esoph)
halc2 <- esoph %>%
  filter(alcgp == "120+") %>%
  summarize(sum(ncontrols))
halc2
tot2 <- esoph %>% summarize(sum(ncontrols))
alcp2 <- halc2/tot2
alcp2
# b)
alcp/alcp2
# c)
head(esoph)
htob2 <- esoph %>%
  filter(tobgp == "30+") %>%
  summarize(sum(ncontrols))
htob2
tobp2 <- htob2/tot2
tobp2
# d)
hat2 <- esoph %>%
  filter(alcgp == "120+" & tobgp == "30+") %>%
  summarize(sum(ncontrols))
hat2
atp2 <- hat2/tot2
atp2
# e)
aot2 <- esoph %>%
  filter(alcgp == "120+" | tobgp == "30+") %>%
  summarize(sum(ncontrols))
aot2
aotp2 <- aot2/tot2
aotp2
# f)
aotp/aotp2







