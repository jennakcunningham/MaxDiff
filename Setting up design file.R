library(tidyverse)
library(crossdes)

n_tasks <- 3
n_alt <- 5

rm(list=ls(pattern="var[[:digit:]]"))

var1 <- c("1", 
          "2", 
          "3", 
          "4", 
          "5", 
          "6", 
          "7", 
          "8")
end <- length(var1)

for (i in 1:n_alt-1) {
  new_var <- paste0("var",i + 1)
  start <- i + 1
  assign(new_var, var1[start:end])
}

vars <- grep("var[[:digit:]]",names(.GlobalEnv), value = TRUE)
grid <- expand.grid(mget(vars))

for (i in 1:n_alt) {
  filter <- var1[i]
  grid[[paste0("n",i)]] <- rowSums(select(grid, starts_with("var")) == filter)
}

for (i in 1:n_alt) {
  grid <- grid %>% filter(grid[[paste0("n",i)]] <= 1)
}

grid <- select(grid, starts_with("var"))

# find bibd

attr <- 8 #Number of total attributes
sets <- 5 #Number of questions, or experimental "blocks"
alts <- 4 #Number of attributes per question (suggest 3 - 5)

bibd <- find.BIB(attr, sets, alts)


ibd <- ibd(v = v, b = b, k = k) #Create design

isGYD(ibd$design) #Test if design is BIBD






grid <- grid %>% mutate(id = row_number())

set.seed(123)

grid_random <- grid[sample(1:nrow(grid)), ] 

grid_random <- grid_random %>% mutate(id_ran = row_number())

grid <- grid_random %>% arrange(id_ran)

n_cell <- nrow(grid)/(n_tasks*n_alt)

grid <- grid %>% mutate(cell = rep(1:n_cell, length.out = nrow(grid))) %>% arrange(cell)
grid <- grid %>% mutate(screen = rep(1:n_tasks, each = n_alt, length.out = nrow(grid))) %>% arrange(cell, screen)
grid <- grid %>% mutate(alt = rep(1:n_alt, length.out = nrow(grid)))

grid <- grid %>% mutate(key = paste0(cell, "_", screen, "_", alt))

cols <- grid[,grepl("var", colnames(grid))]
col_names <- colnames(cols)

grid$order <- grid$var1
for (i in 2:length(col_names)) {
  name <- paste0("var",i)
  grid <- grid %>% mutate(use = grid[[name]])
  grid$order <- paste0(grid$order,", ",grid$use)
}

write.csv(grid, "grid_test.csv")
