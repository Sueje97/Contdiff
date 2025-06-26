## An R package that enables batch differential analysis of quantitative data

```
library(Contdiff)
library(dplyr)

data(mtcars)

mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$gear <- as.factor(mtcars$gear)
mtcars$am <- as.factor(mtcars$am)

result <- run_analysis(
   data = mtcars,
   outcome_var = "mpg",
   group_vars = c("cyl", "gear", "am"))

print(result)
```

## The effect is as follows：
![1cf2f1129f41fff39e8da8f6a548c45](https://github.com/user-attachments/assets/88f4320c-f665-4d8a-aedc-8739105fb841)
