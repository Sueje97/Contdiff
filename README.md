# An R package that enables batch differential analysis of quantitative data

## Example
```
# install.packages("devtools")

# install Contdiff
library(devtools)
install_github("Sueje97/Contdiff")

library(Contdiff)
library(broom)

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
![PixPin_2025-06-26_16-48-40](https://github.com/user-attachments/assets/6b7c0ee9-633a-420a-8984-72eaea340f4f)

