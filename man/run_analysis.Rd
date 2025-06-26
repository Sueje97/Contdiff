\name{run_analysis}
\alias{run_analysis}
\title{Perform a difference analysis of continuous variables}
\description{
  Based on the results of the normality test, parametric or non-parametric tests are conducted on the continuous variables
}
\usage{
  run_analysis(data, outcome_var, group_vars)
}
\arguments{
  \item{data}{Data frame}
  \item{outcome_var}{Outcome variable (continuity variable)}
  \item{group_vars}{Group variable name vector (categorical variable)}
}
\value{
  包含统计结果的表格
}
\details{
  该函数首先对结局变量进行正态性检验(Shapiro-Wilk检验)，
  然后根据检验结果选择参数方法(t检验/ANOVA)或非参数方法(Mann-Whitney U检验/Kruskal-Wallis检验)
  进行差异性分析。最后生成一个格式化的表格，包含描述性统计和检验结果。
}
\examples{
  # 加载包
  library(Contdiff)
  library(dplyr)

  # 使用内置的mtcars数据集测试函数
  data(mtcars)

  # 预处理数据
  mtcars$cyl <- as.factor(mtcars$cyl)
  mtcars$gear <- as.factor(mtcars$gear)
  mtcars$am <- as.factor(mtcars$am)

  # 执行分析
  result <- run_analysis(
    data = mtcars,
    outcome_var = "mpg",
    group_vars = c("cyl", "gear", "am"))

  # 查看结果
  print(result)
}
