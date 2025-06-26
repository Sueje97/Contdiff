\name{Contdif-package}
\alias{Contdiff}
\title{Continuous variable difference analysis tool}
\description{
  The Contdif package provides a complete set of tools for analyzing the differences of continuous variables among different groups. The main functions include data preprocessing and statistical analysis based on normality tests, and ultimately generate formatted statistical tables.
}
\details{
  该包主要包含两个核心函数：
  \itemize{
    \item \code{\link{preprocess_data}}: 用于数据预处理，将指定变量转换为因子或数值类型
    \item \code{\link{run_analysis}}: 执行连续性变量差异性分析，根据正态性自动选择合适的检验方法
  }

  典型的工作流程包括：
  1. 使用preprocess_data()函数准备数据
  2. 使用run_analysis()函数进行统计分析
  3. 查看或导出结果表格

  包中使用的统计方法包括：
  \itemize{
    \item 参数方法：t检验、ANOVA
    \item 非参数方法：Mann-Whitney U检验、Kruskal-Wallis检验
  }
}
\author{
  Your Name <your.email@example.com>
}
\keyword{package}
\seealso{
  \code{\link{preprocess_data}}, \code{\link{run_analysis}}
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
