\name{preprocess_data}
\alias{preprocess_data}
\title{Data preprocessing function}
\description{
  Convert the specified variable to factor or numeric types to prepare for statistical analysis
}
\usage{
  preprocess_data(data, factor_vars, numeric_vars)
}
\arguments{
  \item{data}{待处理的数据框}
  \item{factor_vars}{要转换为因子类型的变量名向量}
  \item{numeric_vars}{要转换为数值类型的变量名向量}
}
\value{
  处理后的数据框，其中指定变量已转换为目标类型
}
\details{
  该函数使用dplyr的across()函数批量转换变量类型，支持以下转换：
  \itemize{
    \item 因子变量：适合分类变量，如性别、组别等
    \item 数值变量：适合连续性测量变量，如年龄、得分等
  }

  所有变量转换均原地进行，不会创建新变量
}
\examples{
  # 创建示例数据
  sample_data <- data.frame(
    id = 1:100,
    gender = sample(c("Male", "Female"), 100, replace = TRUE),
    age = sample(20:60, 100, replace = TRUE),
    group = sample(c("A", "B", "C"), 100, replace = TRUE),
    score = runif(100, 0, 100)
  )

  # 预处理数据
  processed_data <- preprocess_data(
    data = sample_data,
    factor_vars = c("gender", "group"),
    numeric_vars = c("age", "score")
  )

  # 查看变量类型
  str(processed_data)
}
\seealso{
  \code{\link{run_analysis}} 用于后续统计分析的函数
}
\keyword{utilities}
\author{Your Name <your.email@example.com>}
