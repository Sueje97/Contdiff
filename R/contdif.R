#' Data preprocessing function
#'
#' 将指定变量转换为因子或数值类型
#'
#' @param data 数据框
#' @param factor_vars 要转换为因子的变量名向量
#' @param numeric_vars 要转换为数值的变量名向量
#' @return 处理后的数据框
#' @export
preprocess_data <- function(data, factor_vars, numeric_vars) {
  # 转换变量类型
  data <- data %>%
    mutate(across(all_of(factor_vars), as.factor)) %>%
    mutate(across(all_of(numeric_vars), as.numeric))

  return(data)
}

#' 执行连续性变量差异性分析
#'
#' 根据正态性检验结果，对连续性变量进行参数或非参数检验
#'
#' @param data 数据框
#' @param outcome_var 结局变量名(连续性变量)
#' @param group_vars 分组变量名向量(分类变量)
#' @return 包含统计结果的表格
#' @export
#' @import dplyr
#' @import tidyr
#' @import broom
run_analysis <- function(data, outcome_var, group_vars) {
  # 检查变量是否存在
  if(!outcome_var %in% names(data)) {
    stop(paste("结局变量", outcome_var, "不存在于数据中"))
  }

  if(!all(group_vars %in% names(data))) {
    missing_vars <- group_vars[!group_vars %in% names(data)]
    stop(paste("以下分组变量不存在于数据中:", paste(missing_vars, collapse = ", ")))
  }

  # 执行正态性检验
  shapiro_test <- shapiro.test(data[[outcome_var]])
  cat(paste0(outcome_var, "的正态性检验结果:\n"))
  cat("Shapiro-Wilk检验: W =", round(shapiro_test$statistic, 4),
      "p-value =", ifelse(shapiro_test$p.value < 0.001, "<0.001", round(shapiro_test$p.value, 4)), "\n")

  # 根据正态性检验结果确定是否使用参数方法
  use_parametric <- shapiro_test$p.value > 0.05

  if (use_parametric) {
    cat("\n数据满足正态分布假设，使用参数检验方法（t检验和ANOVA）\n\n")
  } else {
    cat("\n数据不满足正态分布假设，使用非参数检验方法（Mann-Whitney U检验和Kruskal-Wallis检验）\n\n")
  }

  # 初始化结果列表
  results_list <- list()

  # 对每个分类变量进行差异性分析
  for (var in group_vars) {
    # 获取变量的类别数
    n_levels <- nlevels(data[[var]])

    if (use_parametric) {
      # 参数方法（正态分布）
      if (n_levels == 2) {
        # 双样本t检验
        test_result <- t.test(as.formula(paste(outcome_var, "~", var)), data = data)
        test_df <- tidy(test_result)
        test_df$method <- "t-test"
        test_df$statistic_name <- "t"
      } else {
        # ANOVA
        anova_model <- aov(as.formula(paste(outcome_var, "~", var)), data = data)
        test_result <- tidy(anova_model)
        test_df <- test_result %>% filter(term == var)
        test_df$method <- "ANOVA"
        test_df$statistic_name <- "F"
      }
    } else {
      # 非参数方法（非正态分布）
      if (n_levels == 2) {
        # Mann-Whitney U检验（秩和检验）
        test_result <- wilcox.test(as.formula(paste(outcome_var, "~", var)), data = data, exact = FALSE)
        test_df <- tidy(test_result)
        test_df$method <- "Mann-Whitney U test"
        test_df$statistic_name <- "W"
      } else {
        # Kruskal-Wallis检验
        test_result <- kruskal.test(as.formula(paste(outcome_var, "~", var)), data = data)
        test_df <- tidy(test_result)
        test_df$method <- "Kruskal-Wallis test"
        test_df$statistic_name <- "K-W"
      }
    }

    # 添加变量名和其他信息
    test_df$variable <- var

    # 添加到结果列表
    results_list[[var]] <- test_df %>%
      select(variable, method, statistic_name, statistic, p.value)
  }

  # 合并所有结果
  test_results <- bind_rows(results_list)

  # 为每个分类变量添加描述性统计
  desc_stats <- list()

  for (var in group_vars) {
    total_n <- nrow(data)
    if (use_parametric) {
      # 参数方法描述性统计（正态分布）
      stats <- data %>%
        group_by(across(all_of(var))) %>%
        summarise(
          n = n(), mean = mean(.data[[outcome_var]], na.rm = TRUE), sd = sd(.data[[outcome_var]], na.rm = TRUE), .groups = "drop"
        ) %>%
        mutate(
          outcome = paste0(round(mean, 2), " ± ", round(sd, 2)), variable = var, category = as.character(.[[var]]),
          percentage = paste0("(", round(n / total_n * 100, 1), "%)")
        ) %>%
        select(variable, category, n, percentage, outcome)
    } else {
      # 非参数方法描述性统计（非正态分布）
      stats <- data %>%
        group_by(across(all_of(var))) %>%
        summarise(
          n = n(), median = median(.data[[outcome_var]], na.rm = TRUE),
          q1 = quantile(.data[[outcome_var]], 0.25, na.rm = TRUE),
          q3 = quantile(.data[[outcome_var]], 0.75, na.rm = TRUE), .groups = "drop"
        ) %>%
        mutate(
          outcome = paste0(round(median, 2), " (", round(q1, 2), ", ", round(q3, 2), ")"),
          variable = var, category = as.character(.[[var]]),
          percentage = paste0("(", round(n / total_n * 100, 1), "%)")
        ) %>%
        select(variable, category, n, percentage, outcome)
    }
    desc_stats[[var]] <- stats
  }

  # 合并所有描述性统计
  desc_results <- bind_rows(desc_stats)

  # 创建最终表格
  final_table <- desc_results %>%
    left_join(test_results, by = "variable") %>%
    group_by(variable) %>%
    mutate(
      Variables = ifelse(row_number() == 1, variable, ""),
      n = paste0(n, percentage),
      Statistics = ifelse(row_number() == 1, round(statistic, 3), ""),
      P = ifelse(row_number() == 1,
                 ifelse(p.value < 0.001, "<0.001", as.character(round(p.value, 3))),
                 "")
    ) %>%
    ungroup() %>%
    select(Variables, Category = category, n,
           # 使用 outcome_var 作为列名
           !!outcome_var := outcome,
           Statistics, P)

  # 在表格下方添加注释行说明结局变量数据类型
  # 使用正确的方法创建带有动态列名的数据框
  note_row <- data.frame(
    Variables = "Note:",
    Category = ifelse(use_parametric, paste0(outcome_var, "：Mean±SD"), paste0(outcome_var, "：M(p25,p75)")),
    n = "",
    stringsAsFactors = FALSE
  )

  # 添加结局变量列
  note_row[[outcome_var]] <- ""

  # 添加剩余列
  note_row$Statistics <- ""
  note_row$P <- ""

  # 重新排列列顺序以匹配final_table
  note_row <- note_row[, names(final_table)]

  # 将说明行添加到表格底部
  final_table <- rbind(final_table, note_row)

  return(final_table)
}
