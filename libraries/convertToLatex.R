convertToLatex <- function(df, table_name) {
  # Название таблицы как заголовок
  caption <- table_name
  
  # Начало таблицы
  latex_table <- "\\begin{table}[!htb]\n"
  latex_table <- paste0(latex_table, "\\caption{", caption, "}\n")
  latex_table <- paste0(latex_table, "\\label{", table_name, "}\n")
  latex_table <- paste0(latex_table, "\\begin{threeparttable}\n")
  latex_table <- paste0(latex_table, "\\begin{tabular}{", paste(rep("c", ncol(df)), collapse = ""), "}\n")
  latex_table <- paste0(latex_table, "% используй {p{3cm}p{7cm}} если нужно переносить с newline \n")
  latex_table <- paste0(latex_table, "\\toprule\n")
  
  # Заголовки
  latex_table <- paste0(latex_table, paste(colnames(df), collapse = "&"), "\\\\\n")
  latex_table <- paste0(latex_table, "\\midrule\n")
  
  # Данные
  for (i in 1:nrow(df)) {
    row_data <- paste(df[i, ], collapse = "&")
    latex_table <- paste0(latex_table, row_data, "\\\\\n")
  }
  
  # Конец таблицы
  latex_table <- paste0(latex_table, "\\bottomrule\n")
  latex_table <- paste0(latex_table, "\\end{tabular}\n")
  latex_table <- paste0(latex_table, "\\end{threeparttable}\n")
  latex_table <- paste0(latex_table, "\\end{table}\n")
  
  return(latex_table)
}