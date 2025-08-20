getFactorTables <- function(ds) {
  # Проверка: является ли ds data.frame
  if (!is.data.frame(ds)) {
    stop("Input must be a data frame.")
  }
  
  # Выбираем только переменные типа factor
  factor_vars <- names(ds)[sapply(ds, is.factor)]
  
  # Если нет факторов, возвращаем пустую таблицу
  if (length(factor_vars) == 0) {
    return(data.frame(variable = character(), frequencyTable = character()))
  }
  
  # Создаем таблицу с переменными и частотами
  result <- data.frame(
    variable = factor_vars,
    frequencyTable = sapply(factor_vars, function(var) {
      freq_table <- table(ds[[var]])  # Таблица частот
      paste(names(freq_table), freq_table, sep = ":", collapse = ", ")  # Форматирование
    }),
    stringsAsFactors = FALSE  # Не преобразовываем в фактор
  )
  
  return(result)
}
