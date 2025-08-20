# модель что бы хотел от других путем рандом форест

#Variable Importance
#age                                             age   9.404192
#experience                               experience   8.501972
#zeroSumMindset                       zeroSumMindset   5.567924
#satisfyValueWarmRelations satisfyValueWarmRelations   5.382991
#jobSatisfaction                     jobSatisfaction   5.118484

  
dsm <- ds %>%
  select(
    job,
    # innovativeBehaviorInventory,
    sex,
    age,
    experience,
    degree, # Теперь это фактор
    talentUseFull,
    jobSatisfaction,
    talentUseWish,
    zeroSumMindset,
    talentUseOpportunity,
    satisfyValueSefRespect,
    satisfyValueSafety,
    satisfyValueWarmRelations,
    satisfyValueFulfillment,
    satisfyValueAccomplishment,
    satisfyValueBeingRespected,
    satisfyValueBelonging,
    satisfyValueJoy,
    wantValueMore, # Теперь это фактор
    #  supportManagerial,
    #  supportOrganizational
  )

# Проверка данных
str(dsm) # Убедитесь, что структура данных корректна
# Преобразование целевой переменной в фактор (если она категориальная)
dsm$wantValueMore <- factor(dsm$wantValueMore)
# Удаление пропусков
dsm <- na.omit(dsm)

# Установка пакета randomForest (если не установлен)
if (!require("randomForest")) install.packages("randomForest")

# Загрузка библиотеки
library(randomForest)

# Обучение модели Random Forest
rf_model <- randomForest(wantValueMore ~ ., data = dsm, importance = TRUE)

# Важность переменных
importance <- data.frame(
  Variable = rownames(rf_model$importance),
  Importance = rf_model$importance[, "MeanDecreaseGini"]
)

# Сортировка переменных по важности
importance <- importance %>%
  arrange(desc(Importance))

# Просмотр топ-5 переменных
print(head(importance, 5))


# Проведение теста Крускала–Уоллиса
kruskal_test <- kruskal.test(innovativeBehaviorInventory ~ zeroSumMindset, data = ds)

# Извлечение p-value
p_value_kruskal <- kruskal_test$p.value



