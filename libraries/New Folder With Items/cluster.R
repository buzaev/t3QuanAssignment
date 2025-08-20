library(dplyr)
library(cluster)
library(factoextra)

# Преобразование переменных в факторный тип вручную (если они не являются факторами)
ds <- ds %>%
  mutate(
    degree = as.factor(degree),
    wantValueMore = as.factor(wantValueMore)
  )

# Подготовка данных для кластеризации
ds_hclust <- ds %>%
  select(
    job,
    innovativeBehaviorInventory,
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
    supportManagerial,
    supportOrganizational
  )

# Преобразование категориальных переменных в числовой формат
ds_hclust <- ds_hclust %>%
  mutate(across(where(is.factor), as.numeric))


# Нормализация данных (важно для кластеризации)
ds_hclust_scaled <- scale(ds_hclust)

# Вычисление матрицы расстояний (Euclidean Distance)
distance_matrix <- dist(ds_hclust_scaled, method = "euclidean")

# Построение иерархической кластеризации (метод "ward.D2")
hclust_result <- hclust(distance_matrix, method = "ward.D2")

# Визуализация дендрограммы
plot(hclust_result, labels = ds$job, main = "Дендрограмма сотрудников", sub = "", xlab = "Job")

# Определение количества кластеров (например, 3)
rect.hclust(hclust_result, k = 3, border = "red")

# Добавление кластеров в исходный набор данных
ds$cluster_hierarchical <- cutree(hclust_result, k = 3)

# Анализ кластеров
table(ds$cluster_hierarchical)


# Установка пакета ggplot2 (если не установлен)
if (!require("ggplot2")) install.packages("ggplot2")

# Загрузка библиотеки
library(ggplot2)

# Выполнение PCA для уменьшения размерности данных
pca_result <- prcomp(ds_hclust_scaled)

# Извлечение первых двух главных компонент
pca_data <- data.frame(pca_result$x[, 1:2])
colnames(pca_data) <- c("PC1", "PC2")

# Добавление кластеров в PCA-данные
pca_data$cluster <- factor(ds$cluster_hierarchical)

# Визуализация кластеров на PCA-графике
ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    title = "Визуализация кластеров (PCA)",
    x = "Главная компонента 1 (PC1)",
    y = "Главная компонента 2 (PC2)"
  ) +
  scale_color_manual(values = c("red", "blue", "green")) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )




#1. Сравнение средних значений переменных в каждом кластере
#Для этого можно рассчитать средние значения каждой переменной в каждом кластере. Это позволит увидеть, какие переменные больше всего различаются между кластерами.
# Добавление кластеров в исходные данные
ds$cluster_hierarchical <- factor(ds$cluster_hierarchical)

# Расчет средних значений переменных по кластерам
cluster_summary <- ds %>%
  group_by(cluster_hierarchical) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# Просмотр результата
print(cluster_summary)

# Визуализация различий (например, для одной переменной)
library(ggplot2)
ggplot(ds, aes(x = cluster_hierarchical, y = age, fill = cluster_hierarchical)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Различия возраста между кластерами",
    x = "Кластеры",
    y = "Возраст"
  )






###
# Выполнение PCA
pca_result <- prcomp(ds_hclust_scaled, scale. = TRUE)

# Вклад переменных в первую главную компоненту
pca_var_contrib <- data.frame(
  Variable = colnames(ds_hclust_scaled),
  Contribution = abs(pca_result$rotation[, 1])
)

# Сортировка переменных по вкладу
pca_var_contrib <- pca_var_contrib %>%
  arrange(desc(Contribution))

# Просмотр топ-5 переменных
print(head(pca_var_contrib, 5))

# Визуализация вклада переменных
ggplot(pca_var_contrib, aes(x = reorder(Variable, Contribution), y = Contribution)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Вклад переменных в первую главную компоненту",
    x = "Переменные",
    y = "Вклад"
  )






# Убедитесь, что целевая переменная является фактором
ds$cluster_hierarchical <- factor(ds$cluster_hierarchical)

# Установка пакета randomForest (если не установлен)
if (!require("randomForest")) install.packages("randomForest")

# Загрузка библиотеки
library(randomForest)

# Обучение Random Forest для кластеров
rf_model <- randomForest(cluster_hierarchical ~ ., data = ds, importance = TRUE)

# Извлечение важности переменных
importance <- data.frame(
  Variable = rownames(rf_model$importance),
  Importance = rf_model$importance[, "MeanDecreaseGini"]
)

# Сортировка переменных по важности
importance <- importance %>%
  arrange(desc(Importance))

# Просмотр топ-5 переменных
print(head(importance, 5))

# Визуализация важности переменных
library(ggplot2)
ggplot(importance, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Важность переменных для кластеров (Random Forest)",
    x = "Переменные",
    y = "Важность"
  )
