getAllCorrelations <- function(ds){

#correlations
#  install.packages("dplyr")  # Если еще не установлено
  library(dplyr)
  library(Hmisc)

numeric_df <- ds %>% select(where(is.numeric))

# Вычисление корреляционной матрицы и p-значений
corr_result <- rcorr(as.matrix(numeric_df), type = "spearman")

# Создание матрицы корреляций с пустыми ячейками для незначимых значений
spearman_corr <- corr_result$r
p_values <- corr_result$P

# Заменяем незначимые корреляции на NA
spearman_corr[p_values > 0.05] <- NA

# Преобразуем в DataFrame
spearman_corr_df <- as.data.frame(spearman_corr)

# Просмотр результата
spearman_corr_df
}

getSelectedCorrelations <- function(ds, columnsList=c("age","experience")){
  #correlations
  #  install.packages("dplyr")  # Если еще не установлено
  library(dplyr)
  library(Hmisc)
  columnsList=c("age","experience","fellowsNumber","talentUseFull","jobSatisfaction",
                "positiveSumMindset","talentUseOpportunity","satisfyValueSefRespect","satisfyValueSafety",
                "satisfyValueWarmRelations",
                "satisfyValueFulfillment",
                "satisfyValueAccomplishment",
                "satisfyValueBeingRespected",
                "satisfyValueBelonging",
                "satisfyValueJoy","ideaGen","ideaSearch","ideaCommunication","ideaImplementationStart",
                "ideaInvolvingOthers","overcomingObstacles","innovationOutput","supportManagerial","supportOrganizational","innovativeBehaviorInventory",
                "innovationSupportInventory")
  
  
  numeric_df <- ds %>% select(all_of(columnsList))
  
  # Вычисление корреляционной матрицы и p-значений
  corr_result <- rcorr(as.matrix(numeric_df), type = "spearman")
  
  # Создание матрицы корреляций с пустыми ячейками для незначимых значений
  spearman_corr <- corr_result$r
  p_values <- corr_result$P
  
  # Заменяем незначимые корреляции на NA
  spearman_corr[p_values > 0.05] <- NA
  
  # Преобразуем в DataFrame
  spearman_corr_df <- as.data.frame(spearman_corr)
  
  # Просмотр результата
  spearman_corr_df
}






# Функция для создания таблицы LaTeX
createSpearmanLatexTable <- function(df) {

 # Выбор только числовых переменных
  numeric_df <- df %>% select(where(is.numeric))

  
################### не все столбцы
  
  
  columnsList=c("age","experience","fellowsNumber","talentUseFull","jobSatisfaction",
                "positiveSumMindset","talentUseOpportunity","satisfyValueSefRespect","satisfyValueSafety",
                "satisfyValueWarmRelations",
                "satisfyValueFulfillment",
                "satisfyValueAccomplishment",
                "satisfyValueBeingRespected",
                "satisfyValueBelonging",
                "satisfyValueJoy","ideaGen","ideaSearch","ideaCommunication","ideaImplementationStart",
                "ideaInvolvingOthers","overcomingObstacles","innovationOutput","supportManagerial","supportOrganizational","innovativeBehaviorInventory",
                "innovationSupportInventory")
  
  
  numeric_df <- ds %>% select(all_of(columnsList))
  
  ###################
  
  
    
  # Вычисление корреляционной матрицы и p-значений
  corr_result <- rcorr(as.matrix(numeric_df), type = "spearman")
  
  # Извлечение коэффициентов и p-значений
  spearman_corr <- corr_result$r
  p_values <- corr_result$P
  
  # Создание таблицы с результатами
  results <- data.frame()
  
  # Проход по всем комбинациям
  for (i in 1:(ncol(spearman_corr) - 1)) {
    for (j in (i + 1):ncol(spearman_corr)) {
      r_value <- spearman_corr[i, j]
      p_value <- p_values[i, j]
      
      if (p_value < 0.05 ){#& r_value>0.4) {
        results <- rbind(results, data.frame(
          X = colnames(spearman_corr)[i],
          Y = colnames(spearman_corr)[j],
          R = round(r_value, 2),
          p = round(p_value, 7)
        ))
      }
    }
  }
  
  # Форматирование R>0.5 жирным
  results$R <- ifelse(results$R > 0.5, paste0("\\textbf{", results$R, "}"), results$R)
  
  # Создание таблицы LaTeX
  latex_table <- "\\clearpage\n\\begin{table}[!htb]\n\\caption{Internal consistency}\n\\label{cronbachs}\n\\begin{threeparttable}\n"
  latex_table <- paste0(latex_table, "\\begin{tabular}{lcccc}\n\\toprule\n& X & Y & R & p \\\\\n\\midrule\n")
  
  for (i in 1:nrow(results)) {
    latex_table <- paste0(latex_table,  results$X[i], " & ", results$Y[i], " & ", results$R[i], " & ", sprintf("%.7f", results$p[i]), " \\\\\n")
  }
  
  latex_table <- paste0(latex_table, "\\bottomrule\n\\end{tabular}\n\\end{threeparttable}\n\\end{table}")
  
  return(latex_table)
}


createPlots <- function(df) {
library(ggpubr) #для stat_cor
  
  gg=ggplot(ds)+ 
  aes(x=satisfyValueBelonging,y=ideaCommunication, color=factor(zeroSumMindsetBool)) + #, shape=factor(gender))) + 
  geom_point(size=1, show.legend = TRUE, alpha = .8) +
  #geom_count(show.legend=F) +
  geom_smooth(aes(group = factor(zeroSumMindsetBool)), method = "lm", se = FALSE,  size = 0.3) +  
  geom_smooth(method="lm", se=TRUE,col="black", size=0.3, level=0.95) +
  theme_minimal()+
  #scale_fill_brewer(palette=color_theme) +
  stat_cor(method = "spearman", label.x.npc =0.3, label.y.nrc = 0.30, size=4)+
  stat_ellipse(color="lightgrey")+
  labs(subtitle="satisfyValueBelonging vs ideaCommunication", 
       x="satisfyValueBelonging", 
       y="ideaCommunication", 
       title="Scatterplot", 
       fill="Zero-sum mindset",
       caption = paste( "Subset: ",subsetName, "from ", filename ))
plot(gg)
plotfilename=paste(dir_plots,"/","corr-satisfyValueBelonging-ideaCommunication.pdf", sep="")
pdf(plotfilename,6,4)
print(gg, newpage = FALSE)
dev.off()
}