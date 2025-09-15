getAllCorrelations = function(ds) {
  library(dplyr)
  library(Hmisc)
  numeric_df = ds %>% select(where(is.numeric))
  if (ncol(numeric_df) == 0) {
    stop("There is no numbers in dataframe.")
  }
  corr_result = rcorr(as.matrix(numeric_df), type = "spearman")
  spearman_corr = corr_result$r
  p_values = corr_result$P
  formatted_corr = matrix(
    nrow = nrow(spearman_corr),
    ncol = ncol(spearman_corr)
  )
  
  for (i in seq_len(nrow(spearman_corr))) {
    for (j in seq_len(ncol(spearman_corr))) {
      if (is.na(p_values[i, j])) {formatted_corr[i, j] <- NA} else 
        {
        formatted_corr[i, j] <- paste0(
          round(spearman_corr[i, j], 2), 
          " (p=", 
          round(p_values[i, j], 3), 
          ifelse(p_values[i, j] <= 0.05, ", *", ", NS"),
          ")"
        )
      }
    }
  }
  formatted_corr_df = as.data.frame(formatted_corr)
  rownames(formatted_corr_df) = rownames(spearman_corr)
  colnames(formatted_corr_df) = colnames(spearman_corr)
  return(formatted_corr_df)
}

getSelectedCorrelations = function(ds, columnsList=c("age","experience")){
  library(dplyr)
  library(Hmisc)
  columnsList=c("Age","YearsWorking")
  numeric_df <- ds %>% select(all_of(columnsList))
  if (ncol(numeric_df) == 0) {stop("There is no numbers in the dataframe.")
  }
  corr_result <- rcorr(as.matrix(numeric_df), type = "spearman")
  spearman_corr <- corr_result$r
  p_values <- corr_result$P
  formatted_corr <- matrix(
    nrow = nrow(spearman_corr),
    ncol = ncol(spearman_corr)
  )
  for (i in seq_len(nrow(spearman_corr))) {
    for (j in seq_len(ncol(spearman_corr))) {
      if (is.na(p_values[i, j])) {
        formatted_corr[i, j] <- NA
      } else {
        # Форматируем коэффициент и p-значение
        formatted_corr[i, j] <- paste0(
          round(spearman_corr[i, j], 2), 
          " (p=", 
          round(p_values[i, j], 3), 
          ifelse(p_values[i, j] <= 0.05, ", *", ", NS"),
          ")"
        )
      }
    }
  }
  formatted_corr_df <- as.data.frame(formatted_corr)
  rownames(formatted_corr_df) <- rownames(spearman_corr)
  colnames(formatted_corr_df) <- colnames(spearman_corr)
  return(formatted_corr_df)
  
}

createSpearmanLatexTable <- function(df) {
  numeric_df = df %>% select(where(is.numeric))
  columnsList=c("Age","YearsWorking")
  numeric_df = ds %>% select(all_of(columnsList))
  corr_result = rcorr(as.matrix(numeric_df), type = "spearman")
  spearman_corr = corr_result$r
  p_values = corr_result$P
  results = data.frame()
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
  results$R <- ifelse(results$R > 0.5, paste0("\\textbf{", results$R, "}"), results$R)
  latex_table <- "\\clearpage\n\\begin{table}[!htb]\n\\caption{Internal consistency}\n\\label{cronbachs}\n\\begin{threeparttable}\n"
  latex_table <- paste0(latex_table, "\\begin{tabular}{lcccc}\n\\toprule\n& X & Y & R & p \\\\\n\\midrule\n")
  
  for (i in 1:nrow(results)) {
    latex_table <- paste0(latex_table,  results$X[i], " & ", results$Y[i], " & ", results$R[i], " & ", sprintf("%.7f", results$p[i]), " \\\\\n")
  }
  
  latex_table <- paste0(latex_table, "\\bottomrule\n\\end{tabular}\n\\end{threeparttable}\n\\end{table}")
  return(latex_table)
}


createCorrelationPlots <- function(df) {
library(ggpubr) 
library(RColorBrewer)
#  display.brewer.all() посмотреть цвета
  gg=ggplot(ds)+ 
  aes(x=Age,y=YearsWorking, color=Sex)+# , shape=factor(Sex)) + 
  geom_count(show.legend=TRUE) +
  geom_smooth(aes(color = Sex), method = "lm", se = FALSE,  size = 0.3) +  
  geom_smooth(method="lm", se=TRUE,col="black", size=0.3, level=0.95) +
  theme_minimal()+
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(-10, max(ds$YearsWorking) * 1.2)) +  # Увеличиваем диапазон оси Y
  stat_cor(aes(color = Sex), method = "spearman",  label.x.npc = 0.05, label.y.npc = 0.99, size = 4, show.legend=FALSE)+
  stat_ellipse(color="lightgrey")+
  labs(subtitle="Age vs Experience", 
       x="Age", 
       y="Experience (years working)", 
       title="Scatterplot", 
       caption = paste( "Subset:",subsetName, "from file:", filename ))
plot(gg)
plotfilename=paste(dir_plots,"/","corr-age-experience.pdf", sep="")
pdf(plotfilename,6,4)
print(gg, newpage = FALSE)
dev.off()
}