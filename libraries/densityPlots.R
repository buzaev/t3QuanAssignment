exportDensityPlots <- function(ds, filename, subsetName){
  
  mean_age <- mean(ds$Age, na.rm = TRUE)  # Среднее значение
  sd_age <- sd(ds$Age, na.rm = TRUE)      # Стандартное отклонение
  
  # Добавляем нормальное распределение на график
  gg <- ggplot(ds, aes(Age)) +
    geom_density(alpha = 0.4, aes(fill = Sex)) + 
    geom_vline(aes(xintercept = median(Age)), color = "darkgrey", linetype = "dashed") +
    geom_vline(aes(xintercept = mean(Age)), color = "darkgrey", linetype = "solid") +
    stat_function(fun = dnorm, args = list(mean = mean_age, sd = sd_age), 
                  color = "darkgrey", linetype = "solid", size = 1, alpha=0.5) +  # Нормальное распределение
    theme_minimal() +
    labs(
      title = "Density plot with Expected Normal Distribution",
      subtitle = "Age (years)",
      caption = paste("Subset: ", subsetName, "from file ", filename),
      x = "Age (years)",
      y = "Density",
      fill = "Sex"
    )
  
  # Визуализация
  plot(gg)
  
  #plotfilename=paste(dir_plots, "/", plotfilenum, "-",subsetName,"-", plotname, ".svg", sep="")
  #ggsave(plotfilename, units="in", width=7, height=5, dpi=600 )
  #savegg(plotfilenum,dir_plots,"sex-innovativeBehaviorInventory",subsetName)
  
  plotfilename=paste(dir_plots,"/","age-density.pdf", sep="")
  pdf(plotfilename,6,4)
  print(gg, newpage = FALSE)
  dev.off()
  
  
  mean_exp <- mean(ds$YearsWorking, na.rm = TRUE)  # Среднее значение
  sd_exp <- sd(ds$YearsWorking, na.rm = TRUE)      # Стандартное отклонение
  
  
  xname="Experience (years)"
  # Добавляем нормальное распределение на график
  gg <- ggplot(ds, aes(YearsWorking)) +
    geom_density(alpha = 0.4, aes(fill = Sex)) + 
    geom_vline(aes(xintercept = median(Age)), color = "darkgrey", linetype = "dashed") +
    geom_vline(aes(xintercept = mean(Age)), color = "darkgrey", linetype = "solid") +
    stat_function(fun = dnorm, args = list(mean = mean_exp, sd = sd_exp), 
                  color = "darkgrey", linetype = "solid", size = 1, alpha=0.5) +  # Нормальное распределение
    theme_minimal() +
    labs(
      title = "Density plot with Expected Normal Distribution",
      subtitle = xname,
      caption = paste("Subset: ", subsetName, "from file ", filename),
      x = xname,
      y = "Density",
      fill = "Sex"
    )
  
  # Визуализация
  plot(gg)
  
  #plotfilename=paste(dir_plots, "/", plotfilenum, "-",subsetName,"-", plotname, ".svg", sep="")
  #ggsave(plotfilename, units="in", width=7, height=5, dpi=600 )
  #savegg(plotfilenum,dir_plots,"sex-innovativeBehaviorInventory",subsetName)
  
  plotfilename=paste(dir_plots,"/","experience-density.pdf", sep="")
  pdf(plotfilename,6,4)
  print(gg, newpage = FALSE)
  dev.off()
  
  
  
  mean_exp <- mean(ds$startedCareer, na.rm = TRUE)  # Среднее значение
  sd_exp <- sd(ds$startedCareer, na.rm = TRUE)      # Стандартное отклонение
  xname="Started Carreer (years)"
  gg <- ggplot(ds, aes(startedCareer)) +
    geom_density(alpha = 0.4, aes(fill = Sex)) + 
    geom_vline(aes(xintercept = median(Age)), color = "darkgrey", linetype = "dashed") +
    geom_vline(aes(xintercept = mean(Age)), color = "darkgrey", linetype = "solid") +
    stat_function(fun = dnorm, args = list(mean = mean_exp, sd = sd_exp), 
                  color = "darkgrey", linetype = "solid", size = 1, alpha=0.5) +  # Нормальное распределение
    theme_minimal() +
    labs(
      title = "Density plot with Expected Normal Distribution",
      subtitle = xname,
      caption = paste("Subset: ", subsetName, "from file ", filename),
      x = xname,
      y = "Density",
      fill = "Sex"
    )
  
  # Визуализация
  plot(gg)
  
  #plotfilename=paste(dir_plots, "/", plotfilenum, "-",subsetName,"-", plotname, ".svg", sep="")
  #ggsave(plotfilename, units="in", width=7, height=5, dpi=600 )
  #savegg(plotfilenum,dir_plots,"sex-innovativeBehaviorInventory",subsetName)
  
  plotfilename=paste(dir_plots,"/","experience-density.pdf", sep="")
  pdf(plotfilename,6,4)
  print(gg, newpage = FALSE)
  dev.off()
  
  
  
}