exportDensityPlots = function(ds, filename, subsetName){
  
  mean_age = mean(ds$Age, na.rm = TRUE)  
  sd_age = sd(ds$Age, na.rm = TRUE)   
  gg = ggplot(ds, aes(Age)) +
    geom_density(alpha = 0.4, aes(fill = Sex)) + 
    geom_vline(aes(xintercept = median(Age)), color = "darkgrey", linetype = "dashed") +
    geom_vline(aes(xintercept = mean(Age)), color = "darkgrey", linetype = "solid") +
    stat_function(fun = dnorm, args = list(mean = mean_age, sd = sd_age), 
    color = "darkgrey", linetype = "solid", size = 1, alpha=0.5) +  
    theme_minimal() +
    labs(
      title = "Density plot with Expected Normal Distribution",
      subtitle = "Age (years)",
      caption = paste("Subset: ", subsetName, "from file ", filename),
      x = "Age (years)",
      y = "Density",
      fill = "Sex"
    )
  plot(gg)
  plotfilename=paste(dir_plots,"/","density-age.pdf", sep="")
  pdf(plotfilename,6,4)
  print(gg, newpage = FALSE)
  dev.off()
  
  
  mean_exp = mean(ds$YearsWorking, na.rm = TRUE) 
  sd_exp = sd(ds$YearsWorking, na.rm = TRUE)     
  xname="Experience (years)"
  gg <- ggplot(ds, aes(YearsWorking)) +
    geom_density(alpha = 0.4, aes(fill = Sex)) + 
    geom_vline(aes(xintercept = median(YearsWorking)), color = "darkgrey", linetype = "dashed") +
    geom_vline(aes(xintercept = mean(YearsWorking)), color = "darkgrey", linetype = "solid") +
    stat_function(fun = dnorm, args = list(mean = mean_exp, sd = sd_exp), 
    color = "darkgrey", linetype = "solid", size = 1, alpha=0.5) +  
    theme_minimal() +
    labs(
      title = "Density plot with Expected Normal Distribution",
      subtitle = xname,
      caption = paste("Subset: ", subsetName, "from file ", filename),
      x = xname,
      y = "Density",
      fill = "Sex"
    )
  plot(gg)
  plotfilename=paste(dir_plots,"/","density-experience.pdf", sep="")
  pdf(plotfilename,6,4)
  print(gg, newpage = FALSE)
  dev.off()
  
  
  
  xname="Experience (years)"
  gg <- ggplot(ds, aes(YearsWorking)) +
    geom_density(alpha = 0.4, aes(fill = Profession)) + 
    geom_vline(aes(xintercept = median(YearsWorking)), color = "darkgrey", linetype = "dashed") +
    geom_vline(aes(xintercept = mean(YearsWorking)), color = "darkgrey", linetype = "solid") +
    theme_minimal() +
    labs(
      title = "Density plot with Expected Normal Distribution",
      subtitle = xname,
      caption = paste("Subset: ", subsetName, "from file ", filename),
      x = xname,
      y = "Density",
      fill = "Profession"
    )
  plot(gg)
  plotfilename=paste(dir_plots,"/","density-experience-profession.pdf", sep="")
  pdf(plotfilename,6,4)
  print(gg, newpage = FALSE)
  dev.off()
  
  
  
  mean_exp = mean(ds$startedCareer, na.rm = TRUE)  
  sd_exp = sd(ds$startedCareer, na.rm = TRUE)      
  xname="Started Carreer (years)"
  gg <- ggplot(ds, aes(startedCareer)) +
    geom_density(alpha = 0.4, aes(fill = Sex)) + 
    geom_vline(aes(xintercept = median(startedCareer)), color = "darkgrey", linetype = "dashed") +
    geom_vline(aes(xintercept = mean(startedCareer)), color = "darkgrey", linetype = "solid") +
    stat_function(fun = dnorm, args = list(mean = mean_exp, sd = sd_exp), 
                  color = "darkgrey", linetype = "solid", size = 1, alpha=0.5) +  
    theme_minimal() +
    labs(
      title = "Density plot with Expected Normal Distribution",
      subtitle = xname,
      caption = paste("Subset: ", subsetName, "from file ", filename),
      x = xname,
      y = "Density",
      fill = "Sex"
    )
  plot(gg)
  plotfilename=paste(dir_plots,"/","experience-density.pdf", sep="")
  pdf(plotfilename,6,4)
  print(gg, newpage = FALSE)
  dev.off()
  
  
  
  xname="Age (years)"
  gg <- ggplot(ds, aes(Age)) +
    geom_density(alpha = 0.4, aes(fill = Profession)) + 
    geom_vline(aes(xintercept = median(Age)), color = "darkgrey", linetype = "dashed") +
    geom_vline(aes(xintercept = mean(Age)), color = "darkgrey", linetype = "solid") +
    theme_minimal() +
    labs(
      title = "Density plot with Expected Normal Distribution",
      subtitle = xname,
      caption = paste("Subset: ", subsetName, "from file ", filename),
      x = xname,
      y = "Density",
      fill = "Profession"
    )
  plot(gg)
  plotfilename=paste(dir_plots,"/","density-age-profession.pdf", sep="")
  pdf(plotfilename,6,4)
  print(gg, newpage = FALSE)
  dev.off()
  
  
}