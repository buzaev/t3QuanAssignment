

convert_to_journal_format<-function(ds_column)
{
  #library("gdata")
  a= paste( quantile(ds_column, 0.5, na.rm=TRUE), " [" , (quantile(ds_column, 0.25, na.rm=TRUE)), "; ", (quantile(ds_column, 0.75, na.rm=TRUE)),"]", sep="")
  a
}


convert_to_disser_format<-function(ds_column)
{
  #library("gdata")
  library("base")
    a= paste(round(quantile(ds_column, 0.5, na.rm=TRUE),2), " (" , round(quantile(ds_column, 0.25, na.rm=TRUE),2), "; ", round(quantile(ds_column, 0.75, na.rm=TRUE),2),")", sep="")
  a
}

#class(x)
#mode(x)
#str(x)
savegg<-function(plotfilenum, dir_plots, plotname, subsetName)
{

  plotfilename=paste(dir_plots,"/",subsetName,"-", plotname,  " (", plotfilenum, ")", ".svg", sep="")
  ggsave(plotfilename, units="in", width=7, height=5, dpi=600 )
}



wilcoxx<-function(x, y, name, filter1=0,filter2=1)
{
  b=x[y==filter1]
  b0=b
  library("base")
  a1= paste(round(quantile(b, 0.5, na.rm=TRUE),2),  " (" , round(quantile(b, 0.25, na.rm=TRUE),2), "; ", round(quantile(b, 0.75, na.rm=TRUE),2),")", sep="")
  b=x[y==filter2]
  b1=b
  a2= paste(round(quantile(b, 0.5, na.rm=TRUE),2), " (" , round(quantile(b, 0.25, na.rm=TRUE),2), "; ", round(quantile(b, 0.75, na.rm=TRUE),2),")", sep="")
  c=wilcox.test(b0,b1)
  if (round(c$p.value, 3) < 0.05) {
    d = "*"
  } else {
    d = ""
  }
  wilcoxx=paste(name, "&",a1,"&",a2,"&",round(c$p.value,3),"&",d,"\\")
  wilcoxx
}

wilcoxp<-function(ds, prm, filter1=0,filter2=1){
  print (wilcoxx(ds$age,prm,"age",filter1,filter2))
  print (wilcoxx(ds$experience,prm,"experience",filter1,filter2))
  print (wilcoxx(ds$jobSatisfaction,prm,"jobSatisfaction",filter1,filter2))
  print (wilcoxx(ds$satisfyValueSefRespect,prm,"satisfyValueSefRespect",filter1,filter2))
  print (wilcoxx(ds$satisfyValueSafety,prm,"satisfyValueSafety",filter1,filter2))
  print (wilcoxx(ds$satisfyValueWarmRelations,prm,"satisfyValueWarmRelations",filter1,filter2))
  print (wilcoxx(ds$satisfyValueFulfillment,prm,"satisfyValueFulfillment",filter1,filter2))
  print (wilcoxx(ds$satisfyValueAccomplishment,prm,"satisfyValueAccomplishment",filter1,filter2))
  print (wilcoxx(ds$satisfyValueBeingRespected,prm,"satisfyValueBeingRespected",filter1,filter2))
  print (wilcoxx(ds$satisfyValueAccomplishment,prm,"satisfyValueAccomplishment",filter1,filter2))
  print (wilcoxx(ds$satisfyValueBelonging,prm,"satisfyValueBelonging",filter1,filter2))
  print (wilcoxx(ds$satisfyValueJoy,prm,"satisfyValueJoy",filter1,filter2))
  print (wilcoxx(ds$ideaGen,prm,"ideaGen",filter1,filter2))
  print (wilcoxx(ds$ideaSearch,prm,"ideaSearch",filter1,filter2))
  print (wilcoxx(ds$ideaCommunication,prm,"ideaCommunication",filter1,filter2))
  print (wilcoxx(ds$ideaImplementationStart,prm,"ideaImplementationStart",filter1,filter2))
  print (wilcoxx(ds$ideaInvolvingOthers,prm,"ideaInvolvingOthers",filter1,filter2))
  print (wilcoxx(ds$overcomingObstacles,prm,"overcomingObstacles",filter1,filter2))
  print (wilcoxx(ds$innovationOutput,prm,"innovationOutput",filter1,filter2))
  print (wilcoxx(ds$supportManagerial,prm,"supportManagerial",filter1,filter2))
  print (wilcoxx(ds$supportOrganizational,prm,"supportOrganizational",filter1,filter2))
  print (wilcoxx(ds$innovativeBehaviorInventory,prm,"innovativeBehaviorInventory",filter1,filter2))
  print (wilcoxx(ds$innovationSupportInventory,prm,"innovationSupportInventory",filter1,filter2))
}

convertToLatex <- function(df, table_name) {
  # Название таблицы как заголовок
  caption <- table_name
  
  # Начало таблицы
  latex_table <- "\\begin{table}[!htb]\n"
  latex_table <- paste0(latex_table, "\\caption{", caption, "}\n")
  latex_table <- paste0(latex_table, "\\label{", table_name, "}\n")
  latex_table <- paste0(latex_table, "\\begin{threeparttable}\n")
  latex_table <- paste0(latex_table, "\\begin{tabular}{", paste(rep("c", ncol(df)), collapse = ""), "}\n")
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