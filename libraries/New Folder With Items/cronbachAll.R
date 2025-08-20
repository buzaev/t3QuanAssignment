cronbachAll<-function(ds)
{
  #Cronbach
  
  
  #Cronbach's Alpha is a measure of internal consistency, which assesses how closely related a set of items are as a group. It indicates whether the items measure the same underlying construct.
  # Cronbach's Alpha: Simple and widely used, but assumes unidimensionality and equal item contributions.
  # Omega More robust, especially for multidimensional constructs, and provides a more accurate estimate of reliability.
  
  #Value of alpha Interpretation
  #<0.6	Poor reliability; items may not measure the same construct.
  #0.6≤ a <0.7	Acceptable reliability; items are moderately consistent.
  #0.7≤ a <0.8	Good reliability; items measure the construct well.
  #0.8≤ a <0.9	Very good reliability; strong internal consistency.
  #≥0.9	Excellent reliability, but may indicate redundancy among items.
  #Limitations:
  #- Assumes equal contribution of items: Cronbach's Alpha assumes all items contribute equally to the construct, which may not always be true.
  #- Affected by the number of items: Adding more items can artificially inflate a, even if they are redundant or irrelevant.
  #- Unidimensionality: assumes that all items measure a single construct. If the scale is multidimensional, may be misleading.
  
  factor_names <- c( "ideaGen", "ideaSearch", "ideaCommunication", "ideaImplementationStart", "ideaInvolvingOthers", "overcomingObstacles",  "innovationOutput",  "supportManagerial",  "supportOrganizational", "innovativeBehaviorInventory","innovationSupportInventory")
  psychDs <- data.frame( factor = factor_names,    a = NA,     w = NA , g=NA)
  
  library(psych)
  # 6 overcomingObstacles
  questions=ds[, c("overcomingObstacles1", "overcomingObstacles2", "overcomingObstacles3", "overcomingObstacles4")]
  alpha_result = alpha(questions)
  a=alpha_result$total$raw_alpha
  omega_result=omega(questions)
  g=(omega_result$G6)
  w=omega_result$omega_h
  psychDs[6, "a"] = a
  psychDs[6, "w"] = w
  psychDs[6, "g"] = g
  
  
  #1 ideaGen
  questions=ds[, c("ideaGen1", "ideaGen2", "ideaGen3")]
  alpha_result = alpha(questions)
  a=alpha_result$total$raw_alpha
  omega_result=omega(questions)
  g=(omega_result$G6)
  omega.diagram(omega_result)
  omega.graph(omega_result)
  w=omega_result$omega_h
  psychDs[1, "a"] = a
  psychDs[1, "w"] = w
  psychDs[1, "g"] = g
  
  #2 ideaSearch
  questions=ds[, c("ideaSearch1", "ideaSearch2", "ideaSearch3")]
  alpha_result = alpha(questions)
  a=alpha_result$total$raw_alpha
  omega_result=omega(questions)
  g=(omega_result$G6)
  w=omega_result$omega_h
  psychDs[2, "a"] = a
  psychDs[2, "w"] = w
  psychDs[2, "g"] = g
  
  #3 ideaCommunication
  questions=ds[, c("ideaCommunication1", "ideaCommunication2", "ideaCommunication3","ideaCommunication3")]
  alpha_result = alpha(questions)
  a=alpha_result$total$raw_alpha
  omega_result=omega(questions)
  g=(omega_result$G6)
  w=omega_result$omega_h
  psychDs[3, "a"] = a
  psychDs[3, "w"] = w
  psychDs[3, "g"] = g
  
  #4 ideaImplementationStart
  questions=ds[, c("ideaImplementationStart1", "ideaImplementationStart2", "ideaImplementationStart3")]
  alpha_result = alpha(questions)
  a=alpha_result$total$raw_alpha
  omega_result=omega(questions)
  g=(omega_result$G6)
  w=omega_result$omega_h
  psychDs[4, "a"] = a
  psychDs[4, "w"] = w
  psychDs[4, "g"] = g
  
  #5 ideaInvolvingOthers
  questions=ds[, c("ideaInvolvingOthers1", "ideaInvolvingOthers2", "ideaInvolvingOthers3")]
  alpha_result = alpha(questions)
  a=alpha_result$total$raw_alpha
  omega_result=omega(questions)
  g=(omega_result$G6)
  w=omega_result$omega_h
  psychDs[5, "a"] = a
  psychDs[5, "w"] = w
  psychDs[5, "g"] = g
  
  #7 innovationOutput
  questions=ds[, c("innovationOutput1", "innovationOutput2", "innovationOutput3")]
  alpha_result = alpha(questions)
  a=alpha_result$total$raw_alpha
  omega_result=omega(questions)
  g=(omega_result$G6)
  w=omega_result$omega_h
  psychDs[7, "a"] = a
  psychDs[7, "w"] = w
  psychDs[7, "g"] = g
  
  #8 supportManagerial
  questions=ds[, c("supportManagerial1", "supportManagerial2", "supportManagerial3","supportManagerial4","supportManagerial5")]
  alpha_result = alpha(questions)
  a=alpha_result$total$raw_alpha
  omega_result=omega(questions)
  g=(omega_result$G6)
  w=omega_result$omega_h
  psychDs[8, "a"] = a
  psychDs[8, "w"] = w
  psychDs[8, "g"] = g
  
  #9 supportOrganizational
  questions=ds[, c("supportOrganizational1", "supportOrganizational2", "supportOrganizational3")]
  alpha_result = alpha(questions)
  a=alpha_result$total$raw_alpha
  omega_result=omega(questions)
  g=(omega_result$G6)
  w=omega_result$omega_h
  psychDs[9, "a"] = a
  psychDs[9, "w"] = w
  psychDs[9, "g"] = g
  
  #10 innovativeBehaviorInventory
  questions=ds[, c(
    "ideaGen1",
    "ideaGen2",
    "ideaGen3",
    "ideaSearch1",
    "ideaSearch2",
    "ideaSearch3",
    "ideaCommunication1",
    "ideaCommunication2",
    "ideaCommunication3",
    "ideaCommunication4",
    "ideaImplementationStart1",
    "ideaImplementationStart2",
    "ideaImplementationStart3",
    "ideaInvolvingOthers1",
    "ideaInvolvingOthers2",
    "ideaInvolvingOthers3",
    "overcomingObstacles1",
    "overcomingObstacles2",
    "overcomingObstacles3",
    "overcomingObstacles4",
    "innovationOutput1",
    "innovationOutput2",
    "innovationOutput3"
  )]
  alpha_result = alpha(questions)
  a=alpha_result$total$raw_alpha
  omega_result=omega(questions)
  g=(omega_result$G6)
  w=omega_result$omega_h
  psychDs[10, "a"] = a
  psychDs[10, "w"] = w
  psychDs[10, "g"] = g
  
  #11 innovationSupportInventory
  questions=ds[, c(
    "supportManagerial1",
    "supportManagerial2",
    "supportManagerial3",
    "supportManagerial4",
    "supportManagerial5",
    "supportOrganizational1",
    "supportOrganizational2",
    "supportOrganizational3"
  )]
  alpha_result = alpha(questions)
  a=alpha_result$total$raw_alpha
  omega_result=omega(questions)
  g=(omega_result$G6)
  w=omega_result$omega_h
  psychDs[11, "a"] = a
  psychDs[11, "w"] = w
  psychDs[11, "g"] = g
  
  
  
  #Omega Total Measures the total reliability of the scale, including all variance (true score + error).
  #Omega Hierarchical Measures the reliability of the general factor in a multidimensional scale.
  #Value of Interpretation
  #w<0.6	Poor reliability; items may not measure the same construct.
  #0.6≤w<0.7	Acceptable reliability; items are moderately consistent.
  #0.7≤w<0.8	Good reliability; items measure the construct well.
  #0.8≤w<0.9	Very good reliability; strong internal consistency.
  #w≥0.9	Excellent reliability, but may indicate redundancy among items.
  #Advantages of Omega over Alpha:
  #- Handles multidimensionality: Omega can account for hierarchical or multidimensional structures in the data.
  #- Accounts for unequal item contributions: Omega incorporates factor loadings, which reflect the varying contributions of items to the construct.
  #- More robust: Omega is less sensitive to the number of items and is considered a better indicator of reliability for complex scales.
  
  psychDs
}