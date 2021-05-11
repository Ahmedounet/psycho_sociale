outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1  <- mean(var_name, na.rm = T)
  sd1 <- sd(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="Avec Outliers", ylab=NA)
  hist(var_name, main="Avec Outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  sdo <- sd(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Sans Outliers", ylab=NA)
  hist(var_name, main="Sans Outliers", xlab=NA, ylab=NA)
  par(mfrow=c(1, 1))
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identifies: ", na2 - na1, " sur ", tot, " observations")
  message("Proportion d'Outliers: ", round(((na2 - na1) / tot*100), digits=2), "%")
  message("Moyenne (Ecart-type) des Outliers: ", 
          round(mo, digits=2), " (", round(sdo, digits=2), ")")
  m2 <- mean(var_name, na.rm = T)
  sd2 <- sd(var_name, na.rm = T)
  message("Moyenne (Ecart-type) de la mesure avec les Outliers: ", 
          round(m1, digits=2), " (", round(sd1, digits=2), ")")
  message("Moyenne (Ecart-type) de la mesure sans les Outliers: ",
          round(m2, digits=2), " (", round(sd2, digits=2), ")")
  response <- readline(prompt="Souhaitez-vous retirer les Outliers et les remplacer par NA ? [oui/non]: ")
  if(response == "y" | response == "yes" | response == "oui" | response== "Oui"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Retrait des Outliers accompli.", "\n")
    return(invisible(dt))} else{
      message("Aucun changement.", "\n")
      return(invisible(var_name))}}