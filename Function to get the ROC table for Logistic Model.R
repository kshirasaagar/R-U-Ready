
#Function to get the ROC table for Logistic Model

logit.roc <- function(model, steps=20) {
  # get the response field
  # from the model object
  field.name <- attr(attr(terms(formula(model)), "factors"),
                     "dimnames")[[1]][1]
  # and extract the T/F from it
  eval(parse(text=paste("tmp <- ",
                        ifelse(class(model$data) == "data.frame", "model$data$", ""),
                        field.name, sep="")))
  r <- data.frame(pts = seq(0, 1-(1/steps), by=1/steps),
                  sens = 0, spec=0);
  for (i in 0:steps) {
    thresh <- i/steps;
    r$sens[i] <- sum((fitted(model) >= thresh) & tmp)/sum(tmp);
    r$spec[i] <- sum((fitted(model) < thresh) & !tmp)/sum(!tmp)
  }
  return(r)
}

roc_table <- logit.roc(model1)