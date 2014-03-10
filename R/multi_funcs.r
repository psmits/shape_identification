multi.mod <- function(data, formula, preProc) {
  ## fit multinomial logistic regression to data
  ## this is so i can map over a list
  ##
  ##  Args:
  ##    data: object of class data.frame, probably the training set
  ##    formula: the formula for the multinomial logistic regression
  ##    trControl: input for trControl in train
  ##    preProc: input for preProc in train
  ##
  ##  Returns:
  ##    object of class train with all the correct attributes

  mod <- train(formula, data = data, 
               method = 'multinom', 
               preProc = preProc)

  mod
}

#' extract important values from list of fitted multinomial logistic regression models
#'
#' @param model list; list of multinom objects
#' @param class vector; observed classes
#' @param test df; testing data frame
#' @return list of important model values
multi.analysis <- function(model, class, test) {
  out <- list()
  out$sel <- lapply(model, function(mod) model.sel(lapply(mod, function(x) x$finalModel)))
  out$best <- mapply(function(sel, mod) mod[[as.numeric(rownames(sel)[1])]],
                     sel = out$sel, mod = model,
                     SIMPLIFY = FALSE)
  pc <- mapply(predict, out$best, test,
               MoreArgs = list(type = 'raw'), SIMPLIFY = FALSE)
  pp <- vector(mode = 'list', length = length(out$best))
  for(ii in seq(length(out$best))) {
    pp[[ii]] <- predict(out$best[[ii]]$finalModel, test[[ii]], type = 'probs')
  }
  out$class <- mapply(function(x, y) cbind(pred = x, as.data.frame(y)), 
                      x = pc, y = pp, SIMPLIFY = FALSE)
    
  #out$conf <- Map(confusionMatrix,
  #                out$class$class, class)

  out
}

# take out important values from random forests results
rf.analysis <- function(model, class, test) {
  out <- list()
  out$varimp <- lapply(model, varImp)
  out$best <- Map(function(x) {
                  rr <- x$results$ROC
                  sel <- which.max(unlist(rr))
                  sel},
                  x = model)
#  out$re <- resamples(model)
  out$class <- mapply(predict, model, test,
                      SIMPLIFY = FALSE)
  out$conf <- Map(function(x, y) confusionMatrix(x$pred, y),
                  x = out$class, y = class)
  out
}

# take out important values from linear discriminate analysis results
lda.analysis <- function(model, test) {
  out <- list()
  out$best <- Map(function(x) {
                  rr <- lapply(x, function(a) a$results$ROC)
                  sel <- which.max(unlist(rr))
                  x[[sel]]},
                  x = model)

  out$class <- Map(function(mm, tt) {
                   nn <- predict(mm$finalModel, 
                                 tt[, seq(length(mm$finalModel$xNames))])
                   oo <- cbind(pred = nn$class, as.data.frame(nn$posterior))
                   oo}, mm = out$best, tt = test)
  out
}
