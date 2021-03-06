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
multi.analysis <- function(model, class, test, train) {
  out <- list()
  out$aic <- lapply(model, function(mod) unlist(lapply(mod, AICc)))
  
  # need to choose a best model
  # do this based on within test auc
  pr <- list()
  for(ii in seq(length(model))) {
    p <- list()
    for(jj in seq(from = 2, to = length(model[[ii]]))) {
      guess <- predict(model[[ii]][[jj]], 
                       newdata = train[[ii]][, model[[ii]][[jj]]$coefnames[-1]])
      guess <- data.frame(guess)
      guess <- cbind(guess, 
                     predict(model[[ii]][[jj]], 
                             newdata = train[[ii]][, model[[ii]][[jj]]$coefnames[-1]], 
                             type = 'probs'))
      p[[jj - 1]] <- guess
    }
    pr[[ii]] <- p
  }

  aucs <- list()
  for(ii in seq(length(pr))) {
    oo <- list()
    for(jj in seq(length(pr[[ii]]))) {
      pred <- pr[[ii]][[jj]][, 1]
      obs <- train[[ii]][, groups[[ii]]]
      pred.res <- pr[[ii]][[jj]][, -1]
      if(length(levels(pred)) == 2) {
        mauc <- roc(obs, pred.res)$auc
      } else {
        prob <- lapply(levels(pred), function(cl) {
                       pp <- ifelse(pred == cl, 1, 0)
                       oo <- ifelse(obs == cl, 1, 0)
                       prob <- pred.res[, cl]

                       ps <- Metrics::auc(oo, prob)
                       ps})

        mauc <- colMeans(do.call(rbind, prob))
      }
      oo[[jj]] <- mauc
    }
    aucs[[ii]] <- unlist(oo)
  }
  out$auc <- aucs

  out$best <- Map(function(x, y) x[y], x = model, y = llply(aucs, which.max))

  pc <- mapply(predict, out$best, test,
               MoreArgs = list(type = 'class'), SIMPLIFY = FALSE)
  pp <- vector(mode = 'list', length = length(out$best))
  for(ii in seq(length(out$best))) {
    pp[[ii]] <- predict(out$best[[ii]], test[[ii]], type = 'probs')
  }
  out$class <- mapply(function(x, y) cbind(pred = x, as.data.frame(y)), 
                      x = pc, y = pp, SIMPLIFY = FALSE)
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
  out$auc <- lapply(model, function(x) x$results$ROC)
  #  out$re <- resamples(model)
  out$class <- mapply(predict, model, test,
                      SIMPLIFY = FALSE)
  out$conf <- Map(function(x, y) confusionMatrix(x$pred, y),
                  x = out$class, y = class)
  out
}

# take out important values from linear discriminate analysis results
lda.analysis <- function(model, train, test, class) {
  out <- list()

  # need to choose a best model
  # do this based on within test auc
  pr <- list()
  for(ii in seq(length(model))) {
    p <- list()
    for(jj in seq(length(model[[ii]]))) {
      guess <- predict(model[[ii]][[jj]], 
                       train[[ii]][, colnames(model[[ii]][[jj]]$means)])
      p[[jj]] <- guess
    }
    pr[[ii]] <- p
  }

  tauc <- function(pred, obs, pred.res) {
    prob <- lapply(levels(pred), function(cl) {
                   pp <- ifelse(pred == cl, 1, 0)
                   oo <- ifelse(obs == cl, 1, 0)
                   prob <- pred.res[, cl]

                   ps <- Metrics::auc(oo, prob)
                   ps})
    mauc <- colMeans(do.call(rbind, prob))
    mauc
  }
  aucs <- Map(function(predic, cla, gr) {
              unlist(lapply(predic, function(x) tauc(x$class, cla[, gr], 
                                                     pred.res = x$posterior)))},
              predic = pr, cla = train, gr = class)

  out$auc <- aucs
  out$best <- Map(function(x, y) x[[y]], model, lapply(aucs, which.max))

  gr <- list()
  for(ii in seq(length(out$best))) {
    guess <- predict(out$best[[ii]],
                     test[[ii]][, colnames(out$best[[ii]]$means)])
    gr[[ii]] <- cbind(guess$class, guess$posterior)
  }
  out$class <- gr
  out$class <- lapply(out$class, data.frame)

  fix.names <- function(cc) {
    ff <- apply(cc, 1, function(x) {
                nn <- names(x[-1])
                nn[x[1]]})
    if(all(grepl('X', ff))) {
      ff <- gsub('X', '', ff)
    }
    cc[, 1] <- ff
    names(cc)[-1] <- gsub('X', '', names(cc)[-1])
    cc
  }
  out$class <- lapply(out$class, fix.names)

  out
}

