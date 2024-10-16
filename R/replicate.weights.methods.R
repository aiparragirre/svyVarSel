
# dCV ---------------------------------------------------------------------


cv.folds <- function(data, k, weights, strata=NULL, cluster=NULL, R=1, rw.test, dCV.sw.test){

  if(is.null(cluster) & !is.null(strata)){
    data$cluster <- 1:nrow(data)
    cluster <- "cluster"
  } else {
    if(!is.null(cluster) & is.null(strata)){
      data$strata <- rep(1, nrow(data))
      strata <- "strata"
    } else {
      if(is.null(cluster) & is.null(strata)){
        data$strata <- rep(1, nrow(data))
        data$cluster <- 1:nrow(data)
        strata <- "strata"
        cluster <- "cluster"
      }
    }
  }

  for(r in 1:R){

    data[,paste0("folds_",r)] <- f.folds(data, k=k, strata=strata, cluster=cluster)

    for(kk in 1:k){
      data[,paste0("rw_r_",r,"_train_", kk)] <- repl.weights(data,
                                                             folds = paste0("folds_",r),
                                                             test.fold=kk,
                                                             weights,
                                                             strata,
                                                             cluster)
    }

    if(rw.test==TRUE & dCV.sw.test == FALSE){
      for(kk in 1:k){
        data[,paste0("rw_r_",r,"_test_", kk)] <- repl.weights.test(data,
                                                                   folds = paste0("folds_",r),
                                                                   test.fold=kk,
                                                                   weights,
                                                                   strata,
                                                                   cluster)

      }
    }


  }

  return(data)

}



f.folds <- function(data, k=5, strata=NULL, cluster=NULL){

  data$hclus <- interaction(data[,strata], data[,cluster], drop=TRUE)
  newids <- levels(data$hclus)
  n <- length(newids)

  v.folds.newids <- sample(cut(seq(1,n),breaks=k,labels=FALSE))
  names(v.folds.newids) <- newids
  v.folds <- v.folds.newids[match(data$hclus, names(v.folds.newids))]

  h.fold <- table(data[,strata], v.folds)!=0
  h.fold.sum <- apply(h.fold, 1, sum)
  h.onefold <- names(which(h.fold.sum==1))
  if(length(h.onefold)!=0){
    for(hh in h.onefold){
      kk <- which(h.fold[hh,]==1)
      id.h <- which(data[,strata]==hh)
      psu.h <- unique(data$hclus[id.h])
      selected.psu <- sample(psu.h, 1)
      newk <- sample(c(1:k)[-kk], 1)
      id.selected.psu <- which(data$hclus==selected.psu)
      v.folds[id.selected.psu] <- newk
    }
  }



  return(v.folds)

}


repl.weights <- function(data, folds, test.fold, weights, strata=NULL, cluster=NULL){

  v.repl.weights <- rep(0, nrow(data))

  id.test <- which(data[,folds]==test.fold)

  data[,strata] <- as.factor(data[,strata])

  str.clus <- table(data[,strata], data[,cluster])!=0
  str.clus.test <- table(data[id.test,strata], data[id.test,cluster])!=0

  v.mh <- apply(str.clus.test, 1, sum)
  v.nh <- apply(str.clus, 1, sum)
  coef <- v.nh/(v.nh - v.mh)

  v.repl.weights[-id.test] <- data[-id.test,weights]*coef[match(data[-id.test,strata], names(coef))]


  return(v.repl.weights)

}

repl.weights.test <- function(data, folds, test.fold, weights, strata=NULL, cluster=NULL){

  v.repl.weights <- rep(0, nrow(data))

  id.test <- which(data[,folds]!=test.fold)

  data[,strata] <- as.factor(data[,strata])

  str.clus <- table(data[,strata], data[,cluster])!=0
  str.clus.test <- table(data[id.test,strata], data[id.test,cluster])!=0

  v.mh <- apply(str.clus.test, 1, sum)
  v.nh <- apply(str.clus, 1, sum)
  coef <- v.nh/(v.nh - v.mh)

  v.repl.weights[-id.test] <- data[-id.test,weights]*coef[match(data[-id.test,strata], names(coef))]


  return(v.repl.weights)

}



# Split-sample ------------------------------------------------------------


rw.split <- function(data, train.prob, method = c("dCV", "bootstrap", "subbootstrap"),
                     weights, strata = NULL, cluster = NULL, R = 1, rw.test){

  if(is.null(cluster) & !is.null(strata)){
    data$cluster <- 1:nrow(data)
    cluster <- "cluster"
  } else {
    if(!is.null(cluster) & is.null(strata)){
      data$strata <- rep(1, nrow(data))
      strata <- "strata"
    } else {
      if(is.null(cluster) & is.null(strata)){
        data$strata <- rep(1, nrow(data))
        data$cluster <- 1:nrow(data)
        strata <- "strata"
        cluster <- "cluster"
      }
    }
  }

  for(r in 1:R){

    data <- splitsample(data, train.prob, r, strata, cluster)
    tags <- as.vector(unique(data[,paste0("set_",r)]))

    if(method == "dCV"){

      if(rw.test == FALSE){
        data[,paste0("rw_r_",r,"_", "train")] <- repl.weights.test(data,
                                                                   folds = paste0("set_",r),
                                                                   test.fold = "train",
                                                                   weights, strata, cluster)
      } else {

        for(tag in tags){
          data[,paste0("rw_r_",r,"_", tag)] <- repl.weights.test(data,
                                                                 folds = paste0("set_",r),
                                                                 test.fold = tag,
                                                                 weights, strata, cluster)
        }

      }



    } else {

      if(method %in% c("bootstrap", "subbootstrap")){

        if(rw.test == FALSE){
          data <- replicate.sample(data, set = paste0("set_",r), "train",
                                   strata, weights, r,
                                   boot.type = method)
        } else {

          for(tag in tags){

            data <- replicate.sample(data, set = paste0("set_",r), tag,
                                     strata, weights, r,
                                     boot.type = method)

          }


        }


      }

    }

  }


  return(data)

}



splitsample <- function(data, train.prob, r,
                         strata = NULL, cluster = NULL){

  data[,strata] <- as.factor(data[,strata])

  set <- paste0("set_",r)

  hclus <- NULL
  data$hclus <- interaction(data[,strata], data[,cluster], drop=TRUE)
  newids <- levels(data$hclus)
  n <- length(newids)

  factor <- c(0, train.prob, 1)
  set.newids <- sample(cut(seq(1,n)/n, factor, labels = c("train", "test")))
  names(set.newids) <- newids
  data[,set] <- as.factor(set.newids[match(data$hclus, names(set.newids))])

  train.0 <- table(data[which(data[,set]=="train"), strata])==0
  if(sum(train.0) != 0){
    h.0 <- which(train.0 == 1)
    for(hh in h.0){
      id.hh <- which(data[,strata]==hh)
      psu.h <- unique(data$hclus[id.hh])
      selected.psu <- sample(psu.h, size=1)
      id.selected.psu <- which(data$hclus==selected.psu)
      data[id.selected.psu,set] <- "train"
    }
  }

  test.0 <- table(data[which(data[,set]=="test"), strata])==0
  if(sum(test.0) != 0){
    h.0 <- which(test.0 == 1)
    for(hh in h.0){
      id.hh <- which(data[,strata]==hh)
      psu.h <- unique(data$hclus[id.hh])
      selected.psu <- sample(psu.h, size=1)
      id.selected.psu <- which(data$hclus==selected.psu)
      data[id.selected.psu,set] <- "test"
    }
  }

  data <- subset(data, select = -hclus)

  return(data)

}



replicate.sample <- function(data, set, tag, strata, weights, r=1,
                             boot.type = c("bootstrap", "subbootstrap")){

  data[,paste0("bootrep_r_",r,"_",tag)] <- rep(0, nrow(data))
  data[which(data[,set] == tag),paste0("bootrep_r_",r,"_",tag)] <- 1

  nh0 <- table(data[,strata])
  if(boot.type == "bootstrap"){
    new.nh <- nh0
  } else {
    if(boot.type == "subbootstrap"){
      new.nh <- nh0 - 1
    }
  }

  nh0.tag <- table(data[which(data[,set] == tag), strata])

  for(hh in 1:length(unique(data[,strata]))){

    if(nh0.tag[hh] < new.nh[hh]){

      n.add <- new.nh[hh] - nh0.tag[hh]
      id.opt <- which(data[,set] == tag & data[,strata] == hh)
      if(length(id.opt)>1){
        selected.id <- sample(id.opt, size = n.add, replace = TRUE)
      } else {
        selected.id <- rep(id.opt, n.add)
      }
      n.adds <- table(selected.id)
      data[as.numeric(names(table(selected.id))),paste0("bootrep_r_",r,"_",tag)] <- data[as.numeric(names(table(selected.id))),paste0("bootrep_r_",r,"_",tag)] + n.adds

    }

  }

  coef.h <- nh0/new.nh
  coef <- coef.h[match(data[,strata], names(coef.h))]

  data[,paste0("rw_r_",r,"_", tag)] <- data[, weights]*data[,paste0("bootrep_r_",r,"_",tag)]*coef

  # Delete bootstrap repetition columns
  col.bootrep <- grep("bootrep_", colnames(data))
  data <- data[,-col.bootrep]

  return(data)

}



# Extrapolation -----------------------------------------------------------


splitstrata <- function(data, train.prob, strata = NULL, weights, R = 1, rw.test){

  if(is.null(strata)){stop("Extrapolation method cannot be applied if strata are not defined")}

  h <- unique(data[,strata])

  for(r in 1:R){

    number.h <- floor(length(h)*train.prob)
    train.h <- sample(1:length(h), number.h)

    h.split <- vector(length = length(h))
    names(h.split) <- h

    h.split[train.h] <- "train"
    h.split[-train.h] <- "test"

    data[, paste0("set_", r)] <- as.vector(h.split[match(data[,strata], names(h.split))])

    data[, paste0("rw_r_",r,"_train")] <- rep(0, nrow(data))
    id.train <- which(data[,paste0("set_",r)] == "train")
    data[id.train, paste0("rw_r_",r,"_train")] <- data[id.train, weights]

    if(rw.test == TRUE){
      data[, paste0("rw_r_",r,"_test")] <- rep(0, nrow(data))
      id.test <- which(data[,paste0("set_",r)] == "test")
      data[id.test, paste0("rw_r_",r,"_test")] <- data[id.test, weights]
    }


  }


  return(data)

}

