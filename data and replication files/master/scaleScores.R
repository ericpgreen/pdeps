# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Peridep
# Scale scores
# .............................................................................
# runs from analysis file
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# subset dataframe by round of data collection ================================
# rd defined in analysis file
  tmp <- dat[dat$time==rd, ]
  #tmp <- tmp[!is.na(tmp$date),]

# misc functions ==============================================================
  options(scipen=999) # turn off scientific notation for numbers

# examine missing data ========================================================
# create object to store results ----------------------------------------------
  miss.item <- as.data.frame(NULL)
  m <- 1
# start loop ------------------------------------------------------------------
  for (s in scales) {
    # subset items
    list <- get(paste("key.list", s, sep="."))   # get all 
    items <- gsub("-", "", unique(unlist(list)))
    scale <- tmp[,items]
    for (i in 1:length(items)) {
      miss.item[m,1] <- colnames(scale[i])
      miss.item[m,2] <- as.numeric(colSums(is.na(scale[i])))
      m <- m+1
    }
  }
    
# save results ----------------------------------------------------------------
  names(miss.item) <- c("scales", "missing") 
  miss.item <- subset(miss.item, !is.na(miss.item$scales))
  gc()
  write.xlsx(x = miss.item, 
             file = paste("data and replication files/master/output/tables/item.miss",
                          rd, "xlsx", sep="."),
             sheetName = "missing", row.names = FALSE)

  
# create scale scores =========================================================
# create objects to store results ---------------------------------------------
  scale.scores <- data.frame(matrix(NA, 
                                    nrow = nrow(tmp), 
                                    ncol = 0))
  scale.scores$pid <- tmp$pid
# create object to receive alpha results
  rel.ic <- as.data.frame(NULL)
# create object to receive expected range
  range <- as.data.frame(NULL)
  r <- 1
# create object to receive valence
  valence <- as.data.frame(NULL)
  v <- 1
# create object to receive missing 
  miss.scale <- as.data.frame(NULL)
  m <- 1    
# standardize
#   any scales that contain items on different metrics need to be standardized
#   list them in this vector
  needsStand <- c()
# score type
  scoreavg <- 0    # 1 will create an average; 0 will create a sum score
# plot
  plot_list = list()
# start loop ------------------------------------------------------------------
  for (s in scales) {
    # subset items
      list <- get(paste("key.list", s, sep="."))    
      items <- gsub("-", "", unique(unlist(list)))
      scale <- tmp[,items]
    # plot responses 
    # TODO -- FOLLOWING IS NOT REQUIRED AND IS NOT WORKING FOR EPDS
      # add back dropped levels
        levellist <- list()
        maxlevels <- list()
        for (i in 1:length(scale)) {
          levellist[[i]] <- list(attributes(scale[, i])$levels)
          maxlevels[[i]] <- length(attributes(scale[, i])$levels)
        }
        indexLevel <-
          min(which((unlist(maxlevels)==max(unlist(maxlevels)))==TRUE))
        levels <- unlist(levellist[indexLevel])
        scale[] <- lapply(scale, `levels<-`, levels)
      # plot
        like <- likert(scale)
        summary(like)
        # dghi colors
        orange <- "#f09906"
        blue <- "#04598fff"
        green <- "#849a0bff"
        darkgrey <- "#888888ff"
        red <- "#cc4125"
        p <- plot(like, low.color=blue, high.color=green, 
                  neutral.color="grey90", text.size=1.75) +
          theme(axis.text.y=element_text(colour=darkgrey, size=7),
                strip.text=element_text(colour="black", size=10)) 
        plot_list[[s]] = p
        #remove(levels)
        remove(like)
        remove(p)
    # convert factor to numeric using levels
      scale <- as.data.frame(sapply(scale, 
                                    function(x) as.numeric(levels(x))[x]))
    # standardize  # todo check this because not used
      if (s %in% needsStand) { # if defined as needing standardization
        # make keys
        keys <- make.keys(scale, list)
        #   Normally scoreItems is used to reverse items, but by the
        #   time we run scoreItems below, items have already been
        #   standardized, so reversing won't work.
        #   Instead, for scales that require standardized items, we 
        #   reverse here prior to scoreItems
        #   IMPORTANT: this code looks for any "-" items in the key list
        #     and redefines the key to reverse code according to the new
        #     rules; this will NOT work if a key list has multiple scales
        #     in which the same item appears on multiple scales with
        #     different direction assignments. For instance, if y.cdi1
        #     is defined on scale1 as + and as "-" on scale2, this code
        #     will see the any() "-" and reverse the scores for y.cdi1
        keysreverse <- as.numeric(apply(keys, 1, 
                                        function(x){any(x==-1)}))
        keysreverse <- ifelse(keysreverse==1, -1, 1)
        keys <- cbind(keys, keysreverse)
        # reverse code items that need to be standardized
        input <- as.data.frame(reverse.code(keys[,ncol(keys)], 
                                            as.matrix(scale)))
        names(input) <- gsub("-", "", names(input))
        remove(scale)
        # standardize
        #   Items in input dataframe have been reverse coded in previous 
        #   step as instructed in the key list
        scale <- data.frame(sapply(input, scale))
        remove(input)
        # re-make keys with all positives
        #   In a later step, we'll call scoreItems to create scale scores
        #   using instructions in the key lists. For scales requiring
        #   standardized items, we we've already reverse coded where
        #   indicated, so we need to remove all references to reverse
        #   coding in the key lists. 
        list <- lapply(list, FUN = function(x) gsub("-", "", x))
        keys <- make.keys(scale, list)
        remove(list)
      } else {
      # make keys 
        keys <- make.keys(scale, list)
        remove(list)
      }
    # create scale scores
    # code will impute mean of given responses if resp is mising some
    # missing all will remain NA
      if (scoreavg==1) { # creates average scale scores
        scores <- scoreItems(keys, scale, totals=FALSE, missing=TRUE,
                             impute="none")
      } else {
        scores <- scoreItems(keys, scale, totals=T, missing=TRUE, impute="median")
      }
      tot <- as.data.frame(scores$scores)
      tot.miss <- tot # will use this below
    # capture item correlation/covariance matrix
      if (s==scales[1]) {  # first time through create file
      # covariance
        item.covmat <- as.data.frame(cov(scale, 
                                         use="pairwise.complete.obs", 
                                         method="pearson"))
        gc()
        write.xlsx(x = item.covmat, 
                   file = paste("data and replication files/master/output/tables/item.covmat",
                                rd, "xlsx", sep="."),
                   sheetName = s, 
                   row.names = TRUE)
        remove(item.covmat)
      # correlation
        item.cormat <- as.data.frame(cor(scale, 
                                         use="pairwise.complete.obs", 
                                         method="pearson"))
        gc()
        write.xlsx(x = item.cormat, 
                   file = paste("data and replication files/master/output/tables/item.cormat",
                                rd, "xlsx", sep="."),
                   sheetName = s, 
                   row.names = TRUE)
        remove(item.cormat)
#       # polychoric correlations
#         if (s %in% needsStand | any(apply(scale[,1:length(scale)], 
#                                           2,
#                                           function(x) length(table(x)))>5)) {
#           print("standardized")
#           #  we don't need to run this on standardized items
#         } else {
#           item.pchmat <- try(as.data.frame(polychoric(scale, 
#                                                   na.rm=TRUE)[1]))
#           gc()
#           try(write.xlsx(x = item.pchmat, 
#                      file = paste("output/tables/item.polycormat",
#                                   rd, "xlsx", sep="."),
#                      sheetName = s, 
#                      row.names = TRUE))
#           try(remove(item.pchmat))
#         }
      } else if (s!=scales[1]) { # will now append results
      # covariance
        item.covmat <- as.data.frame(cov(scale, 
                                         use="pairwise.complete.obs", 
                                         method="pearson"))
        gc()
        write.xlsx(x = item.covmat, 
                   file = paste("data and replication files/master/output/tables/item.covmat",
                                rd, "xlsx", sep="."),
                   sheetName = s, 
                   row.names = TRUE,
                   append=TRUE)
        remove(item.covmat)
      # correlation
        item.cormat <- as.data.frame(cor(scale, 
                                         use="pairwise.complete.obs", 
                                         method="pearson"))
        gc()
        write.xlsx(x = item.cormat, 
                   file = paste("data and replication files/master/output/tables/item.cormat",
                                rd, "xlsx", sep="."),
                   sheetName = s, 
                   row.names = TRUE,
                   append=TRUE)
        remove(item.cormat)
#       # polychoric correlations
#         if (s %in% needsStand | any(apply(scale[,1:length(scale)], 
#                                           2,
#                                           function(x) length(table(x)))>5)) {
#           print("standardized")
#         } else {
#           item.pchmat <- try(as.data.frame(polychoric(scale, 
#                                                   na.rm=TRUE)[1]))
#           gc()
#           try(write.xlsx(x = item.pchmat, 
#                      file = paste("output/tables/item.polycormat",
#                                   rd, "xlsx", sep="."),
#                      sheetName = s, 
#                      row.names = TRUE,
#                      append=TRUE))
#           try(remove(item.pchmat))
#         }
      }
      
    # capture item-by-scale correlations
      item.corrected <- as.data.frame(scores$item.corrected)
      if (s==scales[1]) {
        gc()
        write.xlsx(x = item.corrected, 
                   file = paste("data and replication files/master/output/tables/item.corrected",
                                rd, "xlsx", sep="."),
                   sheetName = s, 
                   row.names = TRUE)
        remove(item.corrected)
      } else {
        gc()
        write.xlsx(x = item.corrected, 
                   file = paste("data and replication files/master/output/tables/item.corrected",
                                rd, "xlsx", sep="."),
                   sheetName = s, 
                   row.names = TRUE,
                   append=TRUE)
        remove(item.corrected)
      }
    # capture response frequencies by item
      missing <- as.data.frame(scores$response.freq)
    # append if not first run
      if (s %in% needsStand) {
        print("standardized")
      } else {
      if (nrow(missing)>0) {
        write.xlsx(x = missing, 
                   file = paste("data and replication files/master/output/tables/item.miss",
                                 rd, "xlsx", sep="."),
                   sheetName = paste("missing", s, sep="-"), 
                   row.names = TRUE,
                   append=TRUE)
        }
      }
      remove(missing)
    # capture item means and sd
      if (s==scales[1]) {
        mean <- as.data.frame(apply(scale, 2, mean, na.rm=TRUE))
        sd <- as.data.frame(apply(scale, 2, sd, na.rm=TRUE))
        itemdestemp <- cbind(mean, sd)
        names(itemdestemp) <- c("mean", "sd")
        assign("itemdes", itemdestemp)
        remove(itemdestemp)
      } else {
        mean <- as.data.frame(apply(scale, 2, mean, na.rm=TRUE))
        sd <- as.data.frame(apply(scale, 2, sd, na.rm=TRUE))
        itemdestemp <- cbind(mean, sd)
        names(itemdestemp) <- c("mean", "sd")
        itemdes <- rbind(get("itemdes"), itemdestemp)
        #assign("itemdes", itemdes)
        remove(itemdestemp)
      }              
    # capture missing by scale
      m.r <- m
      for (i in 1:length(tot)) {                 
        miss.scale[m.r,1] <- colnames(tot.miss[i])            # scale
        m.r <- m.r + 1
        # turn off code to impute median for missing
        #medval <- quantile(tot[i], .50, na.rm=TRUE)
        #tot[,i][is.na(tot[,i])] <- medval
      }
      # add indicator for completed by round
      tot$comp <- 1                                   # add comp indicator
      tot.comp <- sum(tot$comp, na.rm=TRUE)
      m.r <- m                                        # reset start
      for (i in 1:(length(tot)-1)) {  
        tot[,i][is.na(tot$comp)] <- NA                # set no round to NA
        miss.scale[m.r,2] <- tot.comp                 # N
        miss.scale[m.r,3] <- sum(!is.na(tot[i]))      # obs
        m.r <- m.r + 1
      }
    # drop completed var
      tot$comp <- NULL
    #remove(tot.comp)
      remove(tot.miss)
    # add pid
      tot$pid <- tmp$pid   # can do this because impute="none doesn't drop
    # compile scale scores
      scale.scores <- merge(scale.scores, tot, by="pid", all.x=TRUE)
    # extract alpha
      n.items <- data.frame(scores$n.items)
      cor <- data.frame(scores$cor)                 # need this for names
      results <- as.data.frame(row.names(cor))
      colnames(results) <- c("scales")
      results$n.items <- n.items$scores.n.items     # number of items used  
      alpha <- data.frame(scores$alpha)             # get alpha.raw
      alpha <- data.frame(t(alpha))                 # transform rows/cols
      G6 <- data.frame(scores$G6)                   # Guttman's Lambda 6
      G6 <- data.frame(t(G6))
      av.r <- data.frame(scores$av.r)               # avg corr in scale
      av.r <- data.frame(t(av.r))
      des <- NULL
      des <- as.data.frame(des)
      for (i in 1:(length(tot)-1)) {
        des[1,i] <- mean(tot[,i], na.rm=TRUE)             # mean
        des[2,i] <- sd(tot[,i], na.rm=TRUE)               # sd
        des[3,i] <- skew(tot[,i], na.rm=TRUE, type=3)     # skew
        des[4,i] <- kurtosi(tot[,i], na.rm=TRUE, type=3)  # kurtosis
        des[5,i] <- min(tot[,i], na.rm=TRUE)              # min observed
        des[6,i] <- max(tot[,i], na.rm=TRUE)              # max observed
      }
      des <- data.frame(t(des))
      colnames(des) <- c("mean", "sd", "skew", "kurtosis",
                         "min", "max")
      results <- cbind(results, alpha, G6, av.r, des)
      rel.ic <- rbind(rel.ic, results)
    # clean up
      remove(items)
      remove(keys)
      remove(scores)
      remove(tot)
      remove(alpha)
      remove(des)
      remove(results)
      remove(av.r)
      remove(G6)
      remove(n.items)
      remove(cor)
      remove(scale)
    # get valence of scale scores
      for (n in 1:length(names(get(paste("dir.list", s, sep="."))))) {
        valence[v, 1] <- names(get(paste("dir.list", s, sep=".")))[n]
        valence[v, 2] <- get(paste("dir.list", s, sep="."))[[n]]
        v <- v + 1
      }
    # get expected min and max from earlier code extracting factor attributes
      for (n in 1:length(names(get(paste("min.list", s, sep="."))))) {
        range[r, 1] <- names(get(paste("min.list", s, sep=".")))[n]
        range[r, 2] <- get(paste("min.list", s, sep="."))[[n]]
        range[r, 3] <- get(paste("max.list", s, sep="."))[[n]]
        r <- r + 1
      }
    # increase counter for miss.scale round
      m <- m.r
  }
  
  # print plots
  pdf(paste("data and replication files/master/output/figures/responses", rd, "pdf", sep="."),
      width=5)
  print(plot_list)
  dev.off()

  
# complete tables -------------------------------------------------------------
# merge valence with rel.ic
  names(valence) <- c("scales", "valence")
  rel.ic <- merge(rel.ic, valence, by="scales", all.x=TRUE)
  remove(valence)
# merge range with rel.ic
  names(range) <- c("scales", "possMin", "possMax")
  rel.ic <- merge(rel.ic, range, by="scales", all.x=TRUE)
  remove(range)
  # if z, then z; if average scoring, then min/max if total, then multiply
  if (scoreavg==1) {
    rel.ic$possMin <- ifelse(rel.ic$possMin=="z", 
                             "z",
                             as.numeric(rel.ic$possMin))
    rel.ic$possMax <- ifelse(rel.ic$possMax=="z", 
                             "z",
                             as.numeric(rel.ic$possMax))
  } else {
    rel.ic$possMin <- ifelse(rel.ic$possMin=="z", 
                             "z",
                             as.numeric(rel.ic$possMin) * rel.ic$n.items)
    rel.ic$possMax <- ifelse(rel.ic$possMax=="z", 
                             "z",
                             as.numeric(rel.ic$possMax) * rel.ic$n.items)
  }
  
# merge miss with rel.ic
  names(miss.scale) <- c("scales", "N", "obs")
  miss.scale <- subset(miss.scale, !is.na(miss.scale$scales))
  rel.ic <- merge(rel.ic, miss.scale, by=c("scales"), all.x=TRUE)
  remove(miss.scale)

# save ========================================================================
# excel files
  gc()
  write.xlsx(x = rel.ic, 
             file = paste("data and replication files/master/output/tables/scales", 
                          rd, "xlsx", sep="."),
             sheetName = "alphas", row.names = FALSE)
#   this is the first worksheet in scales.who.rd, so no append
  gc()
  write.xlsx(x = itemdes, 
             file = paste("data and replication files/master/output/tables/item.des", 
                          rd, "xlsx", sep="."),
             sheetName = "descriptives", 
             row.names = TRUE)
  gc()

# add scale scores to dat
  tmp <- merge(tmp, scale.scores, by="pid", all.x=T)
  assign(paste0("tmp", rd), tmp)
  remove(tmp)