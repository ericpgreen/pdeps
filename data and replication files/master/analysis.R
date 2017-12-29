# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# peridep 2015
# analysis
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# SETUP
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# paths =======================================================================
# for voice analysis
  #path <- "../../../projects/dghi-peridep master/not shared"
  #voice <- paste0(path, "/data/raw/voice/trimmed/valid")
  
# packages ====================================================================
  
  library(xlsx)
  options(java.parameters = "-Xmx1000m") # for xlsx
  library(lubridate)
  library(xtable)
  library(likert)
  library(psych)
  library(pheatmap)
  library(foreach)
  library(doParallel)
  library(OptimalCutpoints)
  library(hexbin)
  library(plotROC)
  library(cocor)
  library(irr)
  library(stargazer)
  library(car)
  library(reshape2)
  library(plyr)

# functions ===================================================================
# rounding functions to keep trailing zeros
  rd0 <- function(y) sprintf("%.0f", round(y, 0))
  rd1 <- function(y) sprintf("%.1f", round(y, 1))
  rd2 <- function(y) sprintf("%.2f", round(y, 2))
  rd3 <- function(y) sprintf("%.3f", round(y, 3))
  options(scipen=999)
# returns string w/o leading or trailing whitespace
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# import data =================================================================
# measure development ---------------------------------------------------------
  gc()
  fgdTrack <- read.xlsx("data and replication files/master/input/fgd.xlsx", 
                        sheetName="tracking", 
                        stringsAsFactors=FALSE)
  fgdDemo <- read.xlsx("data and replication files/master/input/fgd.xlsx", 
                       sheetName="demo", 
                       stringsAsFactors=FALSE)
  fgdMatch <- read.xlsx("data and replication files/master/input/fgd.xlsx", 
                        sheetName="match", 
                        stringsAsFactors=FALSE)
  fgdNew <- read.xlsx("data and replication files/master/input/fgd.xlsx", 
                      sheetName="new", 
                      stringsAsFactors=FALSE)
  measures <- read.xlsx("data and replication files/master/input/measures.xlsx", 
                        sheetName="measures", 
                        stringsAsFactors=FALSE)
  items <- read.xlsx("data and replication files/master/input/measures.xlsx", 
                     sheetName="items", 
                     stringsAsFactors=FALSE)
  itemsU <- read.xlsx("data and replication files/master/input/measures.xlsx", 
                      sheetName="unique", 
                      stringsAsFactors=FALSE)
# validation ------------------------------------------------------------------
  load("data and replication files/master/input/masterRetestP.RData")
  load("data and replication files/master/input/masterRetestT.RData")
  load("data and replication files/master/input/masterSurvey.RData")
  load("data and replication files/master/input/masterInt.RData")
  fun <- read.csv("data and replication files/master/input/functioning.csv", 
                  stringsAsFactors = F)
  epds <- read.csv("data and replication files/master/input/epds.csv", 
                   stringsAsFactors = F)
  # import dictionaries from final Excel versions
  gc()
  dd.measures <- read.xlsx2("data and replication files/master/input/dictionary.xlsx", 
                            sheetName="measures")
  dd.items <- read.xlsx2("data and replication files/master/input/dictionary.xlsx", 
                         sheetName="items")
  dd.survey <- read.xlsx2("data and replication files/master/input/dictionary.xlsx", 
                          sheetName="survey")
  dd.choices <- read.xlsx2("data and replication files/master/input/dictionary.xlsx", 
                           sheetName="choices")
  gc()

# *****************************************************************************
# 
# --------------------------- MEASURE DEVELOPMENT -----------------------------
#
# *****************************************************************************

    
# admin =======================================================================
# subset demo to keep
  fgdDemo <- fgdDemo[fgdDemo$keep==1,]
# merge in date and format
  fgdDemo <- merge(fgdDemo, fgdTrack, by="groupID", all.x=TRUE)
  fgdDemo$date <- ymd(fgdDemo$date)
  minDate <- min(fgdDemo$date)
  maxDate <- max(fgdDemo$date)
  spanDays <- maxDate-minDate
  
# measures ====================================================================
  measures <- measures[!is.na(measures[,1]),]
  measures <- measures[measures$abbreviation!="scid",]            # drop scid
  measures <- measures[order(measures$measure),]                  # sort
  measures$measure <- trim(measures$measure)
# items
  gc()
  items <- items[!is.na(items[,2]),]
  items <- items[order(items$itemnum),]                           # sort
# count items
  itemsMeasure <- data.frame(aggregate(itemnum ~ measure, 
                                       data=items, FUN=length))
  names(itemsMeasure)[names(itemsMeasure)=="measure"] <- "abbreviation"
  measures <- merge(measures, itemsMeasure, by="abbreviation", all.x=T)
  measures <- measures[order(-measures$itemnum),]
  measures <- measures[measures$abbreviation!="pdss",] # not included
# remove prams and bpds (and scid)
  items <- items[items$measure!="pramsc" & items$measure!="pramss" &
                 items$measure!="scid" & items$measure!="bpds" &
                 items$measure!="pdss",]
  itemsObj <- nrow(items)
  itemsUObj <- nrow(itemsU)
# export table
  t <- xtable(measures[,c("measure", "itemnum")])
  #digits(t) <- c(0,0)
  f <- "data and replication files/master/output/tables/measures.tex"
  print(t,
        only.contents=TRUE,
        include.rownames=FALSE,
        include.colnames=FALSE,
        hline.after=NULL,
        type="latex",
        file=f,
        sanitize.text.function=identity
  )
  
# focus group summary stats ===================================================
  nGroups <- length(unique(fgdDemo$groupID))
  nGroupsW <- length(unique(fgdDemo[fgdDemo$healthFGD==0,]$groupID))
  nGroupsH <- length(unique(fgdDemo[fgdDemo$healthFGD==1,]$groupID))
  nCHW <- sum(fgdDemo$healthFGD)
  nWomen <- nrow(fgdDemo)-sum(fgdDemo$healthFGD)
  ageWM <- mean(fgdDemo[fgdDemo$healthFGD==0,]$age)
  ageWSD <- sd(fgdDemo[fgdDemo$healthFGD==0,]$age)
  ageHM <- mean(fgdDemo[fgdDemo$healthFGD==1,]$age)
  ageHSD <- sd(fgdDemo[fgdDemo$healthFGD==1,]$age)
  femaleH <- mean(fgdDemo[fgdDemo$healthFGD==1,]$female)*100
  grpSize <- aggregate(keep ~ groupID, data = fgdDemo, FUN = length)
  grpSizeM <- mean(grpSize$keep)
  grpSizeSD <- sd(grpSize$keep)
  fgdDemo$fgdCompSec <- ifelse(fgdDemo$eduComp=="College" | 
                               fgdDemo$eduComp=="Form 4" |
                               fgdDemo$eduComp=="Form 4 ", 1, 0)
  fgdCompSecH <- table(fgdDemo$fgdCompSec, fgdDemo$who)[2,1]
  fgdCompSecW <- table(fgdDemo$fgdCompSec, fgdDemo$who)[2,2]
  
# create separate dataframes for women and chw data ===========================
# identify group IDs associated with women/chw
  fgdTrack <- merge(fgdTrack, fgdDemo[,c("groupID", "healthFGD")],
                    all.x = TRUE)
  fgdTrack <- fgdTrack[!duplicated(fgdTrack$groupID),]
  fgdTrack <- fgdTrack[!is.na(fgdTrack$healthFGD),]
# add group ID to new
  fgdNew$groupID <- substring(fgdNew$noteNum, 1, 3)
# identify columns to keep
  grpH <- c("concept", "existing", "noteNum", "groupID",
            fgdTrack[fgdTrack$healthFGD==1,]$groupID)
  grpW <- c("concept", "existing", "noteNum", "groupID",
            fgdTrack[fgdTrack$healthFGD==0,]$groupID)
# "match" means the "match" worksheet, not that there was a match necessarily
  # cell values represent codes
  ##  0 = no opinion
  ##  1 = match
  ##  2 = yes, this is a characteristic
  ##  3 = maybe a characteristic
  ##  4 = no, not a characteristic
  fgdMatchW <- fgdMatch[,names(fgdMatch) %in% grpW]
  fgdMatchH <- fgdMatch[,names(fgdMatch) %in% grpH]
  # "new" means the new items worksheet
  fgdNewW <- fgdNew[,names(fgdNew) %in% grpW]
  fgdNewH <- fgdNew[,names(fgdNew) %in% grpH]
  
# free listing ================================================================
# matches with cover terms based on existing screening items ------------------
# health workers
  matchH <- data.frame(cbind(table(fgdMatchH$g04),
                             table(fgdMatchH$g05),
                             table(fgdMatchH$g06),
                             table(fgdMatchH$g07),
                             table(fgdMatchH$g08),
                             table(fgdMatchH$g10)))
  matchH$order <- c(5,1,2,3,4)
  matchH <- matchH[order(matchH$order),]
  matchH$order <- NULL
  matchHM <- mean(as.numeric(matchH[1,]))
  matchHSD <- sd(as.numeric(matchH[1,]))
# women
  matchW <- data.frame(cbind(table(fgdMatchW$g02),
                             table(fgdMatchW$g09)))
  matchW[5,] <- c(0,0)
  matchWM <- mean(as.numeric(matchW[1,]))
  matchWSD <- sd(as.numeric(matchW[1,]))

# new -------------------------------------------------------------------------
# health workers
  newH <- colSums(fgdNewH[, c("g04", "g05", "g06", "g07", "g08", "g10")],
                  na.rm=T)          # count new cards by group
  newHT <- data.frame(t(newH))      # transform
# number of cards generated is matches plus local non-matches
  listedH <- rbind(as.numeric(matchH[1,]),          # count of matches by group
                   as.numeric(as.character(unlist(newHT[1,]))))  # ct new cards
# total (new + matches) by group
  listedHTot <- colSums(listedH)  # by group
  listedHTotM <- mean(listedHTot)
  listedHTotSD <- sd(listedHTot)
# percent of listed cards matches by group
  listedMatchesByH <- (matchH[1,]/listedHTot)*100
  listedMatchesByHM <- mean(as.numeric(listedMatchesByH))
# percent of listed card matches
  listedMatchesH <- (sum(matchH[1,])/sum(listedHTot))*100
# rejected cards mean
  rejectedHM <- mean(as.numeric((matchH[4,]/itemsUObj)*100))
  
# women
  newW <- colSums(fgdNewW[, c("g02", "g09")],
                  na.rm=T)          # count new cards by group
  newWT <- data.frame(t(newW))
  listedW <- rbind(as.numeric(matchW[1,]), 
                   as.numeric(as.character(unlist(newWT[1,]))))
# total (new + matches)
  listedWTot <- colSums(listedW)
  listedWTotM <- mean(listedWTot)
  listedWTotSD <- sd(listedWTot)
# rejected cards
  rejectedWM <- mean(as.numeric((matchW[4,]/itemsUObj)*100))
  
# card sort ===================================================================
# create dataframes of matches and yes endorsements ---------------------------
# chw
  matchHY <- matchH[1:2,]
  matchHYTot <- colSums(matchHY)
  matchHYTotM <- mean(matchHYTot)
  matchHYTotSD <- sd(matchHYTot)
# women
  matchWY <- matchW[1:2,]
  matchWYTot <- colSums(matchWY)
  matchWYTotM <- mean(matchWYTot)
  matchWYTotSD <- sd(matchWYTot)
# recode matches
  fgdMatchHR <- fgdMatchH
  for (i in 4:length(fgdMatchHR)) {
    fgdMatchHR[i] <- ifelse(fgdMatchHR[,i]==1,4,
                     ifelse(fgdMatchHR[,i]==2,3,
                     ifelse(fgdMatchHR[,i]==3,2,
                     ifelse(fgdMatchHR[,i]==4,-1, 
                     ifelse(fgdMatchHR[,i]==0,0,NA))))) 
  }
  fgdMatchWR <- fgdMatchW
  for (i in 4:length(fgdMatchWR)) {
    fgdMatchWR[i] <- ifelse(fgdMatchWR[,i]==1,4,
                     ifelse(fgdMatchWR[,i]==2,3,
                     ifelse(fgdMatchWR[,i]==3,2,
                     ifelse(fgdMatchWR[,i]==4,-1, 
                     ifelse(fgdMatchWR[,i]==0,0,NA))))) 
  }
# compare scores on most common 54 --------------------------------------------
# chv
  fgdMatchHR54 <- fgdMatchHR[fgdMatchHR$noteNum<55,]
  fgdMatchHR54$score <- rowSums(fgdMatchHR54[,4:length(fgdMatchHR54)],
                                na.rm=TRUE)
  fgdMatchHR54$scoreN <- fgdMatchHR54$score/nGroupsH
  fgdMatchHR54M <- mean(fgdMatchHR54$scoreN)
  fgdMatchHR54SD <- sd(fgdMatchHR54$scoreN)
# women
  fgdMatchWR54 <- fgdMatchWR[fgdMatchWR$noteNum<55,]
  fgdMatchWR54$score <- rowSums(fgdMatchWR54[,4:length(fgdMatchWR54)],
                                na.rm=TRUE)
  fgdMatchWR54$scoreN <- fgdMatchWR54$score/nGroupsW
  fgdMatchWR54M <- mean(fgdMatchWR54$scoreN)
# combine
  fgdMatchHWR54 <- data.frame(cbind(fgdMatchHR54$scoreN, fgdMatchWR54$scoreN))
# correlate chv and women 54
  cor54 <- cor.test(fgdMatchHR54$scoreN, fgdMatchWR54$scoreN)
  cor54df <- cor54$parameter
  cor54r <- cor54$estimate
  cor54p <- cor54$p.value
  cor54p <- ifelse(cor54p<0.001, "<0.001",
            ifelse(cor54p<0.01, "<0.01",
            ifelse(cor54p<0.05, "<0.05",
            ifelse(cor54p>=0.05, rd2(cor54p)))))
  
# calculate scores for all terms using chv group data
  fgdMatchHR$score <- rowSums(fgdMatchHR[,4:length(fgdMatchH)],
                              na.rm=TRUE)
  fgdMatchHR <- fgdMatchHR[order(-fgdMatchHR$score, fgdMatchHR$concept),]
  fgdMatchHR$scoreN <- fgdMatchHR$score/nGroupsH
  matchHScoreM <- mean(fgdMatchHR$scoreN)
  matchHScoreSD <- sd(fgdMatchHR$scoreN)
  
# new items
  fgdNewH$score <- rowSums(fgdNewH[,4:(length(fgdNewH)-1)],
                           na.rm=TRUE)
  fgdNewH <- fgdNewH[order(-fgdNewH$score, fgdNewH$concept),]
  newHScoreM <- mean(fgdNewH$score)
  newHScoreSD <- sd(fgdNewH$score)
  

# *****************************************************************************
# 
# ------------------------------ VALIDITY STUDY -------------------------------
#
# *****************************************************************************
  
  
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# CLEANING/PROCESSING
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
# clean up dictionaries =======================================================
# remove empty rows
  ddobj <- list(dd.survey=dd.survey, 
                dd.choices=dd.choices,
                dd.measures=dd.measures,
                dd.items=dd.items)
  df <- lapply(ddobj, function(x) x[!(is.na(x[1]) | x[1] == ""), ])
  list2env(df, .GlobalEnv)
  remove(df)
# remove dummy columns
  df <- lapply(ddobj, function(x) x[,grep("X", substr(names(x), 1, 1), 
                                          invert=TRUE)])
  list2env(df, .GlobalEnv)
  remove(df)
  
# combine interview and functioning data ======================================
  masterInt <- merge(masterInt, fun, by="pid", all.x=T)
  names(masterInt)[names(masterInt)=="enumeratorID"] <- "interviewerID"
  
# munging in phone retest =====================================================
# shift from 1-4 to 0-3 -------------------------------------------------------
# IVR captured 1-4, but need to shift to 0-3
  masterRetestP[,4:length(masterRetestP)] <- 
    lapply(masterRetestP[,4:length(masterRetestP)],
           FUN = function(x) recode(x, "4=3;3=2;2=1;1=0"))
  
# recode epds 3, 5-10 ---------------------------------------------------------
  epdsRecode <- c("epds3", "epds5", "epds6", "epds7", 
                  "epds8", "epds9", "epds10")
  masterRetestP[,epdsRecode] <- lapply(masterRetestP[,epdsRecode],
                                       FUN = function(x) 
                                       recode(x, "3=0;2=1;1=2;0=3"))
  
# convert to numeric ----------------------------------------------------------
  masterRetestP[,4:length(masterRetestP)] <- 
    lapply(masterRetestP[,4:length(masterRetestP)],
           FUN = function(x) factor(x,
                                    levels=c(0, 1, 2, 3),
                                    labels=c(0, 1, 2, 3),
                                    ordered=T))
  
# add phone indicator ---------------------------------------------------------
  masterRetestP$phoneRetest <- 1
  
# munging in tablet retest ====================================================
# epds coded correctly in dictionary file
  
# convert to factors ==========================================================
#   survey items administered by formhub are listed in dd.survey
#   the type.pick column in dd.survey indicates which items were select_one type
#   for select_one items, response options are identified by the choices column
#   choices are defined in dd.choices, linked to dd.survey by list_name column
#   in dd.choices, label column shows what was presented to respondents
#   however, formhub stores the value in the name column
#   most times name/label are identical and we can use name as the factor LEVEL
#   if stored value in name needs to be replaced with label, replace==1  
  
# do this in test and tablet retest
for (d in c("masterSurvey", "masterRetestT")) {
  raw <- get(d)
  
# select one ------------------------------------------------------------------
# create vector of select_one column names
  tofactor <- as.character(dd.survey$item[dd.survey$type.pick=="select_one"])
  raw.f <- raw[colnames(raw) %in% tofactor]
  raw <- raw[!colnames(raw) %in% tofactor]
# loop through columns to convert responses to factors
  for (i in names(raw.f)) {
    # get name of choice list
    list_name <- as.character(dd.survey$choices[dd.survey$item==i])
    # get name
    levels <- as.character(dd.choices$name[dd.choices$list_name==list_name])
    # get value
    labels <- as.character(dd.choices$value[dd.choices$list_name==list_name])
    # remove dk, refuse, dk_refuse from levels and labels
    levels <- levels[!(levels %in% c("dk", "refuse", 
                                     "dk_refuse", "don't know",
                                     "refuse to answer",
                                     "Don't know/Refuse to answer",
                                     "don't_know", "dk", "Refuse",
                                     "dont_know",
                                     "Dont_knowRefuse_to_answer",
                                     "Don't know/Refuse to answer"))]
    labels <- labels[!(labels %in% c("dk", "refuse", 
                                     "dk_refuse", "don't know",
                                     "refuse to answer",
                                     "Don't know/Refuse to answer",
                                     "don't_know", "dk", "Refuse",
                                     "dont_know",
                                     "Dont_knowRefuse_to_answer",
                                     "Don't know/Refuse to answer"))]
    # get ordered
    ordered <- as.character(dd.choices$ordered[dd.choices$list_name==list_name])
    ordered <- ordered[1]
    # convert responses to factors
    raw.f[i] <- factor(raw.f[,i],
                       levels=levels, 
                       labels=labels,
                       ordered=ordered)
  }
# drop if all NA (administration variables)
  raw.f <- raw.f[,colSums(is.na(raw.f)) != nrow(raw.f)]
# add back to raw
  raw <- cbind(raw, raw.f)
  remove(raw.f)
  assign(d, raw)

# code multi-select variables =================================================
#  multi-select variables are recorded as space-separated responses
#  we take the same approach to coding the response options
#  only we split the responses into different columns and create indicators
  
# multi select ----------------------------------------------------------------
# create vector of select_multiple column names
  multiselect <- 
    as.character(dd.survey$item[dd.survey$type.pick=="select_multiple"])
  raw.ms <- raw[colnames(raw) %in% multiselect]
  raw <- raw[!colnames(raw) %in% multiselect]
# loop through columns to convert responses to factors
  for (i in names(raw.ms)) {
    # get name of choice list
    list_name <- as.character(dd.survey$choices[dd.survey$item==i])
    # get name
    levels <- as.character(dd.choices$name[dd.choices$list_name==list_name])
    # get value
    labels <- as.character(dd.choices$value[dd.choices$list_name==list_name])
    # subset
    temp <- raw.ms[i]
    # split into columns
    temp <- colsplit(temp[,i], " ", 
                     names=paste(i, 
                                 tolower(gsub("[[:punct:]]", 
                                              "", 
                                              labels)), 
                                 sep="."))
    # convert to indicator
    temp <- colwise(function(x) ifelse(x=="", NA, 
                                       ifelse(!is.na(x), 1, 0)))(temp)
    temp[is.na(temp)] <- 0
    # add temp to raw.ms
    raw.ms <- cbind(raw.ms, temp)
    raw.ms[i] <- NULL
  }
  # add back to raw
  raw <- cbind(raw, raw.ms)
  remove(raw.ms)
  assign(d, raw)
  remove(raw)
}

# combine retests =============================================================
  masterRetestT$phoneRetest <- 0
  masterRetest <- rbind(masterRetestP, masterRetestT)

# combine test and retest =====================================================
  overlap <- names(masterSurvey)[names(masterSurvey) %in% names(masterRetest)]
  overlap <- overlap[!(overlap %in% c("pid"))]
  names(masterSurvey)[names(masterSurvey)%in% overlap] <- 
    paste(names(masterSurvey)[names(masterSurvey)%in% overlap], "1", sep=".")
  names(masterRetest)[names(masterRetest)%in% overlap] <- 
    paste(names(masterRetest)[names(masterRetest)%in% overlap], "2", sep=".")
  rawW <- merge(masterSurvey, masterRetest, by="pid", all=T)
  varying <- grep(paste(overlap, collapse="|"), names(rawW))
  raw <- reshape(rawW, 
                 varying = grep(paste(overlap, collapse="|"), names(rawW)), 
                 timevar = "time", 
                 times = c("1", "2"), 
                 direction = "long")
  raw <- raw[order(raw$pid),]
  raw <- raw[!is.na(raw$enumeratorID),]
  
  
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# VARIABLE CONSTRUCTION
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
# create a list to track constructed variables ================================
  cv <- NULL
  
# all raw =====================================================================
  dat <- raw[, c("pid", "time", "phoneRetest")]
  valid <- as.character(dd.survey$item[dd.survey$type.pick=="select_multiple" | dd.survey$type.pick=="select_one" | dd.survey$type.pick=="integer" | dd.survey$type.pick=="decimal" | dd.survey$type.pick=="text"])
  dat <- cbind(dat, raw[colnames(raw) %in% valid])
  dat <- dat[,-1] 
  
# add column to separate raw from constructed variables
  dat$CONSTRUCTED <- "CONSTRUCTED VARIABLES"  
  
# for hawa
  dat.w <- reshape(dat, 
                   timevar="time",
                   idvar="pid",
                   direction="wide")
  dat.w$phoneRetest.1 <- NULL
  #write.csv(dat.w, file=paste0(path, "/data/anon/wide.csv"))
  
# household ===================================================================
# head or joint
  dat$h.hhORjointhh <- ifelse(dat$demo2=="yes" | dat$demo2=="joint",
                              1, 0)
# household size capped at 95 percentile
  dat$h.hhSize.p95 <- ifelse(dat$demo3>quantile(dat$demo3, .95, na.rm=T), 
                             quantile(dat$demo3, .95, na.rm=T),
                             dat$demo3)
# number of members per sleeping room
  dat$h.memsleep <- dat$h.hhSize.p95/dat$dhs6117
# if cooking is done outdoors, set dhs6113 to 0
  dat$dhs6113[is.na(dat$dhs6113)] <- 0
# water source
  dat$h.improvedWater <- ifelse(dat$dhs6102=="unprotected well" |
                                  dat$dhs6102=="unprotected spring" |
                                  dat$dhs6102=="tanker truck" |
                                  dat$dhs6102=="cart with small tank" |
                                  dat$dhs6102=="surface water", 0, 1)
  dat$h.waterMore30Min <- ifelse(dat$dhs6104>30, 1, 0)
# sanitation
  dat$h.sanitation <- ifelse(dat$dhs6107=="flush or pour flush toilet" & 
                             dat$dhs6108=="No", "improved, not shared",
                      ifelse(dat$dhs6107=="pit latrine, ventilated improved" & 
                             dat$dhs6108=="No", "improved, not shared",
                      ifelse(dat$dhs6107=="pit latrine with slab" & 
                             dat$dhs6108=="No", "improved, not shared",
                      ifelse(dat$dhs6107=="flush or pour flush toilet" & 
                             dat$dhs6108=="Yes", "improved, shared",
                      ifelse(dat$dhs6107=="pit latrine, ventilated improved" & 
                             dat$dhs6108=="Yes", "improved, shared",
                      ifelse(dat$dhs6107=="pit latrine with slab" & 
                             dat$dhs6108=="Yes", "improved, shared",
                      ifelse(dat$dhs6107=="composting toilet" & 
                             dat$dhs6108=="Yes", "improved, shared",
                             "non-improved")))))))
  dat$h.improvedSan <- ifelse(dat$h.sanitation=="improved, not shared" |
                              dat$h.sanitation=="improved, shared", 1, 0)
# cooking fuel
  dat$h.solidFuelCook <- ifelse(dat$dhs6111=="coal, lignite" |
                                dat$dhs6111=="charcoal" |
                                dat$dhs6111=="wood" |
                                dat$dhs6111=="straw/shrubs/grass" |
                                dat$dhs6111=="agricultural crop" | 
                                dat$dhs6111=="animal dung", 1, 0)
 
  cv <- rbind(cv, 
              cbind(
                item=c("h.hhORjointhh",
                       "h.hhSize.p95",
                       "h.improvedWater",
                       "h.waterMore30Min",
                       "h.sanitation",
                       "h.improvedSan",
                       "h.solidFuelCook"
                ),
                english=c("head of household or joint head of household",
                          "household size (capped at 95 percentile)",
                          "has improved water source",
                          "takes 30 minutes or more to fetch water",
                          "improved, non-improved, and shared sanitation",
                          "improved sanitation",
                          "cooks with solid fuel"
                ))) 
  
# wealth index ================================================================
# http://dhsprogram.com/programming/wealth%20index/DHS_Wealth_Index_Files.pdf
# http://dhsprogram.com/programming/wealth%20index/Steps_to_constructing_the_new_DHS_Wealth_Index.pdf
  
# import dhs weights ----------------------------------------------------------
  wealth <- read.xlsx2("data and replication files/master/input/wealthindex_dhs_kenya_2008-09.xlsx",
                       sheetName="pca_r")
# drop if not in survey
  wealth <- wealth[wealth$wiName!="",]  
  wealth <- wealth[wealth$svyName!="",] 
# format names
  keep <- as.character(wealth[nchar(as.character(wealth$svyName))<11,]$svyName)
  keep <- keep[-20]
  keep <- keep[!(keep %in% "dhs6111lpg")]
  
# create indicators from factors ----------------------------------------------
  datWI <- with(dat, data.frame(model.matrix(~dhs6102+0)))        # water
# toilet
  # need two sets of variables for shared and not shared
  # shared
  datTShared <- with(dat, data.frame(model.matrix(~dhs6107+0)))
  for (i in 1:length(datTShared)) {
    datTShared[i] <- ifelse(dat$dhs6108==0 | is.na(dat$dhs6108), 
                            0, datTShared[,i])
  }
  names(datTShared) <- paste0(names(datTShared), ".s")
  # not shared
  datTNotShared <- with(dat, data.frame(model.matrix(~dhs6107+0)))
  for (i in 1:length(datTNotShared)) {
    datTNotShared[i] <- ifelse(dat$dhs6108==0 | is.na(dat$dhs6108), 
                               datTNotShared[,i], 0)
  }
  names(datTNotShared) <- paste0(names(datTNotShared), ".ns")
  datWI <- cbind(datWI, datTNotShared, datTShared)                # combine
  datWI <- cbind(datWI, 
                 with(dat, data.frame(model.matrix(~dhs6114+0)))) # floor
  datWI <- cbind(datWI,
                 with(dat, data.frame(model.matrix(~dhs6116+0)))) # walls
  datWI <- cbind(datWI,
                 with(dat, data.frame(model.matrix(~dhs6115+0)))) # roof
  datWI <- cbind(datWI,
                 with(dat, data.frame(model.matrix(~dhs6111+0)))) # cooking
  
# combine response options to match 2008-09 DHS
  datWI$dhs6102bag.water[datWI$dhs6102bottled.water==1] <- 1
  datWI$dhs6102bottled.water <- NULL
  
  datWI$dhs6102tanker.truck[datWI$dhs6102cart.with.small.tank==1] <- 1
  datWI$dhs6102cart.with.small.tank <- NULL
  
  datWI$dhs6102surface.water[datWI$dhs6102river==1] <- 1
  datWI$dhs6102river <- NULL
  
  datWI$dhs6102surface.water[datWI$dhs6102unprotected.spring==1] <- 1
  datWI$dhs6102unprotected.spring <- NULL
  
  datWI$dhs6102surface.water[datWI$dhs6102unprotected.well==1] <- 1
  datWI$dhs6102unprotected.well <- NULL
  
  datWI$ddhs6107other.s[datWI$dhs6107other.ns==1] <- 1
  datWI$dhs6107other.ns <- NULL

  datWI$ddhs6107other.s[datWI$dhs6107hanging.toilet.ns==1] <- 1
  datWI$dhs6107hanging.toilet.ns <- NULL
  
  datWI$ddhs6107other.s[datWI$dhs6107hanging.toilet.s==1] <- 1
  datWI$dhs6107hanging.toilet.s <- NULL
  
  datWI$ddhs6107other.s[datWI$dhs6107bucket.toilet.chumber.toilet.ns==1] <- 1
  datWI$dhs6107bucket.toilet.chumber.toilet.ns <- NULL
  
  datWI$ddhs6107other.s[datWI$dhs6107bucket.toilet.chumber.toilet.s==1] <- 1
  datWI$dhs6107bucket.toilet.chumber.toilet.s <- NULL
  
  datWI$ddhs6107other.s[datWI$dhs6107composting.toilet.ns==1] <- 1
  datWI$dhs6107composting.toilet.ns <- NULL
  
  datWI$ddhs6107other.s[datWI$dhs6107composting.toilet.s==1] <- 1
  datWI$dhs6107composting.toilet.s <- NULL
  
  datWI$dhs6111wood[datWI$dhs6111agricultural.crop==1] <- 1
  datWI$dhs6111agricultural.crop <- NULL
  
  datWI$dhs6111wood[datWI$dhs6111animal.dung==1] <- 1
  datWI$dhs6111animal.dung <- NULL
  
  datWI$dhs6111wood[datWI$dhs6111none==1] <- 1
  datWI$dhs6111none <- NULL
  
  datWI$dhs6111wood[datWI$dhs6111straw.shrubs.grass==1] <- 1
  datWI$dhs6111straw.shrubs.grass <- NULL
  
  datWI$dhs6111lpg[datWI$datWI$dhs6111biogas==1] <- 1
  datWI$dhs6111biogas <- NULL
  
  datWI$dhs6111lpg[datWI$datWI$dhs6111natural.gas==1] <- 1
  datWI$dhs6111natural.gas <- NULL
  
  datWI$dhs6111coal..lignite[datWI$datWI$dhs6111charcoal==1] <- 1
  datWI$dhs6111charcoal <- NULL
  
  datWI$dhs6111coal..lignite[datWI$datWI$dhs6111other==1] <- 1
  datWI$dhs6111other <- NULL
  
  datWI$dhs6114finshed..cement[datWI$dhs6114other==1] <- 1
  datWI$dhs6114other <- NULL
  
  datWI$dhs6114rudimentary..wood.planks[datWI$dhs6114rudimentary..palm.bamboo==1] <- 1
  datWI$dhs6114rudimentary..palm.bamboo <- NULL
  
  datWI$dhs6115finished..calamine.cement.fiber[datWI$dhs6115finished..cement==1] <- 1
  datWI$dhs6115finished..cement <- NULL
  
  datWI$dhs6115rudimentary..iron.sheet[datWI$dhs6115finished..metal==1] <- 1
  datWI$dhs6115finished..metal <- NULL
  
  datWI$dhs6115rudimentary..iron.sheet[datWI$dhs6115finished..wood==1] <- 1
  datWI$dhs6115finished..wood <- NULL
  
  datWI$dhs6115rudimentary..iron.sheet[datWI$dhs6115other==1] <- 1
  datWI$dhs6115other <- NULL
  
  datWI$dhs6115natural..thatch.palm.leaf[datWI$dhs6115no.roof==1] <- 1
  datWI$dhs6115no.roof <- NULL
  
  datWI$dhs6115natural..thatch.palm.leaf[datWI$dhs6115rudimentary..palm.bamboo==1] <- 1
  datWI$dhs6115rudimentary..palm.bamboo <- NULL
  
  datWI$dhs6115rudimentary..rustic.mat[datWI$dhs6115rudimentary..cardboard==1] <- 1
  datWI$dhs6115rudimentary..cardboard <- NULL
  
  datWI$dhs6115rudimentary..rustic.mat[datWI$dhs6115rudimentary..wood.planks==1] <- 1
  datWI$dhs6115rudimentary..wood.planks <- NULL
  
  datWI$dhs6116no.walls[datWI$dhs6116natural..cane.palm.trunks==1] <- 1
  datWI$dhs6116natural..cane.palm.trunks <- NULL
  
  datWI$dhs6116rudimentary..bamboo[datWI$dhs6116rudimentary..bamboo.with.mud==1] <- 1
  datWI$dhs6116rudimentary..bamboo.with.mud <- NULL
  
  datWI$dhs6116rudimentary..plywood[datWI$dhs6116rudimentary..cardboard==1] <- 1
  datWI$dhs6116rudimentary..cardboard <- NULL
  
  datWI$dhs6116rudimentary..plywood[datWI$dhs6116rudimentary..reused.wood==1] <- 1
  datWI$dhs6116rudimentary..reused.wood <- NULL
  
  datWI$dhs6116rudimentary..stone.with.mud[datWI$dhs6116rudimentary..uncovered.adobe==1] <- 1
  datWI$dhs6116rudimentary..uncovered.adobe <- NULL
 
# add categorical indicators to dat
  #dat <- cbind(dat, datWI)
  datWI <- cbind(datWI, dat[keep])
  
# construct index -------------------------------------------------------------
# iteratively construct by adding value of indicator if present or absent
  dat$h.wealthIndex <- 0
  for (i in 1:nrow(wealth)) {
    name <- as.character(wealth$svyName[i])
    n <- which(names(datWI)==name)
    if (length(n)>0) { # skip if level does not appear in data
      for (r in 1:nrow(datWI)) {
        dat$h.wealthIndex[r] <- sum(dat$h.wealthIndex[r], 
          ifelse(as.numeric(as.character(datWI[r,n]))==1, 
                 as.numeric(as.character(wealth$ifyes[wealth$svyName==name])),
                 as.numeric(as.character(wealth$ifno[wealth$svyName==name]))),
          na.rm=TRUE)
      }
    }
  }
# add continuous variable HH members per sleeping room using 2008-09 DHS
#   Standardize the wealth indicator variables for each case by subtracting 
#   the mean givenin the spreadsheet from the case’s value and dividing the 
#   difference by the standard deviation from the spreadsheet. 
#   Multiply the standardized value of each indicator by its component score 
#   coefficient and sum over all the resulting products to get the wealth score
#   for that case.
  dat$h.wealthIndex <- dat$h.wealthIndex + 
    ((dat$h.memsleep-2.6848)/1.81520)*-.037  
# add constant based on assumptions about DHS questions not asked
# sum of assume column
  dat$h.wealthIndex <- dat$h.wealthIndex + -0.239018264
# median impute
  dat$h.wealthIndex <- ifelse(is.na(dat$h.wealthIndex), 
                              quantile(dat$h.wealthIndex, .5, na.rm=TRUE), 
                              dat$h.wealthIndex)
  
# assign to quintile from 2008-09 Kenya DHS
  dat$h.wealthQ <- ifelse(dat$h.wealthIndex<=-.88908, 1,
                   ifelse(dat$h.wealthIndex<=-.6763653, 2,
                   ifelse(dat$h.wealthIndex<=-.2896174, 3,
                   ifelse(dat$h.wealthIndex<=.5629462, 4,
                   ifelse(dat$h.wealthIndex>.5629462, 5, NA)))))
  
  table(dat$h.wealthQ)/nrow(dat)
  #1     2     3     4      5 
  #0.12  0.50  0.31  0.07   0           # sample
  #0.18  0.33  0.25  0.19   0.05        # 2008-09 Kenya DHS Western Province
  #0.12  0.31  0.34  0.17   0.06        # 2014 Kenya DHS Western Province
  
# bottom 2 quintiles
  dat$h.wealthQ12 <- ifelse(dat$h.wealthQ==1 | dat$h.wealthQ==2, 1, 0)
  
  # add to list of constructed variables
  cv <- rbind(cv, cbind(item=c("h.wealthIndex", 
                               "h.wealthQ"),
                        english=c("DHS wealth index score, KDHS 2008-09",
                                  "DHS wealth quintile, KDHS 2008-09")))
  
  
# woman =======================================================================
# parity
  dat$parity <- ifelse(dat$dhs7w201==0 | is.na(dat$dhs7w201), 0,
                       rowSums(cbind(dat$dhs7w203, # kids living with her
                                     dat$dhs7w205, # kids not living with her
                                     dat$dhs7w207), # kids died
                                     na.rm=T))
  dat$parityG0 <- ifelse(dat$parity>0, 1, 0)
# one or more biological kids died
  dat$kidDied <- ifelse(dat$dhs7w206==0 | is.na(dat$dhs7w206), 0, 1)
# years completed
  dat$eduComp <- ifelse(dat$dhs6w16==0, 0,
                 ifelse(dat$dhs6w17_1=="primary",
                        as.numeric(as.character(dat$demo9)),
                 ifelse(dat$dhs6w17_1=="secondary",
                        8 + as.numeric(as.character(dat$demo10)),
                 ifelse(dat$dhs6w17_1=="vocational/trade",
                        8 + as.numeric(as.character(dat$demo11)),
                 ifelse(dat$dhs6w17_1=="higher",
                        8 + 4 + as.numeric(as.character(dat$demo11)), NA)))))
# completed primary
  dat$compPri <- ifelse(dat$eduComp>8, 1, 0)
# Luhya
  dat$luhyaTribe <- ifelse(dat$demo12=="Luhya", 1, 0)
# pentecostal
  dat$pentecostal <- ifelse(dat$demo13=="pentecostal", 1, 0)
# literate
  dat$literate <- ifelse(dat$dhs6w108_3=="able to read whole sentence", 1, 0)
# currently married or cohabiting
  dat$currentMarried <- ifelse(dat$dhs6w601!="no, not in a union", 1, 0)
# ever married
  dat$everMarried <- ifelse(dat$currentMarried==1, 1,
                            ifelse(dat$dhs6w602=="yes, formerly married" |
                                   dat$dhs6w602=="yes, lived with a man", 1,
                                   0))
# polygamous
  dat$polygamous <- ifelse(dat$currentMarried==0, 0,
                           ifelse(dat$dhs6w606=="yes (more than one)", 1, 0))
# total wives in polygamous unions
  dat$numWives <- ifelse(dat$polygamous==1 & !is.na(dat$dhs6w607),
                         dat$dhs6w607, NA)
# work past 7 days
  dat$work7days <- ifelse(dat$dhs6w807==1 | dat$dhs6w808==1, 1, 0)
# working (even if absent past 7 days)
  dat$working <- ifelse(dat$work7days==1 | dat$dhs6w809==1, 1, 0)
# work past 12 months 
  dat$work12mo <- ifelse(dat$working==1 | dat$dhs6w810==1, 1, 0)
# work for money
  dat$workMoney <- ifelse(dat$work12mo==1 & 
                          (dat$dhs6w814=="cash only" | 
                           dat$dhs6w814=="cash and kind"), 1, 0)
# voice in decision making on money matters
  dat$decidesMoney <- ifelse(dat$currentMarried==0, NA,
                      ifelse(dat$dhs6w817=="respondent" |
                             dat$dhs6w817=="respondent and partner jointly",
                             1, 0))
# voice in decision making on household purchases
  dat$decidesHHSpend <- ifelse(dat$currentMarried==0, NA,
                        ifelse(dat$dhs6w821=="respondent" |
                               dat$dhs6w821=="respondent and partner jointly",
                               1, 0))
# breadwinner
  dat$breadwinner <- ifelse(dat$currentMarried==0, NA,
                     ifelse(dat$dhs6w818=="more than partner" |
                            dat$dhs6w818=="about the same" |
                            dat$dhs6w818=="partner has no earnings", 1, 0))
# lived in village less than 3 years
  dat$villageLess3Yr <- ifelse(dat$dhs7w102<3, 1, 0)
# head of household
  dat$headHH <- ifelse(dat$demo2=="yes", 1, 0)
  
  
  cv <- rbind(cv, 
              cbind(
                item=c("parity",
                       "parityG0",
                       "kidDied",
                       "eduComp",
                       "compPri",
                       "pentecostal",
                       "literate",
                       "currentMarried",
                       "everMarried",
                       "polygamous",
                       "numWives",
                       "work7days",
                       "working",
                       "work12mo",
                       "workMoney",
                       "decidesMoney",
                       "decidesHHSpend",
                       "breadwinner",
                       "villageLess3Yr",
                       "headHH"
                ),
                english=c("parity",
                          "ever given birth",
                          "A child died",
                          "years education completed",
                          "completed primary",
                          "pentecostal religion",
                          "literate",
                          "currently married or cohabiting",
                          "ever married",
                          "polygamous household",
                          "number of wives (including respondent)",
                          "worked in the past 7 days",
                          "currently working",
                          "worked in the past 12 months",
                          "works for money",
                          "has a say in how money she earns is spent",
                          "has a say in household spending decisions",
                          "is the family breadwinner",
                          "lived in village less than 3 years",
                          "head of household"
                )))

  
# clincal =====================================================================
# create gold standard
  masterInt$poorFunct <- ifelse(masterInt$functioning<6, 1, 0)
  masterInt$classLocalA <- ifelse(masterInt$poorFunct==1 & 
                                 masterInt$localDx=="yes", 1, 0)
  masterInt$classLocalC <- ifelse(masterInt$localDx=="yes", 1, 0)
  masterInt$classDSM <- ifelse(masterInt$meetsMDE=="yes", 1, 0)
  dat <- merge(dat, masterInt, by="pid", all.x=T)
  
  cv <- rbind(cv, 
              cbind(
                item=c("poorFunct",
                       "classLocalA",
                       "classLocalC",
                       "classDSM"
                ),
                english=c("client rates functioning 5 or less on 10 step ladder",
                          "concordance between client and interviewer",
                          "interviewer classifies client as depressed",
                          "meets DSMV criteria for MDE"
                )))
  
  
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# SCALE SCORES, VERSION 1
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 
# create key.lists for scoreItems() ===========================================
  
  # key.list: in a later step, we use the scoreItems() function of the psych 
  #           package to create scale scores and assess internal consistency 
  #           reliability..to do this, we must first define scoring instructions
  #           in lists...reverse scored items entered as "-y.item"
  # dir.list: for each scale we create, we can indicate what higher scores mean
  #           something good (+) or something bad (-)
  # scale.name: as long as scale.name not defined multiple times, can refer to
  #             this object later
  # min/max.list: possible min and max, taken from raw since unused levels are
  #               dropped in dat
  
# create vector of scales
  scales <- c()
  scaleLabels <- c()
  
# EPDS ------------------------------------------------------------------------
  scale.name <- "epdsTot"
  scales <- c(scales, scale.name)
  scale.labels <- "EPDS, Original"
  scaleLabels <- c(scaleLabels, scale.labels)
  key.list <- list(epdsTot=c(paste0("epds", seq(from=1, to=10, by=1))))
  assign(paste("key.list", scale.name, sep="."), key.list)
  remove(key.list)
  
# EPDS items reverse scored so that higher scores = more depression
  #c("epds3", "epds5", "epds6", "epds7", "epds8", "epds9", "epds10")
  
# create lists indicating the valence of higher scores
# list every element defined in key.list
  dir.list <- list(epdsTot="$-$")
  assign(paste("dir.list", scale.name, sep="."), dir.list)
  remove(dir.list)
  
# extract intended min and max 
  max <- attributes(raw[,"epds1"])$levels
  max <- max[max!="NA"]
  scale.min <- min(as.numeric(max), na.rm=TRUE)
  scale.max <- max(as.numeric(max), na.rm=TRUE)
  min.list <- list(epdsTot=scale.min)
  max.list <- list(epdsTot=scale.max)
  assign(paste("min.list", scale.name, sep="."), min.list)
  assign(paste("max.list", scale.name, sep="."), max.list)
  remove(min.list)
  remove(max.list)
  
# EPDS-R ----------------------------------------------------------------------
# re-wrote the EPDS items to fit the PHQ9 response scale
  scale.name <- "epdsRTot"
  scales <- c(scales, scale.name)
  scale.labels <- "EPDS, Revised"
  scaleLabels <- c(scaleLabels, scale.labels)
  key.list <- list(epdsRTot=c("epds1r", "new2r", "new3r", "new4r", "new45", 
                              "new6r", "new32", "new8r", "new9r", "new10r"))
  assign(paste("key.list", scale.name, sep="."), key.list)
  remove(key.list)

# create lists indicating the valence of higher scores
# list every element defined in key.list
  dir.list <- list(epdsRTot="$-$")
  assign(paste("dir.list", scale.name, sep="."), dir.list)
  remove(dir.list)
  
# extract intended min and max 
  max <- attributes(raw[,"epds1r"])$levels
  max <- max[max!="NA"]
  scale.min <- min(as.numeric(max), na.rm=TRUE)
  scale.max <- max(as.numeric(max), na.rm=TRUE)
  min.list <- list(epdsRTot=scale.min)
  max.list <- list(epdsRTot=scale.max)
  assign(paste("min.list", scale.name, sep="."), min.list)
  assign(paste("max.list", scale.name, sep="."), max.list)
  remove(min.list)
  remove(max.list)
  
# PHQ-9 -----------------------------------------------------------------------
  scale.name <- "phq9Tot"
  scales <- c(scales, scale.name)
  scale.labels <- "PHQ-9"
  scaleLabels <- c(scaleLabels, scale.labels)
  key.list <- list(phq9Tot=c(paste0("phq9", seq(from=1, to=9, by=1))))
  assign(paste("key.list", scale.name, sep="."), key.list)
  remove(key.list)
  
# create lists indicating the valence of higher scores
# list every element defined in key.list
  dir.list <- list(phq9Tot="$-$")
  assign(paste("dir.list", scale.name, sep="."), dir.list)
  remove(dir.list)
  
# extract intended min and max 
  max <- attributes(raw[,"phq91"])$levels
  max <- max[max!="NA"]
  scale.min <- min(as.numeric(max), na.rm=TRUE)
  scale.max <- max(as.numeric(max), na.rm=TRUE)
  min.list <- list(phq9Tot=scale.min)
  max.list <- list(phq9Tot=scale.max)
  assign(paste("min.list", scale.name, sep="."), min.list)
  assign(paste("max.list", scale.name, sep="."), max.list)
  remove(min.list)
  remove(max.list)
  
# all -------------------------------------------------------------------------
  scale.name <- "allPHQ9Format"
  #scales <- c(scales, scale.name)  # comment out to keep from scoring
  features <- c("phq91", 	"phq92", 	"phq93", 	"phq94", 	"phq95",
                "phq96", 	"phq97", 	"phq98", 	"phq99", 	"new42",
                "new34", 	"new10", 	"new6", 	"new68", 	"new168",
                "new16", 	"new54", 	"new75", 	"new35", 	"new32",
                "new101", 	"new137", 	"new153", 	"new41",
                "new43", 	"new12", 	"new8", 	"new3", 	"new27",
                "new2", 	"new106", 	"new78", 	"new139",
                "new38", 	"new30", 	"new45", 	"new26", 	
                "newg0208", 	"newg0501", 	"newg0504",
                "newg0806", 	"newki003", 	"epds1r", 	"new2r",
                "new3r", 	"new4r", 	"new6r", 	"new8r", 	"new9r",
                "new10r")
  key.list <- list(allPHQ9Format=features)
  assign(paste("key.list", scale.name, sep="."), key.list)
  remove(key.list)
  
# create lists indicating the valence of higher scores
# list every element defined in key.list
  dir.list <- list(allPHQ9Format="$-$")
  assign(paste("dir.list", scale.name, sep="."), dir.list)
  remove(dir.list)
  
# extract intended min and max 
  max <- attributes(raw[,"phq91"])$levels
  max <- max[max!="NA"]
  scale.min <- min(as.numeric(max), na.rm=TRUE)
  scale.max <- max(as.numeric(max), na.rm=TRUE)
  min.list <- list(allPHQ9Format=scale.min)
  max.list <- list(allPHQ9Format=scale.max)
  assign(paste("min.list", scale.name, sep="."), min.list)
  assign(paste("max.list", scale.name, sep="."), max.list)
  remove(min.list)
  remove(max.list)
  
# discriminating items --------------------------------------------------------
  scale.name <- "pdepsTot"
  scales <- c(scales, scale.name)
  scale.labels <- "PDEPS"
  scaleLabels <- c(scaleLabels, scale.labels)
  features <- c("epds1",
                "new101",
                "new106",
                "new16",
                "new168",
                "new27",
                "new4r",
                "new75",
                "newg0208",
                "newg0504",
                "newg0806",
                "newki003",
                "phq99",
                "new38",
                "new10r",
                "new2r",
                "new9r",
                "phq98",
                # also discriminating for local clinician diagnosis
                "new2",
                "new45")
  selectedFeatures <- c(#"epds1",
                        #"new101",
                        "new106",
                        #"new16",
                        #"new168",
                        "new4r",
                        "new75",
                        "newg0208",
                        #"newg0504",
                        "newg0806",
                        "newki003",
                        #"phq99",
                        "new38",
                        #"new10r",
                        #"new2r",
                        "new9r",
                        #"phq98",
                        # also discriminating for local clinician diagnosis
                        #"new2",
                        #"new45",
                        "new27")
  key.list <- list(pdepsTot=selectedFeatures)
  assign(paste("key.list", scale.name, sep="."), key.list)
  remove(key.list)
  
# create lists indicating the valence of higher scores
# list every element defined in key.list
  dir.list <- list(pdepsTot="$-$")
  assign(paste("dir.list", scale.name, sep="."), dir.list)
  remove(dir.list)
  
# extract intended min and max 
  max <- attributes(raw[,"phq99"])$levels
  max <- max[max!="NA"]
  scale.min <- min(as.numeric(max), na.rm=TRUE)
  scale.max <- max(as.numeric(max), na.rm=TRUE)
  min.list <- list(pdepsTot=scale.min)
  max.list <- list(pdepsTot=scale.max)
  assign(paste("min.list", scale.name, sep="."), min.list)
  assign(paste("max.list", scale.name, sep="."), max.list)
  remove(min.list)
  remove(max.list)
  
# source file for scoring =====================================================
# setup for scoring
  rd <- 1    # test
  source("data and replication files/master/scaleScores.R")
  rd <- 2    # re-test
  source("data and replication files/master/scaleScores.R")
  dat <- rbind(tmp1, tmp2)
  dat <- dat[order(dat$pid, dat$time), ]
  

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# PARTICIPANT CHARACTERISTICS
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  df <- dat[dat$time==1,]
  
# detailed table ==============================================================
  
# DSM definition --------------------------------------------------------------
  part <- as.data.frame(NULL)
  part[1,1] <- paste0("\\hspace{.5cm}", "\\textit{N}")
  N <- aggregate(df$pid, 
                 by=list(df$dhs7w226, df$classDSM), FUN=length)
  part[1,2] <- N[1,3]
  part[1,3] <- N[2,3]
  part[1,4] <- N[1,3] + N[2,3]
  part[1,5] <- N[3,3]
  part[1,6] <- N[4,3]
  part[1,7] <- N[3,3] + N[4,3]
  part[1,8] <- N[1,3] + N[2,3] + N[3,3] + N[4,3]
  
  part[2,1] <- paste0("\\hspace{.5cm}", "Mean age (SD)")
  ageM <- aggregate(df$demo15, 
                    by=list(df$dhs7w226, df$classDSM), 
                    FUN=mean, na.rm=TRUE)
  ageSD <- aggregate(df$demo15, 
                     by=list(df$dhs7w226, df$classDSM), 
                     FUN=sd, na.rm=TRUE)
  age_M <- aggregate(df$demo15, 
                     by=list(df$classDSM), 
                     FUN=mean, na.rm=TRUE)
  age_SD <- aggregate(df$demo15, 
                      by=list(df$classDSM), 
                      FUN=sd, na.rm=TRUE)
  part[2,2] <- paste0(rd1(ageM[1,3]),                  # pregnant, non-case
                      " (",
                      rd1(ageSD[1,3]),
                      ")")
  part[2,3] <- paste0(rd1(ageM[2,3]),                  # mom, non-case
                      " (",
                      rd1(ageSD[2,3]),
                      ")")        
  part[2,4] <- paste0(rd1(age_M[1,2]),                 # non-case
                      " (",
                      rd1(age_SD[1,2]),
                      ")")
  part[2,5] <- paste0(rd1(ageM[3,3]),                  # pregnant, case
                      " (",
                      rd1(ageSD[3,3]),
                      ")")           
  part[2,6] <- paste0(rd1(ageM[4,3]),                  # mom, case
                      " (",
                      rd1(ageSD[4,3]),
                      ")")
  part[2,7] <- paste0(rd1(age_M[2,2]),                 # case
                      " (",
                      rd1(age_SD[2,2]),
                      ")")
  part[2,8] <- paste0(rd1(mean(df$demo15, na.rm=T)),   # all
                      " (",
                      rd1(sd(df$demo15, na.rm=T)),
                      ")")
  
  part[3,1] <- paste0("\\hspace{.5cm}", "Completed primary school (\\%)")
  eduM <- aggregate(df$compPri, 
                    by=list(df$dhs7w226, df$classDSM), 
                    FUN=mean, na.rm=TRUE)
  edu_M <- aggregate(df$compPri, 
                     by=list(df$classDSM), 
                     FUN=mean, na.rm=TRUE)
  part[3,2] <- rd1(eduM[1,3]*100)                     # pregnant, non-case
  part[3,3] <- rd1(eduM[2,3]*100)                     # mom, non-case
  part[3,4] <- rd1(edu_M[1,2]*100)                    # non-case
  part[3,5] <- rd1(eduM[3,3]*100)                     # pregnant, case
  part[3,6] <- rd1(eduM[4,3]*100)                     # mom, case
  part[3,7] <- rd1(edu_M[2,2]*100)                    # case
  part[3,8] <- rd1(mean(df$compPri, na.rm=T)*100)     # all
  
  part[4,1] <- paste0("\\hspace{.5cm}", "Literate (\\%)")
  litM <- aggregate(df$literate, 
                    by=list(df$dhs7w226, df$classDSM), 
                    FUN=mean, na.rm=TRUE)
  lit_M <- aggregate(df$literate, 
                     by=list(df$classDSM), 
                     FUN=mean, na.rm=TRUE)
  part[4,2] <- rd1(litM[1,3]*100)                     # pregnant, non-case
  part[4,3] <- rd1(litM[2,3]*100)                     # mom, non-case
  part[4,4] <- rd1(lit_M[1,2]*100)                    # non-case
  part[4,5] <- rd1(litM[3,3]*100)                     # pregnant, case
  part[4,6] <- rd1(litM[4,3]*100)                     # mom, case
  part[4,7] <- rd1(lit_M[2,2]*100)                    # case
  part[4,8] <- rd1(mean(df$literate, na.rm=T)*100)    # all
  
  part[5,1] <- paste0("\\hspace{.5cm}", "Married or cohabiting (\\%)")
  mchM <- aggregate(df$currentMarried, 
                    by=list(df$dhs7w226, df$classDSM), 
                    FUN=mean, na.rm=TRUE)
  mch_M <- aggregate(df$currentMarried, 
                     by=list(df$classDSM), 
                     FUN=mean, na.rm=TRUE)
  part[5,2] <- rd1(mchM[1,3]*100)                     # pregnant, non-case
  part[5,3] <- rd1(mchM[2,3]*100)                     # mom, non-case
  part[5,4] <- rd1(mch_M[1,2]*100)                    # non-case
  part[5,5] <- rd1(mchM[3,3]*100)                     # pregnant, case
  part[5,6] <- rd1(mchM[4,3]*100)                     # mom, case
  part[5,7] <- rd1(mch_M[2,2]*100)                    # case
  part[5,8] <- rd1(mean(df$currentMarried, na.rm=T)*100)    # all
  
  part[6,1] <- paste0("\\hspace{.5cm}", "Mean parity (SD)")
  parM <- aggregate(df$parity, 
                    by=list(df$dhs7w226, df$classDSM), 
                    FUN=mean, na.rm=TRUE)
  parSD <- aggregate(df$parity, 
                     by=list(df$dhs7w226, df$classDSM), 
                     FUN=sd, na.rm=TRUE)
  par_M <- aggregate(df$parity, 
                     by=list(df$classDSM), 
                     FUN=mean, na.rm=TRUE)
  par_SD <- aggregate(df$parity, 
                      by=list(df$classDSM), 
                      FUN=sd, na.rm=TRUE)
  part[6,2] <- paste0(rd1(parM[1,3]),                  # pregnant, non-case
                      " (",
                      rd1(parSD[1,3]),
                      ")")
  part[6,3] <- paste0(rd1(parM[2,3]),                  # mom, non-case
                      " (",
                      rd1(parSD[2,3]),
                      ")") 
  part[6,4] <- paste0(rd1(par_M[1,2]),                 # non-case
                      " (",
                      rd1(par_SD[1,2]),
                      ")")
  part[6,5] <- paste0(rd1(parM[3,3]),                  # pregnant, case
                      " (",
                      rd1(parSD[3,3]),
                      ")")           
  part[6,6] <- paste0(rd1(parM[4,3]),                  # mom, case
                      " (",
                      rd1(parSD[4,3]),
                      ")")
  part[6,7] <- paste0(rd1(par_M[2,2]),                 # case
                      " (",
                      rd1(par_SD[2,2]),
                      ")")
  part[6,8] <- paste0(rd1(mean(df$parity, na.rm=T)),   # all
                      " (",
                      rd1(sd(df$parity, na.rm=T)),
                      ")")
  
  part[7,1] <- paste0("\\hspace{.5cm}", "Worked for money past 12 mo (\\%)")
  wrkM <- aggregate(df$workMoney, 
                    by=list(df$dhs7w226, df$classDSM), 
                    FUN=mean, na.rm=TRUE)
  wrk_M <- aggregate(df$workMoney, 
                     by=list(df$classDSM), 
                     FUN=mean, na.rm=TRUE)
  part[7,2] <- rd1(wrkM[1,3]*100)                     # pregnant, non-case
  part[7,3] <- rd1(wrkM[2,3]*100)                     # mom, non-case
  part[7,4] <- rd1(wrk_M[1,2]*100)                    # non-case
  part[7,5] <- rd1(wrkM[3,3]*100)                     # pregnant, case
  part[7,6] <- rd1(wrkM[4,3]*100)                     # mom, case
  part[7,7] <- rd1(wrk_M[2,2]*100)                    # case
  part[7,8] <- rd1(mean(df$workMoney, na.rm=T)*100)   # all
  
  part[8,1] <- paste0("\\hspace{.5cm}", "Head of household (\\%)")
  hhhM <- aggregate(df$headHH, 
                    by=list(df$dhs7w226, df$classDSM), 
                    FUN=mean, na.rm=TRUE)
  hhh_M <- aggregate(df$headHH, 
                     by=list(df$classDSM), 
                     FUN=mean, na.rm=TRUE)
  part[8,2] <- rd1(hhhM[1,3]*100)                     # pregnant, non-case
  part[8,3] <- rd1(hhhM[2,3]*100)                     # mom, non-case
  part[8,4] <- rd1(hhh_M[1,2]*100)                    # non-case
  part[8,5] <- rd1(hhhM[3,3]*100)                     # pregnant, case
  part[8,6] <- rd1(hhhM[4,3]*100)                     # mom, case
  part[8,7] <- rd1(hhh_M[2,2]*100)                    # mom, case
  part[8,8] <- rd1(mean(df$headHH, na.rm=T)*100)      # all
  
  part[9,1] <- paste0("\\hspace{.5cm}", "Mean household size (SD)")
  hhsM <- aggregate(df$demo3, 
                    by=list(df$dhs7w226, df$classDSM), 
                    FUN=mean, na.rm=TRUE)
  hhsSD <- aggregate(df$demo3, 
                     by=list(df$dhs7w226, df$classDSM), 
                     FUN=sd, na.rm=TRUE)
  hhs_M <- aggregate(df$demo3, 
                     by=list(df$classDSM), 
                     FUN=mean, na.rm=TRUE)
  hhs_SD <- aggregate(df$demo3, 
                      by=list(df$classDSM), 
                      FUN=sd, na.rm=TRUE)
  part[9,2] <- paste0(rd1(hhsM[1,3]),                  # pregnant, non-case
                      " (",
                      rd1(hhsSD[1,3]),
                      ")")
  part[9,3] <- paste0(rd1(hhsM[2,3]),                  # mom, non-case
                      " (",
                      rd1(hhsSD[2,3]),
                      ")")  
  part[9,4] <- paste0(rd1(hhs_M[1,2]),                 # non-case
                      " (",
                      rd1(hhs_SD[1,2]),
                      ")")
  part[9,5] <- paste0(rd1(hhsM[3,3]),                  # pregnant, case
                      " (",
                      rd1(hhsSD[3,3]),
                      ")")           
  part[9,6] <- paste0(rd1(hhsM[4,3]),                  # mom, case
                      " (",
                      rd1(hhsSD[4,3]),
                      ")")
  part[9,7] <- paste0(rd1(hhs_M[2,2]),                 #case
                      " (",
                      rd1(hhs_SD[2,2]),
                      ")")
  part[9,8] <- paste0(rd1(mean(df$demo3, na.rm=T)),    # all
                      " (",
                      rd1(sd(df$demo3, na.rm=T)),
                      ")")
  
  part[10,1] <- paste0("\\hspace{.5cm}", 
                       "Poorest wealth quintiles (\\%)")
  bwqM <- aggregate(df$h.wealthQ12, 
                    by=list(df$dhs7w226, df$classDSM), 
                    FUN=mean, na.rm=TRUE)
  bwq_M <- aggregate(df$h.wealthQ12, 
                     by=list(df$classDSM), 
                     FUN=mean, na.rm=TRUE)
  part[10,2] <- rd1(bwqM[1,3]*100)                     # pregnant, non-case
  part[10,3] <- rd1(bwqM[2,3]*100)                     # mom, non-case
  part[10,4] <- rd1(bwq_M[1,2]*100)                    # non-case
  part[10,5] <- rd1(bwqM[3,3]*100)                     # pregnant, case
  part[10,6] <- rd1(bwqM[4,3]*100)                     # mom, case
  part[10,7] <- rd1(bwq_M[2,2]*100)                    # case
  part[10,8] <- rd1(mean(df$h.wealthQ12, na.rm=T)*100) # all
  
  # export table
  t <- xtable(part)
  #digits(t) <- c(0,0)
  f <- "data and replication files/master/output/tables/part.tex"
  print(t,
        only.contents=TRUE,
        include.rownames=FALSE,
        include.colnames=FALSE,
        hline.after=NULL,
        type="latex",
        file=f,
        sanitize.text.function=identity
  )
  
# local definition, concordance -----------------------------------------------
  part2 <- as.data.frame(NULL)
  part2[1,1] <- paste0("\\hspace{.5cm}", "\\textit{N}")
  N <- aggregate(df$pid, 
                 by=list(df$dhs7w226, df$classLocalA), FUN=length)
  part2[1,2] <- N[1,3]
  part2[1,3] <- N[2,3]
  part2[1,4] <- N[1,3] + N[2,3]
  part2[1,5] <- N[3,3]
  part2[1,6] <- N[4,3]
  part2[1,7] <- N[3,3] + N[4,3]
  part2[1,8] <- N[1,3] + N[2,3] + N[3,3] + N[4,3]
  
  part2[2,1] <- paste0("\\hspace{.5cm}", "Mean age (SD)")
  ageM <- aggregate(df$demo15, 
                    by=list(df$dhs7w226, df$classLocalA), 
                    FUN=mean, na.rm=TRUE)
  ageSD <- aggregate(df$demo15, 
                     by=list(df$dhs7w226, df$classLocalA), 
                     FUN=sd, na.rm=TRUE)
  age_M <- aggregate(df$demo15, 
                     by=list(df$classLocalA), 
                     FUN=mean, na.rm=TRUE)
  age_SD <- aggregate(df$demo15, 
                      by=list(df$classLocalA), 
                      FUN=sd, na.rm=TRUE)
  part2[2,2] <- paste0(rd1(ageM[1,3]),                  # pregnant, non-case
                       " (",
                       rd1(ageSD[1,3]),
                       ")")
  part2[2,3] <- paste0(rd1(ageM[2,3]),                  # mom, non-case
                       " (",
                       rd1(ageSD[2,3]),
                       ")")        
  part2[2,4] <- paste0(rd1(age_M[1,2]),                 # non-case
                       " (",
                       rd1(age_SD[1,2]),
                       ")")
  part2[2,5] <- paste0(rd1(ageM[3,3]),                  # pregnant, case
                       " (",
                       rd1(ageSD[3,3]),
                       ")")           
  part2[2,6] <- paste0(rd1(ageM[4,3]),                  # mom, case
                       " (",
                       rd1(ageSD[4,3]),
                       ")")
  part2[2,7] <- paste0(rd1(age_M[2,2]),                 # case
                       " (",
                       rd1(age_SD[2,2]),
                       ")")
  part2[2,8] <- paste0(rd1(mean(df$demo15, na.rm=T)),   # all
                       " (",
                       rd1(sd(df$demo15, na.rm=T)),
                       ")")
  
  part2[3,1] <- paste0("\\hspace{.5cm}", "Completed primary school (\\%)")
  eduM <- aggregate(df$compPri, 
                    by=list(df$dhs7w226, df$classLocalA), 
                    FUN=mean, na.rm=TRUE)
  edu_M <- aggregate(df$compPri, 
                     by=list(df$classLocalA), 
                     FUN=mean, na.rm=TRUE)
  part2[3,2] <- rd1(eduM[1,3]*100)                     # pregnant, non-case
  part2[3,3] <- rd1(eduM[2,3]*100)                     # mom, non-case
  part2[3,4] <- rd1(edu_M[1,2]*100)                    # non-case
  part2[3,5] <- rd1(eduM[3,3]*100)                     # pregnant, case
  part2[3,6] <- rd1(eduM[4,3]*100)                     # mom, case
  part2[3,7] <- rd1(edu_M[2,2]*100)                    # case
  part2[3,8] <- rd1(mean(df$compPri, na.rm=T)*100)     # all
  
  part2[4,1] <- paste0("\\hspace{.5cm}", "Literate (\\%)")
  litM <- aggregate(df$literate, 
                    by=list(df$dhs7w226, df$classLocalA), 
                    FUN=mean, na.rm=TRUE)
  lit_M <- aggregate(df$literate, 
                     by=list(df$classLocalA), 
                     FUN=mean, na.rm=TRUE)
  part2[4,2] <- rd1(litM[1,3]*100)                     # pregnant, non-case
  part2[4,3] <- rd1(litM[2,3]*100)                     # mom, non-case
  part2[4,4] <- rd1(lit_M[1,2]*100)                    # non-case
  part2[4,5] <- rd1(litM[3,3]*100)                     # pregnant, case
  part2[4,6] <- rd1(litM[4,3]*100)                     # mom, case
  part2[4,7] <- rd1(lit_M[2,2]*100)                    # case
  part2[4,8] <- rd1(mean(df$literate, na.rm=T)*100)    # all
  
  part2[5,1] <- paste0("\\hspace{.5cm}", "Married or cohabiting (\\%)")
  mchM <- aggregate(df$currentMarried, 
                    by=list(df$dhs7w226, df$classLocalA), 
                    FUN=mean, na.rm=TRUE)
  mch_M <- aggregate(df$currentMarried, 
                     by=list(df$classLocalA), 
                     FUN=mean, na.rm=TRUE)
  part2[5,2] <- rd1(mchM[1,3]*100)                     # pregnant, non-case
  part2[5,3] <- rd1(mchM[2,3]*100)                     # mom, non-case
  part2[5,4] <- rd1(mch_M[1,2]*100)                    # non-case
  part2[5,5] <- rd1(mchM[3,3]*100)                     # pregnant, case
  part2[5,6] <- rd1(mchM[4,3]*100)                     # mom, case
  part2[5,7] <- rd1(mch_M[2,2]*100)                    # case
  part2[5,8] <- rd1(mean(df$currentMarried, na.rm=T)*100)    # all
  
  part2[6,1] <- paste0("\\hspace{.5cm}", "Mean parity (SD)")
  parM <- aggregate(df$parity, 
                    by=list(df$dhs7w226, df$classLocalA), 
                    FUN=mean, na.rm=TRUE)
  parSD <- aggregate(df$parity, 
                     by=list(df$dhs7w226, df$classLocalA), 
                     FUN=sd, na.rm=TRUE)
  par_M <- aggregate(df$parity, 
                     by=list(df$classLocalA), 
                     FUN=mean, na.rm=TRUE)
  par_SD <- aggregate(df$parity, 
                      by=list(df$classLocalA), 
                      FUN=sd, na.rm=TRUE)
  part2[6,2] <- paste0(rd1(parM[1,3]),                  # pregnant, non-case
                       " (",
                       rd1(parSD[1,3]),
                       ")")
  part2[6,3] <- paste0(rd1(parM[2,3]),                  # mom, non-case
                       " (",
                       rd1(parSD[2,3]),
                       ")") 
  part2[6,4] <- paste0(rd1(par_M[1,2]),                 # non-case
                       " (",
                       rd1(par_SD[1,2]),
                       ")")
  part2[6,5] <- paste0(rd1(parM[3,3]),                  # pregnant, case
                       " (",
                       rd1(parSD[3,3]),
                       ")")           
  part2[6,6] <- paste0(rd1(parM[4,3]),                  # mom, case
                       " (",
                       rd1(parSD[4,3]),
                       ")")
  part2[6,7] <- paste0(rd1(par_M[2,2]),                 # case
                       " (",
                       rd1(par_SD[2,2]),
                       ")")
  part2[6,8] <- paste0(rd1(mean(df$parity, na.rm=T)),   # all
                       " (",
                       rd1(sd(df$parity, na.rm=T)),
                       ")")
  
  part2[7,1] <- paste0("\\hspace{.5cm}", "Worked for money past 12 mo (\\%)")
  wrkM <- aggregate(df$workMoney, 
                    by=list(df$dhs7w226, df$classLocalA), 
                    FUN=mean, na.rm=TRUE)
  wrk_M <- aggregate(df$workMoney, 
                     by=list(df$classLocalA), 
                     FUN=mean, na.rm=TRUE)
  part2[7,2] <- rd1(wrkM[1,3]*100)                     # pregnant, non-case
  part2[7,3] <- rd1(wrkM[2,3]*100)                     # mom, non-case
  part2[7,4] <- rd1(wrk_M[1,2]*100)                    # non-case
  part2[7,5] <- rd1(wrkM[3,3]*100)                     # pregnant, case
  part2[7,6] <- rd1(wrkM[4,3]*100)                     # mom, case
  part2[7,7] <- rd1(wrk_M[2,2]*100)                    # case
  part2[7,8] <- rd1(mean(df$workMoney, na.rm=T)*100)   # all
  
  part2[8,1] <- paste0("\\hspace{.5cm}", "Head of household (\\%)")
  hhhM <- aggregate(df$headHH, 
                    by=list(df$dhs7w226, df$classLocalA), 
                    FUN=mean, na.rm=TRUE)
  hhh_M <- aggregate(df$headHH, 
                     by=list(df$classLocalA), 
                     FUN=mean, na.rm=TRUE)
  part2[8,2] <- rd1(hhhM[1,3]*100)                     # pregnant, non-case
  part2[8,3] <- rd1(hhhM[2,3]*100)                     # mom, non-case
  part2[8,4] <- rd1(hhh_M[1,2]*100)                    # non-case
  part2[8,5] <- rd1(hhhM[3,3]*100)                     # pregnant, case
  part2[8,6] <- rd1(hhhM[4,3]*100)                     # mom, case
  part2[8,7] <- rd1(hhh_M[2,2]*100)                    # mom, case
  part2[8,8] <- rd1(mean(df$headHH, na.rm=T)*100)      # all
  
  part2[9,1] <- paste0("\\hspace{.5cm}", "Mean household size (SD)")
  hhsM <- aggregate(df$demo3, 
                    by=list(df$dhs7w226, df$classLocalA), 
                    FUN=mean, na.rm=TRUE)
  hhsSD <- aggregate(df$demo3, 
                     by=list(df$dhs7w226, df$classLocalA), 
                     FUN=sd, na.rm=TRUE)
  hhs_M <- aggregate(df$demo3, 
                     by=list(df$classLocalA), 
                     FUN=mean, na.rm=TRUE)
  hhs_SD <- aggregate(df$demo3, 
                      by=list(df$classLocalA), 
                      FUN=sd, na.rm=TRUE)
  part2[9,2] <- paste0(rd1(hhsM[1,3]),                  # pregnant, non-case
                       " (",
                       rd1(hhsSD[1,3]),
                       ")")
  part2[9,3] <- paste0(rd1(hhsM[2,3]),                  # mom, non-case
                       " (",
                       rd1(hhsSD[2,3]),
                       ")")  
  part2[9,4] <- paste0(rd1(hhs_M[1,2]),                 # non-case
                       " (",
                       rd1(hhs_SD[1,2]),
                       ")")
  part2[9,5] <- paste0(rd1(hhsM[3,3]),                  # pregnant, case
                       " (",
                       rd1(hhsSD[3,3]),
                       ")")           
  part2[9,6] <- paste0(rd1(hhsM[4,3]),                  # mom, case
                       " (",
                       rd1(hhsSD[4,3]),
                       ")")
  part2[9,7] <- paste0(rd1(hhs_M[2,2]),                 #case
                       " (",
                       rd1(hhs_SD[2,2]),
                       ")")
  part2[9,8] <- paste0(rd1(mean(df$demo3, na.rm=T)),    # all
                       " (",
                       rd1(sd(df$demo3, na.rm=T)),
                       ")")
  
  part2[10,1] <- paste0("\\hspace{.5cm}", 
                        "Poorest wealth quintiles (\\%)")
  bwqM <- aggregate(df$h.wealthQ12, 
                    by=list(df$dhs7w226, df$classLocalA), 
                    FUN=mean, na.rm=TRUE)
  bwq_M <- aggregate(df$h.wealthQ12, 
                     by=list(df$classLocalA), 
                     FUN=mean, na.rm=TRUE)
  part2[10,2] <- rd1(bwqM[1,3]*100)                     # pregnant, non-case
  part2[10,3] <- rd1(bwqM[2,3]*100)                     # mom, non-case
  part2[10,4] <- rd1(bwq_M[1,2]*100)                    # non-case
  part2[10,5] <- rd1(bwqM[3,3]*100)                     # pregnant, case
  part2[10,6] <- rd1(bwqM[4,3]*100)                     # mom, case
  part2[10,7] <- rd1(bwq_M[2,2]*100)                    # case
  part2[10,8] <- rd1(mean(df$h.wealthQ12, na.rm=T)*100) # all
  
  # export table
  t <- xtable(part2)
  #digits(t) <- c(0,0)
  f <- "data and replication files/master/output/tables/part2.tex"
  print(t,
        only.contents=TRUE,
        include.rownames=FALSE,
        include.colnames=FALSE,
        hline.after=NULL,
        type="latex",
        file=f,
        sanitize.text.function=identity
  )
  
# local definition, counselor only --------------------------------------------
  part3 <- as.data.frame(NULL)
  part3[1,1] <- paste0("\\hspace{.5cm}", "\\textit{N}")
  N <- aggregate(df$pid, 
                 by=list(df$dhs7w226, df$classLocalC), FUN=length)
  part3[1,2] <- N[1,3]
  part3[1,3] <- N[2,3]
  part3[1,4] <- N[1,3] + N[2,3]
  part3[1,5] <- N[3,3]
  part3[1,6] <- N[4,3]
  part3[1,7] <- N[3,3] + N[4,3]
  part3[1,8] <- N[1,3] + N[2,3] + N[3,3] + N[4,3]
  
  part3[2,1] <- paste0("\\hspace{.5cm}", "Mean age (SD)")
  ageM <- aggregate(df$demo15, 
                    by=list(df$dhs7w226, df$classLocalC), 
                    FUN=mean, na.rm=TRUE)
  ageSD <- aggregate(df$demo15, 
                     by=list(df$dhs7w226, df$classLocalC), 
                     FUN=sd, na.rm=TRUE)
  age_M <- aggregate(df$demo15, 
                     by=list(df$classLocalC), 
                     FUN=mean, na.rm=TRUE)
  age_SD <- aggregate(df$demo15, 
                      by=list(df$classLocalC), 
                      FUN=sd, na.rm=TRUE)
  part3[2,2] <- paste0(rd1(ageM[1,3]),                  # pregnant, non-case
                       " (",
                       rd1(ageSD[1,3]),
                       ")")
  part3[2,3] <- paste0(rd1(ageM[2,3]),                  # mom, non-case
                       " (",
                       rd1(ageSD[2,3]),
                       ")")        
  part3[2,4] <- paste0(rd1(age_M[1,2]),                 # non-case
                       " (",
                       rd1(age_SD[1,2]),
                       ")")
  part3[2,5] <- paste0(rd1(ageM[3,3]),                  # pregnant, case
                       " (",
                       rd1(ageSD[3,3]),
                       ")")           
  part3[2,6] <- paste0(rd1(ageM[4,3]),                  # mom, case
                       " (",
                       rd1(ageSD[4,3]),
                       ")")
  part3[2,7] <- paste0(rd1(age_M[2,2]),                 # case
                       " (",
                       rd1(age_SD[2,2]),
                       ")")
  part3[2,8] <- paste0(rd1(mean(df$demo15, na.rm=T)),   # all
                       " (",
                       rd1(sd(df$demo15, na.rm=T)),
                       ")")
  
  part3[3,1] <- paste0("\\hspace{.5cm}", "Completed primary school (\\%)")
  eduM <- aggregate(df$compPri, 
                    by=list(df$dhs7w226, df$classLocalC), 
                    FUN=mean, na.rm=TRUE)
  edu_M <- aggregate(df$compPri, 
                     by=list(df$classLocalC), 
                     FUN=mean, na.rm=TRUE)
  part3[3,2] <- rd1(eduM[1,3]*100)                     # pregnant, non-case
  part3[3,3] <- rd1(eduM[2,3]*100)                     # mom, non-case
  part3[3,4] <- rd1(edu_M[1,2]*100)                    # non-case
  part3[3,5] <- rd1(eduM[3,3]*100)                     # pregnant, case
  part3[3,6] <- rd1(eduM[4,3]*100)                     # mom, case
  part3[3,7] <- rd1(edu_M[2,2]*100)                    # case
  part3[3,8] <- rd1(mean(df$compPri, na.rm=T)*100)     # all
  
  part3[4,1] <- paste0("\\hspace{.5cm}", "Literate (\\%)")
  litM <- aggregate(df$literate, 
                    by=list(df$dhs7w226, df$classLocalC), 
                    FUN=mean, na.rm=TRUE)
  lit_M <- aggregate(df$literate, 
                     by=list(df$classLocalC), 
                     FUN=mean, na.rm=TRUE)
  part3[4,2] <- rd1(litM[1,3]*100)                     # pregnant, non-case
  part3[4,3] <- rd1(litM[2,3]*100)                     # mom, non-case
  part3[4,4] <- rd1(lit_M[1,2]*100)                    # non-case
  part3[4,5] <- rd1(litM[3,3]*100)                     # pregnant, case
  part3[4,6] <- rd1(litM[4,3]*100)                     # mom, case
  part3[4,7] <- rd1(lit_M[2,2]*100)                    # case
  part3[4,8] <- rd1(mean(df$literate, na.rm=T)*100)    # all
  
  part3[5,1] <- paste0("\\hspace{.5cm}", "Married or cohabiting (\\%)")
  mchM <- aggregate(df$currentMarried, 
                    by=list(df$dhs7w226, df$classLocalC), 
                    FUN=mean, na.rm=TRUE)
  mch_M <- aggregate(df$currentMarried, 
                     by=list(df$classLocalC), 
                     FUN=mean, na.rm=TRUE)
  part3[5,2] <- rd1(mchM[1,3]*100)                     # pregnant, non-case
  part3[5,3] <- rd1(mchM[2,3]*100)                     # mom, non-case
  part3[5,4] <- rd1(mch_M[1,2]*100)                    # non-case
  part3[5,5] <- rd1(mchM[3,3]*100)                     # pregnant, case
  part3[5,6] <- rd1(mchM[4,3]*100)                     # mom, case
  part3[5,7] <- rd1(mch_M[2,2]*100)                    # case
  part3[5,8] <- rd1(mean(df$currentMarried, na.rm=T)*100)    # all
  
  part3[6,1] <- paste0("\\hspace{.5cm}", "Mean parity (SD)")
  parM <- aggregate(df$parity, 
                    by=list(df$dhs7w226, df$classLocalC), 
                    FUN=mean, na.rm=TRUE)
  parSD <- aggregate(df$parity, 
                     by=list(df$dhs7w226, df$classLocalC), 
                     FUN=sd, na.rm=TRUE)
  par_M <- aggregate(df$parity, 
                     by=list(df$classLocalC), 
                     FUN=mean, na.rm=TRUE)
  par_SD <- aggregate(df$parity, 
                      by=list(df$classLocalC), 
                      FUN=sd, na.rm=TRUE)
  part3[6,2] <- paste0(rd1(parM[1,3]),                  # pregnant, non-case
                       " (",
                       rd1(parSD[1,3]),
                       ")")
  part3[6,3] <- paste0(rd1(parM[2,3]),                  # mom, non-case
                       " (",
                       rd1(parSD[2,3]),
                       ")") 
  part3[6,4] <- paste0(rd1(par_M[1,2]),                 # non-case
                       " (",
                       rd1(par_SD[1,2]),
                       ")")
  part3[6,5] <- paste0(rd1(parM[3,3]),                  # pregnant, case
                       " (",
                       rd1(parSD[3,3]),
                       ")")           
  part3[6,6] <- paste0(rd1(parM[4,3]),                  # mom, case
                       " (",
                       rd1(parSD[4,3]),
                       ")")
  part3[6,7] <- paste0(rd1(par_M[2,2]),                 # case
                       " (",
                       rd1(par_SD[2,2]),
                       ")")
  part3[6,8] <- paste0(rd1(mean(df$parity, na.rm=T)),   # all
                       " (",
                       rd1(sd(df$parity, na.rm=T)),
                       ")")
  
  part3[7,1] <- paste0("\\hspace{.5cm}", "Worked for money past 12 mo (\\%)")
  wrkM <- aggregate(df$workMoney, 
                    by=list(df$dhs7w226, df$classLocalC), 
                    FUN=mean, na.rm=TRUE)
  wrk_M <- aggregate(df$workMoney, 
                     by=list(df$classLocalC), 
                     FUN=mean, na.rm=TRUE)
  part3[7,2] <- rd1(wrkM[1,3]*100)                     # pregnant, non-case
  part3[7,3] <- rd1(wrkM[2,3]*100)                     # mom, non-case
  part3[7,4] <- rd1(wrk_M[1,2]*100)                    # non-case
  part3[7,5] <- rd1(wrkM[3,3]*100)                     # pregnant, case
  part3[7,6] <- rd1(wrkM[4,3]*100)                     # mom, case
  part3[7,7] <- rd1(wrk_M[2,2]*100)                    # case
  part3[7,8] <- rd1(mean(df$workMoney, na.rm=T)*100)   # all
  
  part3[8,1] <- paste0("\\hspace{.5cm}", "Head of household (\\%)")
  hhhM <- aggregate(df$headHH, 
                    by=list(df$dhs7w226, df$classLocalC), 
                    FUN=mean, na.rm=TRUE)
  hhh_M <- aggregate(df$headHH, 
                     by=list(df$classLocalC), 
                     FUN=mean, na.rm=TRUE)
  part3[8,2] <- rd1(hhhM[1,3]*100)                     # pregnant, non-case
  part3[8,3] <- rd1(hhhM[2,3]*100)                     # mom, non-case
  part3[8,4] <- rd1(hhh_M[1,2]*100)                    # non-case
  part3[8,5] <- rd1(hhhM[3,3]*100)                     # pregnant, case
  part3[8,6] <- rd1(hhhM[4,3]*100)                     # mom, case
  part3[8,7] <- rd1(hhh_M[2,2]*100)                    # mom, case
  part3[8,8] <- rd1(mean(df$headHH, na.rm=T)*100)      # all
  
  part3[9,1] <- paste0("\\hspace{.5cm}", "Mean household size (SD)")
  hhsM <- aggregate(df$demo3, 
                    by=list(df$dhs7w226, df$classLocalC), 
                    FUN=mean, na.rm=TRUE)
  hhsSD <- aggregate(df$demo3, 
                     by=list(df$dhs7w226, df$classLocalC), 
                     FUN=sd, na.rm=TRUE)
  hhs_M <- aggregate(df$demo3, 
                     by=list(df$classLocalC), 
                     FUN=mean, na.rm=TRUE)
  hhs_SD <- aggregate(df$demo3, 
                      by=list(df$classLocalC), 
                      FUN=sd, na.rm=TRUE)
  part3[9,2] <- paste0(rd1(hhsM[1,3]),                  # pregnant, non-case
                       " (",
                       rd1(hhsSD[1,3]),
                       ")")
  part3[9,3] <- paste0(rd1(hhsM[2,3]),                  # mom, non-case
                       " (",
                       rd1(hhsSD[2,3]),
                       ")")  
  part3[9,4] <- paste0(rd1(hhs_M[1,2]),                 # non-case
                       " (",
                       rd1(hhs_SD[1,2]),
                       ")")
  part3[9,5] <- paste0(rd1(hhsM[3,3]),                  # pregnant, case
                       " (",
                       rd1(hhsSD[3,3]),
                       ")")           
  part3[9,6] <- paste0(rd1(hhsM[4,3]),                  # mom, case
                       " (",
                       rd1(hhsSD[4,3]),
                       ")")
  part3[9,7] <- paste0(rd1(hhs_M[2,2]),                 #case
                       " (",
                       rd1(hhs_SD[2,2]),
                       ")")
  part3[9,8] <- paste0(rd1(mean(df$demo3, na.rm=T)),    # all
                       " (",
                       rd1(sd(df$demo3, na.rm=T)),
                       ")")
  
  part3[10,1] <- paste0("\\hspace{.5cm}", 
                        "Poorest wealth quintiles (\\%)")
  bwqM <- aggregate(df$h.wealthQ12, 
                    by=list(df$dhs7w226, df$classLocalC), 
                    FUN=mean, na.rm=TRUE)
  bwq_M <- aggregate(df$h.wealthQ12, 
                     by=list(df$classLocalC), 
                     FUN=mean, na.rm=TRUE)
  part3[10,2] <- rd1(bwqM[1,3]*100)                     # pregnant, non-case
  part3[10,3] <- rd1(bwqM[2,3]*100)                     # mom, non-case
  part3[10,4] <- rd1(bwq_M[1,2]*100)                    # non-case
  part3[10,5] <- rd1(bwqM[3,3]*100)                     # pregnant, case
  part3[10,6] <- rd1(bwqM[4,3]*100)                     # mom, case
  part3[10,7] <- rd1(bwq_M[2,2]*100)                    # case
  part3[10,8] <- rd1(mean(df$h.wealthQ12, na.rm=T)*100) # all
  
  # export table
  t <- xtable(part3)
  #digits(t) <- c(0,0)
  f <- "data and replication files/master/output/tables/part3.tex"
  print(t,
        only.contents=TRUE,
        include.rownames=FALSE,
        include.colnames=FALSE,
        hline.after=NULL,
        type="latex",
        file=f,
        sanitize.text.function=identity
  )
  
# simple table with DHS =======================================================
  maternalStat <- aggregate(df$pid, 
                            by=list(df$dhs7w226), 
                            FUN=length)
  pregN <- maternalStat[1,2]
  ppN <- maternalStat[2,2]
  
  part4 <- as.data.frame(NULL)
  part4[1,1] <- "Mean age (SD)"
  ageM <- aggregate(df$demo15, 
                    by=list(df$dhs7w226), 
                    FUN=mean, na.rm=TRUE)
  ageSD <- aggregate(df$demo15, 
                     by=list(df$dhs7w226), 
                     FUN=sd, na.rm=TRUE)
  part4[1,2] <- paste0(rd1(ageM[1,2]),                  # pregnant
                       " (",
                       rd1(ageSD[1,2]),
                       ")")
  part4[1,3] <- paste0(rd1(ageM[1,2]),                   # mom
                       " (",
                       rd1(ageSD[2,2]),
                       ")")           
  part4[1,4] <- paste0(rd1(mean(df$demo15, na.rm=T)),    # all
                       " (",
                       rd1(sd(df$demo15, na.rm=T)),
                       ")")
  part4[1,5] <- ""
  part4[1,6] <- ""
  
  part4[2,1] <- "Completed primary school (\\%)"
  eduM <- aggregate(df$compPri, 
                    by=list(df$dhs7w226), 
                    FUN=mean, na.rm=TRUE)
  part4[2,2] <- rd1(eduM[1,2]*100)                       # pregnant
  part4[2,3] <- rd1(eduM[2,2]*100)                       # mom
  compPri <- rd1(mean(df$compPri, na.rm=T)*100)
  part4[2,4] <- compPri                                  # all
  part4[2,5] <- "58.0"
  part4[2,6] <- "Bungoma County, 2014 KDHS"
  
  part4[3,1] <- "Literate (\\%)"
  litM <- aggregate(df$literate, 
                    by=list(df$dhs7w226), 
                    FUN=mean, na.rm=TRUE)
  part4[3,2] <- rd1(litM[1,2]*100)                      
  part4[3,3] <- rd1(litM[2,2]*100)
  literate <- rd1(mean(df$literate, na.rm=T)*100)
  part4[3,4] <- literate
  part4[3,5] <- "88.7"
  part4[3,6] <- "Bungoma County, 2014 KDHS"
  
  part4[4,1] <- "Married or cohabiting (\\%)"
  mchM <- aggregate(df$currentMarried, 
                    by=list(df$dhs7w226), 
                    FUN=mean, na.rm=TRUE)
  part4[4,2] <- rd1(mchM[1,2]*100)                     
  part4[4,3] <- rd1(mchM[2,2]*100)                     
  part4[4,4] <- rd1(mean(df$currentMarried, na.rm=T)*100) 
  part4[4,5] <- "59.7"
  part4[4,6] <- "National, 2014 KDHS"
  
  part4[5,1] <- "Mean parity (SD)"
  parM <- aggregate(df$parity, 
                    by=list(df$dhs7w226), 
                    FUN=mean, na.rm=TRUE)
  parSD <- aggregate(df$parity, 
                     by=list(df$dhs7w226), 
                     FUN=sd, na.rm=TRUE)
  part4[5,2] <- paste0(rd1(parM[1,2]),                  
                       " (",
                       rd1(parSD[1,2]),
                       ")")
  part4[5,3] <- paste0(rd1(parM[2,2]),                  
                       " (",
                       rd1(parSD[2,2]),
                       ")")    
  paraM <- rd1(mean(df$parity, na.rm=T))
  part4[5,4] <- paste0(paraM,   
                       " (",
                       rd1(sd(df$parity, na.rm=T)),
                       ")")
  part4[5,5] <- "3.8"
  part4[5,6] <- "Currently married women, 2014 KDHS"
  
  part4[6,1] <- "Worked for money past 12 mo (\\%)"
  wrkM <- aggregate(df$workMoney, 
                    by=list(df$dhs7w226), 
                    FUN=mean, na.rm=TRUE)
  part4[6,2] <- rd1(wrkM[1,2]*100)                    
  part4[6,3] <- rd1(wrkM[2,2]*100)                     
  part4[6,4] <- rd1(mean(df$workMoney, na.rm=T)*100) 
  part4[6,5] <- "61.7"
  part4[6,6] <- "Western region, 2014 KDHS"
  
  part4[7,1] <- "Head of household (\\%)"
  hhhM <- aggregate(df$headHH, 
                    by=list(df$dhs7w226), 
                    FUN=mean, na.rm=TRUE)
  part4[7,2] <- rd1(hhhM[1,2]*100)                    
  part4[7,3] <- rd1(hhhM[2,2]*100)                     
  part4[7,4] <- rd1(mean(df$headHH, na.rm=T)*100)
  part4[7,5] <- "35.8"
  part4[7,6] <- "Rural, 2014 KDHS"
  
  part4[8,1] <- "Mean household size (SD)"
  hhsM <- aggregate(df$demo3, 
                    by=list(df$dhs7w226), 
                    FUN=mean, na.rm=TRUE)
  hhsSD <- aggregate(df$demo3, 
                     by=list(df$dhs7w226), 
                     FUN=sd, na.rm=TRUE)
  part4[8,2] <- paste0(rd1(hhsM[1,2]),                  
                       " (",
                       rd1(hhsSD[1,2]),
                       ")")
  part4[8,3] <- paste0(rd1(hhsM[2,2]),                 
                       " (",
                       rd1(hhsSD[2,2]),
                       ")")           
  part4[8,4] <- paste0(rd1(mean(df$demo3, na.rm=T)),   # all
                       " (",
                       rd1(sd(df$demo3, na.rm=T)),
                       ")")
  part4[8,5] <- "4.4"
  part4[8,6] <- "Rural, 2014 KDHS"
  
  part4[9,1] <- "Poorest two wealth quintiles (\\%)"
  bwqM <- aggregate(df$h.wealthQ12, 
                    by=list(df$dhs7w226), 
                    FUN=mean, na.rm=TRUE)
  bwqSD <- aggregate(df$h.wealthQ12, 
                     by=list(df$dhs7w226), 
                     FUN=sd, na.rm=TRUE)
  part4[9,2] <- rd1(bwqM[1,2]*100)                     
  part4[9,3] <- rd1(bwqM[2,2]*100)                     
  part4[9,4] <- rd1(mean(df$h.wealthQ12, na.rm=T)*100)      
  part4[9,5] <- "50.9"
  part4[9,6] <- "Western province, 2008-09 KDHS"
  
  # export table
  t <- xtable(part4)
  #digits(t) <- c(0,0)
  f <- "data and replication files/master/output/tables/part4.tex"
  print(t,
        only.contents=TRUE,
        include.rownames=FALSE,
        include.colnames=FALSE,
        hline.after=NULL,
        type="latex",
        file=f,
        sanitize.text.function=identity
  )
  
# diagnostic ==================================================================
  part5 <- as.data.frame(NULL)
  part5[1,1] <- "A. Mean functioning, counselor rating (0-100)"
  sofM <- aggregate(df$sofas, 
                    by=list(df$dhs7w226), 
                    FUN=mean, na.rm=TRUE)
  sofSD <- aggregate(df$sofas, 
                     by=list(df$dhs7w226), 
                     FUN=sd, na.rm=TRUE)
  part5[1,2] <- paste0(rd1(sofM[1,2]),                  
                       " (",
                       rd1(sofSD[1,2]),
                       ")")
  part5[1,3] <- paste0(rd1(sofM[2,2]),                  
                       " (",
                       rd1(sofSD[2,2]),
                       ")")    
  part5[1,4] <- paste0(rd1(mean(df$sofas, na.rm=T)),
                       " (",
                       rd1(sd(df$sofas, na.rm=T)),
                       ")")
  
  part5[2,1] <- "B. Mean functioning, client rating (1-10)"
  funM <- aggregate(df$functioning, 
                    by=list(df$dhs7w226), 
                    FUN=mean, na.rm=TRUE)
  funSD <- aggregate(df$functioning, 
                     by=list(df$dhs7w226), 
                     FUN=sd, na.rm=TRUE)
  part5[2,2] <- paste0(rd1(funM[1,2]),                  
                       " (",
                       rd1(funSD[1,2]),
                       ")")
  part5[2,3] <- paste0(rd1(funM[2,2]),                  
                       " (",
                       rd1(funSD[2,2]),
                       ")")    
  part5[2,4] <- paste0(rd1(mean(df$functioning, na.rm=T)),
                       " (",
                       rd1(sd(df$functioning, na.rm=T)),
                       ")")
  
  part5[3,1] <- "C. Poor functioning (<6), client rating (\\%)"
  poorFun <- aggregate(df$poorFunct,                  
                       by=list(df$dhs7w226, df$poorFunct), 
                       FUN=length)
  poorFunP <- aggregate(df$poorFunct,                  
                        by=list(df$dhs7w226, df$poorFunct), 
                        FUN=length)
  
  part5[3,2] <- paste0(rd1(poorFun[3,3]),
                       " (",
                       rd1((poorFunP[3,3]/pregN)*100),
                       ")")
  part5[3,3] <- paste0(rd1(poorFun[4,3]),
                       " (",
                       rd1((poorFunP[4,3]/ppN)*100),
                       ")")
  casenessLWP <- rd1(((poorFunP[3,3]+poorFunP[4,3])/(pregN+ppN))*100)
  part5[3,4] <- paste0(rd1(poorFun[3,3]+poorFun[4,3]),
                       " (",
                       casenessLWP,
                       ")")
  
  part5[4,1] <- "D. Depression, counselor's `local' diagnosis (\\%)"
  caseMatLCC <- aggregate(df$classLocalC,               
                          by=list(df$dhs7w226, df$classLocalC), 
                          FUN=length)
  caseMatLoC <- aggregate(df$classLocalC,               
                          by=list(df$dhs7w226), 
                          FUN=sum, na.rm=T)
  
  part5[4,2] <- paste0(rd1(caseMatLCC[3,3]),
                       " (",
                       rd1((caseMatLoC[1,2]/pregN)*100),
                       ")")
  part5[4,3] <- paste0(rd1(caseMatLCC[4,3]),
                       " (",
                       rd1((caseMatLoC[2,2]/ppN)*100),
                       ")")
  casenessLCP <- rd1(((caseMatLoC[1,2]+caseMatLoC[2,2])/(pregN+ppN))*100)
  part5[4,4] <- paste0(rd1(caseMatLCC[3,3]+caseMatLCC[4,3]),
                       " (",
                       casenessLCP,
                       ")")
  
  part5[5,1] <- "E. Depression, local condordance, C and E (\\%)"
  caseMatLoA <- aggregate(df$classLocalA,               
                          by=list(df$dhs7w226), 
                          FUN=sum, na.rm=T)
  part5[5,2] <- rd1((caseMatLoA[1,2]/pregN)*100)         
  part5[5,3] <- rd1((caseMatLoA[2,2]/ppN)*100)
  casenessLAP <- rd1(((caseMatLoA[1,2]+caseMatLoA[2,2])/(pregN+ppN))*100)
  part5[5,4] <- casenessLAP
  
  part5[6,1] <- "F. Depression, DSM-5 (\\%)"
  caseMatDSM <- aggregate(df$classDSM,                   # DSM
                          by=list(df$dhs7w226), 
                          FUN=sum, na.rm=T)
  part5[6,2] <- rd1((caseMatDSM[1,2]/pregN)*100)         # DSM Yes, Preg
  part5[6,3] <- rd1((caseMatDSM[2,2]/ppN)*100)           # DSM Yes, Post
  caseDSMP <- rd1(((caseMatDSM[1,2]+caseMatDSM[2,2])/(pregN+ppN))*100)
  part5[6,4] <- caseDSMP
  
  caseRange <- rd1(
    (((caseMatLoC[1,2]+caseMatLoC[2,2])/(pregN+ppN))*100) -                          (((caseMatLoA[1,2]+caseMatLoA[2,2])/(pregN+ppN))*100)
  )
  
  # export table
  t <- xtable(part5)
  #digits(t) <- c(0,0)
  f <- "data and replication files/master/output/tables/part5.tex"
  print(t,
        only.contents=TRUE,
        include.rownames=FALSE,
        include.colnames=FALSE,
        hline.after=NULL,
        type="latex",
        file=f,
        sanitize.text.function=identity
  )
  
  casenessClientOnly <- rd1(((poorFunP[3,3]+poorFunP[4,3])/(pregN+ppN))*100)

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# DX HEATMAP
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  dx <- dat[dat$time==1, c("classDSM", "classLocalC", "poorFunct")]
  dx <- dx[complete.cases(dx),]
  dx <- dx[order(-dx$classDSM, -dx$classLocalC, -dx$poorFunct),]
  # d3heatmap(dx, labCol=c("DSM", "Counselor", "Client"), colors=c("#d3d3d3", blue), labRow=(rep("", 190)), width=250, height=800, xaxis_font_size=10, show_grid=T, dendrogram = "none")
 
  pheatmap(dx[1:82,], color=c("#d3d3d3", blue),
           border_color=darkgrey,
           cluster_cols=F, 
           cluster_rows=F,
           legend = FALSE,
           cellwidth = 7, cellheight = 7,
           show_rownames=F,
           width=4,
           height=9,
           filename="data and replication files/master/output/figures/heat.pdf",
           fontsize_col=6,
           labels_col = c("DSM", "Counselor", "Client"))
  
# alternative table approach
  dxtbl <- data.frame(label=c("Meets all 3 definitions",
                              "DSM-5 \\& counselor",
                              "DSM-5 \\& client",
                              "Client \\& counselor",
                              "DSM-5 only",
                              "Counselor only",
                              "Client only",
                              "None"),
                      count=rep(NA, 8),
                      per=rep(NA,8),
                      cper=rep(NA,8))
  dxtbl[1,2] <- nrow(dx[dx$classDSM==1 & 
                        dx$classLocalC==1 & 
                        dx$poorFunct==1,])
  dxtbl[2,2] <- nrow(dx[dx$classDSM==1 & 
                        dx$classLocalC==1 & 
                        dx$poorFunct==0,])
  dxtbl[3,2] <- nrow(dx[dx$classDSM==1 & 
                        dx$classLocalC==0 & 
                        dx$poorFunct==1,])
  dxtbl[4,2] <- nrow(dx[dx$classDSM==0 & 
                        dx$classLocalC==1 & 
                        dx$poorFunct==1,])
  dxtbl[5,2] <- nrow(dx[dx$classDSM==1 & 
                        dx$classLocalC==0 & 
                        dx$poorFunct==0,])
  dxtbl[6,2] <- nrow(dx[dx$classDSM==0 & 
                        dx$classLocalC==1 & 
                        dx$poorFunct==0,])
  dxtbl[7,2] <- nrow(dx[dx$classDSM==0 & 
                        dx$classLocalC==0 & 
                        dx$poorFunct==1,])
  dxtbl[8,2] <- nrow(dx[dx$classDSM==0 & 
                        dx$classLocalC==0 & 
                        dx$poorFunct==0,])
  dxtbl$per <- rd1((dxtbl$count/sum(dxtbl$count))*100)
  dxtbl$cper <- rd1(cumsum((dxtbl$count/sum(dxtbl$count))*100))
  # export table
  t <- xtable(dxtbl)
  #digits(t) <- c(0,0)
  f <- "data and replication files/master/output/tables/dxtbl.tex"
  print(t,
        only.contents=TRUE,
        include.rownames=FALSE,
        include.colnames=FALSE,
        hline.after=NULL,
        type="latex",
        file=f,
        sanitize.text.function=identity)
  
  
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ITEM-ANALYSIS
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# gradient ====================================================================
# setup  
  allItems <- NULL
  nonCaseCut <- .25
  gradCut <- .05
# create df of items
  for (s in c("epdsTot", "allPHQ9Format")) {
    list <- get(paste("key.list", s, sep="."))   # get all 
    sItems <- gsub("-", "", unique(unlist(list)))
    allItems <- c(allItems, sItems)
  }
  itemGradient <- as.data.frame(allItems)
  dfnames <- c("sItems")
  for (case in c("classLocalA", "classLocalC", "classDSM")) {
    sItems <- dat[dat$time==1, c(case, allItems)]
    gradient <- as.data.frame(NULL)
  # calculate response frequencies by caseness (assumes 0/1)
    gr <- 1
      for (it in names(sItems)[2:length(sItems)]) {
        fr <- count(sItems, c(case, it))
        gradient[gr, 1] <- it               # item
        gradient[gr, 2] <- 0                # case
        gradient[gr, 3] <- fr[1, 3]         # option 0
        gradient[gr, 4] <- fr[2, 3]         # option 1
        gradient[gr, 5] <- fr[3, 3]         # option 2
        gradient[gr, 6] <- fr[4, 3]         # option 3
        gradient[gr+1, 1] <- it             # item
        gradient[gr+1, 2] <- 1              # case
        gradient[gr+1, 3] <- fr[5, 3]       # option 0
        gradient[gr+1, 4] <- fr[6, 3]       # option 1
        gradient[gr+1, 5] <- fr[7, 3]       # option 2
        gradient[gr+1, 6] <- fr[8, 3]       # option 3
        gr <- gr+2
      }
    names(gradient) <- c("sItems", "case", "r0", "r1", "r2", "r3")
  # calculate item score
    gradient[,3][is.na(gradient[,3])] <- 0
    gradient[,4][is.na(gradient[,4])] <- 0
    gradient[,5][is.na(gradient[,5])] <- 0
    gradient[,6][is.na(gradient[,6])] <- 0
    gradient$n <- gradient[,3] + gradient[,4] + gradient[,5] + gradient[,6]
    gradient$itemScore <- (gradient$r2 + gradient$r3)/gradient$n
    gradient$nonCaseCut <- ifelse(gradient$case==1, NA,
                           ifelse(gradient$itemScore>nonCaseCut, 1, 0))
    assign(paste("gradient", case, sep="."), gradient)
    gc()
    if (case=="classLocalA") {
      write.xlsx(x = gradient, 
                 file = paste("data and replication files/master/output/tables/gradient", 
                              "xlsx", sep="."),
                 sheetName = case, row.names = FALSE)
    } else {
      write.xlsx(x = gradient, 
                 file = paste("data and replication files/master/output/tables/gradient", 
                              "xlsx", sep="."),
                 sheetName = case, row.names = FALSE,
                 append=T)
    }
  # calculate gradient
    grad <- diff(gradient$itemScore)
    grad <- grad[seq(1, length(grad), 2)]
    itemGradient <- data.frame(cbind(itemGradient, grad))
    dfnames <- c(dfnames, case)
    names(itemGradient) <- dfnames
    #names(itemGradient) <- c("item", "gradient")
    itemGradient[,case] <- as.numeric(as.character(itemGradient[,case]))
    remove(gradient)
  }
# add indicator that item is endorsed by non-cases
  itemGradient <- merge(itemGradient, 
                        gradient.classLocalA[gradient.classLocalA$case==0,
                                            c("sItems", "nonCaseCut")],
                        by="sItems",
                        all.x=T)
  names(itemGradient)[names(itemGradient)=="nonCaseCut"] <- "ncc.classLocalA"
  itemGradient <- merge(itemGradient, 
                        gradient.classLocalC[gradient.classLocalC$case==0,
                                             c("sItems", "nonCaseCut")],
                        by="sItems",
                        all.x=T)
  names(itemGradient)[names(itemGradient)=="nonCaseCut"] <- "ncc.classLocalC"
  itemGradient <- merge(itemGradient, 
                        gradient.classDSM[gradient.classDSM$case==0,
                                          c("sItems", "nonCaseCut")],
                        by="sItems",
                        all.x=T)
  names(itemGradient)[names(itemGradient)=="nonCaseCut"] <- "ncc.classDSM"
# identify items to keep
  itemGradient$keep.classLocalA <- ifelse(itemGradient$classLocalA>gradCut & 
                                         itemGradient$ncc.classLocalA==0, 1, 0)
  itemGradient$keep.classLocalC <- ifelse(itemGradient$classLocalC>gradCut & 
                                            itemGradient$ncc.classLocalC==0, 1, 0)
  itemGradient$keep.classDSM <- ifelse(itemGradient$classDSM>gradCut & 
                                       itemGradient$ncc.classDSM==0, 1, 0)
# write to excel
  write.xlsx(x = itemGradient, 
             file = paste("data and replication files/master/output/tables/gradient", 
                          "xlsx", sep="."),
             sheetName = "gradient", 
             row.names = FALSE,
             append=T)

# plot all gradients
  itemGradient <- merge(itemGradient, dd.survey[,c("item", "shortlabel")],
                        by.x="sItems", by.y="item", all.x=T)
  itemGradientT = transform(itemGradient, 
                            Var1 = reorder(shortlabel, classLocalC))
  itemGradientTW <- reshape(itemGradientT,
                            varying=c("classLocalC", "classDSM"),
                            v.names="gradient",
                            timevar="definition",
                            times=c("classLocalC", "classDSM"),
                            direction="long")
  itemGradientTW$cut <- ifelse(itemGradientTW$definition=="classLocalC", 
                               itemGradientTW$ncc.classLocalC,
                        ifelse(itemGradientTW$definition=="classDSM",
                               itemGradientTW$ncc.classDSM, NA))
  itemGradientTW$cut <- factor(itemGradientTW$cut,
                               levels=c(0,1),
                               labels=c("no", "yes")) # no means below cut
  itemGradientTW$plot <- ifelse(itemGradientTW$definition=="classLocalC" &
                                itemGradientTW$cut=="no", 1,
                         ifelse(itemGradientTW$definition=="classLocalC" &
                                itemGradientTW$cut=="yes", 2,
                         ifelse(itemGradientTW$definition=="classDSM" &
                                itemGradientTW$cut=="no", 3,
                         ifelse(itemGradientTW$definition=="classDSM" &
                                itemGradientTW$cut=="yes", 4, NA))))
  p <- ggplot(itemGradientTW, aes(y=gradient, x=Var1,
                                  shape=factor(plot))) +
          geom_point(size=3) +
          scale_shape_manual(values=c(19, 1, 17, 2),
                    name = "diagnosis definition by \n non-case endorsement",
                             labels = c("Local, <25%", "Local, >=25%",
                                        "DSM-5, <25%", "DSM-5, >=25%")) +
  
          geom_hline(yintercept=0.05, linetype=2) +
          geom_hline(yintercept=0, linetype=1) +
          coord_flip() +
          theme_bw() +
          guides(fill=guide_legend(ncol=2)) +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(color = 'black'),
            panel.background = element_blank(),
            legend.position="bottom") +
          xlab("Items") +
          ylab("Gradients") +
          annotate("text", x = 16, y = -.25, label = "more non-cases endorse") +
          annotate("text", x = 16, y = .25, label = "more cases endorse")
  ggsave(p, file="data and replication files/master/output/figures/gradient.pdf", 
         height = 11, width=10)
  
  
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ITEM SELECTION
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  df <- dat[dat$time==1,]
  #   features <- c("phq91", 	"phq92", 	"phq93", 	"phq94", 	"phq95",
  #                 "phq96", 	"phq97", 	"phq98", 	"phq99", 	"new42",
  #                 "new34", 	"new10", 	"new6", 	"new68", 	"new168",
  #                 "new16", 	"new54", 	"new75", 	"new35", 	"new32",
  #                 "new101", 	"new137", 	"new153", 	"new41",
  #                 "new43", 	"new12", 	"new8", 	"new3", 	"new27",
  #                 "new2", 	"new106", 	"new78", 	"new139",
  #                 "new38", 	"new30", 	"new45", 	"new26", 	
  #                 "newg0208", 	"newg0501", 	"newg0504",
  #                 "newg0806", 	"newki003", 	"epds1r", 	"new2r",
  #                 "new3r", 	"new4r", 	"new6r", 	"new8r", 	"new9r",
  #                 "new10r")
  
  df <- df[,c("classDSM", features)] # defined last for new scale
  
# combinations ================================================================
# do not set runCombo to 1 unless you have computing power and time!!!!  
  
  runCombo <- 0
  if (runCombo==1) {
    # would need to run second time with classLocalA
    registerDoParallel(detectCores()-2)  
    df[,2:length(df)] <- lapply(df[,2:length(df)], 
                                function(x) as.character(x))
    df[,2:length(df)] <- lapply(df[,2:length(df)], 
                                function(x) as.numeric(x))
    # outer function
      outer <- function(s, d) {
        p <- combinations(n = length(d)-1, r = s, v = names(d[2:(length(d))]))
        return(p)
      } 
    # inner function
    combo <- function(i, r, p, d) {
      keep <- c("classDSM", p[r,])
      v <- keep[-1]
      d_ <- d[, keep]
      a <- psych::alpha(d_[,2:length(d_)], na.rm=TRUE)      # alpha
      d_$T <- rowSums(d_[,2:length(keep)])
      oc <- summary(optimal.cutpoints(X = "T", 
                                      status = "classDSM",
                                      tag.healthy = 0,
                                      methods = "SpEqualSe",
                                      data = d_,
                                      control = control.cutpoints(),
                                      ci.fit = TRUE,
                                      conf.level = 0.95, 
                                      trace = FALSE))
      dfoc[ri,1] <- i                                       # num vars in set
      dfoc[ri,2] <- r                                       # combo number
      dfoc[ri,3] <- paste(v, collapse=",")                  # var names in set
      dfoc[ri,4] <- as.numeric(a$total[2])                  # std.alpha
      dfoc[ri,5] <- oc$p.table$Global$SpEqualSe[[1]][1]     # cutoff
      dfoc[ri,6] <- oc$p.table$Global$SpEqualSe[[1]][2]     # sen
      dfoc[ri,7] <- oc$p.table$Global$SpEqualSe[[1]][3]     # spe
      dfoc[ri,8] <- oc$p.table$Global$SpEqualSe[[1]][4]     # ppv
      dfoc[ri,9] <- oc$p.table$Global$SpEqualSe[[1]][5]     # npv
      dfoc[ri,10] <- oc$p.table$Global$SpEqualSe[[1]][2,2]  # sen l95
      dfoc[ri,11] <- oc$p.table$Global$SpEqualSe[[1]][2,3]  # sen u95
      dfoc[ri,12] <- oc$p.table$Global$SpEqualSe[[1]][3,2]  # spe l95
      dfoc[ri,13] <- oc$p.table$Global$SpEqualSe[[1]][3,3]  # spe u95
      dfoc[ri,14] <- oc$p.table$Global$SpEqualSe[[1]][4,2]  # ppv l95
      dfoc[ri,15] <- oc$p.table$Global$SpEqualSe[[1]][4,3]  # ppv u95
      dfoc[ri,16] <- oc$p.table$Global$SpEqualSe[[1]][5,2]  # npv l95
      dfoc[ri,17] <- oc$p.table$Global$SpEqualSe[[1]][5,3]  # npv u95
      dfoc[ri,18] <- oc$p.table$Global$SpEqualSe[[1]][8]    # FP
      dfoc[ri,19] <- oc$p.table$Global$SpEqualSe[[1]][9]    # FN
      #dfoc[ri,20] <- dfoc[ri,18]+dfoc[ri,19]                # errors
      #dfoc[ri,21] <- dfoc[ri,20]/nrow(df_[complete.cases(df_),]) # error rate
      dfoc[ri,20] <- oc$p.table$Global$AUC_CI               # auc
      ri <- ri+1
      remove(d_)
      remove(keep)
      remove(v)
      remove(oc)
      return(dfoc)
    }
  
    gc()
    dfoc <- data.frame(NULL)
    ri <- 1
    y <- foreach(s=2:10) %do% {
      p <- outer(s, df)
      x <- foreach(r=1:nrow(p), .combine=rbind) %dopar% combo(s, r, p, df)
    }
    
    classDSM <- do.call(rbind.data.frame, y)
    
    names(classDSM) <- c("numVars", "comboNum", "vars", "alpha", "cutoff", 
                         "sen", "spe", "ppv", "npv", "senl95", "senu95", 
                         "spel95", "speu95", "ppvl95", "ppvu95", "npvl95", 
                         "npvu95", "fp", "fn", "auc")
    
    classDSM$auc_ <- as.numeric(gsub( " .*$", "", classDSM$auc))
  } else {
    load("data and replication files/master/input/classDSM.RData")
  }
  
# error rates and accuracy
  classDSM$errors <- classDSM$fp + classDSM$fn
  classDSM$errorRate <- classDSM$errors/(nrow(df[complete.cases(df),]))
  classDSM$accuracy <- 1-classDSM$errorRate
  
# plot ========================================================================
# DSM
  p <- ggplot(classDSM, aes(x=accuracy, y=alpha)) +
    stat_binhex(bins=50) +
    scale_fill_gradient(low=blue, high=orange) +
    #geom_point(shape=1, alpha = 0.3) +
    facet_wrap(~numVars) +
    geom_vline(xintercept=.85, color=darkgrey, linetype=2) +
    geom_hline(yintercept=.7, color=darkgrey, linetype=2) +
    scale_y_continuous(limits=c(0,1),
                       breaks=c(0, .35, .7, .85)) +
    scale_x_continuous(limits=c(0,1),
                       breaks=c(0, .2, .6, .85)) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = 'black'),
      panel.background = element_blank()) +
    xlab("Accuracy (1-error rate)") +
    ylab("Reliability (Cronbach's Alpha)")
  ggsave(p, file="data and replication files/master/output/figures/aucValpha_DSM.pdf")
  
  # # local 
  #   p <- ggplot(classLocalA, aes(x=accuracy, y=alpha)) +
  #     stat_binhex(bins=50) +
  #     scale_fill_gradient(low=blue, high=orange) +
  #     #geom_point(shape=1, alpha = 0.3) +
  #     facet_wrap(~numVars) +
  #     xlim(0.6,1) +
  #     ylim(0.6,1) +
  #     geom_vline(xintercept=.85, color=darkgrey, linetype=2) +
  #     geom_hline(yintercept=.8, color=darkgrey, linetype=2) +
  #     theme(
  #       panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank(),
  #       panel.border = element_blank(),
  #       axis.line = element_line(color = 'black'),
  #       panel.background = element_blank()) +
  #     xlab("Accuracy (1-error rate)") +
  #     ylab("Reliability (Cronbach's Alpha)")
  #   ggsave(p, file="output/figures/aucValpha_LocalA.pdf")
  
# items above threshold =======================================================
# DSM -------------------------------------------------------------------------
# limits
  alp <- .7
  acc <- .89
  cvDSM <- classDSM[classDSM$accuracy>=acc & 
                    classDSM$alpha>=alp,]
  nBest <- nrow(cvDSM)
  
# item wording ================================================================
  itemWording <- dd.survey[dd.survey$item %in% selectedFeatures,
                           c("itemlabel", "scale", "english", "kiswahili")]
  # export table
  t <- xtable(itemWording)
  #digits(t) <- c(0,0)
  f <- "data and replication files/master/output/tables/itemWording.tex"
  print(t,
        only.contents=TRUE,
        include.rownames=FALSE,
        include.colnames=FALSE,
        hline.after=NULL,
        type="latex",
        file=f,
        sanitize.text.function=identity)
  
# item stats ==================================================================
  df <- dat[dat$time==1,]
  dfitems <- df[, selectedFeatures]
  dfitems[] <- lapply(dfitems, function(x) as.character(x))
  dfitems[] <- lapply(dfitems, function(x) as.numeric(x))
  a <- psych::alpha(dfitems, na.rm=TRUE) # to get item total correlation
  aItemStats <- a$item.stats
  aItemStats$item <- row.names(aItemStats)
  aItemStats <- aItemStats[,c("item", "r.cor")]
  aItemStats$r.cor <- rd2(aItemStats$r.cor)
  
  itemStats <- dd.survey[dd.survey$item %in% selectedFeatures,
                         c("item", "itemlabel", "short")]
  for (i in 1:nrow(itemStats)) {
    v <- as.character(itemStats$item[i])
    itemMDSM <- aggregate(dfitems[,v],               
                          by=list(df$classDSM), 
                          FUN=mean, na.rm=T)
    itemSDDSM <- aggregate(dfitems[,v],               
                           by=list(df$classDSM), 
                           FUN=sd, na.rm=T)
    itemStats[i,4] <- paste0(rd1(itemMDSM[1,2]),           # DSM, non-case
                             " (",
                             rd1(itemSDDSM[1,2]),
                             ")")
    itemStats[i,5] <- paste0(rd1(itemMDSM[2,2]),           # DSM, case
                             " (",
                             rd1(itemSDDSM[2,2]),
                             ")")
    itemMLocalA <- aggregate(dfitems[,v],               
                             by=list(df$classLocalA), 
                             FUN=mean, na.rm=T)
    itemSDLocalA <- aggregate(dfitems[,v],               
                              by=list(df$classLocalA), 
                              FUN=sd, na.rm=T)
    itemStats[i,6] <- paste0(rd1(itemMLocalA[1,2]),         # LocalA, non-case
                             " (",
                             rd1(itemSDLocalA[1,2]),
                             ")")
    itemStats[i,7] <- paste0(rd1(itemMLocalA[2,2]),         # LocalA, case
                             " (",
                             rd1(itemSDLocalA[2,2]),
                             ")")
  }
  itemStats <- merge(itemStats, aItemStats, by="item")
  itemStats <- itemStats[,-1]
  names(itemStats) <- c("item", "label", "DSM.noncase", "DSM.case", 
                        "localA.noncase", "localA.case", "itemTotal")
  
  # export table
  t <- xtable(itemStats)
  #digits(t) <- c(0,0)
  f <- "data and replication files/master/output/tables/itemStats.tex"
  print(t,
        only.contents=TRUE,
        include.rownames=FALSE,
        include.colnames=FALSE,
        hline.after=NULL,
        type="latex",
        file=f,
        sanitize.text.function=identity)
  
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# DIAGNOSTIC VALIDITY
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  df <- dat[dat$time==1,]
  dv <- data.frame(NULL)
  
# determine cuts for EPDS and PHQ9, all =======================================
  ocEPDS <- summary(optimal.cutpoints(X = "epdsTot", 
                                      status = "classDSM",
                                      tag.healthy = 0,
                                      methods = "SpEqualSe",
                                      data = df,
                                      control = control.cutpoints(),
                                      ci.fit = TRUE,
                                      conf.level = 0.95, 
                                      trace = FALSE))
  ocEPDSR <- summary(optimal.cutpoints(X = "epdsRTot", 
                                       status = "classDSM",
                                       tag.healthy = 0,
                                       methods = "SpEqualSe",
                                       data = df,
                                       control = control.cutpoints(),
                                       ci.fit = TRUE,
                                       conf.level = 0.95, 
                                       trace = FALSE))
  ocPHQ9 <- summary(optimal.cutpoints(X = "phq9Tot", 
                                      status = "classDSM",
                                      tag.healthy = 0,
                                      methods = "SpEqualSe",
                                      data = df,
                                      control = control.cutpoints(),
                                      ci.fit = TRUE,
                                      conf.level = 0.95, 
                                      trace = FALSE))
  ocPDEPS <- summary(optimal.cutpoints(X = "pdepsTot", 
                                       status = "classDSM",
                                       tag.healthy = 0,
                                       methods = "SpEqualSe",
                                       data = df,
                                       control = control.cutpoints(),
                                       ci.fit = TRUE,
                                       conf.level = 0.95, 
                                       trace = FALSE))

  cuts <- c(ocEPDS$p.table$Global$SpEqualSe[[1]][1], 
            ocEPDSR$p.table$Global$SpEqualSe[[1]][1], 
            ocPHQ9$p.table$Global$SpEqualSe[[1]][1],
            ocPDEPS$p.table$Global$SpEqualSe[[1]][1])
  # [1] "epdsTot"  "epdsRTot" "phq9Tot"  "pdepsTot"

  for (i in 1:length(scales)) {
    s <- scales[i]
    l <- scaleLabels[i]
    cut <- cuts[i]
    tmp <- data.frame(cbind(s=df[,s], 
                            classDSM=df$classDSM, 
                            classLocalA=df$classLocalA))
    tmp$test <- ifelse(tmp[,1]>=cut, "case", "non-case")
    # confusion matrix
      cmD <- table(tmp$test, tmp$classDSM)
      Dfp <- cmD[1,1]
      Dtp <- cmD[1,2]
      Dfn <- cmD[2,2]
      Dtn <- cmD[2,1]

      cmL <- table(tmp$test, tmp$classLocalA)
      Lfp <- cmL[1,1]
      Ltp <- cmL[1,2]
      Lfn <- cmL[2,2]
      Ltn <- cmL[2,1]
    # auc
      ggrocD <- ggplot(tmp, aes(m = s, d = classDSM)) + geom_roc()
      Dauc <- rd2(calc_auc(ggrocD)[1,3])
      
      ggrocL <- ggplot(tmp, aes(m = s, d = classLocalA)) + geom_roc()
      Lauc <- rd2(calc_auc(ggrocL)[1,3])
      
    dv[i,1] <- l
    dv[i,2] <- rd0(cut)
    dv[i,3] <- rd2(Dtp/(Dtp+Dfn))                          # DSM-5, sen
    dv[i,4] <- rd2(Dtn/(Dtn+Dfp))                          # DSM-5, spe
    dv[i,5] <- rd2(1-((Dfp+Dfn)/sum(cmD)))                 # DSM-5, accuracy
    dv[i,6] <- rd2((Dtp/(Dtp+Dfn))/(1-(Dtn/(Dtn+Dfp))))    # DSM-5, lrp
    dv[i,7] <- rd2((1-(Dtp/(Dtp+Dfn)))/(Dtn/(Dtn+Dfp)))    # DSM-5, lrn
    dv[i,8] <- Dauc                                        # DSM-5 AUC
    dv[i,9] <- rd2(Ltp/(Ltp+Lfn))                          # LocalA, sen
    dv[i,10] <- rd2(Ltn/(Ltn+Lfp))                         # LocalA, spe
    dv[i,11] <- rd2(1-((Lfp+Lfn)/sum(cmL)))                # LocalA, accuracy
    dv[i,12] <- rd2((Ltp/(Ltp+Lfn))/(1-(Ltn/(Ltn+Lfp))))   # LocalA, lrp
    dv[i,13] <- rd2((1-(Ltp/(Ltp+Lfn)))/(Ltn/(Ltn+Lfp)))   # LocalA, lrn
    dv[i,14] <- Lauc                                       # LocalA AUC
  }
  names(dv) <- c("scale", "cut",
                 "senD", "speD", "accD", "lrpD", "lrnD", "aucD",
                 "senL", "speL", "accL", "lrpL", "lrnL", "aucL")
  dv$cut <- paste0("\\geq", dv$cut)
  
  pdeps.accD <- as.numeric(dv$accD[dv$scale=="PDEPS"])*100
  epds.senD <- dv$senD[dv$scale=="EPDS, Original"]
  epds.speD <- dv$speD[dv$scale=="EPDS, Original"]
  
  # export table
  t <- xtable(dv)
  #digits(t) <- c(0,0)
  f <- "data and replication files/master/output/tables/dv.tex"
  print(t,
        only.contents=TRUE,
        include.rownames=FALSE,
        include.colnames=FALSE,
        hline.after=NULL,
        type="latex",
        file=f,
        sanitize.text.function=identity)
  
# pregnant ====================================================================
  dvPr <- data.frame(NULL)
  for (i in 1:length(scales)) {
    s <- scales[i]
    l <- scaleLabels[i]
    cut <- cuts[i]
    tmp <- data.frame(cbind(s=df[df$dhs7w226==1,s], 
                            classDSM=df[df$dhs7w226==1,]$classDSM, 
                            classLocalA=df[df$dhs7w226==1,]$classLocalA))
    tmp$test <- ifelse(tmp[,1]>=cut, "case", "non-case")
    # confusion matrix
    cmD <- table(tmp$test, tmp$classDSM)
    Dfp <- cmD[1,1]
    Dtp <- cmD[1,2]
    Dfn <- cmD[2,2]
    Dtn <- cmD[2,1]
    
    cmL <- table(tmp$test, tmp$classLocalA)
    Lfp <- cmL[1,1]
    Ltp <- cmL[1,2]
    Lfn <- cmL[2,2]
    Ltn <- cmL[2,1]
    # auc
    ggrocD <- ggplot(tmp, aes(m = s, d = classDSM)) + geom_roc()
    Dauc <- rd2(calc_auc(ggrocD)[1,3])
    
    ggrocL <- ggplot(tmp, aes(m = s, d = classLocalA)) + geom_roc()
    Lauc <- rd2(calc_auc(ggrocL)[1,3])
    
    dvPr[i,1] <- l
    dvPr[i,2] <- rd0(cut)
    dvPr[i,3] <- rd2(Dtp/(Dtp+Dfn))                          # DSM-5, sen
    dvPr[i,4] <- rd2(Dtn/(Dtn+Dfp))                          # DSM-5, spe
    dvPr[i,5] <- rd2(1-((Dfp+Dfn)/sum(cmD)))                 # DSM-5, accuracy
    dvPr[i,6] <- rd2((Dtp/(Dtp+Dfn))/(1-(Dtn/(Dtn+Dfp))))    # DSM-5, lrp
    dvPr[i,7] <- rd2((1-(Dtp/(Dtp+Dfn)))/(Dtn/(Dtn+Dfp)))    # DSM-5, lrn
    dvPr[i,8] <- Dauc                                        # DSM-5 AUC
    dvPr[i,9] <- rd2(Ltp/(Ltp+Lfn))                          # LocalA, sen
    dvPr[i,10] <- rd2(Ltn/(Ltn+Lfp))                         # LocalA, spe
    dvPr[i,11] <- rd2(1-((Lfp+Lfn)/sum(cmL)))                # LocalA, accuracy
    dvPr[i,12] <- rd2((Ltp/(Ltp+Lfn))/(1-(Ltn/(Ltn+Lfp))))   # LocalA, lrp
    dvPr[i,13] <- rd2((1-(Ltp/(Ltp+Lfn)))/(Ltn/(Ltn+Lfp)))   # LocalA, lrn
    dvPr[i,14] <- Lauc                                       # LocalA AUC
  }
  names(dvPr) <- c("scale", "cut",
                   "senD", "speD", "accD", "lrpD", "lrnD", "aucD",
                   "senL", "speL", "accL", "lrpL", "lrnL", "aucL")
  dvPr$cut <- paste0("\\geq", dvPr$cut)
  
  # export table
  t <- xtable(dvPr)
  #digits(t) <- c(0,0)
  f <- "data and replication files/master/output/tables/dvPr.tex"
  print(t,
        only.contents=TRUE,
        include.rownames=FALSE,
        include.colnames=FALSE,
        hline.after=NULL,
        type="latex",
        file=f,
        sanitize.text.function=identity)
  
# post-partum =================================================================
  dvPp <- data.frame(NULL)
  for (i in 1:length(scales)) {
    s <- scales[i]
    l <- scaleLabels[i]
    cut <- cuts[i]
    tmp <- data.frame(cbind(s=df[df$dhs7w226==0,s], 
                            classDSM=df[df$dhs7w226==0,]$classDSM, 
                            classLocalA=df[df$dhs7w226==0,]$classLocalA))
    tmp$test <- ifelse(tmp[,1]>=cut, "case", "non-case")
    # confusion matrix
    cmD <- table(tmp$test, tmp$classDSM)
    Dfp <- cmD[1,1]
    Dtp <- cmD[1,2]
    Dfn <- cmD[2,2]
    Dtn <- cmD[2,1]
    
    cmL <- table(tmp$test, tmp$classLocalA)
    Lfp <- cmL[1,1]
    Ltp <- cmL[1,2]
    Lfn <- cmL[2,2]
    Ltn <- cmL[2,1]
    # auc
    ggrocD <- ggplot(tmp, aes(m = s, d = classDSM)) + geom_roc()
    Dauc <- rd2(calc_auc(ggrocD)[1,3])
    
    ggrocL <- ggplot(tmp, aes(m = s, d = classLocalA)) + geom_roc()
    Lauc <- rd2(calc_auc(ggrocL)[1,3])
    
    dvPp[i,1] <- l
    dvPp[i,2] <- rd0(cut)
    dvPp[i,3] <- rd2(Dtp/(Dtp+Dfn))                          # DSM-5, sen
    dvPp[i,4] <- rd2(Dtn/(Dtn+Dfp))                          # DSM-5, spe
    dvPp[i,5] <- rd2(1-((Dfp+Dfn)/sum(cmD)))                 # DSM-5, accuracy
    dvPp[i,6] <- rd2((Dtp/(Dtp+Dfn))/(1-(Dtn/(Dtn+Dfp))))    # DSM-5, lrp
    dvPp[i,7] <- rd2((1-(Dtp/(Dtp+Dfn)))/(Dtn/(Dtn+Dfp)))    # DSM-5, lrn
    dvPp[i,8] <- Dauc                                        # DSM-5 AUC
    dvPp[i,9] <- rd2(Ltp/(Ltp+Lfn))                          # LocalA, sen
    dvPp[i,10] <- rd2(Ltn/(Ltn+Lfp))                         # LocalA, spe
    dvPp[i,11] <- rd2(1-((Lfp+Lfn)/sum(cmL)))                # LocalA, accuracy
    dvPp[i,12] <- rd2((Ltp/(Ltp+Lfn))/(1-(Ltn/(Ltn+Lfp))))   # LocalA, lrp
    dvPp[i,13] <- rd2((1-(Ltp/(Ltp+Lfn)))/(Ltn/(Ltn+Lfp)))   # LocalA, lrn
    dvPp[i,14] <- Lauc                                       # LocalA AUC
  }
  names(dvPp) <- c("scale", "cut",
                   "senD", "speD", "accD", "lrpD", "lrnD", "aucD",
                   "senL", "speL", "accL", "lrpL", "lrnL", "aucL")
  dvPp$cut <- paste0("\\geq", dvPp$cut)
  
  # export table
  t <- xtable(dvPp)
  #digits(t) <- c(0,0)
  f <- "data and replication files/master/output/tables/dvPp.tex"
  print(t,
        only.contents=TRUE,
        include.rownames=FALSE,
        include.colnames=FALSE,
        hline.after=NULL,
        type="latex",
        file=f,
        sanitize.text.function=identity)
  
# epds compare ================================================================
  # export table
  epds <- epds[order(-epds$Spe),]
  t <- xtable(epds[,2:length(epds)])
  #digits(t) <- c(0,0)
  f <- "data and replication files/master/output/tables/epdscompare.tex"
  print(t,
        only.contents=TRUE,
        include.rownames=FALSE,
        include.colnames=FALSE,
        hline.after=NULL,
        type="latex",
        file=f,
        sanitize.text.function=identity)

  
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# DIAGNOSTIC VALIDITY, 95% CI
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  df <- dat[dat$time==1,]
  dv95 <- data.frame(NULL)
  
# all =========================================================================
  for (i in 1:length(scales)) {
    s <- scales[i]
    l <- scaleLabels[i]
    cut <- cuts[i]
    tmp <- data.frame(cbind(s=df[,s], 
                            classDSM=df$classDSM, 
                            classLocalA=df$classLocalA))
    tmp$test <- ifelse(tmp[,1]>=cut, "case", "non-case")
    ocDSM <- summary(optimal.cutpoints(X = s, 
                                      status = "classDSM",
                                      tag.healthy = 0,
                                      methods = "SpEqualSe",
                                      data = df,
                                      control = control.cutpoints(),
                                      ci.fit = TRUE,
                                      ci.SeSp = "RubinSchenker",
                                      conf.level = 0.95, 
                                      trace = FALSE))  
    ocLCA <- summary(optimal.cutpoints(X = s, 
                                       status = "classLocalA",
                                       tag.healthy = 0,
                                       methods = "SpEqualSe",
                                       data = df,
                                       control = control.cutpoints(),
                                       ci.fit = TRUE,
                                       ci.SeSp = "RubinSchenker",
                                       conf.level = 0.95, 
                                       trace = FALSE))
    
    dv95[i,1] <- l
    dv95[i,2] <- rd0(cut)
    dv95[i,3] <- rd2(ocDSM$p.table$Global$SpEqualSe[[1]][2])     # sen
    dv95[i,4] <- rd2(ocDSM$p.table$Global$SpEqualSe[[1]][12])    # sen l95
    dv95[i,5] <- rd2(ocDSM$p.table$Global$SpEqualSe[[1]][22])    # sen u95
    dv95[i,6] <- rd2(ocDSM$p.table$Global$SpEqualSe[[1]][3])     # spe
    dv95[i,7] <- rd2(ocDSM$p.table$Global$SpEqualSe[[1]][13])    # spe l95
    dv95[i,8] <- rd2(ocDSM$p.table$Global$SpEqualSe[[1]][23])    # spe u95
    dv95[i,9] <- rd2(ocLCA$p.table$Global$SpEqualSe[[1]][2])     # sen
    dv95[i,10] <- rd2(ocLCA$p.table$Global$SpEqualSe[[1]][12])   # sen l95
    dv95[i,11] <- rd2(ocLCA$p.table$Global$SpEqualSe[[1]][22])   # sen u95
    dv95[i,12] <- rd2(ocLCA$p.table$Global$SpEqualSe[[1]][3])    # spe
    dv95[i,13] <- rd2(ocLCA$p.table$Global$SpEqualSe[[1]][13])   # spe l95
    dv95[i,14] <- rd2(ocLCA$p.table$Global$SpEqualSe[[1]][23])   # spe u95
    
  }
  names(dv95) <- c("scale", "cut", 
                   "senD", "senDl95", "senDu95", 
                   "speD", "speDl95", "speDu95",
                   "senL", "senLl95", "senLu95", 
                   "speL", "speLl95", "speLu95")
  dv95$cut <- paste0("\\geq", dv95$cut)
  # export table
  t <- xtable(dv95)
  #digits(t) <- c(0,0)
  f <- "data and replication files/master/output/tables/dv95.tex"
  print(t,
        only.contents=TRUE,
        include.rownames=FALSE,
        include.colnames=FALSE,
        hline.after=NULL,
        type="latex",
        file=f,
        sanitize.text.function=identity)
  
# pregnant ====================================================================
  dv95Pr <- data.frame(NULL)
  
  for (i in 1:length(scales)) {
    s <- scales[i]
    l <- scaleLabels[i]
    cut <- cuts[i]
    tmp <- data.frame(cbind(s=df[df$dhs7w226==1,s], 
                            classDSM=df[df$dhs7w226==1,]$classDSM, 
                            classLocalA=df[df$dhs7w226==1,]$classLocalA))
    tmp$test <- ifelse(tmp[,1]>=cut, "case", "non-case")
    ocDSM <- summary(optimal.cutpoints(X = "s", 
                                       status = "classDSM",
                                       tag.healthy = 0,
                                       methods = "SpEqualSe",
                                       data = tmp,
                                       control = control.cutpoints(),
                                       ci.fit = TRUE,
                                       conf.level = 0.95, 
                                       trace = FALSE))  
    ocLCA <- summary(optimal.cutpoints(X = "s", 
                                       status = "classLocalA",
                                       tag.healthy = 0,
                                       methods = "SpEqualSe",
                                       data = tmp,
                                       control = control.cutpoints(),
                                       ci.fit = TRUE,
                                       conf.level = 0.95, 
                                       trace = FALSE))
    
    dv95Pr[i,1] <- l
    dv95Pr[i,2] <- rd0(cut)
    dv95Pr[i,3] <- rd2(ocDSM$p.table$Global$SpEqualSe[[1]][2])     # sen
    dv95Pr[i,4] <- rd2(ocDSM$p.table$Global$SpEqualSe[[1]][12])    # sen l95
    dv95Pr[i,5] <- rd2(ocDSM$p.table$Global$SpEqualSe[[1]][22])    # sen u95
    dv95Pr[i,6] <- rd2(ocDSM$p.table$Global$SpEqualSe[[1]][3])     # spe
    dv95Pr[i,7] <- rd2(ocDSM$p.table$Global$SpEqualSe[[1]][13])    # spe l95
    dv95Pr[i,8] <- rd2(ocDSM$p.table$Global$SpEqualSe[[1]][23])    # spe u95
    dv95Pr[i,9] <- rd2(ocLCA$p.table$Global$SpEqualSe[[1]][2])     # sen
    dv95Pr[i,10] <- rd2(ocLCA$p.table$Global$SpEqualSe[[1]][12])   # sen l95
    dv95Pr[i,11] <- rd2(ocLCA$p.table$Global$SpEqualSe[[1]][22])   # sen u95
    dv95Pr[i,12] <- rd2(ocLCA$p.table$Global$SpEqualSe[[1]][3])    # spe
    dv95Pr[i,13] <- rd2(ocLCA$p.table$Global$SpEqualSe[[1]][13])   # spe l95
    dv95Pr[i,14] <- rd2(ocLCA$p.table$Global$SpEqualSe[[1]][23])   # spe u95
    
  }
  names(dv95Pr) <- c("scale", "cut", 
                   "senD", "senDl95", "senDu95", 
                   "speD", "speDl95", "speDu95",
                   "senL", "senLl95", "senLu95", 
                   "speL", "speLl95", "speLu95")
  dv95Pr$cut <- paste0("\\geq", dv95Pr$cut)
  # export table
  t <- xtable(dv95Pr)
  #digits(t) <- c(0,0)
  f <- "data and replication files/master/output/tables/dv95Pr.tex"
  print(t,
        only.contents=TRUE,
        include.rownames=FALSE,
        include.colnames=FALSE,
        hline.after=NULL,
        type="latex",
        file=f,
        sanitize.text.function=identity)
  
# postpartum ==================================================================
  dv95Pp <- data.frame(NULL)
  
  for (i in 1:length(scales)) {
    s <- scales[i]
    l <- scaleLabels[i]
    cut <- cuts[i]
    tmp <- data.frame(cbind(s=df[df$dhs7w226==0,s], 
                            classDSM=df[df$dhs7w226==0,]$classDSM, 
                            classLocalA=df[df$dhs7w226==0,]$classLocalA))
    tmp$test <- ifelse(tmp[,1]>=cut, "case", "non-case")
    ocDSM <- summary(optimal.cutpoints(X = "s", 
                                       status = "classDSM",
                                       tag.healthy = 0,
                                       methods = "SpEqualSe",
                                       data = tmp,
                                       control = control.cutpoints(),
                                       ci.fit = TRUE,
                                       conf.level = 0.95, 
                                       trace = FALSE))  
    ocLCA <- summary(optimal.cutpoints(X = "s", 
                                       status = "classLocalA",
                                       tag.healthy = 0,
                                       methods = "SpEqualSe",
                                       data = tmp,
                                       control = control.cutpoints(),
                                       ci.fit = TRUE,
                                       conf.level = 0.95, 
                                       trace = FALSE))
    
    dv95Pp[i,1] <- l
    dv95Pp[i,2] <- rd0(cut)
    dv95Pp[i,3] <- rd2(ocDSM$p.table$Global$SpEqualSe[[1]][2])     # sen
    dv95Pp[i,4] <- rd2(ocDSM$p.table$Global$SpEqualSe[[1]][12])    # sen l95
    dv95Pp[i,5] <- rd2(ocDSM$p.table$Global$SpEqualSe[[1]][22])    # sen u95
    dv95Pp[i,6] <- rd2(ocDSM$p.table$Global$SpEqualSe[[1]][3])     # spe
    dv95Pp[i,7] <- rd2(ocDSM$p.table$Global$SpEqualSe[[1]][13])    # spe l95
    dv95Pp[i,8] <- rd2(ocDSM$p.table$Global$SpEqualSe[[1]][23])    # spe u95
    dv95Pp[i,9] <- rd2(ocLCA$p.table$Global$SpEqualSe[[1]][2])     # sen
    dv95Pp[i,10] <- rd2(ocLCA$p.table$Global$SpEqualSe[[1]][12])   # sen l95
    dv95Pp[i,11] <- rd2(ocLCA$p.table$Global$SpEqualSe[[1]][22])   # sen u95
    dv95Pp[i,12] <- rd2(ocLCA$p.table$Global$SpEqualSe[[1]][3])    # spe
    dv95Pp[i,13] <- rd2(ocLCA$p.table$Global$SpEqualSe[[1]][13])   # spe l95
    dv95Pp[i,14] <- rd2(ocLCA$p.table$Global$SpEqualSe[[1]][23])   # spe u95
    
  }
  names(dv95Pp) <- c("scale", "cut", 
                     "senD", "senDl95", "senDu95", 
                     "speD", "speDl95", "speDu95",
                     "senL", "senLl95", "senLu95", 
                     "speL", "speLl95", "speLu95")
  dv95Pp$cut <- paste0("\\geq", dv95Pp$cut)
  # export table
  t <- xtable(dv95Pp)
  #digits(t) <- c(0,0)
  f <- "data and replication files/master/output/tables/dv95Pp.tex"
  print(t,
        only.contents=TRUE,
        include.rownames=FALSE,
        include.colnames=FALSE,
        hline.after=NULL,
        type="latex",
        file=f,
        sanitize.text.function=identity)
  
  
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# CONSTRUCT VALIDITY
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  # convergent and discriminant  
  df <- dat[dat$time==1,]
  conv <- data.frame(NULL)
  
  for (i in 1:length(scales)) {
    s <- scales[i]
    l <- scaleLabels[i]
    conv[i,1] <- l
    conv[i,2] <- paste0(rd1(mean(df[,s], na.rm=T)),      # all
                        " (",
                        rd1(sd(df[,s], na.rm=T)),
                        ")")
    scaleMDSM <- aggregate(df[,s],               
                           by=list(df$classDSM), 
                           FUN=mean, na.rm=T)
    scaleSDDSM <- aggregate(df[,s],               
                            by=list(df$classDSM), 
                            FUN=sd, na.rm=T)
    conv[i,3] <- paste0(rd1(scaleMDSM[1,2]),              # DSM, non-case
                        " (",
                        rd1(scaleSDDSM[1,2]),
                        ")")
    conv[i,4] <- paste0(rd1(scaleMDSM[2,2]),              # DSM, case
                        " (",
                        rd1(scaleSDDSM[2,2]),
                        ")")
    tt.dsm <- t.test(df[,s]~df$classDSM)
    tt.dsmp <- ifelse(tt.dsm$p.value<0.001, "***",
               ifelse(tt.dsm$p.value<0.01, "**",
               ifelse(tt.dsm$p.value<0.05, "*",
               ifelse(tt.dsm$p.value<0.1, "//cdot", 
               ""))))
    conv[i,5] <- paste0(rd1(((scaleMDSM[2,2]-scaleMDSM[1,2])/
                              scaleMDSM[1,2])*100),
                        tt.dsmp)
    scaleMLocalA <- aggregate(df[,s],               
                              by=list(df$classLocalA), 
                              FUN=mean, na.rm=T)
    scaleSDLocalA <- aggregate(df[,s],               
                               by=list(df$classLocalA), 
                               FUN=sd, na.rm=T)
    conv[i,6] <- paste0(rd1(scaleMLocalA[1,2]),           # LocalA, non-case
                        " (",
                        rd1(scaleSDLocalA[1,2]),
                        ")")
    conv[i,7] <- paste0(rd1(scaleMLocalA[2,2]),           # LocalA, case
                        " (",
                        rd1(scaleSDLocalA[2,2]),
                        ")")
    tt.localA <- t.test(df[,s]~df$classLocalA)
    tt.localAp <- ifelse(tt.localA$p.value<0.001, "***",
                  ifelse(tt.localA$p.value<0.01, "**",
                  ifelse(tt.localA$p.value<0.05, "*",
                  ifelse(tt.localA$p.value<0.1, "//cdot", 
                         ""))))
    conv[i,8] <- paste0(rd1(((scaleMLocalA[2,2]-scaleMLocalA[1,2])/
                              scaleMLocalA[1,2])*100),
                        tt.localAp)
    corScaleFunW <- cor.test(df[,s], df$functioning)
    corScaleFunWp <- ifelse(corScaleFunW$p.value<0.001, "***",
                     ifelse(corScaleFunW$p.value<0.01, "**",
                     ifelse(corScaleFunW$p.value<0.05, "*",
                     ifelse(corScaleFunW$p.value<0.1, "//cdot", 
                            ""))))
    conv[i,9] <- paste0(rd2(corScaleFunW$estimate), corScaleFunWp)
    corScaleFunC <- cor.test(df[,s], df$sofas)
    corScaleFunCp <- ifelse(corScaleFunC$p.value<0.001, "***",
                     ifelse(corScaleFunC$p.value<0.01, "**",
                     ifelse(corScaleFunC$p.value<0.05, "*",
                     ifelse(corScaleFunC$p.value<0.1, "//cdot", 
                            ""))))
    conv[i,10] <- paste0(rd2(corScaleFunC$estimate), corScaleFunCp)
  }
  names(conv) <- c("scale", "mean.all", "mean.noncase.dsm", "mean.case.dsm",
                   "pdiff.dsm", "mean.noncase.localA", "mean.case.localA",
                   "p.diff.localA", "r.funW", "r.funC")
  
  minCV <- max(c(as.numeric(substr(conv$r.funW, 1, 5)),  # max of neg
                 as.numeric(substr(conv$r.funC, 1, 5))))
  maxCV <- min(c(as.numeric(substr(conv$r.funW, 1, 5)),  # min of neg
                 as.numeric(substr(conv$r.funC, 1, 5))))
  
  # export table
  t <- xtable(conv)
  #digits(t) <- c(0,0)
  f <- "data and replication files/master/output/tables/conv.tex"
  print(t,
        only.contents=TRUE,
        include.rownames=FALSE,
        include.colnames=FALSE,
        hline.after=NULL,
        type="latex",
        file=f,
        sanitize.text.function=identity)
  
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# RELIABILITY
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# internal consistency reliability ============================================
  icrel <- data.frame(NULL)
  
  for (i in 1:length(scales)) {
    s <- scales[i]
    l <- scaleLabels[i]
    list <- get(paste("key.list", scales[i], sep="."))    
    items <- gsub("-", "", unique(unlist(list)))
    scale <- df[,items]
    keys <- make.keys(scale, list)
    scale[] <- lapply(scale, function(x) as.character(x))
    scale[] <- lapply(scale, function(x) as.numeric(x))
    a <- psych::alpha(scale, keys, na.rm=TRUE)
    icrel[i,1] <- l
    icrel[i,2] <- nrow(scale[complete.cases(scale),])
    icrel[i,3] <- rd2(as.numeric(a$total[2]))
  }
  names(icrel) <- c("scale", "n.alpha", "alpha")
  
  minAlp <- min(as.numeric(substr(icrel$alpha, 1, 4)))
  maxAlp <- max(as.numeric(substr(icrel$alpha, 1, 4)))
  
  
# test-retest reliability =====================================================
  rel <- dat[, c("pid", "time", "enumeratorID", "phoneRetest",
                 scales)]
  relw <- reshape(rel, 
                  timevar="time",
                  idvar="pid",
                  direction="wide")
  relw$phoneRetest.1 <- NULL               # meaningless...all tablet rd 1
  #relw$days <- difftime(relw$date.2, relw$date.1, units="days")
  
# import days between test and retest (separate to mask dates)
  load("data and replication files/master/input/days.RData")
  relw <- merge(relw, days, by="pid")
  relw.trt <- relw[relw$days<=7 & !is.na(relw$days) & relw$phoneRetest.2==0, ]
 
# test-retest limited to tablet only at time 1 and time 2
  trt.n <- nrow(relw.trt)
  trt.phq9 <- cor(relw.trt$phq9Tot.1, relw.trt$phq9Tot.2, 
                  use="complete.obs")
  trt.pdeps <- cor(relw.trt$pdepsTot.1, relw.trt$pdepsTot.2,
                   use="complete.obs")
  trt.epds <- cor(relw.trt$epdsTot.1, relw.trt$epdsTot.2, 
                  use="complete.obs")
  trt.epdsR <- cor(relw.trt$epdsRTot.1, relw.trt$epdsRTot.2, 
                   use="complete.obs")
  
# test-retest for phone
  relw.trtp <- relw[relw$days<=7 & !is.na(relw$days) & relw$phoneRetest.2==1, ]
  trtp.n <- nrow(relw.trtp)
  trtp.phq9 <- cor(relw.trtp$phq9Tot.1, relw.trtp$phq9Tot.2, 
                  use="complete.obs")
  trtp.pdeps <- cor(relw.trtp$pdepsTot.1, relw.trtp$pdepsTot.2,
                    use="complete.obs")
  trtp.epds <- cor(relw.trtp$epdsTot.1, relw.trtp$epdsTot.2, 
                  use="complete.obs")
  trtp.epdsR <- cor(relw.trtp$epdsRTot.1, relw.trtp$epdsRTot.2, 
                   use="complete.obs")
  
# compare correlations
  trt.cor <- as.data.frame(NULL)
  cors <- list(tablet=c(trt.n, trt.phq9, trt.pdeps, trt.epds, trt.epdsR),
               phone=c(trtp.n, trtp.phq9, trtp.pdeps, 
                       trtp.epds, trtp.epdsR),
               names=c("N", "PHQ-9", "PDEPS", "EPDS, Original", 
                       "EPDS, Revised"))
  for (i in 2:length(cors[[1]])) {
    tmp <- cocor.indep.groups(r1.jk=cors[[1]][i], 
                              r2.hm=cors[[2]][i], 
                              n1=cors[[1]][1], 
                              n2=cors[[2]][1], 
                              alternative="two.sided", 
                              alpha=0.05, 
                              conf.level=0.95, 
                              null.value=0)
    trt.cor[i-1, 1] <- cors[[3]][i]             # scale name
    trt.cor[i-1, 2] <- rd0(cors[[1]][1])        # N tablet
    trt.cor[i-1, 3] <- rd2(cors[[1]][i])        # r tablet
    trt.cor[i-1, 4] <- rd0(cors[[2]][1])        # N phone
    trt.cor[i-1, 5] <- rd2(cors[[2]][i])        # r phone
    trt.cor[i-1, 6] <- rd2(tmp@diff)            # difference in r
    trt.cor[i-1, 7] <- rd2(tmp@zou2007[[1]][1]) # l95ci
    trt.cor[i-1, 8] <- rd2(tmp@zou2007[[1]][2]) # u95ci
    trt.cor[i-1, 9] <- rd2(tmp@fisher1925$statistic) # z
    trt.cor[i-1, 10] <- rd2(tmp@fisher1925$p.value)  # p value 
  }
  names(trt.cor) <- c("scale", "nTablet", "rTablet", "nPhone", "rPhone",
                      "diffR", "l95", "u95", "z", "p")
  
# merge reliability ===========================================================
  reldf <- merge(icrel, trt.cor[,1:8], by="scale")
# export table
  t <- xtable(reldf)
  #digits(t) <- c(0,0)
  f <- "data and replication files/master/output/tables/rel.tex"
  print(t,
        only.contents=TRUE,
        include.rownames=FALSE,
        include.colnames=FALSE,
        hline.after=NULL,
        type="latex",
        file=f,
        sanitize.text.function=identity)
  
  minTrt <- min(as.numeric(substr(reldf$rTablet, 1, 4)))
  maxTrt <- max(as.numeric(substr(reldf$rTablet, 1, 4)))
  minTrtP <- min(as.numeric(substr(reldf$rPhone, 1, 4)))
  maxTrtP <- max(as.numeric(substr(reldf$rPhone, 1, 4)))
  
# lower reliability for phone, but is mean depression score higher? ===========
  dat2 <- dat[dat$time==2,]
  phEPDS <- summary(lm(epdsTot~phoneRetest, data=dat2))
  phEPDSr <- summary(lm(epdsRTot~phoneRetest, data=dat2))
  phPDEPS <- summary(lm(pdepsTot~phoneRetest, data=dat2))
  phPHQ9 <- summary(lm(phq9Tot~phoneRetest, data=dat2))
  
  ph <- data.frame(NULL)
  ph[1,1] <- "EPDS, Original"
  ph[1,2] <- rd1(phEPDS$coefficients[1,1])  # mean tablet
  ph[1,3] <- rd1(phEPDS$coefficients[2,1])  # est
  ph[1,4] <- rd2(phEPDS$coefficients[2,2])  # stderr
  ph[1,5] <- rd1((phEPDS$coefficients[2,1]/phEPDS$coefficients[1,1])*100)
  ph[1,6] <- rd1(phEPDS$coefficients[2,3])  # t
  ph[1,7] <- rd2(phEPDS$coefficients[2,4])  # p
  
  ph[2,1] <- "EPDS, Revised"
  ph[2,2] <- rd1(phEPDSr$coefficients[1,1])  # mean tablet
  ph[2,3] <- rd1(phEPDSr$coefficients[2,1])  # est
  ph[2,4] <- rd2(phEPDSr$coefficients[2,2])  # stderr
  ph[2,5] <- rd1((phEPDSr$coefficients[2,1]/phEPDSr$coefficients[1,1])*100)
  ph[2,6] <- rd1(phEPDSr$coefficients[2,3])  # t
  ph[2,7] <- rd2(phEPDSr$coefficients[2,4])  # p
  
  ph[3,1] <- "PDEPS"
  ph[3,2] <- rd1(phPDEPS$coefficients[1,1])  # mean tablet
  ph[3,3] <- rd1(phPDEPS$coefficients[2,1])  # est
  ph[3,4] <- rd2(phPDEPS$coefficients[2,2])  # stderr
  ph[3,5] <- rd1((phPDEPS$coefficients[2,1]/phPDEPS$coefficients[1,1])*100)
  ph[3,6] <- rd1(phPDEPS$coefficients[2,3])  # t
  ph[3,7] <- rd2(phPDEPS$coefficients[2,4])  # p
  
  ph[4,1] <- "PHQ-9"
  ph[4,2] <- rd1(phPHQ9$coefficients[1,1])  # mean tablet
  ph[4,3] <- rd1(phPHQ9$coefficients[2,1])  # est
  ph[4,4] <- rd2(phPHQ9$coefficients[2,2])  # stderr
  ph[4,5] <- rd1((phPHQ9$coefficients[2,1]/phPHQ9$coefficients[1,1])*100)
  ph[4,6] <- rd1(phPHQ9$coefficients[2,3])  # t
  ph[4,7] <- rd2(phPHQ9$coefficients[2,4])  # p
  
  names(ph) <- c("scale", "meanT", "est", "stderr", "pdiff", "t", "p")
  
  maxPhone <- max(as.numeric(substr(ph$pdiff, 1, 4)))
  
  # export table
  t <- xtable(ph)
  #digits(t) <- c(0,0)
  f <- "data and replication files/master/output/tables/phone.tex"
  print(t,
        only.contents=TRUE,
        include.rownames=FALSE,
        include.colnames=FALSE,
        hline.after=NULL,
        type="latex",
        file=f,
        sanitize.text.function=identity)
  
  
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ITEM TEST-RETEST RELIABILITY
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# tablet only
  itemtrt <- data.frame(NULL)
  ct <- 1
  for (i in selectedFeatures) {
    v1 <- paste(i, 1, sep=".")
    v2 <- paste(i, 2, sep=".")
    df <- dat.w[dat.w$phoneRetest.2==0, c(v1, v2)]
    itemtrt[ct, 1] <- i
    rel.k <- kappa2(df, weight = "squared", sort.levels = FALSE)
    rel.k$subjects
    print(rel.k$value)
    itemtrt[ct, 2] <- rd2(rel.k$value)
    ct <- ct + 1
  }
  names(itemtrt) <- c("item", "wkappa")
  
  
# update itemStats table
  itemLabs <- dd.survey[dd.survey$item %in% selectedFeatures,
                        c("item", "itemlabel")]
  names(itemStats)[names(itemStats)=="item"] <- "itemlabel"
  itemStats <- merge(itemStats, itemLabs, by="itemlabel")
  itemStats <- merge(itemStats, itemtrt, by="item")
  itemStats$item <- NULL
  
  t <- xtable(itemStats)
  #digits(t) <- c(0,0)
  f <- "data and replication files/master/output/tables/itemStats.tex"
  print(t,
        only.contents=TRUE,
        include.rownames=FALSE,
        include.colnames=FALSE,
        hline.after=NULL,
        type="latex",
        file=f,
        sanitize.text.function=identity)

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# DETERMINANTS OF DEPRESSION
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  df <- dat[dat$time==1,]
  det <- lm(pdepsTot~h.wealthIndex + demo15 + working + currentMarried + 
                     parityG0 + dhs7w226 + eduComp + literate,
            data=df)
  stargazer(det, type="latex", single.row = TRUE,
            digits = 1,
            title = "Correlates of depression severity",
            covariate.labels = c("Wealth index value",
                                 "Age",
                                 "Working (0/1)",
                                 "Currently married or living with a partner (0/1)",
                                 "Parity \\geq 1 (0/1)",
                                 "Pregnant (0/1)",
                                 "Years education completed",
                                 "Literate (0/1)"),
            dep.var.labels="PDEPS score",
            label="tbl:det",
            out="data and replication files/master/output/tables/det.tex")
  
  
# *****************************************************************************
# 
# --------------------------- VOICE BIOMETRICS -------------------------------
#
# *****************************************************************************
  

  
  
  
  
  
  