### 06 April 2011

## OBJECTIVE:
## Are the pre- and post-anthesis models misclassifying the same observations?


## LIBRARIES
library(PresenceAbsence)  # needed for classification tables, auc etc

# Remove all objects:
rm(list = ls(all = TRUE))

source("ROCCurves.R")


# This neat way of removing all but specified objects is due to Peter Alspach
# R site search on "How to remove all objects except a few specified objects?"
rm(list = ls()[!(ls() %in% c("rocdf", "fhb.i.1.mod"))])
ls()
names(rocdf)

# This gets the observed (y) and the predicted (y) from 2 models (x,y) to compare:
# NOTE: x & y correspond to columns in rocdf; base model is in col=3
x <- 31
y <- 43

# Set the subgroup for calculations:
# Values for subgroup are: "all", "noresidue", "residue"
subgroup <- "all"
 
# Need the corresponding Youden's Index for each:
## Youden's Index
yi.x <- as.numeric(PresenceAbsence::optimal.thresholds(rocdf, which.model = x - 2, opt.methods = 3)[2])
yi.y <- as.numeric(PresenceAbsence::optimal.thresholds(rocdf, which.model = y - 2, opt.methods = 3)[2])

# intdf stands for intermediate data.frame
intdf <- data.frame(rocdf, corn = fhb.i.1.mod$corn)

# Get the subset of rocdf that contains the data for comparing the two classifiers:
if (subgroup == "all") {
mydf <- rocdf[, c(2, x, y)] 
} else if (subgroup == "noresidue") {
mydf.0 <- subset(intdf, subset = corn == 0, select = c(-corn))
mydf <- mydf.0[, c(2, x, y)]
} else {
mydf.1 <- subset(intdf, subset = corn == 1, select = c(-corn))
mydf <- mydf.1[, c(2, x, y)]
}  # end if

# xc and yc are the classification of an obs based on comparing the score to YI:
mydf$xc <- ifelse(mydf[, 2] > yi.x, 1, 0) 
mydf$yc <- ifelse(mydf[, 3] > yi.y, 1, 0)


# Now create the following variables representing misclassifications:
# n00 = number misclassified by both x & y
# n01 = number misclassified by x but not by y
# n10 = number misclassified by y but not by x
# n11 = number misclassified by neither x or y

mydf$n00 <- ifelse((mydf$yobs != mydf$xc & mydf$yobs != mydf$yc), 1, 0)
mydf$n01 <- ifelse((mydf$yobs != mydf$xc & mydf$yobs == mydf$yc), 1, 0)
mydf$n10 <- ifelse((mydf$yobs == mydf$xc & mydf$yobs != mydf$yc), 1, 0)
mydf$n11 <- ifelse((mydf$yobs == mydf$xc & mydf$yobs == mydf$yc), 1, 0)

# Which gives the following sums:
n00 <- sum(mydf$n00)
n01 <- sum(mydf$n01)
n10 <- sum(mydf$n10)
n11 <- sum(mydf$n11)

# nrow(mydf)
# c(n00, n01, n10, n11)
# sum(c(n00, n01, n10, n11))


### You can further break down the misclassifications:
# Table of n01 misclassifications subset by yobs:
table(subset(mydf, subset = n01 == 1)$yobs)/n01

# Table of n10 misclassifications subset by yobs:
table(subset(mydf, subset = n10 == 1)$yobs)/n10


### Simple committee model 
# Unweighted predicted average of the two classifiers
rocdf$cm <- (rocdf[, x] + rocdf[, y])/2
head(rocdf)
ncol(rocdf)
# Get the YI:
yi.cm <- as.numeric(PresenceAbsence::optimal.thresholds(rocdf, which.model = ncol(rocdf) - 2, opt.methods = 3)[2])

## Classification matrices
cmx <- PresenceAbsence::cmx(rocdf, threshold = yi.cm, which.model = ncol(rocdf) - 2, na.rm = FALSE)

cmx <- PresenceAbsence::cmx(rocdf, threshold=yi.cm, which.model=29, na.rm = FALSE)

## Sensitivity
PresenceAbsence::sensitivity(cmx, st.dev = FALSE)
## Specificity
PresenceAbsence::specificity(cmx, st.dev = FALSE)
## Kappa
PresenceAbsence::Kappa(cmx, st.dev = FALSE)
## Misclassification rate
1 - PresenceAbsence::pcc(cmx, st.dev = FALSE)

## ROC plot example
PresenceAbsence::auc.roc.plot(rocdf, threshold = 101, find.auc = TRUE, which.model = ncol(rocdf) - 2, add.legend = TRUE, opt.thresholds = TRUE, opt.methods = 3)






### Hastie et al. p385, Ex 10.6
#  The McNemar test is another way of assessing whether one classifier is better than another:
# Use R's mcnemar test function
res <- matrix(c(1434, 33, 18, 51),
       nrow = 2,
       dimnames = list("GAM" = c("Correct", "Error"),
                       "GBM" = c("Correct", "Error")))

mcnemar.test(res, correct = FALSE )


