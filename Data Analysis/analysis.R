setwd('/Users/sherr/School/Sp20/Psych101/Final Project/Data Analysis/')
placebo <- read.csv("Psych 101 Project Survey - Final_April 17, 2020_17.53.csv", header=T, na.strings=c(""))
head(placebo)[,17:30]
nrow(placebo)

# libraries
library(dplyr)
library(psych)
library(gplots)
library(arm)
library(scales)

# correct responses to memory tests
correct1 <- c('BEJ',
              'BUW',
              'DIJ',
              'FEP',
              'FUP',
              'JEX',
              'JIH',
              'KOJ',
              'MEP',
              'NAF'
)

correct2 <- c('BIW',
              'DEJ',
              'FAJ',
              'FOV',
              'HAJ',
              'JID',
              'KEF',
              'MAF',
              'MIV',
              'NUV'
)

# removing unnecessary columns
names(placebo)
placebo <- placebo[, 18:47]

# results of 2 memory tests
names(placebo)
test1_df <- placebo[, 1:10]
test2_df <- placebo[, 11:20]

test1_df[] <- lapply(test1_df, as.character)
test2_df[] <- lapply(test2_df, as.character)

# turning empty responses into incorrect ones
test1_df[is.na(test1_df)] <- 'WRONG'
test2_df[is.na(test2_df)] <- 'WRONG'
test1_df
test2_df

# converting all responses to uppercase only
test1_df <- data.frame(lapply(test1_df, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))
test2_df <- data.frame(lapply(test2_df, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))

# creating 2 vectors of correctness scores
test1_scores <- array()
for (r in c(1:nrow(test1_df))) {
  correct <- 0
  for (i in c(1:10)) {
    correct <- correct + (test1_df[r, i] %in% correct1)
  }
  test1_scores[r] <- correct
}

test2_scores <- array()
for (r in c(1:nrow(test2_df))) {
  correct <- 0
  for (i in c(1:10)) {
    correct <- correct + (test2_df[r, i] %in% correct2)
  }
  test2_scores[r] <- correct
}
length(test1_scores)
length(test2_scores)
test1_scores
test2_scores

# checking to make sure scores actually correspond to reality
tail(test1_df)

# merging scores vectors with original dataframe
names(placebo)
placebo$test1 <- test1_scores
placebo$test2 <- test2_scores
head(placebo)

# removing all unecessary columns
names(placebo)
placebo <- with(placebo, data.frame(Q8, Q9, Q10, Q11, Q12, Q13, FL_6_DO, test1, test2))
head(placebo)  

# combining test scores
test_comb <- cbind(placebo$test1, placebo$test2)
placebo$test_tot <- placebo$test1 + placebo$test2
alpha(test_comb)

# renaming columns to be more informative
placebo <- placebo %>% rename(Age = Q8, Sex = Q9, Ed = Q10, PsychClass = Q11, KnowPlacebo = Q12, PlaceboFamiliarity = Q13, Cond = FL_6_DO)

# removing participant 19 who didn't complete the survey
placebo <- placebo[-c(19), ]
tail(placebo)

# shortening education levels
levels(placebo$Ed) <- c('college', 'grad', 'HS', 'no HS')
placebo$Ed

# changing psych knowledge levels to numeric
# normalized so all equally impactful
levels(placebo$PsychClass) <- c(0, 4, 2) 
levels(placebo$PlaceboFamiliarity) <- c(4, 2, 0, 1, 3)

# scoring psych knowledge
psych_know <- cbind(placebo$PsychClass, placebo$PlaceboFamiliarity)
placebo$PsychKnow <- rowMeans(psych_know)
head(placebo)

alpha(psych_know) # this is bad :(
# alternative: using as categorical variable; not continuous scale

# splitting by condition

ctrl <- subset(placebo, Cond == 'Block1')
nrow(ctrl)

tmt <- subset(placebo, Cond == 'Block4')
nrow(tmt)

levels(placebo$Cond) <- c('C', 'T')
# not an even split, but so what?

# FINALLY DATA CLEANING IS DONE
bivar <- standardize(lm(test_tot ~ Cond, data = placebo), NULL, T)
bivar2 <- standardize(lm(test_tot ~ PsychKnow, placebo), NULL, T)
par(mar=c(2, 2, 2, 2))
plotmeans(scale(test_tot) ~ Cond, data = placebo,
          main = 'Condition vs. Memory', ylab = 'Memory Score', cex.axis = 0.75, cex.main = 0.75, cex.lab = 0.75)
# people in treatment group performed worse??!! but nonsignificant
# treatment performed worse, but nonsignificant
# R2: explains about 2% of variation in memory scores
summary(bivar) 

# seems like 0 relationship
par(mar=c(4, 4, 2, 1))
plot(scale(placebo$PsychKnow), scale(placebo$test_tot),
     main = 'PK vs. Memory Performance', 
     xlab = 'PK', ylab = 'Memory',
     cex.main = 0.75, cex.lab = 0.75, cex.axis = 0.75)# yep, slope is basically 0, and p value is almost 1 lol
abline(bivar2, lwd = 3, col = 'red')
summary(bivar2)

# people who knew about placebo effect
# WOW
# condt:psycknow: adjust slope up by 1.3 for each pt increase in psychknow btwn tmt/ctrl
# people who knew about placebo had MORE improvement in memory btwn tmt/contrl
# p = 0.006!!!!
know_mod <- standardize(lm(test_tot ~ Cond * PsychKnow, data = placebo), NULL, T)
summary(know_mod)

# plotting diff btwn tmt/ctrl at diff levels of psychknow
# at higher levels of psychknow a diff btwn results emerges
par(mar = c(4, 4, 2, 1))
plot(scale(placebo$PsychKnow), scale(placebo$test_tot), col = placebo$Cond, pch = 19,
     main = 'Treatment vs. Control Across PK Levels', 
     xlab = 'PK', ylab = 'Memory', 
     cex.main = 0.75, cex.lab = 0.75, cex.axis = 0.75)
legend(x = 'topright', legend = c('T', 'C'), col=c("red", "black"),
       pch = c(19, 19), cex = 0.8)
tmt_pk <- standardize(lm(test_tot ~ PsychKnow, data = tmt), NULL, T)
ctrl_pk <- standardize(lm(test_tot ~ PsychKnow, data = ctrl), NULL, T)
abline(tmt_pk, col = alpha('red', 0.5), lwd = 3, lty = 'longdash')
abline(ctrl_pk, col = alpha('black', 0.5), lwd = 2, lty = 'longdash')
summary(tmt_pk)
summary(ctrl_pk)
# wow that's INTERSTING

# demographic assessments
describe(placebo$Age)
summary(placebo$Sex)

describe(tmt$Age)
summary(tmt$Sex)

summary(placebo$Ed)
supp
describe(ctrl$Age)
summary(ctrl$Sex)

describe(placebo)
summary(placebo$KnowPlacebo)

hist(tmt$Age,
     main = 'Age Distribution (T)', 
     xlab = 'Age',
     cex.main = 0.75, cex.lab = 0.75, cex.axis = 0.75)

hist(ctrl$Age,
     main = 'Age Distribution (C)', 
     xlab = 'Age',
     cex.main = 0.75, cex.lab = 0.75, cex.axis = 0.75)

hist(tmt$PsychKnow,
     main = 'PK (T)', 
     xlab = 'Age',
     cex.main = 0.75, cex.lab = 0.75, cex.axis = 0.75)

hist(ctrl$PsychKnow,
     main = 'PK (C)', 
     xlab = 'Age',
     cex.main = 0.75, cex.lab = 0.75, cex.axis = 0.75)

