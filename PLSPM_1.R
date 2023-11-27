library(plspm)
education <- read.csv("http://www.gastonsanchez.com/education.csv", header = TRUE, row.names = 1)

dim(education)
summary(education)

# Support Variable
# sup.help :- I feel comfortable asking for help from the program’s staff
# sup.under:- I feel underappreciated in the program
# sup.safe :- I can find a place where I feel safe in the program
# sup.conc :- I go to the program when I have concerns about school

# distribution of first column
aux_distrib = table(education[, 1])/nrow(education)

# barplot of the distribution
barplot(aux_distrib, border = NA, main = colnames(education)[1])

library(RColorBrewer)
# questions for Support Indicators
sq1 = "Help when not doing well"
sq2 = "I feel underappreciated"
sq3 = "I can find a place where I feel safe"
sq4 = "Concerns about school"

# put sample questions in one vector
sup_questions = c(sq1, sq2, sq3, sq4)

# setting graphical parameters 
op = par(mfrow = c(2,2), mar = c(2.5, 3.2, 2, 0.8))
# for each indicator of support
for (j in 1:4){
  
  # calculate the distribution
  distrib = table(education[, j])/nrow(education)
  # cute barplot
  barplot(
    distrib, border = NA, col = brewer.pal(8, "Blues")[2:8],
    axes = FALSE, main = sup_questions[j], cex.main = 1
  )
  # add vertical axis
  axis(side = 2, las = 2)
  # add rectangle around figure
  box("figure", col = "gray70")
  
}
# reset default graphical parameters
par(op)

# Correlation for support indicators
cor(education[, 1:4])

# install.packages("plsdepot")
library(plsdepot)

# PCA of support indicators with nipals
support_pca = nipals(education[, 1:4])

# plot
plot(support_pca, main = "Support indicators (circle of correlations)",
     cex.main = 1)


# Advising variables
# adv.comp:- Competence of advisors
# adv.access:- Access to advisors
# adv.comm:- Communication skills of advisors
# adv.qual:- Overall quality of advising

sq1 = "Competence of advisors"
sq2 = "Access to advisors"
sq3 = "Communication skills"
sq4 = "Overall quality"

# put sample questions in one vector
adv_questions = c(sq1, sq2, sq3, sq4)

# setting graphical parameters 
op = par(mfrow = c(2,2), mar = c(2.5, 3.2, 2, 0.8))
# for each indicator of support
for (j in 5:8){
  
  # calculate the distribution
  distrib = table(education[, j])/nrow(education)
  # cute barplot
  barplot(
    distrib, border = NA, col = brewer.pal(8, "Blues")[2:8],
    axes = FALSE, main = adv_questions[j-4], cex.main = 1
  )
  # add vertical axis
  axis(side = 2, las = 2)
  # add rectangle around figure
  box("figure", col = "gray70")
  
}
# reset default graphical parameters
par(op)

# Correlation for Advicing indicators
cor(education[, 5:8])

# install.packages("plsdepot")
library(plsdepot)

# PCA of support indicators with nipals
advic_pca = nipals(education[, 5:8])

# plot
plot(advic_pca, main = "Advicing indicators (circle of correlations)",
     cex.main = 1)

# Tutoring
# tut.prof Proficiency of tutors
# tut.sched Tutoring schedules
# tut.stud Variety of study groups
# tut.qual Overall quality of tutoring

sq1 = "Proficiency of tutors"
sq2 = "Tutoring schedules"
sq3 = "Variety of study groups"
sq4 = "overall tutoring quality"

# put sample questions in one vector
tut_questions = c(sq1, sq2, sq3, sq4)

# setting graphical parameters 
op = par(mfrow = c(2,2), mar = c(2.5, 3.2, 2, 0.8))
# for each indicator of support
for (j in 9:12){
  
  # calculate the distribution
  distrib = table(education[, j])/nrow(education)
  # cute barplot
  barplot(
    distrib, border = NA, col = brewer.pal(8, "Blues")[2:8],
    axes = FALSE, main = tut_questions[j-8], cex.main = 1
  )
  # add vertical axis
  axis(side = 2, las = 2)
  # add rectangle around figure
  box("figure", col = "gray70")
  
}
# reset default graphical parameters
par(op)

# Correlation for Advicing indicators
cor(education[, 9:12])

# install.packages("plsdepot")
library(plsdepot)

# PCA of support indicators with nipals
tut_pca = nipals(education[, 9:12])

# plot
plot(tut_pca, main = "Tut indicators (circle of correlations)",
     cex.main = 1)

# Value
# val.devel :- Helpfulness in my personal development
# val.deci :-  Helpfulness in personal decision making
# val.meet :-  Facilitating meeting people and contacts
# val.info :-  Accessibility to support and information



sq1 = "Help in personal dev"
sq2 = "Help in personal decision making"
sq3 = "Facilitate in meeting and contacts"
sq4 = "Accessibility to support and information"

# put sample questions in one vector
val_questions = c(sq1, sq2, sq3, sq4)

# setting graphical parameters 
op = par(mfrow = c(2,2), mar = c(2.5, 3.2, 2, 0.8))
# for each indicator of support
for (j in 13:16){
  
  # calculate the distribution
  distrib = table(education[, j])/nrow(education)
  # cute barplot
  barplot(
    distrib, border = NA, col = brewer.pal(8, "Blues")[2:8],
    axes = FALSE, main = val_questions[j-12], cex.main = 1
  )
  # add vertical axis
  axis(side = 2, las = 2)
  # add rectangle around figure
  box("figure", col = "gray70")
  
}
# reset default graphical parameters
par(op)

# Correlation for Advicing indicators
cor(education[, 13:16])

# install.packages("plsdepot")
library(plsdepot)

# PCA of support indicators with nipals
val_pca = nipals(education[, 13:16])

# plot
plot(val_pca, main = "Value indicators (circle of correlations)",
     cex.main = 1)


# rows for inner matrix
Support = c(0, 0, 0, 0, 0, 0)
Advicing = c(0, 0, 0, 0, 0, 0)
Tutoring = c(0, 0, 0, 0, 0, 0)
Value = c(1, 1, 1, 0, 0, 0)
Satisfaction = c(1, 1, 1, 1, 0, 0)
Loyalty = c(0, 0, 0, 0, 1, 0)

# matrix by row binding
edu_inner = rbind(Support, Advicing, Tutoring, Value, Satisfaction, Loyalty)

# add column names
colnames(edu_inner) = rownames(edu_inner)

# plot the inner matrix
innerplot(edu_inner, box.size = 0.1)

edu_inner

# outer model and modes
edu_outer = list(1:4, 5:8, 9:12, 13:16, 17:19, 20:23)
edu_modes = rep("A", 6)

# apply plspm
edu_pls1 = plspm(education, edu_inner, edu_outer, edu_modes)

edu_pls1

# check unidimensionality
edu_pls1$unidim

# plottings the loadings
plot(edu_pls1, what = "loadings")


# invert the scale for ill fated manifest variables
# adding Support 'appreciated'
education$sup.apre = 8 - education$sup.under
# adding 'Loyalty' pleased
education$loy.pleas = 8 - education$loy.asha

# outer model 2
edu_outer2 = list(c(1, 27, 3,4), 5:8, 9:12, 13:16, 17:19, c(20, 21, 28, 23))

# apply plspm
edu_pls2 = plspm(education, edu_inner, edu_outer2, edu_modes)

edu_pls2$unidim

# plottings the loadings
plot(edu_pls2, what = "loadings")

# Loadings and communalities
edu_pls2$outer_mod

edu_outer3 = list(c(1, 3, 4), 5:8, 9:12, 13:16, 17:19, c(20, 21, 23))
# re-apply plspm
edu_pls3 = plspm(education, edu_inner, edu_outer3, edu_modes)

edu_pls3$unidim

# check the outer model
edu_pls3$outer_model

# check the cross loadings
edu_pls3$crossloadings

plot(edu_pls3, arr.pos = 0.35, box.col = "gray95", lcol = "black",
     txt.col = "gray40")

# matrix of path coefficients
Paths = edu_pls3$path_coefs
# matrix with values based on path coeffs
arrow_lwd = 10 * round(Paths, 2)
# arrows of different sizes reflecting the values of the path
op = par(mar = rep(0, 4))

plot(edu_pls3, arr.lwd = arrow_lwd, arr.pos = 0.35, box.col = "gray95", lcol = "black",
     txt.col = "gray40")


edu_pls3$inner_model


# effects
edu_pls3$effects

# selecting effects ('active' rows)
good_rows = c(3:5, 7:15)

# active effects in matrix format
path_effs = as.matrix(edu_pls3$effects[good_rows, 2:3])
rownames(path_effs) = edu_pls3$effects[good_rows, 1]
# setting margin size
op = par(mar = c(8, 3, 1, 0.5))
# barplots of total effects (direct + indirect)
barplot(t(path_effs), border = NA, col = c("#9E9AC8", "#DADAEB"),
        las = 2, cex.names = 0.8, cex.axis = 0.8, legend = c("Direct",
                                                             "Indirect"), args.legend = list(x = "top", ncol = 2, border = NA,
                                                                                             bty = "n", title = "Effects"))
# resetting default margins
par(op)
edu_val = plspm(education, edu_inner, edu_outer3, edu_modes, boot.val = TRUE, br = 200)

# bootstrap results
edu_val$boot
