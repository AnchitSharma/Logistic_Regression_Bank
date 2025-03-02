# Load packages for data handling and plotting
library(tidyverse)
library(patchwork)
library(broom)

set.seed(40)

rnorm_fixed <- function(N, mu = 0, sd = 1)
  scale(rnorm(N))*sd + mu

# plot style
theme_axis <- function(P,
                       jitter = FALSE,
                       xlim=c(-0.5, 2),
                       ylim=c(-0.5, 2),
                       legend.position = NULL
){
  P = P + theme_bw(15)+
    geom_segment(
      x = -1000, xend = 1000,
      y = 0, yend = 0,
      lty = 2, color = 'dark gray', lwd = 0.5
    )+
    geom_segment(
      x = 0, xend = 0,
      y = -1000, yend = 1000,
      lty = 2, color = 'dark gray', lwd = 0.5
    )+
    coord_cartesian(xlim = xlim, ylim = ylim)+
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      legend.position = legend.position
    )
  if(jitter){
    P + geom_jitter(width = 0.1, size = 2)
  }else{
    P + geom_point(size = 2)
  }
}

y = c(rnorm(15), exp(rnorm(15)), runif(20, min = -3, max = 0))
x = rnorm_fixed(50, mu = 0, sd = 1)
y2 = rnorm_fixed(50, mu = 0.5, sd = 1.5)

value = c(y, y2)
group = rep(c('y1', 'y2'), each = 50)
group

# fixed correlation
D_correlation = data.frame(
  MASS::mvrnorm(30, mu=c(0.9, 0.9),
                Sigma = matrix(c(1, 0.8, 1, 0.8), ncol = 2), empirical = T)
)

# add label
D_correlation$label_num = sprintf('(%.1f, %.1f)', D_correlation$X1, D_correlation$X2)
D_correlation$label_rank = sprintf('(%i, %i)', rank(D_correlation$X1), rank(D_correlation$X2))

fit = lm(I(X2*0.5 + 0.4) ~ I(X1 * 0.5 + 0.2), D_correlation)
intercept_pearson = coefficients(fit)[1]

P_pearson <- ggplot(D_correlation, aes(x = X1*0.5+0.2, y=X2*0.5+0.4))+
  geom_smooth(method = lm, se = FALSE, lwd = 2, aes(colour='beta_1'))+
  geom_segment(x = -100, xend=100,
               y=intercept_pearson, yend=intercept_pearson,
               lwd = 2, aes(color="beta_0")) +
  scale_color_manual(name = NULL, values = c("blue", "red"), labels=c(bquote(beta[0]*" (intercept)"), bquote(beta[1]*" (slope)")))

theme_axis(P_pearson, legend.position = c(0.4, 0.9))

intercept_spearman <- coefficients(
  lm(rank(X2)~rank(X1), D_correlation)
)[1]

P_spearman <- ggplot(D_correlation, aes(x = rank(X1), y=rank(X2)))+
  geom_smooth(method = lm, se = FALSE, lwd = 2, aes(colour='beta_1'))+
  geom_segment(x = -100, xend=100,
               y=intercept_spearman, yend=intercept_spearman,
               lwd = 2, aes(color="beta_0")) +
  scale_color_manual(name = NULL, values = c("blue", "red"), labels=c(bquote(beta[0]*" (intercept)"), bquote(beta[1]*" (slope)")))

P_spearman = ggplot(D_correlation, aes(x=rank(X1), y=rank(X2))) +
  geom_smooth(method=lm, se=FALSE, lwd=2, aes(color='beta_1')) + 
  geom_text(aes(label=label_rank), nudge_y=1, size=3, color='dark gray') + 
  geom_segment(x=-100, xend=100, 
               y=intercept_spearman, yend=intercept_spearman, 
               lwd=2, aes(color='beta_0')) + 
  scale_color_manual(name=NULL, values=c("blue", "red"), labels=c(bquote(beta[0]*" (intercept)"), bquote(beta[1]*" (slope)")))

theme_axis(
  P_pearson, legend.position = c(0.5, 0.1)) +
    geom_text(aes(label=label_num),nudge_y = 0.1, size =3, color='dark gray')+
    labs(title = '         Pearson')
theme_axis(
    P_spearman, xlim = c(-7.5, 30),
    ylim = c(-7.5, 30), legend.position = c(0.5, 0.1))+
  labs(title = '         Spearman')



a = cor.test(y, x, method = 'pearson')
a
b = lm(y ~ 1 + x)
b # correlation coeff from model summary is obtain from 
# sqrt of coefficient of x mutiply by sign of coefficient of x
# conf.low
qt(0.975, 48)
-0.08718+(2.010635*0.25907)
-0.08718-(2.010635*0.25907)
c = lm(scale(y) ~ 1 + scale(x))
summary(c)
-1*sqrt(0.002354) # correlation


# Spearman correlation
a = cor(y, x, method = 'spearman')
a
b = lm(rank(y)~1+rank(x))
summary(b)

-1*sqrt(0.00295)


# one sample t test
a = t.test(y)
a
b = lm(y ~ 1)
summary(b)
confint(b)

# wilcoxon signed rank test
a = wilcox.test(y)
a
y_ranked = rank(abs(y))*sign(y)
b = lm(y_ranked~1)
summary(b)

c <- t.test(y_ranked)
c

# paired sample t test
a = t.test(y, y2, paired = TRUE)
a$estimate
a$p.value
a$statistic

b <- lm(y-y2~1)
summary(b)
confint(b)

# paired sample t test non parametric counterpart
# wilcoxon matched pair 
a = wilcox.test(y, y2, paired = T)
a
temp = y - y2
y_minus_y2_ranked = rank(abs(temp))*sign(temp)
b = lm(y_minus_y2_ranked~1)
summary(b)
c = t.test(y_minus_y2_ranked)
c


# independent t test
a = t.test(y, y2, var.equal = TRUE)
a
# Be explicit about the underlying linear model by hand-dummy coding
group_y2 = ifelse(group == 'y2', 1,0)
b = lm(value ~ 1 + group_y2)
summary(b)

# Mann-Whitney U
a = wilcox.test(y, y2)
a
b = lm(rank(value) ~ 1 + group_y2)
summary(b)


# Welch's T-test
a = t.test(y, y2, var.equal = FALSE)
a
# As a Linear Model with per-group variances
b = nlme::gls(value ~ 1 + group_y2, weights = nlme::varIdent(form =~1|group), method = "ML")
summary(b)
confint(b)

# Example data
# Three variable in long format
N = 20
D = data.frame(
  value = c(rnorm_fixed(N, 0), rnorm_fixed(N, 1), rnorm_fixed(N, 0.5)),
  group = rep(c('a', 'b', 'c'), each = N),
  
  # Explicitly add indicator/dummy variables
  # could also be done using model.matrix(~D$group)
  # group_a = rep(c(1, 0, 0), each = N), this is the intercept, No need to code
  group_b = rep(c(0, 1, 0), each = N),
  group_c = rep(c(0, 0, 1), each = N)
)


# one way ANOVA
a = car::Anova(aov(value ~ group, D))
a
b = lm(value ~ 1 +group_b + group_c, data = D)
summary(b)

# kruskal-wallis
a = kruskal.test(value~group,D)
a
b = lm(rank(value)~1+group_b + group_c, data = D)
summary(b)
c = car::Anova(aov(rank(value) ~ group, D))
c

# Crossing factor
D$mood <- c('happy', 'sad')
# dummy coding
D$mood_happy <- ifelse(D$mood == 'happy', 1, 0)

# Dedicated two-way anova
a = car::Anova(aov(value ~ mood*group, D), type='II')
a

b = car::Anova(aov(value ~ mood + group + mood:group, D))
b


# testing the interaction terms as linear model
full <- lm(value ~ 1 + group_b + group_c + mood_happy + group_b:mood_happy + group_c:mood_happy, D)
summary(full)

null = lm(value ~ 1 + group_b + group_c + mood_happy, D)
summary(null)

anova(null, full)

# main effect of group
e = lm(value ~ 1 + group_b + group_c, D)

f = lm(value ~ 1 + mood_happy, D)
summary(e)
summary(f)
anova(e,f)
car::Anova(aov(value ~ mood_happy, D))
