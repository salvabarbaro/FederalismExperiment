library(dplyr)
library(boot)
library(purrr)
library(ggplot2)
library(rstatix)

#
setwd("~/Documents/JGU/Lehrveranstaltungen/Fiskalföderalismus/WinterTerm2324/Umfrage Studierende Fiskalföderalismus/Zweiter Durchlauf/")
### Daten einlesen  
numcol <- c(paste0("q", 3:22), paste0("q", 24:27),
            paste0("q", 29:33))

df <- data.frame(jsonlite::fromJSON("survey2.json")) %>%
  setNames(c("ID", "Date", "Last.Page", "Language", "Seed", 
             paste0("q", 1:36))) %>%
  mutate(Durchlauf = c(rep(1, 56), rep(2, (82 - 56))), .after = "ID" ) %>%
  mutate(Last.Page = as.numeric(Last.Page)) %>% 
  filter(., Last.Page == 14 & is.na(Last.Page) == F) %>%
  mutate_at(numcol, as.numeric)
#
d1 <- df %>% filter(Durchlauf == 1)
d2 <- df %>% filter(Durchlauf == 2)
var.list <- list("q11", "q16", "q17", "q19", "q24", "q29")
#
###########################################################################
## Function: Bootstrap Mean
bootstrap_mean <- function(data, indices) {
  sample_data <- data[indices]
  return(mean(sample_data, na.rm = TRUE))
}
############################################################################

df.01 <- d1 %>% select(., c("q17", "q11", "q16", "q19", "q24", "q29")) %>%
  summarise(across(everything(), ~ list(boot(data = ., 
                                             statistic = bootstrap_mean, 
                                             R = 1000)))) %>%
  summarise(across(everything(), ~ map(., "t"))) 

res01.df <- data.frame(q16 = unlist(df.01$q16),
                       q17 = unlist(df.01$q17),
                       q11 = unlist(df.01$q11),
                       q19 = unlist(df.01$q19),
                       q24 = unlist(df.01$q24),
                       q29 = unlist(df.01$q29)
)
############################################################################
df.02 <- d2 %>% select(., c("q17", "q11", "q16", "q19", "q24", "q29")) %>%
  summarise(across(everything(), ~ list(boot(data = ., 
                                             statistic = bootstrap_mean, 
                                             R = 1000)))) %>%
  summarise(across(everything(), ~ map(., "t"))) 

res02.df <- data.frame(q16 = unlist(df.02$q16),
                       q17 = unlist(df.02$q17),
                       q11 = unlist(df.02$q11),
                       q19 = unlist(df.02$q19),
                       q24 = unlist(df.02$q24),
                       q29 = unlist(df.02$q29)
                       )

rm(df.01, df.02, numcol)
###############################################################
# Bootstrap evaluation function
quantile.fun <- function(v){
  quantile.df <- data.frame(d1.mean = mean(d1[[v]], na.rm = TRUE),
                            d2.mean = mean(d2[[v]], na.rm = TRUE),
                            d1_0.05 = quantile(res01.df[[v]], probs = 0.05, na.rm = TRUE),
                            d1_0.95 = quantile(res01.df[[v]], probs = 0.95, na.rm = TRUE),
                            d2_0.05 = quantile(res02.df[[v]], probs = 0.05, na.rm = TRUE),
                            d2_0.95 = quantile(res02.df[[v]], probs = 0.95, na.rm = TRUE))
  return(quantile.df)
}
#################################################################################################
# Mann-Whitney-U-Test Function (Wilcoxon-Test)
wilcoxon_test.fun <- function(df, var_list){
  mwutest <- function(variable){
    res <- df %>%
      wilcox_test(as.formula(paste(variable, "~ Durchlauf")))
    res <- res %>%
      mutate(variable = variable)
    return(res)
  }
  res2 <- map_dfr(var_list, mwutest)
  return(res2)
}
#################################################################################################
# Mann-Whitney U-Test results
res2 <- wilcoxon_test.fun(df, var.list)
##########################################

##########################################################
res <- lapply(var.list,  quantile.fun) %>% do.call(rbind, .) %>%
  mutate(Bootstrap = c("**", "**", "*", "**", 0, 0)) %>%
  mutate(MWUT.p = res2$p) %>%
  mutate(MWUT.Sign. = c("**", "**", "*", "**", 0, "*"))
rownames(res) <- c("q11", "q16", "q17", "q19", "q24", "q29")

## Cross-check
#wilcox.test(d1$q11, d2$q11)
#wilcox.test(d1$q16, d2$q16)
#wilcox.test(d1$q17, d2$q17)
#wilcox.test(d1$q19, d2$q19)
#wilcox.test(d1$q24, d2$q24)
#wilcox.test(d1$q29, d2$q29)

#var.list2 <- as.list(c("q11", "q16", "q17", "q19", "q24", "q29"))

ggplot(data = res01.df %>% filter(., q16 < quantile(res01.df$q16, probs = 0.95, na.rm = TRUE))) +
  geom_histogram(aes(x = q16, fill = "violet")) +
  geom_histogram(data = res02.df %>% filter(., q16 > quantile(res02.df$q16, probs = 0.05, na.rm = TRUE)), 
                 aes(x = q16), fill = "orange")


ggplot(data = res01.df %>% filter(., q29 < quantile(res01.df$q29, probs = 0.9, na.rm = TRUE))) +
  geom_histogram(aes(x = q29, fill = "violet")) +
  geom_histogram(data = res02.df %>% 
                   filter(., q29 > quantile(res02.df$q29, 
                                                 probs = 0.1, na.rm = TRUE)), 
                 aes(x = q29), fill = "orange", alpha = 0.4)


ggplot(data = res01.df %>% filter(., q17 > quantile(res01.df$q17, 
                                                         probs = 0.10, 
                                                         na.rm = TRUE))) +
  geom_histogram(aes(x = q17, fill = "violet")) +
  geom_histogram(data = res02.df %>% 
                   filter(., q17 < quantile(res02.df$q17, 
                                                 probs = 0.9, na.rm = TRUE)), 
                 aes(x = q17), fill = "orange", alpha = 0.4)

gebiet.df <- data.frame(q2 = unique(df$q2),
                        Q2 = c(3,4,2,1, NA),
                        Land = c(0,0,1,0, NA), # Land = 1, 0 else
                        LKvsBE = c(0,0,1,1, NA))   # Kommune, Land = 0, Bund, EU = 1
df <- df %>% left_join(x = ., y = gebiet.df, by = "q2") %>%
  mutate(q28 = na_if(q23, "N/A")) %>%
  mutate(q28 = na_if(q28, "N/A")) %>%
  mutate(q34 = na_if(q34, "N/A")) %>%
  mutate(q35 = na_if(q35, "N/A")) %>%
  mutate(q36 = na_if(q36, "N/A"))

# logreg01: log reg with Land = 1, 0 else --> not significant
logreg01 <- glm(formula = Land ~ Durchlauf, family = binomial, data = df)
summary(logreg01)
# logreg02: logistic regression for q36 --> significant effect of Durchlauf
logreg02 <- glm(formula = as.factor(q36) ~ Durchlauf, family = binomial, data = df) # Model 01
summary(logreg02)
# logreg03: log reg for LKvsBE (Land oder Kommune versus Bund oder EU) --> Effekt auf dem 10% Signi.niveau
logreg03 <- glm(formula = LKvsBE ~ Durchlauf, family = binomial, data = df) # Model 02
summary(logreg03)
summary.glm(logreg03)

### -->Ergebnisse in tex:
library(texreg)
texreg(l = list(logreg02, logreg03), stars = c(0.01, 0.05, 0.1), booktabs = TRUE)
## Interpretationshilfe: Coeff bei logreg03 : 0.85. Wahrscheinlichkeit für eine Nennung Land oder Kommune ist exp(0.85) = 2.33 mal höher
## nach einer Beschäftigung mit dem Föderalismus als vorher. 
plotreg(l = list(logreg02, logreg03))
