library("gtalibrary")
library("xlsx")
library("tidyverse")
library("WDI")
library("ggplot2")

rm(list=ls())

gta_setwd()

source("4 data queries/190617 Top10 Swiss export markets/code/0 Definitions.R")
gta_colour_palette()

# GET TOP MARKETS AND SECTORS
load(paste0(data.path,"Top CPC and markets.Rdata"))
top.cpc <- top.cpc$cpc
top.markets <- top.markets$un_code

load(paste0(data.path,"results long.Rdata"))

# QUERY
# ln(TR)= logarithm of the trade ratio in 2017 to 2010.
# diffd= difference in distortions faced by Swiss exporters between 2017 and 2010 in the importing country
# diffe=difference in distortions faced by Swiss exporters between 2017 and 2010 due to third party export incentives
# diffg=difference in government spending share between 2017 and 2010 in importing nation
# diffchf=appreciation of the Swiss france against its importing country from 2010 to 2017

master <- results.long
names(master) <- c("country","cpc","diffd","diffe","lnTR","diffchf","diffg")

stat.values <- data.frame()

# 1. Plot ln(TR) against diffd. Please add in a line or curve that best fits the data plus the R2 of the line.


# FUNCTION FOR LATER USE 
scatterplot <- function(x.axis = NULL,
                        y.axis = NULL,
                        formula.name = NULL,
                        output.name = NULL,
                        min.max = "max",
                        labs = c("x","y"),
                        position.y = c(0,0.5),
                        position.x = c(0,0.5)) 
  {
  
  
  master.function <- master[,c(x.axis,y.axis)]
  names(master.function) <- c("x","y")
  
  fig1.lm <- lm(y ~ x, master.function)

  stat.values <<- rbind(stat.values, data.frame(r.squared = summary(fig1.lm)$r.squared,
                                               t.value = summary(fig1.lm)$coefficients[2,3],
                                               p.value = summary(fig1.lm)$coefficients[2,4],
                                               formula = formula.name))
  fig1.lm.predict.val <-ifelse(min.max == "max", max(master.function$x), min(master.function$x))
  fig1.lm.predict <- predict(fig1.lm, newdata = data.frame(x = fig1.lm.predict.val))

  fig1 <- ggplot()+
    geom_point(data=master.function, aes(x = x, y = y), colour=gta_colour$blue[1], size=2)+
    geom_smooth(data=master.function, aes(x=x, y = y), method="lm", se = F)+
    scale_y_continuous(sec.axis = dup_axis())+
    labs(x=labs[1],y=labs[2])+
    geom_label(aes(x=fig1.lm.predict.val, y=fig1.lm.predict, label=paste0("p-value: ",sprintf("%0.2f",round(summary(fig1.lm)$coefficients[2,4], 2)))),nudge_y = position.y[1], nudge_x = position.x[1], hjust=position.x[2], vjust = position.y[2],size=3)+
    gta_theme()
  
  fig1
  
  gta_plot_saver(plot = fig1,
                 name = output.name,
                 path = output.path,
                 eps = F)
}

scatterplot(x.axis = "diffd",
            y.axis = "lnTR",
            formula.name = "log of trade ratio ~ Difference in distortions coverage",
            output.name = "Figure 1 - Log of trade ratio against difference in distortions",
            min.max = "min",
            labs = c("Difference in trade affected","Log of trade ratio from 2010 to 2017"),
            position.y = c(-0.05, 1),
            position.x = c(0, 0))
            

# 2. Plot ln(TR) against diffe.??Please add in a line or curve that best fits the data plus the R2 of the line.

scatterplot(x.axis = "diffe",
            y.axis = "lnTR",
            formula.name = "log of trade ratio ~ Difference in 3rd party export incentives coverage",
            output.name = "Figure 2 - Log of trade ratio against difference in 3rd party export incentives coverage",
            min.max = "max",
            labs = c("Difference in trade affected \nby 3rd party export incentives","Log of trade ratio from 2010 to 2017"),
            position.y = c(-0.2, 0),
            position.x = c(0, 1))

# 3. Plot ln(TR) against diffg.??Please add in a line or curve that best fits the data plus the R2 of the line.

scatterplot(x.axis = "diffg",
            y.axis = "lnTR",
            formula.name = "log of trade ratio ~ Percentage change in government spending",
            output.name = "Figure 3 - Log of Trade Ratio against difference in government spending",
            min.max = "max",
            labs = c("Percentage change in government spending,\nfrom 2010 to 2017","Log of trade ratio from 2010 to 2017"),
            position.y = c(0.05, 0),
            position.x = c(-2, 1))

# 4. Plot ln(TR) against diffchf.??Please add in a line or curve that best fits the data plus the R2 of the line.

scatterplot(x.axis = "diffchf",
            y.axis = "lnTR",
            formula.name = "log of trade ratio ~ Appreciation LCU against CHF",
            output.name = "Figure 4 - Log of Trade Ratio against appreciation LCU against CHF",
            min.max = "max",
            labs = c("Appreciation of LCU against\nCHF, from 2010 to 2017","Log of trade ratio from 2010 to 2017"),
            position.y = c(-0.2, 0),
            position.x = c(-1, 1))

# 5. Plot diffd against diffg.??Please add in a line or curve that best fits the data plus the R2 of the line.

scatterplot(x.axis = "diffg",
            y.axis = "diffd",
            formula.name = "Difference in trade affected by distortions ~ Percentage change in government spending",
            output.name = "Figure 5 - Difference in trade affected by distortions against difference in government spending",
            min.max = "max",
            labs = c("Percentage change in government spending,\nfrom 2010 to 2017","Difference in trade affected by distortions"),
            position.y = c(0.05, 0),
            position.x = c(0, 1))

# 6. Plot diffd against diffchf.??Please add in a line or curve that best fits the data plus the R2 of the line.

scatterplot(x.axis = "diffchf",
            y.axis = "diffd",
            formula.name = "Difference in trade affected by distortions ~ Appreciation LCU against CHF",
            output.name = "Figure 6 - Difference in trade affected by distortions against appreciation LCU against CHF",
            min.max = "max",
            labs = c("Appreciation LCU against CHF,\nfrom 2010 to 2017","Difference in trade affected by distortions"),
            position.y = c(0.05, 0),
            position.x = c(-0.8, 1))

write.xlsx(stat.values, file=paste0(output.path,"Statistical Values.xlsx"),sheetName = "Fig1-6", row.names = F, append = F)

# 7. Perform an OLS regression of diffd on a constant and diffg and diffchf. Please report the R2 and estimated parameters and standard errors. Please also compute the regression residuals, which I denote diffdres.

ols7 <- lm(diffd ~ diffg + diffchf, master)
summary(ols7)

ols7.summary <- data.frame(rsquared = summary(ols7)$r.squared,
                          parameters = row.names(summary(ols7)$coefficients),
                          estimate = summary(ols7)$coefficients[,1],
                          standard.errors = summary(ols7)$coefficients[,2])

row.names(ols7.summary) <- NULL
write.xlsx(ols7.summary, file=paste0(output.path,"Table 7 - Regression of diffd on diffg and diffchf.xlsx"), sheetName = "Results",row.names=F, append=F)

master$diffdres <- ols7$residuals

# 8. Plot ln(TR) against diffdres.??Please add in a line or curve that best fits the data plus the R2 of the line.

scatterplot(x.axis = "diffdres",
            y.axis = "lnTR",
            formula.name = "Log of Trade Ratio ~ Residuals of regression(diffd ~ diffg + diffchf)",
            output.name = "Figure 7 - Log of trade ratio against residuals of regression on diffd",
            min.max = "min",
            labs = c("Residuals of regression diffd ~ diffg + diffchf","Log of trade ratio\nfrom 2010 to 2017"),
            position.y = c(-0.2, 0),
            position.x = c(0, 0))

# 9. Perform an OLS regression of ln(TR)??on a constant and diffd, diffe,??diffg and diffchf. Please report the R2??and??estimated parameters and standard errors.

ols9 <- lm(lnTR ~ diffd + diffe + diffg + diffchf, master)
summary(ols9)

ols9.summary <- data.frame(rsquared = summary(ols9)$r.squared,
                           parameters = row.names(summary(ols9)$coefficients),
                           estimate = summary(ols9)$coefficients[,1],
                           standard.errors = summary(ols9)$coefficients[,2])

row.names(ols9.summary) <- NULL
# write.xlsx(ols9.summary, file=paste0(output.path,"Table 9 - Regression of lnTR on diffd, diffe, diffg and diffchf.xlsx"), sheetName = "Results",row.names=F, append=F)


# 10. Perform an OLS regression of??ln(TR)??on a constant and diffdres, diffe,??diffg and diffchf. Please report the R2??and??estimated parameters and standard errors.

ols10 <- lm(lnTR ~ diffdres + diffe + diffg + diffchf, master)
summary(ols10)

ols10.summary <- data.frame(rsquared = summary(ols10)$r.squared,
                           parameters = row.names(summary(ols10)$coefficients),
                           estimate = summary(ols10)$coefficients[,1],
                           standard.errors = summary(ols10)$coefficients[,2])

row.names(ols10.summary) <- NULL
# write.xlsx(ols10.summary, file=paste0(output.path,"Table 10 - Regression of lnTR on diffdres, diffe, diffg and diffchf.xlsx"), sheetName = "Results",row.names=F, append=F)


# 11. Please take the regression in 10 above and, one by one, drop each of the four independent variables. For each of these four regressions please report the R2??and??estimated parameters and standard errors.

ols10.drop.diffdres <- lm(lnTR ~ diffe + diffg + diffchf, master)
ols10.drop.diffdres.summary <- data.frame(rsquared = summary(ols10.drop.diffdres)$r.squared,
                            parameters = row.names(summary(ols10.drop.diffdres)$coefficients),
                            estimate = summary(ols10.drop.diffdres)$coefficients[,1],
                            standard.errors = summary(ols10.drop.diffdres)$coefficients[,2])
row.names(ols10.drop.diffdres.summary) <- NULL


ols10.drop.diffe <- lm(lnTR ~ diffdres + diffg + diffchf, master)
ols10.drop.diffe.summary <- data.frame(rsquared = summary(ols10.drop.diffe)$r.squared,
                            parameters = row.names(summary(ols10.drop.diffe)$coefficients),
                            estimate = summary(ols10.drop.diffe)$coefficients[,1],
                            standard.errors = summary(ols10.drop.diffe)$coefficients[,2])
row.names(ols10.drop.diffe.summary) <- NULL


ols10.drop.diffg <- lm(lnTR ~ diffdres + diffe + diffchf, master)
ols10.drop.diffg.summary <- data.frame(rsquared = summary(ols10.drop.diffg)$r.squared,
                            parameters = row.names(summary(ols10.drop.diffg)$coefficients),
                            estimate = summary(ols10.drop.diffg)$coefficients[,1],
                            standard.errors = summary(ols10.drop.diffg)$coefficients[,2])
row.names(ols10.drop.diffg.summary) <- NULL


ols10.drop.diffchf <- lm(lnTR ~ diffdres + diffe + diffg, master)
ols10.drop.diffchf.summary <- data.frame(rsquared = summary(ols10.drop.diffchf)$r.squared,
                                       parameters = row.names(summary(ols10.drop.diffchf)$coefficients),
                                       estimate = summary(ols10.drop.diffchf)$coefficients[,1],
                                       standard.errors = summary(ols10.drop.diffchf)$coefficients[,2])
row.names(ols10.drop.diffchf.summary) <- NULL


# 12. Please prepare a table with the regression results for 9,10, and??11. In total there should be six sets??of regression results presented.

table12 <- data.frame(rsquared = numeric(),
                      parameters = character(),
                      estimate = numeric(),
                      standard.errors = numeric())

table12[nrow(table12)+1,] <- c("Regression of lnTR on constant and diffd, diffe, diffg and diffchf", NA, NA, NA)
table12 <- rbind(table12, ols9.summary)

table12[nrow(table12)+1,] <- c(NA, NA, NA, NA)
table12[nrow(table12)+1,] <- c("Regression of lnTR on constant diffdres, diffe, diffg and diffchf", NA, NA, NA)
table12 <- rbind(table12, ols10.summary)

table12[nrow(table12)+1,] <- c(NA, NA, NA, NA)
table12[nrow(table12)+1,] <- c("Regression of lnTR on constant, diffe, diffg and diffchf", NA, NA, NA)
table12 <- rbind(table12, ols10.drop.diffdres.summary)

table12[nrow(table12)+1,] <- c(NA, NA, NA, NA)
table12[nrow(table12)+1,] <- c("Regression of lnTR on constant, diffdres, diffg and diffchf", NA, NA, NA)
table12 <- rbind(table12, ols10.drop.diffe.summary)

table12[nrow(table12)+1,] <- c(NA, NA, NA, NA)
table12[nrow(table12)+1,] <- c("Regression of lnTR on constant, diffdres, diffe and diffchf", NA, NA, NA)
table12 <- rbind(table12, ols10.drop.diffg.summary)

table12[nrow(table12)+1,] <- c(NA, NA, NA, NA)
table12[nrow(table12)+1,] <- c("Regression of lnTR on constant, diffdres, diffe and diffg", NA, NA, NA)
table12 <- rbind(table12, ols10.drop.diffchf.summary)

# SAVE RESULTS
write.xlsx(table12, file=paste0(output.path,"Table 12 - Regression results.xlsx"), sheetName = "Results", row.names = F)
