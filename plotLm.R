library(scales)
library(ggplot2)
library(data.table)
library(gridExtra)
library(dplyr)
source('multiplot.R')

by1var = function(oldLm, var, thin=1) {
  #toString(substitute(var))
  #varName = dputToString(substitute(var))
  ##fix error handling here
  #if (typeof(substitute(var)) != "character") {varName = dputToString(substitute(var))}
  #else {varName = var}
  varName = var
  outcomeVar = attributes(terms(formula(oldLm)))$variables[2]
  varAsFormula = reformulate(termlabels=varName,intercept=FALSE)
  
  #find if varName is a factor
  print(varName)
  print(head(oldLm$model))
  if (typeof(oldLm$model[, varName]) == "integer") {isfactor = TRUE} else {isfactor = FALSE}
  
  #turn the irrelevant factors to 0
  for (j in 1:ncol(oldLm$model)) {
    if (typeof(oldLm$model[, j]) == 'integer' & length(unique(oldLm$model[, j])) < 8 & names(oldLm$model)[j] != varName) {
      oldLm$model[, j] = 0
    }
  }
  
  newLm = lm(formula(oldLm) - varAsFormula, data=oldLm$model)
  
  #if categorical, group by it. if not, take the mean
  
  #newLm
  #data=cbind(oldLm$model,newLm$resid)
  d1 = data.table(oldLm$model)
  #data
  #print(newLm$model[, 3:dim(newLm$model)[2]])
  new.coef.minus.0 = newLm$coef
  new.coef.minus.0[is.na(new.coef.minus.0)] = 0
  adjustment = sum(new.coef.minus.0 * c(1, colMeans(as.matrix(newLm$model[,2:dim(newLm$model)[2]]))))
  adjResid = newLm$resid + adjustment
  d2 = data.table(d1[,c("gp_","tp_"):=list(1:dim(d1)[1],"raw")])
  d2 = d2[,c(toString(outcomeVar),"tp_"):=list(adjResid,"adj")]
  bothData = rbind(d1,d2)
  bothData = bothData[,residOfFull:=oldLm$resid + adjustment]
  
  print(outcomeVar)
  print(varName)
  print(names(bothData))
  print(newLm$coef)
  print(c(1, sapply(newLm$model[,2:dim(newLm$model)[2]],mean)))
  print(head(bothData))
  print(dim(subset(bothData,tp_ == "raw")))
  
  bothData = data.frame(bothData)
  if (isfactor == FALSE) {  sampled.data = sample(1:nrow(bothData), round(thin * nrow(bothData)) )
                            p <- ggplot(data=bothData[sampled.data, ], aes_string(y=toString(outcomeVar),x=varName ))+
                              geom_point(data=subset(bothData[sampled.data, ],tp_ == "raw"),alpha=0.3)+
                              geom_point(color="red", data=subset(bothData[sampled.data, ],tp_ == "adj")) + 
                              geom_line(aes(group=gp_), alpha=0.3)+
                              geom_smooth(method=lm,data=subset(bothData,tp_ == "adj"), color="red", fill='red', alpha=.3, se=F)+
                              geom_smooth(data=subset(bothData,tp_ == "adj"), size=0, fill = 'blue', color = 'blue', alpha=.2, se=T)+
                              geom_rug(aes(y=residOfFull),data=subset(bothData,tp_ == "adj"),col=rgb(.5,0,0,alpha=.2)) + 
                              theme_bw()}
  
  else { bothData[, varName] = as.factor(bothData[, varName])
         p <- ggplot(data=sample_n(bothData, round(thin * nrow(bothData))), aes_string(y=toString(outcomeVar),x=varName ))+
           geom_boxplot(data=subset(bothData,tp_ == "raw"),alpha=0.3)+
           geom_boxplot(color="red", data=subset(bothData,tp_ == "adj")) + geom_line(aes(group=gp_), alpha=0.3)+
           #geom_smooth(method=lm,data=subset(bothData,tp_ == "adj"), color="red", fill='red', alpha=.3, se=F)+
           #geom_smooth(data=subset(bothData,tp_ == "adj"), size=0, fill = 'blue', color = 'blue', alpha=.2, se=T)+
           geom_rug(aes(y=residOfFull),data=subset(bothData,tp_ == "adj"),col=rgb(.5,0,0,alpha=.2)) + 
           theme_bw()
  }
  
  
  gcall = quote(p)
  
  #lastcall = eval(bquote(substitute(.(gcall),list(RESPONSE=attributes(terms(formula(oldLm)))$variables[[2]],
  #                                          RESID=quote(newLm$resid))
  #                            )))
  print(gcall)
  eval(gcall)
  return(p)
}

dputToString <- function (obj) {
  con <- textConnection(NULL,open="w")
  tryCatch({dput(obj,con);
            textConnectionValue(con)},
           finally=close(con))
}

`-.formula` = function(e1, e2) {
  .terms <- lapply(c(e1,e2), terms)
  reformulate(response=attributes(terms(e1))$variables[[2]],
              termlabels=do.call(setdiff, lapply(.terms, attr, which = 'term.labels')),
              intercept=!attributes(.terms[[2]])$intercept)
}

by1var.seq <- function(l, thin.val=1) {
  p.list <- list()
  vars <- names(l$model)[2:length(names(l$coefficients))]
  for (i in 1:length(vars)) { p <- by1var(l, vars[i], thin=thin.val)
                              p.list[[i]] <- p}
  multiplot(plotlist=p.list, cols = 2)}


#tests

y <- rnorm(100, 10, 1)
x <- exp(y) + rnorm(100, 0, .5)
z <- y + rnorm(100, 5, .5)
w <- z + rnorm(100, 1, 1)

l <- lm(y ~  x + z + w)
by1var(l, "x", thin=.3)
by1var(l, "z")


pdf("test.pdf")
by1var.seq(l)
dev.off()

l <- lm(mpg ~ wt + hp, data=mtcars)
by1var(l, "wt")

