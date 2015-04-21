library(scales)
library(ggplot2)
library(data.table)
library(gridExtra)
library(dplyr)
library(mvtnorm)
source('multiplot.R')

by1var = function(oldLm, var, thin=1, breakupby=FALSE) {

  varName <- var
  print(varName)
  outcomeVar = attributes(terms(formula(oldLm)))$variables[2]
  varAsFormula = reformulate(termlabels=varName,intercept=FALSE)
  if (varName == breakupby) {breakupby=FALSE}
  #find if varName is a factor
  if (typeof(oldLm$model[, varName]) == "integer") {isfactor = TRUE} else {isfactor = FALSE}
  
  cat.variables.old = find.cat.variables(oldLm, varName)
  
  newLm = lm(formula(oldLm) - varAsFormula, data=oldLm$model)
  cat.variables.new = find.cat.variables(newLm, varName)

  d1 = data.table(oldLm$model)

  new.data = dplyr::select(newLm$model, which(cat.variables.new == T)) %>% mutate_each(funs(mean))
  if (breakupby != FALSE & breakupby %in% names(newLm$model)) {new.data2 = data.frame(newLm$model[, breakupby])
                                                               names(new.data2) = breakupby
                                                              new.data = cbind(new.data, new.data2)}
  if (length(which(cat.variables.new == F & names(newLm$model) != breakupby & names(newLm$model) != varName )) > 0) {
      new.data3 = dplyr::select(newLm$model, which(cat.variables.new == F & names(newLm$model) != breakupby & names(newLm$model) != varName )) 
      new.data3[T] = 0 
      new.data = cbind(new.data, new.data3) 
  }
  new.data[, varName] <-d1[, eval(as.symbol(varName))]
  print(head(new.data))
  d2 = new.data
  if (breakupby != FALSE) {d1[ ,means_:=mean(eval(as.symbol(toString(outcomeVar)))), by=eval(as.symbol(breakupby))]} else {
    d1[ ,means_:=mean(eval(as.symbol(toString(outcomeVar))))]}
  d2[, toString(outcomeVar)] = c(d1[, eval(as.symbol(toString(outcomeVar)))+means_]) - fitted(newLm, newdata=new.data)   #residuals for new
  d1[,means_:=NULL]
  d2$tp_ = 'adj'
  d1$tp_ = 'raw'
  d1$gp_ = 1:nrow(d1)
  d2$gp_ = 1:nrow(d2)
  #d2$tp_ = "raw" #
  #d1$tp_ = 'adj'
  bothData = rbind(d1,d2)
  bothData$residOfFull = predict(oldLm) #oldLm$resid + adjustment # = bothData[,residOfFull:=oldLm$resid + adjustment]
  
  #print(outcomeVar)
  #print(varName)
  #print(names(bothData))
  #print(newLm$coef)
  #print(c(1, sapply(newLm$model[,2:dim(newLm$model)[2]],mean)))
  #print(head(bothData))
  #print(dim(subset(bothData,tp_ == "raw")))
  sampled.data = sample(1:nrow(bothData), round(thin * nrow(bothData)) )
  bothData = data.frame(bothData)
  
  title.vars = new.data[1, names(newLm$model)]
  title.vars = title.vars[names(title.vars) %in% breakupby == F & names(title.vars) != toString(outcomeVar)]
  title.val = "holding constant...\n"
  for (i in 1:length(title.vars)) {
    title.val = paste(title.val, names(title.vars[i]), ": ", round(title.vars[i], 2), "\n", sep="")
  }
  
  if (breakupby == FALSE) {bothData[, 'breakupby'] = 1} else {bothData[, 'breakupby'] = bothData[, breakupby]}
  if (isfactor == FALSE) {  
                            p <- ggplot(data=bothData[sampled.data, ], aes_string(y=toString(outcomeVar),x=varName ))+
                              geom_point(data=subset(bothData[sampled.data, ],tp_ == "raw"),alpha=0.3)+
                              geom_point(color="red", data=subset(bothData[sampled.data, ],tp_ == "adj")) + 
                              geom_line(aes(group=gp_), alpha=0.3)+
                              geom_smooth(method=lm,data=subset(bothData,tp_ == "adj"), color="red", fill='red', alpha=.3, se=F)+
                              geom_smooth(data=subset(bothData,tp_ == "adj"), size=0, fill = 'blue', color = 'blue', alpha=.2, se=T)+
                              geom_rug(aes(y=residOfFull),data=subset(bothData,tp_ == "adj"),col=rgb(.5,0,0,alpha=.2)) + 
                              theme_bw() + facet_grid(. ~ breakupby) + ggtitle(title.val) + theme(plot.title = element_text(size=10, hjust=1))}  else { 
                                bothData[, varName] = as.factor(bothData[, varName])
                              p <- ggplot(data=bothData[sampled.data, ], aes_string(y=toString(outcomeVar),x=varName ))+
           geom_boxplot(data=subset(bothData,tp_ == "raw"),alpha=0.3)+
           geom_boxplot(color="red", data=subset(bothData,tp_ == "adj")) + geom_line(aes(group=gp_), alpha=0.3)+
           #geom_smooth(method=lm,data=subset(bothData,tp_ == "adj"), color="red", fill='red', alpha=.3, se=F)+
           #geom_smooth(data=subset(bothData,tp_ == "adj"), size=0, fill = 'blue', color = 'blue', alpha=.2, se=T)+
           geom_rug(aes(y=residOfFull),data=subset(bothData,tp_ == "adj"),col=rgb(.5,0,0,alpha=.2)) + 
           theme_bw() + facet_grid(. ~ breakupby) + ggtitle(title.val) + theme(plot.title = element_text(size=10, hjust=1))
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

makeNumeric <- function(x) {as.numeric(as.factor(x))}

find.cat.variables <- function(oldLm, varName) {
  #get 0's for categorical variables
  cat.variables = rep(1, ncol(oldLm$model))
  for (j in 1:ncol(oldLm$model)) {
    if (typeof(oldLm$model[, j]) == 'integer' & length(unique(oldLm$model[, j])) < 8 & names(oldLm$model)[j] != varName) {
      cat.variables[j] = 0
    }
  }
  return(cat.variables)
}

by1var.seq <- function(l, thin.val=1, bub=FALSE) {
  p.list <- list()
  vars <- names(l$model)[2:length(names(l$coefficients))]
  for (i in 1:length(vars)) { p <- by1var(l, vars[i], thin=thin.val, breakupby=bub)
                              p.list[[i]] <- p}
  multiplot(plotlist=p.list, cols = 2)}


#tests

y <- rnorm(100, 10, 1)
x <- exp(y) + rnorm(100, 0, .5)
z <- y + rnorm(100, 5, .5)
w <- z + rnorm(100, 1, 1)

x <- data.frame(rmvnorm(100, c(0, 0, 1), diag(3) + matrix(.2, 3, 3)))
y <- x$X1 + x$X2 + x$X3^2
l <- lm(y ~ X1 + X2 + X3, data=x)
by1var.seq(l)

l <- lm(y ~  x + z + w)
by1var(l, "x", thin=1)
by1var(l, "z")

y <- rnorm(100, 10, 1)
x <- exp(y) + rnorm(100, 1, 1)
z <- rnorm(100, 1, 1)
l <- lm(y ~ x + z)
by1var.seq(l)


pdf("test.pdf")
by1var.seq(l)
dev.off()

l <- lm(mpg ~ wt + hp, data=mtcars)
by1var(l, "wt")

d <- read.csv('rts.csv')
d$AgeSubjectNum <- as.numeric(d$AgeSubject)
d$RTnaming.e = exp(d$RTnaming)
l <- lm(RTnaming.e ~ AgeSubject + WrittenFrequency + LengthInLetters + Familiarity, data=d)
by1var.seq(l, thin.val=.1, bub='AgeSubject')
