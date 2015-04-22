library(scales)
library(ggplot2)
library(data.table)
library(dplyr)
library(gridExtra)
library(dplyr)
library(mvtnorm)
source('multiplot.R')

dotests = F

call.with = function(f,fixedArgs,varArgs,elseResult=geom_blank()) {
  if (!is.null(varArgs)) {
    return(do.call(f,c(fixedArgs,varArgs)))
  }
  return(elseResult)
}

by1var = function(oldLm, var, thin=1, breakupby=FALSE, 
                  adjustedData=F, 
                  rawData=F,
                  connections=F, 
                  loess=F,
                  line=F,
                  rug=F
                  ) {
  
  #set up defaults
  if (identical(FALSE,adjustedData)) adjustedData=list(color="red")
  if (identical(FALSE,rawData)) rawData=list(alpha=0.3)
  if (identical(FALSE,connections)) connections=list(alpha=0.3) 
  if (identical(FALSE,loess)) loess=list(size=0, fill = 'blue', color = 'blue', alpha=.2, se=T)
  if (identical(FALSE,line)) line=list(color="red", fill='red', alpha=.3, se=F)
  if (identical(FALSE,rug)) rug=list(col=rgb(.5,0,0,alpha=.2))
  
  
  varName <- var
  #print(varName)
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
  bothData = rbind(d1,d2,use.names=TRUE) #I have no idea why we have to say use.names=TRUE. Cols are same order but it whines without this.
  bothData$residOfFull = predict(oldLm) #oldLm$resid + adjustment # = bothData[,residOfFull:=oldLm$resid + adjustment]
  
  #print(outcomeVar)
  #print(varName)
  #print(names(bothData))
  #print(newLm$coef)
  #print(c(1, sapply(newLm$model[,2:dim(newLm$model)[2]],mean)))
  #print(head(bothData))
  #print(dim(subset(bothData,tp_ == "raw")))
  bothData = data.frame(bothData)
  
  sampled.data = sample(1:max(bothData$gp_), round(thin * max(bothData$gp_)) )
  #sampled.data = sample(1:nrow(bothData), round(thin * nrow(bothData)) )
  
  title.vars = new.data[1, names(newLm$model)]
  title.vars = title.vars[names(title.vars) %in% breakupby == F & names(title.vars) != toString(outcomeVar)]
  title.val = "Linear adjustment to values...\n"
  for (i in 1:length(title.vars)) {
    title.val = paste(title.val, names(title.vars[i]), ": ", round(title.vars[i], 2), "\n", sep="")
  }
  
  if (breakupby == FALSE) {bothData[, 'breakupby'] = 1} else {bothData[, 'breakupby'] = bothData[, breakupby]}
  if (isfactor == FALSE) {  
     p <- ggplot(data=bothData[bothData$gp_ %in% sampled.data, ], aes_string(y=toString(outcomeVar),x=varName ))+
          call.with(geom_point,
                    list(data=subset(bothData[bothData$gp_ %in% sampled.data, ],tp_ == "raw")), rawData)+
          call.with(geom_point, 
                    list(data=subset(bothData[bothData$gp_ %in% sampled.data, ],tp_ == "adj")), adjustedData) + 
          call.with(geom_line,list(aes(group=gp_)), connections)+
          call.with(geom_smooth,
                    list(method=lm,data=subset(bothData,tp_ == "adj")), line)+
          call.with(geom_smooth,
                    list(method="loess",data=subset(bothData,tp_ == "adj")), loess)+
          call.with(geom_rug,list(aes(y=residOfFull),data=subset(bothData,tp_ == "adj")),rug) + 
          theme_bw() + facet_grid(. ~ breakupby) + ggtitle(title.val) + theme(plot.title = element_text(size=10, hjust=1))
  }  else { 
      bothData[, varName] = as.factor(bothData[, varName])
      p <- ggplot(data=bothData[bothData$gp_ %in% sampled.data, ], aes_string(y=toString(outcomeVar),x=varName ))+
        geom_boxplot(color="black", data=subset(bothData,tp_ == "raw"), alpha=.3)+
           geom_boxplot(color="red", data=subset(bothData,tp_ == "adj")) + geom_line(aes(group=gp_), alpha=0.3)+
           geom_smooth(method=lm,data=subset(bothData,tp_ == "adj"), color="red", fill='red', alpha=.3, se=F)+
           geom_smooth(data=subset(bothData,tp_ == "adj"), size=0, fill = 'blue', color = 'blue', alpha=.2, se=T)+
           geom_rug(aes(y=residOfFull),data=subset(bothData,tp_ == "adj"),col=rgb(.5,0,0,alpha=.2)) + 
           theme_bw() + facet_grid(. ~ breakupby) + ggtitle(title.val) + theme(plot.title = element_text(size=10, hjust=1))
  }
  
  
  
  #lastcall = eval(bquote(substitute(.(gcall),list(RESPONSE=attributes(terms(formula(oldLm)))$variables[[2]],
  #                                          RESID=quote(newLm$resid))
  #                            )))
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

by1var.seq <- function(l, ...) {
  p.list <- list()
  vars <- names(l$model)[2:length(names(l$model))]
  for (i in 1:length(vars)) { p <- by1var(l, vars[i], ...)
                              p.list[[i]] <- p}
  multiplot(plotlist=p.list, cols = 2)
}


#tests
if (dotests) {
  
  
  n = 160
  x <- data.frame(rmvnorm(n, c(0, 0, 1), .5 * (diag(3) + matrix(.3, 3, 3))))
  x$X4 <- c(rep(1, n/2), rep(0, n/2))
  y <- x$X1 + 2*x$X2 + x$X3^2 + rnorm(n,0,.2) + rnorm(n, x$X4, 1)
  x$X4 <- as.factor(x$X4)
  l <- lm(y ~ X1 + X2 + X3 + X4 , data=x)
  plot(l)
  by1var.seq(l)
  
  by1var(l, "X1", thin=1)
  by1var(l, "X3")
  
  y <- rnorm(100, 10, 1)
  x <- exp(y) + rnorm(100, 1, 1)
  z <- rnorm(100, 1, 1)
  l <- lm(y ~ x + z)
  by1var.seq(l)
  
  
  pdf("test.pdf")
  by1var.seq(l)
  dev.off()
  
  #example with 1
  x <- rnorm(n)
  y <- x + rnorm(n)
  xy <- data.frame(cbind(x, y))
  xy$z <- rep(0, n)
  with(xy, plot(x, y))
  l <- lm(y ~ 1 + x + z, data=xy)
  by1var(l, 'x') #(l, 'x', 'z')
  
  l <- lm(mpg ~ wt + hp, data=mtcars)
  by1var(l, "wt")
  
  d <- read.csv('rts.csv')
  d$AgeSubjectNum <- as.numeric(d$AgeSubject)
  d$RTnaming.e = exp(d$RTnaming)
  d$WrittenFrequency.e <- exp(d$WrittenFrequency)
  l <- lm(RTnaming.e ~ AgeSubject + WrittenFrequency.e + LengthInLetters , data=filter(d, WrittenFrequency.e < 50000))
  l <- lm(RTnaming.e ~ AgeSubject + WrittenFrequency + LengthInLetters , data=filter(d, WrittenFrequency.e < 50000))
  l <- lm(RTnaming ~ AgeSubject + WrittenFrequency + LengthInLetters , data=filter(d, WrittenFrequency.e < 50000))
  
  by1var.seq(l, thin=.1, breakupby='AgeSubject')
}