library(scales)
library(ggplot2)
library(data.table)
library(dplyr)

by1var = function(oldLm, var) {
  #toString(substitute(var))
  varName = dputToString(substitute(var))
  outcomeVar = attributes(terms(formula(oldLm)))$variables[2]
  varAsFormula = reformulate(termlabels=varName,intercept=FALSE)
  
  newLm = lm(formula(oldLm) - varAsFormula, data=oldLm$model)
  
  #newLm
  #data=cbind(oldLm$model,newLm$resid)
  d1 = data.table(oldLm$model)
  #data
  adjustment = sum(newLm$coef * c(1, colMeans(newLm$model)[2:dim(newLm$model)[2]]))
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
  print(adjustment)
  print(sapply(newLm$model[,2:dim(newLm$model)[2]],mean))
  print(newLm$coef)
  print(head(bothData))
  print(dim(subset(bothData,tp_ == "raw")))
  gcall = quote(ggplot(data=bothData, aes_string(y=toString(outcomeVar),x=varName ))+
                  geom_point(data=subset(bothData,tp_ == "raw"),alpha=0.2)+
                  geom_point(color="red", data=subset(bothData,tp_ == "adj"))+
                  geom_line(aes(group=gp_), alpha=0.1)+
                  geom_smooth(method=lm,data=subset(bothData,tp_ == "adj"), color="red")+
                  geom_smooth(data=subset(bothData,tp_ == "adj"), color = alpha("red",0.2),alpha=0.2)+
                  geom_rug(aes(y=residOfFull),data=subset(bothData,tp_ == "adj"),col=rgb(.5,0,0,alpha=.2))
                )
  
  #lastcall = eval(bquote(substitute(.(gcall),list(RESPONSE=attributes(terms(formula(oldLm)))$variables[[2]],
  #                                          RESID=quote(newLm$resid))
  #                            )))
  print(gcall)
  eval(gcall)
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