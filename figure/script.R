library(lavaan)
library(semPlot)
library(RColorBrewer)

## Base
dados<-baseEdic[[1]]

## Dic

dic<-baseEdic[[2]]

## Transforma as variáveis em ordinais
meusniveis <- c("Discordo totalmente","Discordo","Mais discordo que concordo", "Não discordo, nem concordo", "Mais concordo que discordo", "Concordo", "Concordo totalmente")

dados[,1:42]<-lapply(dados[,1:42],function(x) ordered(x,levels=meusniveis))

dados[,c(43,45,47)]<-lapply(dados[,c(43,45,47)], function(x) as.numeric(as.factor(as.character(x))))



## Modelo completo

modelo1<-"autonomo =~ v41+v19+v20+v18+v17
      dialogal =~ v6+v5+v25+v4+v9
disciplinar =~ v2+v12+v15
punitivo =~ v3+v7+v10+v14+v11+v8\nautonomo + dialogal + disciplinar + punitivo ~ v43 + v44 + v45 + v46 + v47"

## Tweaking
modelo1<-"autonomia =~ v1+v41+v19+v20+v18+v17
          proximidade=~ v4+v5+v6+v9
          punitivismo =~ v7+v8+v10+v27
          autonomia + proximidade + punitivismo ~ v43 + v44 + v45 + v46 + v47"

punitivo =~ v3+v11+v14


# Modelo sem a parte estrutural

modelo2<-"autonomo =~ v41+v19+v20+v18+v17
dialogal =~ v6+v5+v25+v4+v9
disciplinar =~ v2+v12+v15
punitivo =~ v3+v7+v10+v14+v11+v8
"

## Sem disciplina
modelo3<-"autonomo =~ v41+v19+v20+v18+v17
        proximidade=~v25+v5+v9
      dialogal =~ v6+v10

punitivo =~ v3+v7+v10+v14+v11+v8\nautonomo + proximidade+ + punitivo ~ v43 + v44 + v45 + v46 + v47"


fit1<-cfa(modelo1,data=dados,estimator="WLSMV")

summary(fit1,standardized=T,fit.measures=T)

mod_ind <- modificationindices(fit1)

head(mod_ind[order(mod_ind$mi, decreasing=TRUE), ], 10)
subset(mod_ind[order(mod_ind$mi, decreasing=TRUE), ], mi > 5)


fit2<-cfa(modelo2,data=dados)

fit3<-cfa(modelo3,data=dados)
