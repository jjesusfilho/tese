setwd("~/Documents/tese/tese")
library(lavaan)
library(semPlot)
library(RColorBrewer)
library(stringr)

modelo1<-"autonomia =~ v1+v41+v19+v20+v18+v17
proximidade=~ v5+v4+v6+v9
punitivismo =~ v7+v8+v10+v27
autonomia + proximidade + punitivismo ~ v43 + v44 + v45 + v46 + v47"

## Variáveis utilizadas:
s<-unlist(str_extract_all(modelo1,"v\\d+"))

# Extração das descrições
endogenas<-as.character(dic[match(s,dic$var),]$descricao[1:14])

## Quebra as linhas para caber nas caixas de diálogo
endogenas<-str_wrap(endogenas,width=25)

fit1<-cfa(modelo1,data=dados)

pal3<-brewer.pal(5,"Set3")
pal2<-brewer.pal(4,"Set2")
pal1<-brewer.pal(4,"Set1")


nodeW<-c(rep(2.5,14),rep(1.2,5),rep(1,3))
nodeH<-c(rep(2.2,14),rep(1,5),rep(1,3))

label.size<-c(rep(1.2,14),rep(1,5),rep(1,3))

labs<-c(endogenas,c("idade","sexo","segurança da unidade","unidades administradas","tempo de SAP"),c("autonomia","proximidade","punitivista"))

colors<-c(rep(pal2[1],6),rep(pal2[2],4),rep(pal2[3],4),pal3,pal2[c(1:3)])




modelo1<-"autonomia =~ v1+v41+v19+v20+v18+v17
proximidade=~ v4+v5+v6+v9
punitivismo =~ v7+v8+v10+v27
autonomia + proximidade + punitivismo ~ v43 + v44 + v45 + v46 + v47"

## Isso precisar ser feito, do contrário, não plota
fit1@SampleStats@cov<-fit1@SampleStats@res.cov


semPaths(modelo1,
         #what="est",
         nodeLabels=labs,
         layout="tree2",
         nCharNodes = 0,
         intercepts = F,
         curve=F,
         rotation=2,
         residuals=F,
         exoVar = F,
         exoCov=F,
         groups=c("man","lat"),
         sizeMan=5,
         sizeMan2 = 2.5,
         sizeLat = 3,
         sizeLat2 = 3,
         color=colors,
         vTrans=50,
         #trans=0,
         levels=c(2,3,4,7),
         node.width=nodeW,
         node.height=nodeH,
         normalize=T,
         label.cex=label.size,
         label.prop=.8,
         label.scale=T,
         height=20,
         width=15,
         mar=c(1,5,1,5),
         filetype="svg",
         filename="~/Desktop/TESE/figure/graficoSEM"
)





summary(fit1,standardized=T,rsquare=T,fit.measures=T)

### Verificando os coeficientes e intervalos de confiança

parameterEstimates(fit1,standardized=T)

### Verificando a matriz de covariância ajustada

fitted(fit)

### Verificando o resíduo

fit1.res.cor<-residuals(fit1,type="cor")

residuos<-as.data.frame(fit1.res.cor$cor)
residuos<-rownames_to_column(residuos,var="indicadores")
saveRDS(residuos,"fit_res_cor.rds")

### Medidas de Ajuste do Modelo

ajuste<-inspect(fit1,"fit")

ajuste<-as.data.frame(ajuste)
ajuste<-rownames_to_column(ajuste,"Medida")
names(ajuste)[2]<-"Valor"
ajuste$Valor<-round(ajuste$Valor,2)
saveRDS(ajuste,"ajuste.rds")

11.671/22.784

myData <- matrix(c(19,89,23,23,74,44,16,39,67),ncol=3,byrow=TRUE)
colnames(myData) <- c("A","B","C")
rownames(myData) <- c("1","2","3")
myData2 <- myData * 2

