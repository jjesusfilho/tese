a<-summary(fe.spml)

names<-row.names(a$CoefTable)
co<-a$CoefTable[,1]
se<-a$CoefTable[,2]
pval<-a$CoefTable[,4]
gof.names<-c("logLik","AIC","BIC")
gof<-c(logLik(a),AIC(a),BIC(a))
trFE <- createTexreg( # create a texreg object
  coef.names=names,
  coef=co,
  se=se,
  pvalues=pval,
  gof.names=gof.names,
  gof=gof
)

a<-summary(re.spml)

names<-row.names(a$CoefTable)
co<-a$CoefTable[,1]
se<-a$CoefTable[,2]
pval<-a$CoefTable[,4]
gof.names<-c("logLik","AIC","BIC")
gof<-c(logLik(a),AIC(a),BIC(a))
trRE <- createTexreg( # create a texreg object
  coef.names=names,
  coef=co,
  se=se,
  pvalues=pval,
  gof.names=gof.names,
  gof=gof
)


a<-summary(ENCAR)

names<-row.names(a$tTable)
co<-a$tTable[,1]
se<-a$tTable[,2]
pval<-a$tTable[,5]
gof.names<-c("Pseudo-R&^2$M","Pseudo-R&^2$C","logLik","AIC","BIC")
gof<-c(r.squaredGLMM(a)[1],r.squaredGLMM(a)[2],logLik(a),AIC(a),BIC(a))
trEN <- createTexreg( # create a texreg object
  coef.names=names,
  coef=co,
  se=se,
  pvalues=pval,
  gof.names=gof.names,
  gof=gof
)


names<-row.names(a$tTable)
co<-a$tTable[,1]
se<-a$tTable[,2]
pval<-a$tTable[,5]
gof.names<-c("Pseudo-R&^2$M","Pseudo-R&^2$C","logLik","AIC","BIC")
gof<-c(r.squaredGLMM(a)[1],r.squaredGLMM(a)[2],logLik(a),AIC(a),BIC(a))
trEN <- createTexreg( # create a texreg object
  coef.names=names,
  coef=co,
  se=se,
  pvalues=pval,
  gof.names=gof.names,
  gof=gof
)


texreg(modelos,
       custom.model.names=c("Modelo1","Modelo2",
                            "Modelo3","Modelo4"),
       caption="Modelos de regressão com dados em painel",
       caption.above = T,
       label="tab:coeficientes"
       )

ENCAR <- lme(form4, random=~1|sigla/ano, cor=corAR1(0.8, form= ~ 1|sigla/ano), data=df7L)




