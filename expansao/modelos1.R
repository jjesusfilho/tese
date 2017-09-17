Mod1<-plm(form4,data=df7L,index=c("sigla","ano"),model="within",effect="individual")
Mod2<-plm(form4,data=df7L,index=c("sigla","ano"),model="random",effect="individual")
Mod3<-plm(update(form4,~.-gini-desem),data=df7L,index=c("sigla","ano"),model="within",effect = "individual")
Mod4<-plm(update(form4,~.-gini-desem),data=df7L,index=c("sigla","ano"),model="random",effect="individual")
Mod5<-spml(formula=update(form4,~.-gini-desem-juizes.brancos),data=df7L,index=c("sigla","ano"),
              model="within",  listw=s)
Mod6<-spml(formula=update(form4,~.-gini-desem),data=df7L,index=c("sigla","ano"),model="random",listw=s)
Mod7 <- lme(update(form4,~.-gini-desem), random=~1|sigla/ano, cor=corAR1(0.8, form= ~ 1|sigla/ano), data=df7L)

#Diagnóstico


sst <- with(df7L, sum((taxaEnc - mean(taxaEnc))^2))
m1.sse <- t(residuals(Mod7)) %*% residuals(Mod7)
(sst - m1.sse) / sst

htmlreg(modelos,
custom.model.names=c("Mod1","Mod2",
"Mod3","Mod4","Mod5","Mod6","Mod7")

)

htmlreg(modelos,
        custom.model.names=c("Mod1","Mod2", "Mod3","Mod4","Mod5","Mod6","Mod7"),
        file = "modelos.docx", inline.css = FALSE,
        doctype = TRUE, html.tag = TRUE, head.tag = TRUE, body.tag = TRUE)


library(ggplot2)

dfA<-data.frame()
for (i in 1:7){
bacf <- acf(residuals(a[[i]], plot = FALSE))
bacfdf <- with(bacf, data.frame(lag, acf,modelo=paste0("Mod",i)))
dfA<-rbind(dfA,bacfdf)
}

q <- ggplot(data = dfA, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  facet_wrap(~modelo,ncol=3)
q

ggsave("~/Desktop/TESE/figure/autocorrelacao.pdf",width=15,height=10)

