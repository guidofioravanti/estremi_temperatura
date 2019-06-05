rm(list=objects())
library("tidyverse")

annoI<-1961
annoF<-2018

#eventuale suffisso da aggiungere al nome del file pdf di output
SUFFISSO<-""

#Lista dei file: ci aspettiamo solo un file
list.files(pattern="^anom_.+_.+csv")->nomeFile
stopifnot(length(nomeFile)==1)

#Ricavo dal nome file il parametro da elaborare
unlist(stringr::str_split(nomeFile,pattern = "_",n=3))[2]->>PARAM

#Possibili parametri
estremiTemp<-c("fd","tr","su","wsdi","tn10p","tx10p","tx90p","tn90p")
possibiliParametri<-c("tmax","tmin","tmean","prcp",estremiTemp)

if(!PARAM %in% possibiliParametri){
  stop(glue::glue("Parametro {PARAM} non riconosciuto"))
}else{
  print("carico utilityGrafici.R")
  source("utilityGrafici.R")
}

#############################
#Creazione etichette asseX
creaEtichetteX(xi=annoI,xf=annoF,step=5)->etichetteX

#############################
#Lettura dei dati
read_delim(nomeFile,delim=";",col_names = TRUE)->df 

#############################
#Se df contiene la colonna "serie" allora si tratta del grafico della serie di anomalia Italia vs Globale
#############################
if(length(grep("Globale",names(df)))){
  GLOBALE<-TRUE
  SUFFISSO<-"_Italia_vs_Globale"
  dplyr::select(df,yy,anom,Globale)->df
}else{
  GLOBALE<-FALSE
  df %>%
    dplyr::select(yy,anom)->df 
}#fine if

mutate(df,segno=ifelse(anom>=0,"1","0"))->df


#Grafico
ggplot(data=df)+
  tema+
  geom_hline(yintercept = 0,col="#333333")+
  xlab("")+
  ylab(plista$simboloAsseY)+
  scale_y_continuous(limits=plista$limitiY,breaks =plista$breaksY,expand = c(0,0),labels=plista$etichetteY)+
  scale_x_continuous(limits=c(annoI-1,annoF+1),breaks=seq(annoI,annoF),labels=etichetteX,expand = c(0,0))->graficoOut

graficoOut+
  geom_bar(data=df,aes(x=yy,y=anom,fill=segno,alpha=1),stat="identity")+
  scale_fill_manual(values=coloriBarre,guide=F)+
  scale_alpha(range=c(0.7,1),guide=F)->graficoOut 


if(GLOBALE){
  graficoOut+
    geom_line(data=df,aes(x=yy,y=Globale),alpha=1,lwd=1.5,colour="#1E334B")+
    geom_point(data=df,aes(x=yy,y=Globale),pch=21,bg="#FFFFFF",size=1.5)->graficoOut  
}
  
if(!is.null(plista$testoANNX)){

  graficoOut+annotate("text",x=plista$testoANNX,y=plista$testoANNY,label=c(Hmisc::capitalize(PARAM)),family="Lato",size=7)->graficoOut

}#FINE IF SU testoANNX    

cairo_pdf(glue::glue("grafico_{PARAM}_anomalie{SUFFISSO}.pdf"),12,8)
print(graficoOut)
dev.off()