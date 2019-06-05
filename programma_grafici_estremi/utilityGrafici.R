##############################################################
#Parametri che variano con il parametro da elaborare
#Precipitazione e i suoi estremi
if(grepl("^(P|p)r.?c.*$",PARAM)){
  
  #valori positivi e negativi delle barre 
  c("green","yellow")->coloriBarre
  
  
}else{ #Temperatura e i suoi estremi
 
  #valori positivi e negativi delle barre 
  #c("steelblue4","firebrick")->coloriBarre
  c("#2A65CF","#CF2A42")->coloriBarre

}
##############################################################

creaListaParametri<-function(symAsseY,limitiAsseY,stepAsseY,tx,ty,modificaEtichette){

  BREAKS<-seq(limitiAsseY[1],limitiAsseY[2],stepAsseY)
  ETICHETTE<-BREAKS
  ETICHETTE[grepl(modificaEtichette,ETICHETTE)]<-""

  list(
    simboloAsseY=symAsseY,
    limitiY=limitiAsseY,
    stepY=stepAsseY,
    breaksY=BREAKS,
    etichetteY=ETICHETTE,
    testoANNX=tx,
    testoANNY=ty
  )
  
}#fine funzione

##############################################################
#Range dell asse Y
if(grepl("^(P|p)r.?c.*$",PARAM)){
  
}else{
  
  #anomalie di temperatura
  if(grepl("^(T|t)m(a|i|e).+$",PARAM)){
    
    if(grepl("^(T|t)max$",PARAM))  SIMBOLO<-"Temperatura Massima (°C)"
    if(grepl("^(T|t)min$",PARAM))  SIMBOLO<-"Temperatura Minima (°C)"    
    if(grepl("^(T|t)mean$",PARAM)) SIMBOLO<-"Temperatura Media (°C)"    
    
    creaListaParametri(symAsseY=SIMBOLO,limitiAsseY=c(-1,2),stepAsseY=0.25,tx=NULL,ty=NULL,modificaEtichette="^-?.\\..5$")->plista
    
  }else if(PARAM %in% c("fd")){#FINE TMAX/TMIN...
  
    SIMBOLO<-"Giorni con Gelo"
    creaListaParametri(symAsseY=SIMBOLO,limitiAsseY=c(-35,15),stepAsseY=5,tx=NULL,ty=NULL,modificaEtichette="^-?.+5$")->plista
    
    
  }else if(PARAM %in% c("tr")){#FINE TMAX/TMIN...
    
    SIMBOLO<-"Notti Tropicali"
    creaListaParametri(symAsseY=SIMBOLO,limitiAsseY=c(-15,35),stepAsseY=5,tx=NULL,ty=NULL,modificaEtichette="^.*5$")->plista
    
    
  }else if(PARAM %in% c("su")){#FINE TMAX/TMIN...
    
    SIMBOLO<-"Giorni Estivi"
    creaListaParametri(symAsseY=SIMBOLO,limitiAsseY=c(-20,35),stepAsseY=5,tx=NULL,ty=NULL,modificaEtichette="^.*5$")->plista
  }else if(PARAM %in% c("wsdi")){#FINE TMAX/TMIN...
    
    SIMBOLO<-"WSDI (giorni)"
    creaListaParametri(symAsseY=SIMBOLO,limitiAsseY=c(-10,50),stepAsseY=5,tx=NULL,ty=NULL,modificaEtichette="^.*5$")->plista
    
  }else if(PARAM %in% c("tn10p")){#FINE TMAX/TMIN...
    
    SIMBOLO<-"Notti Fredde (%)"
    creaListaParametri(symAsseY=SIMBOLO,limitiAsseY=c(-8,5),stepAsseY=1,tx=NULL,ty=NULL,modificaEtichette="^-?(9|7|5|3|1)$")->plista
    
  }else if(PARAM %in% c("tx10p")){#FINE TMAX/TMIN...
    
    SIMBOLO<-"Giorni Freddi (%)"
    creaListaParametri(symAsseY=SIMBOLO,limitiAsseY=c(-8,6),stepAsseY=1,tx=NULL,ty=NULL,modificaEtichette="^-?(9|7|5|3|1)$")->plista
    
  }else if(PARAM %in% c("tx90p")){#FINE TMAX/TMIN...
    
    SIMBOLO<-"Giorni Caldi (%)"
    creaListaParametri(symAsseY=SIMBOLO,limitiAsseY=c(-8,18),stepAsseY=2,tx=NULL,ty=NULL,modificaEtichette="3$")->plista
    
  }else if(PARAM %in% c("tn90p")){#FINE TMAX/TMIN...
    
    SIMBOLO<-"Notti Calde (%)"
    creaListaParametri(symAsseY=SIMBOLO,limitiAsseY=c(-6,18),stepAsseY=2,tx=NULL,ty=NULL,modificaEtichette="3$")->plista
  }      

}
###############################################################



###############################################################
#Tema ggplot2 per grafici: questo tema comune a tutti i grafici
theme_bw()+
theme(panel.grid.major.y = element_line(color="#CCCCCC",size=0.125,linetype = 2),
      panel.grid.major.x=element_blank(),
      panel.grid.minor = element_blank(),
      text=element_text(family="Lato",size=16),
      axis.ticks =   element_line(colour="#333333"),
      axis.ticks.length = unit(0.15,"cm"),
      axis.text.x = element_text(family="Lato",colour = "#000000",size = 14,margin=margin(5.5,0,0,0)), 
      axis.text.y = element_text(family="Lato",colour = "#000000",size = 14,margin=margin(0,5.5,0,2.5)),
      axis.title = element_text(family="Lato",colour = "#000000"),
      strip.background = element_blank(),
      strip.text = element_blank(),
      panel.spacing = unit(1.0, "lines"))->tema
###############################################################


###############################################################
#Funzione per creare le etichette per l'asse x
#xi: primo anno disponibile
#xf: ultimo anno disponibile (annoF)
creaEtichetteX<-function(xi,xf,step=10){
  
  trovaEstremi<-function(x){
    
    #primo anno a terminare con lo zero dopo x, ad esempio 1970 se xi un anno nella decade dei '60
    as.integer(ceiling(x/10)*10)->y
    
    #ultimo anno a finire con 0 prima di x
    as.integer(floor(x/10)*10)->z
    
    list(z,y)    
    
  }#fine trovaEstremi
  
  
  trovaEstremi(x=xi)->estremiXi
  trovaEstremi(x=xf)->estremiXf  
  
  purrr::map(seq(estremiXi[[1]],estremiXf[[2]],by=step),.f=function(.x){
    c(.x,rep("",step-1))
  }) %>% unlist->innerPart
  
  seq(estremiXi[[1]],estremiXf[[2]])->serie
  which(serie==xi)->gapI
  which(serie==xf)->gapF
  
  innerPart[gapI:gapF]->innerPart
  innerPart[1]<-xi
  innerPart[length(innerPart)]<-xf
  
  innerPart 
  
}#fine creaEtichette
###############################################################



