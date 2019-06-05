  #5 giugno dicembre 2019: programma aggiornato
  #Utilizzato sulle serie passate ai controlli giornalieri 
  #e sugli indici calcolati mediante il pacchetto R clim.pcic
  #Il programma elimina le serie con più del 50% dei valori pari a 0
  rm(list=objects())
  library("dplyr")
  library("stringr")
  library("readr")
  library("magrittr")
  #calcolo delle anomalie rispetto al trentennio 1961-1990
  #Il valore climatologico viene calcolato dalla funzione climatol6190
  library(zyp)
  options(error=recover)
  
  annoI<-1961
  annoF<-2018
  
  #anni per il calcolo del climatologico
  climYS<-1961
  climYE<-1990
  

  ########################################################################    
  #Funzioni
  ############################################
  media<-function(x,toll){
    
    length(which(is.na(x))) ->numero.na
    if(numero.na) if(numero.na>toll) return(NA)
    
    return(round(mean(x,na.rm=TRUE),2))
  
  }#funzione media
  #######################
  
  ########################################
  #funzione per lettura dati
  leggiFile<-function(nome){
    
    separatori<-c(",",";")
    
    for(ss in separatori){
      tbl_df(read.csv(file=nome,sep=ss,check.names=F,head=T,stringsAsFactors=FALSE))->dati
      if(ncol(dati)>1) return(dati)
    }#fine ciclo for
    
    return(NULL)
    
  }#fine leggiFile
  ############################  
  ########################################################################
  
  nome.file<-list.files(pattern="^risultati.+csv$")

  #questi due comandi servono per estrarre dal nome file il codice identificativo dell'indicatore.
	#il comando unlist serve perchè strsplit restituisce una lista
	unlist(str_split(nome.file,"_"))[2]->indice
	str_replace(indice,"\\..+","")->indice
	
	#lettura file dati di input
	leggiFile(nome=nome.file)->dati
	stopifnot(!is.null(dati))
	
	#2019: Eliminiamo le serie di 0
	dati %>%
	  dplyr::select(-yy) %>%
	  mutate_all(.funs=function(x){x[x!=0]<-NA;x[x==0]<-1;x  }) %>%
	  summarise_all(.funs=sum,na.rm=TRUE)->numeroGiorniNulli
	
	#numero di anni
	annoF-annoI+1->numeroAnni
	
	#percentuale dati nulli
	((numeroGiorniNulli/numeroAnni)*100)->percentualeGiorniNulli
	
	which(percentualeGiorniNulli>50)->colonneConSoloZeri
	
	if(length(colonneConSoloZeri)!=0){
	  
	  dati[,-(colonneConSoloZeri+1)]->dati
	  
	}#fine if
	
	
	(ncol(dati)-1)->numero.stazioni

	
  #dati senza colonna yy: applico la funzione media e mi restituisce un vettore di valori
	#climatologici: uno per stazione

	#al massimo 10 anni NA per il calcolo del climatologico
	dati %>% filter(yy>=climYS & yy<= climYE) %>% select(-yy)->subDati
	
	purrr::map_df(subDati,.f=media,toll=10)->mclimatol
	
	mclimatol %>%
	  tidyr::gather(key="idstaz",val="climatologico-1961-1990")->df.mclimatol

	#data.frame per scrittura risultati
#	tbl_df(as.data.frame(cbind(names(mclimatol),t(mclimatol))))->df.mclimatol
#	names(df.mclimatol)<-c("idstaz",paste0("climatologico-",climYS,"-",climYE))
	
	#calcolo anomalie
	mapply(FUN=function(x,y){
	  round(x-y,2)
	},x=dati[,2:ncol(dati)],y=mclimatol,SIMPLIFY=FALSE)->lista.anomalie #fine mapply
	as_tibble(lista.anomalie)->anomalie.ok
	
  #medie per riga: anomalia Italia anno per anno
	#Soglia: 100. Si era inizialmente deciso di tollerare il 20% di NA, ma di fatto 
	#(per questioni grafiche) Franco ha richiesto il calcolo di tutte le anomalie.
	#Solo gli anni particolarmente pieni di NA vengono annullati. QUesti anni variano
	#a seconda dell'indicatore
	apply(anomalie.ok,1,FUN=media,toll=100)->anomalia.italia
	tbl_df(as.data.frame(cbind(dati$yy,anomalie.ok)))->df.anomalie.ok	
	names(df.anomalie.ok)[1]<-"yy"
	
	#annulliamo gli anni
	tbl_df(data.frame(yy=dati$yy,anom.originale=anomalia.italia))->df.anomalia.italia
	df.anomalia.italia %<>%mutate(anom=anomalia.italia)	
# 	which(df.anomalia.italia$yy %in% annullare)->index.annullare
#   if(length(index.annullare)){df.anomalia.italia$anom[index.annullare]<-0}

	write_delim(df.anomalia.italia,path=glue::glue("anom_{indice}_{annoI}_{annoF}_acmant1961_1990_spline.csv"),col_names=T,delim=";")
	write_csv(df.mclimatol,path=sprintf("climatologici_%s.csv",indice),col_names=T)
	write_csv(df.anomalie.ok,path=sprintf("anomalie_%s.csv",indice),col_names=T)

	#calcolo del trend mediante Theil-Sen (utilizzando pre-whitening)
  df.anomalia.italia %>% extract2("anom") %>% zyp.zhang(.)->zyp.out
	names(zyp.out)->nomi.zyp
	
	tbl_df(data.frame(nomi.zyp,zyp.out)) %>% write_csv(.,path=sprintf("trend_%s.csv",indice),col_names=TRUE)





