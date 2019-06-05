rm(list=objects())
options(error=NULL)
library("readr")
library("RPostgres")
library("purrr")
library("magrittr")
library("dplyr")
library("tidyr")
library("parallel")
library("stringr")
library("chron")
library("climdex.pcic")
library("sf")
library("maps")
source("ClimateData.R")


#quale parametro?
PARAM<-c("Tmax","Tmin","Prcp")[2]
#calcolare indici a livello mensile?
CALCOLA.MENSILI<-FALSE
ANNOI<-1961
ANNOF<-2018

#file dati con le serie omogeneizzate
list.files(pattern=sprintf("^%s.csv$",PARAM))->file.dati
stopifnot(length(file.dati)==1)

#lettura anagrafica dal database
dbConnect(RPostgres::Postgres(),dbname="scia",user="guido",host="localhost",port=5432)->myconn
dbGetQuery(myconn,statement="SELECT * FROM anagrafica.stazioni")->ana
dbGetQuery(myconn,statement = "SELECT * FROM tbl_lookup.rete_guido_lp")->reti
dbDisconnect(myconn)

left_join(ana,reti,by=c("cod_rete_guido"="cod_rete")) %>%
  mutate(codice=paste0(nome_rete,"_",siteid))->ana

###################################################
#lista indici
if(PARAM=="Tmax"){
  indici<-c("txx","txn","tx10p","tx90p","su","wsdi")  
}else if(PARAM=="Tmin"){
  indici<-c("tnx","tnn","tn10p","tn90p","fd","tr","csdi")  
}else if(PARAM=="Prcp"){
  indici<-c("cdd","cwd","prcptot","r10mm","r20mm","r95ptot","r99ptot","rx1day","rx5day","sdii")
}else{
  stop("parametro non riconosciuto")
}
###################################################


#quali di questi sono mensili
indiciMensili<-c("rx1day","rx5day","tn10p","tn90p","tx10p","tx90p","tnn","tnx","txn","txx")

#lettura file dati
read_delim(file.dati,delim=";",col_names = TRUE,col_types = cols(yy="i",mm="i",dd="i",.default = col_double())) %>% 
  filter(yy>=ANNOI & yy<=ANNOF)->dati # %>% dplyr::select(yy,mm,dd,trentino_6521)->dati


(dati %>% mutate(calendario.pcic=as.PCICt(paste(yy,mm,dd,sep="-"),cal="gregorian")))[["calendario.pcic"]]->calendario

cpar<-tolower(PARAM)


purrr::walk(indici,.f=function(indice){
  
  #crea directory con il nomde dell'indicatore
  cat(sprintf("ELABORAZIONE INDICI PER: %s\n",indice))  
  if(!dir.exists(indice)) try(dir.create(indice))  
  
  #gli indici in generale nn richiedono questa opzione, default la stringa è vuota
  opzione.stringa.climdex<-""    
  
  ###################################################
  #però se un indice è un indice calcolabile anche su base mensile e si vuole
  #calcolare su base mensile allora dobbiamo distinguire
  if(indice %in% indiciMensili){
    
    if(CALCOLA.MENSILI){
      #indici mensili
      opzione.stringa.climdex<-",freq=c('monthly')"
    }else{
      opzione.stringa.climdex<-",freq=c('annual')"
    }

  }    

  purrr::imap(dati %>%select(-yy,-mm,-dd) ,.f=function(.x,.y){
    

    #stringa: varia in base a cpar, fissato nel ciclo for su ff 
    stringa.climdex<-paste0("climdexInput.raw(",cpar,"=.x,",cpar,".dates=calendario,",
                                    "base.range=c(1961, 1990), max.missing.days=c(annual=15,monthly=31),min.base.data.fraction.present=0.1 )") 
                      
        #creazione oggetto climdex
        eval(parse(text=stringa.climdex))->ci
        
        #calcolo indice estremo: la stringa formata con paste0 crea ad esempio: climdex.wsdi
        # o climdex.txx(ci,freq="monthly") nel caso di indici con aggregazione mensile
        eval(parse(text=paste0("climdex.",indice,"(ci",opzione.stringa.climdex,")")))->out

        ClimateData(data.frame(yy=names(out),x=out),par=tolower(PARAM))->annuale
        checkSeriesValidity2(x = annuale,max.size.block.na = 5,percentualeAnniPresenti = 80,minLen = 40)->annuale

        if(is.null(annuale)) {return(NULL)}
        as.data.frame(annuale)->annuale
        
        names(annuale)[ncol(annuale)]<-.y
        annuale
  
  }) %>% purrr::compact(.) %>% purrr::reduce(left_join)->lista.out  #fine lapply su codici.stazioni
  
  write_delim(lista.out,path=paste0("./",indice,"/risultati_",indice,".csv"),col_names=TRUE,delim=";")

  #creazione mappa degli indici

  ana %>%
    filter(codice %in% names(lista.out)) %>%
    st_as_sf(coords=c("longitude","latitude"))->puntiStazioni
  
  st_crs(puntiStazioni)<-4326
  
  pdf(file = paste0("./",indice,"/risultati_",indice,".pdf"),width=7,height=12)
  map("italy")
  plot(st_geometry(puntiStazioni),add=TRUE,pch=21,bg="red",axes=FALSE)
  dev.off()
  
})#fine purrr::map su indice  



