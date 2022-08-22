##### Funcio'n para el ca'lculo de los indicadores #####

CalcularInd <- function(BD_DEL, BD_POB, RUTA, METADATOS, TIPO_DATO){
  CAT_MES <- data.frame(CVE_MES = 1:12,
                        MES = c("Enero", "Febrero" ,"Marzo","Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
                        stringsAsFactors = F)
  
  if(TIPO_DATO=="IDEFC"){
    
    ##### Nu'mero total de delitos del Fuero Comu'n (ID_INDICADOR: 1)
    BD_IND <- rbind(aggregate(DELITOS~ANIO+CVE_MES+CVE_ENT,data=BD_DEL,FUN=sum),
                    data.frame(CVE_ENT=0, aggregate(DELITOS~ANIO+CVE_MES, data=BD_DEL, FUN=sum), stringsAsFactors = F)[,c(2,3,1,4)],
                    data.frame(CVE_MES=0, aggregate(DELITOS~ANIO+CVE_ENT, data=BD_DEL, FUN=sum), stringsAsFactors = F)[,c(2,1,3,4)],
                    data.frame(CVE_MES=0, CVE_ENT=0, aggregate(DELITOS~ANIO, data=BD_DEL, FUN=sum), StringsAsFactors = F)[,c(3,1,2,4)])
    names(BD_IND)[which(names (BD_IND)=="DELITOS")] <- "INDICADOR"
    write.csv(BD_IND, paste0(RUTA, "Indicadores/Ind_1.csv"),row.names = F)
    
    ##### Nu'mero total de delitos del Fuero Comu'n por cada 100 mil habitantes (ID_INDICADOR: 2) #####
    
    BD_IND <- merge(rbind(aggregate(DELITOS~ANIO+CVE_MES+CVE_ENT, data=BD_DEL, FUN=sum),
                          data.frame(CVE_ENT=0, aggregate(DELITOS~ANIO+CVE_MES, data=BD_DEL, FUN=sum), StringsAsFactors = F)[,c(2,3,1,4)],
                          data.frame(CVE_MES=0, aggregate(DELITOS~ANIO+CVE_ENT, data=BD_DEL, FUN=sum), stringsAsFactors = F)[,c(2,1,3,4)],
                          data.frame(CVE_MES=0 , CVE_ENT=0, aggregate(DELITOS~ANIO, data=BD_DEL, FUN=sum), StringsAsFactors = F)[,c(3,1,2,4)]),
                          aggregate(POB~ANIO+CVE_ENT,data=BD_POB[, c("ANIO","CVE_ENT","POB")], FUN=sum),
                    by=c("ANIO", "CVE_ENT"), all.x=T)
    
    BD_IND$INDICADOR <- 100000 * BD_IND$DELITOS / BD_IND$POB
    
    write.csv(BD_IND,paste0(RUTA, "Indicadores/Ind_2.csv"),row.names =F)
    
    ##### Indicadores restantes #####
    # Filtros de delitos especi'ficos para el ca'lculo de los indicadores
    
    FILTROS <- data.frame(ID_IND=c(3,7,11,13,15,15,15,15,15,15,15,15,15,15,15,15,15,15,19),
                          ID_DEL=c (10101, 10301, 20101, 40401, 40101, 40102 , 40103 , 40104, 40105 , 40106, 40107 , 40108 , 40109 , 40110, 40111, 40112 , 40113 , 40114, 60201),
                          stringsAsFactors = F)
    
    for(i in c(3, 7, 11, 13, 15, 19)){
      # Absolutos (ID_INDICADOR: Impares)
      BD_IND <- rbind(aggregate(DELITOS~ANIO+CVE_MES+CVE_ENT,data=BD_DEL[BD_DEL$SUBTIPO_DELITO%in%FILTROS$ID_DEL[FILTROS$ID_IND==i],],FUN=sum),
                      data.frame(CVE_ENT=0,aggregate(DELITOS~ANIO+CVE_MES,data=BD_DEL[BD_DEL$SUBTIPO_DELITO%in%FILTROS$ID_DEL[FILTROS$ID_IND==i],],FUN=sum),
                                 stringsAsFactors = F)[,c(2,3,1,4)],
                      data.frame(CVE_MES=0, aggregate(DELITOS~ANIO+CVE_ENT,data=BD_DEL[BD_DEL$SUBTIPO_DELITO%in%FILTROS$ID_DEL[FILTROS$ID_IND==i],],FUN=sum),
                                 stringsAsFactors = F)[,c(2,1,3,4)],
                      data.frame(CVE_MES=0,CVE_ENT=0,aggregate (DELITOS~ANIO,data=BD_DEL[BD_DEL$SUBTIPO_DELITO%in%FILTROS$ID_DEL[FILTROS$ID_IND==i],],FUN=sum),
                                 stringsAsFactors = F)[,c(3,1,2,4)])
      
      names(BD_IND)[which(names(BD_IND)=="DELITOS")] <- "INDICADOR"
      
      write.csv(BD_IND, paste0(RUTA, "Indicadores/Ind_",i,".csv"),row.names = F)
      
      # Tasas (ID_INDICADOR: Pares)
      BD_IND <- merge(rbind(aggregate(DELITOS~ANIO+CVE_MES+CVE_ENT,data=BD_DEL[BD_DEL$SUBTIPO_DELITO%in%FILTROS$ID_DEL[FILTROS$ID_IND==i],],FUN=sum),
                            data.frame(CVE_ENT=0,aggregate(DELITOS~ANIO+CVE_MES,data=BD_DEL[BD_DEL$SUBTIPO_DELITO%in%FILTROS$ID_DEL[FILTROS$ID_IND==i],],FUN=sum),
                                       stringsAsFactors = F)[,c(2,3,1,4)],
                            data.frame(CVE_MES=0,aggregate(DELITOS~ANIO+CVE_ENT,data=BD_DEL[BD_DEL$SUBTIPO_DELITO%in%FILTROS$ID_DEL[FILTROS$ID_IND==i],],FUN=sum),
                                       stringsAsFactors = F)[,c(2,1,3,4)],
                            data.frame(CVE_MES=0,CVE_ENT=0,aggregate(DELITOS~ANIO,data=BD_DEL[BD_DEL$SUBTIPO_DELITO%in%FILTROS$ID_DEL[FILTROS$ID_IND==i],],FUN=sum),
                                       stringsAsFactors = F)[,c(3,1,2,4)]),
                      aggregate(POB~ANIO+CVE_ENT,data=BD_POB[BD_POB$SEXO>=ifelse(i==7,2,1),c("ANIO","CVE_ENT","POB")],FUN=sum),
                      by=c("ANIO","CVE_ENT"),all.x=T)
      BD_IND$INDICADOR <- 100000 * BD_IND$DELITOS / BD_IND$POB
      
      write.csv(BD_IND,paste0(RUTA,"Indicadores/Ind_",i+1,".csv"),row.names = F)
    }
    
    # ID_INDICADOR: 17
    BD_IND <- rbind(aggregate(DELITOS~ANIO+CVE_MES+CVE_ENT,data=BD_DEL[BD_DEL$SUBTIPO_DELITO%in%c(70101),],FUN=sum),
                    data.frame(CVE_ENT=0, aggregate(DELITOS~ANIO+CVE_MES,data=BD_DEL[BD_DEL$SUBTIPO_DELITO%in%c(70101),],FUN=sum),
                               stringsAsFactors = F)[,c(2,3,1,4)],
                    data.frame(CVE_MES=0, aggregate(DELITOS~ANIO+CVE_ENT,data=BD_DEL[BD_DEL$SUBTIPO_DELITO%in%c(70101),],FUN=sum),
                               stringsAsFactors = F)[,c(2,1,3,4)],
                    data.frame(CVE_MES=0, CVE_ENT=0, aggregate(DELITOS~ANIO, data=BD_DEL[BD_DEL$SUBTIPO_DELITO%in%c(70101),],FUN=sum),
                               stringsAsFactors = F)[c(3,1,2,4)])
    
    names(BD_IND)[which(names(BD_IND)=="DELITOS")] <- "INDICADOR"
    write.csv(BD_IND, paste0(RUTA, "Indicadores/Ind_17.csv"), row.names = F)
    
    # ID_INDICADOR: 18
    BD_IND <- rbind(aggregate(DELITOS~ANIO+CVE_MES+CVE_ENT,data=BD_DEL[BD_DEL$MODALIDAD%in%c(401021, 401022),],FUN=sum),
                    data.frame(CVE_ENT=0,aggregate(DELITOS~ANIO+CVE_MES,data=BD_DEL[BD_DEL$MODALIDAD%in%c(401021, 401022),],FUN=sum),
                               stringsAsFactors = F)[,c(2,3,1,4)],
                    data.frame(CVE_MES=0,aggregate(DELITOS~ANIO+CVE_ENT,data=BD_DEL[BD_DEL$MODALIDAD%in%c(401021, 401022),],FUN=sum),
                               stringsAsFactors = F)[,c(2,1,3,4)],
                    data.frame(CVE_MES=0,CVE_ENT=0,aggregate(DELITOS~ANIO,data=BD_DEL[BD_DEL$MODALIDAD%in%c(401021, 401022),],FUN=sum),
                               stringsAsFactors = F)[,c(3,1,2,4)])
    
    names(BD_IND)[which(names(BD_IND)=="DELITOS")] <- "INDICADOR"
    
    write.csv(BD_IND, paste0(RUTA, "Indicadores/Ind_18.csv"), row.names = F)
  }
  
  if(TIPO_DATO=="IDVFC"){
    # Filtros de delitos especificos para el calculo de los indicadores
      
    FILTROS <- data.frame(ID_IND=c(5,9,21),
                          ID_DEL=c(10101,10301,60201),
                          stringsAsFactors = F)
    
    for(i in c(5,9,21)){
      # Absolutos (ID_INDICADOR: Impares)
      BD_IND <- rbind(aggregate(DELITOS~ANIO+CVE_MES+CVE_ENT,data=BD_DEL[BD_DEL$SUBTIPO_DELITO%in%FILTROS$ID_DEL[FILTROS$ID_IND==i],],FUN=sum),
                      data.frame(CVE_ENT=0,aggregate(DELITOS~ANIO+CVE_MES,data=BD_DEL[BD_DEL$SUBTIPO_DELITO%in%FILTROS$ID_DEL[FILTROS$ID_IND==i],],FUN=sum),
                                 stringsAsFactors = F)[,c(2,3,1,4)],
                      data.frame(CVE_MES=0,aggregate(DELITOS~ANIO+CVE_ENT,data=BD_DEL[BD_DEL$SUBTIPO_DELITO%in%FILTROS$ID_DEL[FILTROS$ID_IND==i],],FUN=sum),
                                 stringsAsFactors = F)[,c(2,1,3,4)],
                      data.frame(CVE_MES=0,CVE_ENT=0,aggregate(DELITOS~ANIO,data=BD_DEL[BD_DEL$SUBTIPO_DELITO%in%FILTROS$ID_DEL[FILTROS$ID_IND==i],],FUN=sum),
                                 stringsAsFactors = F)[,c(3,1,2,4)])
      names(BD_IND)[which(names(BD_IND)=="DELITOS")] <- "INDICADOR"
      
      write.csv(BD_IND, paste0(RUTA, "Indicadores/Ind_",i,".csv"),row.names = F)
      
      # Tasas (ID_INDICADOR: Pares)
      BD_IND <- merge(rbind(aggregate(DELITOS~ANIO+CVE_MES+CVE_ENT,data=BD_DEL[BD_DEL$SUBTIPO_DELITO%in%FILTROS$ID_DEL[FILTROS$ID_IND==i],],FUN=sum),
                            data.frame(CVE_ENT=0,aggregate(DELITOS~ANIO+CVE_MES,data=BD_DEL[BD_DEL$SUBTIPO_DELITO%in%FILTROS$ID_DEL[FILTROS$ID_IND==i],],FUN=sum),
                                       stringsAsFactors = F)[,c(2,3,1,4)],
                            data.frame(CVE_MES=0,aggregate(DELITOS~ANIO+CVE_ENT,data=BD_DEL[BD_DEL$SUBTIPO_DELITO%in%FILTROS$ID_DEL[FILTROS$ID_IND==i],],FUN=sum),
                                       stringsAsFactors = F)[,c(2,1,3,4)],
                            data.frame(CVE_MES=0,CVE_ENT=0,aggregate(DELITOS~ANIO,data=BD_DEL[BD_DEL$SUBTIPO_DELITO%in%FILTROS$ID_DEL[FILTROS$ID_IND==i],],FUN=sum),
                                       stringsAsFactors = F)[,c(3,1,2,4)]),
                      aggregate(POB~ANIO+CVE_ENT,data=BD_POB[BD_POB$SEXO>=ifelse(i==9,2,1),c("ANIO", "CVE_ENT", "POB")], FUN=sum),
                      by=c("ANIO", "CVE_ENT") ,all.x=T)
      
      BD_IND$INDICADOR <- 100000 * BD_IND$DELITOS / BD_IND$POB
      
      write.csv(BD_IND, paste0(RUTA, "Indicadores/Ind_",i+1,".csv"), row.names = F)
    }
  }
  
  ##### Actualizar Metadatos #####
  MAX_MES <- max(BD_DEL$CVE_MES[BD_DEL$ANIO==max(BD_DEL$ANIO)])
  
  MAX_ANIO <- max(BD_DEL$ANIO)
  MIN_ANIO <- min(BD_DEL$ANIO)
  
  if(MAX_MES==12){
    NOTA_MET <- "No aplica"
  } else if(MAX_MES==1){
    NOTA_MET <- paste0("La información del año ",max(BD_DEL$ANIO)," refiere únicamente al mes de Enero.")
  } else{
    NOTA_MET <- paste0("La información del afo ",max(BD_DEL$ANIO) ," refiere únicamente al periodo Enero-" ,CAT_MES$MES[CAT_MES$CVE_MES==MAX_MES],".")
  }
  
  if(TIPO_DATO=="IDEFC") COND_MET <- !METADATOS$ID_INDICADOR%in%c(5,6,9,10,21,22)
  if(TIPO_DATO=="IDVFC") COND_MET <- METADATOS$ID_INDICADOR%in%c(5,6,9,10,21,22)
  
  # PERIODICIDAD
  METADATOS$PERIODICIDAD[COND_MET] <- paste0(MIN_ANIO,"-",MAX_ANIO)
  
  # FUENTE
  for(i in 1:sum(COND_MET))
    if(grepl("\n",METADATOS$FUENTE[COND_MET][i])){
      METADATOS $ FUENTE[COND_MET][3] <- paste0("SESNSP. Incidencia Delictiva del Fuero Común, ",paste0(MIN_ANIO,"-",MAX_ANIO),".")
    } else{
      METADATOS$FUENTE[COND_MET][1] <- paste0("SESNSP. Incidencia Delictiva del Fuero Común, ",paste0(MIN_ANIO,"-",MAX_ANIO),".\nCONAPO. Proyecciones de la población de México 2016-2030 (Población a mitad de año).")
    }
  
  # Nota
  METADATOS$NOTA[COND_MET] <- NOTA_MET
  
  write.csv(METADATOS,paste0(RUTA,"Catálogos/Metadatos.csv"), row.names = F)
}
