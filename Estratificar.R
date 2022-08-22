estratificar <- function(BD, NUM_ESTRATOS , METODO_ESTRAT, PALETA, CAT_ENT) {
  # Revisar paleta de colores RGB
  if(length(PALETA)<2) PALETA <- c("cyan3", "purple")
  
  # Revisar me'todo de estratificacio'n
  if(!METODO_ESTRAT%in%c("Intervalos iguales","valores unicos")) METODO_ESTRAT <- "Intervalos iguales"
  
  # Revisar nu'mero de estratos para el me'todo 'Intervalos iguales'
  if(METODO_ESTRAT=="Intervalos iguales" & NUM_ESTRATOS<2) NUM_ESTRATOS <- 2
  
  # Estratificacio'n por el me'todo 'Intervalos iguales'
  if(METODO_ESTRAT=="Intervalos iguales"){
    val_min <- min(BD$VAR_MAP)
    val_max <- max (BD$VAR_MAP)
    interv <- (val_max-val_min)/NUM_ESTRATOS
    rangos <- val_min+((1:NUM_ESTRATOS)*interv)
    
    BD$GPO_VAR_MAP <- 0
    
    eval(parse(text=paste0("BD$GPO_VAR_MAP[BD$VAR_MAP<=", rev(rangos) ,"] <- which(round(rangos,10)==",round(rev(rangos),10),")")))
    
    BD$COLOR_VAR_MAP <- colorRampPalette(PALETA, space = "rgb")(NUM_ESTRATOS)[BD$GPO_VAR_MAP]
    
    rangos <- round(rangos,1)
    leyenda <- data.frame(GPO_VAR_MAP=paste0(paste0(c("[",rep("(",NUM_ESTRATOS-1)),c(round(val_min,1),rev(rev(rangos)[-1]))),",",paste0(rangos,rep("]",NUM_ESTRATOS-1))),COLOR_VAR_MAP=colorRampPalette(PALETA, space = "rgb")(NUM_ESTRATOS),stringsAsFactors = F)
    
    frecuencia_rangos <- as.data.frame.table(table(BD$COLOR_VAR_MAP))
    leyenda$Freq <- 0
    
    for(i in frecuencia_rangos$var1) leyenda$Freq[leyenda$COLOR_VAR_MAP==i] <- frecuencia_rangos$Freq[frecuencia_rangos$var1==i]
    
    leyenda$GPO_VAR_MAP <- paste0(leyenda$GPO_VAR_MAP, "(",leyenda$Freq,")")
    leyenda$Freq <- NULL
    
    rm(val_min, val_max, interv, rangos,i, frecuencia_rangos)
  }
  
  # Estratificacio'n por el me'todo 'valores unicos'
  if(METODO_ESTRAT=="Valores unicos"){
  BD$GPO_VAR_MAP <- order(order(BD$VAR_MAP))
  
  BD$COLOR_VAR_MAP <- colorRampPalette(PALETA, space = "rgb")(length(unique(BD$GPO_VAR_MAP)))[BD$GPO_VAR_MAP]
  
  leyenda <- as.data.frame(unique(cbind(BD$GPO_VAR_MAP,BD$COLOR_VAR_MAP)),stringsAsFactors = F)
  
  colnames(leyenda)<-c("GPO_VAR_MAP","COLOR_VAR_MAP")
  leyenda$GPO_VAR_MAP <- as.numeric(leyenda$GPO_VAR_MAP)
  leyenda <- leyenda[order(leyendafGPO_VAR_MAP),]
  }
  
  # Pegar etiquetas
  BD <- merge(BD,CAT_ENT[,c("ENT","NOM_ABR")],by=c("ENT"),all.x=T)
  
  return(list(BD=BD, leyenda=leyenda))
}
