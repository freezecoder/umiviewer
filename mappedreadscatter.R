

output$mappedreadscatter<-renderPlot({
    
  fs=input$umifiles
  
  if (! is.null(fs)) {
    myfiles= fs$datapath #get path to all files
    fnames=fs$name
    for ( fp in myfiles) {
        
        dat=fread(fp) 
        dat=as.data.table(dat)
        mlt=melt.data.table(dat,id.vars="GENE")
        mlt=subset(mlt, value>0)
        names(mlt)=sub("value","readcount",names(mlt))
        mlt$ORG="NON-HUMAN"
        mlt[grepl("HUMAN",mlt$GENE),][,"ORG"]="HUMAN"
        mlt$filename=fp
        #GENE ORG variable readcount
    }
    
    plot(diamonds)
  }
})