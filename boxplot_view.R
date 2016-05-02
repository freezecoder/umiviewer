 countsbyorgdata<-reactive({
     dat=filTable()
    filename=input$ifile
    dat=as.data.table(dat)
    mlt=melt.data.table(dat,id.vars="GENE")
    names(mlt)=sub("value","readcount",names(mlt))
    mlt$ORG="NON-HUMAN"
    mlt[grepl("HUMAN",mlt$GENE),][,"ORG"]="HUMAN"
	return(mlt)
 })
  
  
 #Boxplot Tab faceted by organism
 output$boxplotcmp<-renderPlot({
	tmax=50
	mlt=countsbyorgdata()
	filename=input$ifile
	mlt=subset(mlt,readcount>0)
	tagcount=length(unique(mlt$variable))
	tgs=unique(mlt$variable)
	
	if (tagcount>tmax){
		mlt=subset(mlt,variable %in% head(tgs,tmax) )
		p=ggplot(mlt,aes(variable,log10(readcount)))
		p= p + geom_boxplot() 
	}	else {
		p=ggplot(mlt,aes(variable,log10(readcount)))
		p= p + geom_boxplot(aes(color=variable)) 
	}
    p = p + facet_grid(ORG~.,scales="free")
    p + labs(title=sprintf("Boxplot for %s",filename)) + theme_bw() + theme(axis.text.x = element_text(angle=90)) #+ theme_bw()
  })
  