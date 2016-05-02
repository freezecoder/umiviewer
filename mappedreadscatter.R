

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


combinedMetricsTable<-reactive({
	files=list.files(path=topdir,full.names=TRUE,recursive=T,pattern="metrics.txt")
	
	mdf=do.call("rbind",lapply(files,function(x) {
		cbind(sub(".metrics.txt","",basename(x)),fread(x))
		}))

	mdf=as.data.table(mdf)
	setnames(mdf,"V1","sourcefile")
	mdf$PCT_ALIGNED_BASES=100*mdf$PF_ALIGNED_BASES/mdf$PF_BASES
	
	if (!is.null(input$tags)) {
		mdf<-subset(mdf, SAMPLE %in% input$tags)
	}
	
	  if (!is.null(input$tagfile)) {
		infile=input$tagfile
		path=infile$datapath
		vec=read.csv2(path,header=F)
		mytags=vec$V1
		mdf<-subset(mdf, SAMPLE %in% mytags)
	 }
	
	mdf
})

output$metricsfiles<-renderDataTable({
	df=combinedMetricsTable()
	df 
})

output$metsel<-renderUI({
	df=combinedMetricsTable()
	files=unique(df$sourcefile)
	fluidRow(
		column(3,selectInput("mmeasure","Measure:",selected="PCT_USABLE_BASES",multiple=F,grep("SAMPLE|LIBRARY|READGROUP",value=TRUE,invert=T,names(df)))),
		column(6,selectInput("mmfiles","Samples to Show:",selected=head(files),multiple=T,files,width=400)),
		column(2,selectInput("mctype","Chart Type:",selected="line",c("line","bar"),multiple=F))
	
	)
	
})

output$metricsbarchart <-renderChart({
        mydata=combinedMetricsTable()
		
		if (!is.null(input$mmfiles)) 
			mydata=subset(mydata,sourcefile %in% input$mmfiles)
		
        d1 <- dPlot(
          y = input$mmeasure,
          x= c("SAMPLE","sourcefile"),
		  groups="sourcefile",
          data = mydata,
          type = input$mctype
        )
        d1$xAxis( type = "addCategoryAxis")
        d1$yAxis( type = "addMeasureAxis" )
		d1$xAxis( orderRule = "SAMPLE")
        d1$params$width=1200
        d1$params$height=800
		
		d1$legend(
			x = 60,
			y = 10,
			width = 500,
		height = 300,
  horizontalAlign = "right"
)
		
        d1$addParams(dom = 'metricsbarchart')
        return(d1)
})

 output$multimetricsdownload <- downloadHandler(
                 filename = function() {
                    paste('combinedMetrics.',input$ifile,".",Sys.Date(), '.csv', sep="")
                  },
                 content = function(file) {
						write.table(combinedMetricsTable(),file,sep=",",row.names=F,quote=F)
				}
 )
  
  
 org_counts_table<- function(infile) {
    dat=fread(infile)
   print(infile)
   mlt=melt.data.table(dat,id.vars="GENE")
    names(mlt)=sub("variable","DNAtag",names(mlt))
    #filter zero-count sample-gene  values
    mlt=subset(mlt, value>0)
    mlt$ORG="NONHUMAN"
    mlt[grepl("HUMAN",mlt$GENE),][,"ORG"]="HUMAN"
    mlt=as.data.table(mlt)
    sumvar=c("DNAtag","ORG")
    mlt= mlt[,by=sumvar,list(sum=sum(value))]
    newd=dcast.data.table(mlt,DNAtag~ORG,value.var="sum",fun.aggregate=sum)
    newd$total=newd$HUMAN+newd$NONHUMAN
    newd$PERCENT_HUMAN=newd$HUMAN/newd$total
    newd$PERCENT_NONHUMAN=newd$NONHUMAN/newd$total
   sourcefile=basename(infile)
   sourcefile=gsub(".umi.txt","",sourcefile)
    newd=cbind(sourcefile,  newd)
    return(newd)
}


output$orgcountplot<-renderChart({

			metric=input$ocmetric

	        mydata=merged_org_counts()
			
			if (!is.null(input$mmfiles)) 
				mydata=subset(mydata,sourcefile %in% input$mmfiles)
						
			d1 <- dPlot(
			y = metric,
			x= c("DNAtag","sourcefile"),
			groups="sourcefile",
          data = mydata,
          type = "bar"
        )
        d1$xAxis( type = "addCategoryAxis")
        d1$yAxis( type = "addMeasureAxis" )
		d1$xAxis( orderRule = "DNAtag")
		#d1$yAxis( type = "addMeasureAxis" )
        d1$params$width=1200
        d1$params$height=800
		
		d1$legend(
			x = 60,
			y = 10,
			width = 500,
				height = 300,
			horizontalAlign = "right"
		)
		
        d1$addParams(dom = 'orgcountplot')
        return(d1)


})

merged_org_counts<-reactive({
	files=list.files(path=topdir,full.names=TRUE,recursive=T,pattern="umi.txt")
	df=do.call("rbind", lapply(files,function(x)org_counts_table(x) ))
	 
	if (!is.null(input$tags)) {
		df<-subset(df, DNAtag %in% input$tags)
	}
	 
	 if (!is.null(input$tagfile)) {
		infile=input$tagfile
		path=infile$datapath
		vec=read.csv2(path,header=F)
		mytags=vec$V1
		df<-subset(df, DNAtag %in% mytags)
	 }

	 df
})


output$merged_counts_table<-renderDataTable({
	merged_org_counts()
})  
  
 output$ocdownload <- downloadHandler(
                 filename = function() {
                    paste('multimappedreads.',Sys.Date(), '.csv', sep="")
                  },
                 content = function(file) {
						write.table(merged_org_counts(),file,sep=",",row.names=F,quote=F)
				}
 )
  
