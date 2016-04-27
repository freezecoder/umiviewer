
#read metrics file data
metricsData<-reactive({
	infile=datafile()
	metfile=sub("\\.umi",".metrics",infile)
	dat=data.frame()
	if (file.exists(metfile)){
		#dat=read.csv2(metfile,sep="\t",header=T,comment.char="#")
		dat=fread(sprintf("grep -v '#' %s",metfile))
	}
	dat
})

metricsFilt<-reactive({
	dat=metricsData()
	#Filter tags from user input box 
	if (!is.null(input$tags)){
			dat=subset(dat,SAMPLE %in% input$tags)
	}
	 #Filter DNA tags from input file
  if (!is.null(input$tagfile)) {
      infile=input$tagfile
      path=infile$datapath
      vec=read.csv2(path,header=F)
      mytags=vec$V1
      mytags=as.character(mytags)
	  dat=subset(dat,SAMPLE %in% mytags)
  }
	
	
	dat
})

metricsSum<-reactive({
		#dat=metricsData()
		dat=metricsFilt()
		dat$RIBOSOMAL_BASES=NULL
      	dat$PCT_RIBOSOMAL_BASES=NULL
		tot=sum(dat$PF_BASES)
		sb=subset(dat,select=c(1:9))
		sms=as.data.frame(colSums(sb))
		sms$Column_name=names(sb)
		names(sms)=c("Sum","FieldName")
		sms$Percent=100*sms$Sum/tot
		if (!is.null(input$ifile))
			sms$InputFile=input$ifile
		sms
})

    #Download Handler 
 output$metricsum_download <- downloadHandler(
                 filename = function() {
                    paste('metricsSum.',input$ifile,".",Sys.Date(), '.csv', sep="")
                  },
                 content = function(file) {
						write.table(metricsSum(),file,sep=",",row.names=F,quote=F)
				}
 )
  


metricsMean<-reactive({
		#dat=metricsData()
		
		sb=subset(dat,select=c(1:9))
		sms=as.data.frame(colSums(sb))
		sms$Column_name=names(sb)
		
		sms
})

 output$metsum<-renderDataTable({
		metricsSum()
		
	})
	

	
	
   output$metall<-renderDataTable({
		dat=metricsFilt()
		fs=subset(dat,select=c("SAMPLE","LIBRARY"))
		ot=subset(dat,select=c(1:20))
		dat=cbind(fs,ot)
		dat
	})
  

 #Bar chart 
output$schart <-renderChart({
	mydata=metricsSum()
	d1 <- dPlot(
	  y = c("Sum"),
	  x= "FieldName",
	  data = mydata,
	  type = "bar"
	)
	d1$xAxis( type = "addCategoryAxis", orderRule = "pos" )
	d1$yAxis( type = "addMeasureAxis" )
	d1$params$width=600
	d1$params$height=500
	d1$addParams(dom = 'schart')
	return(d1)
})
