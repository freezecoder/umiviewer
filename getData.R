
output$choosefolder<-renderUI({
	validate(
			need(topdir !="", "Please select a data  folder ")
	)

  indir=paste(topdir,sep="/")
  folders=list.dirs(topdir,recursive=FALSE) 
  selectInput("ifolder","Choose Folder with UMI files:",selected=tail(folders,1),folders)
})

output$choosefile<-renderUI({
	validate(
			need(input$ifolder !="", "Please select a data  folder ")
	)
  indir=input$ifolder
  allfiles=list.files(indir,pattern=".umi.txt")
  selectInput("ifile","Choose UMI file:",selected=NULL,allfiles)
})


datafile<-function(){
  infile=  paste(input$ifolder,input$ifile,sep="/")
  alt="/Users/ibourzgui/Documents/Methods Development/SS2/Seq metrics/HiSeq_030216/Imane_1ngRNApool_exo_S2.umi.txt"
  if (file.exists(infile)) {
      return(infile) 
  }else {
     if (file.exists(alt)) {
		return(alt)
	 }else {
		return(NULL)
	 }
      
  }
}



#get the column names from input data file
getColNames<-reactive({
    dat=getTable()
    nms=grep("GENE",invert=T,value=T,names(dat))
    nms
})

getTable<-reactive({
		validate(
			need(!is.null(input$ifolder), "Please select a data  folder ")
		)

  infile=datafile()
  if (file.exists(infile)) {
	dat=fread(infile,sep="\t",header=T)
  }else {
  #empty data frame
   dat=data.frame()
   dat=as.data.table()
  }
  dat
})

filTable<-reactive({
  dat=getTable()
  if (!is.null(input$gene)) {
      dat<-subset(dat, GENE %in% input$gene)
  }
  if (!is.null(input$tags)) {
    dat<-subset(dat, select=c("GENE",input$tags))
   }
  
  #Filter DNA tags from input file
  if (!is.null(input$tagfile)) {
      infile=input$tagfile
      path=infile$datapath
      vec=read.csv2(path,header=F)
      mytags=vec$V1
      mytags=as.character(mytags)
      nms=names(dat)
      matching=intersect(mytags,nms)
      dat<-subset(dat, select=c("GENE",matching))
  }
  
  
  
  dat
})

#print tags uploaded 
output$validtags<-renderText({
	  if (!is.null(input$tagfile)) {
      infile=input$tagfile
      path=infile$datapath
      vec=read.csv2(path,header=F)
      mytags=vec$V1
	  count= length(mytags)
	  lengths= mean(nchar(as.character(mytags)))
	  sprintf("Uploaded File Status: %s Barcodes of length %s uploaded",count,lengths)
  }else {
		""
  }
})


tbl2mat<-reactive({
  d=getTable()
  dat=subset(d,select=-GENE)
  mat=as.matrix(dat)
  rownames(mat)=d$GENE
  class(mat)<-"numeric"
  mat
})


statTable<-reactive({
    sumvar=input$svar
    filename=input$ifile
    dat=filTable()
    dat=as.data.table(dat)
    mlt=melt.data.table(dat,id.vars="GENE")
    names(mlt)=sub("variable","DNAtag",names(mlt))
    #filter zero-count sample-gene  values
    mlt=subset(mlt, value>0)
    mlt$ORG="NON-HUMAN"
    mlt[grepl("HUMAN",mlt$GENE),][,"ORG"]="HUMAN"
    res= mlt[,by=sumvar,list(
                        filename=filename,
                        sum=sum(value),
                        n=length(value),
                        mean=mean(value),
                        sd=sd(value),
                        min=min(value),
                        max=max(value))][order(-max)]
    res$logsum=log10(res$sum)
  
	return(res)
  })