library(data.table)
library(ggplot2)
library(scales)
library(d3heatmap)
library(rCharts)
library(shinyjs)

#genes=readRDS("allgenes.rds")

shinyServer(function(input, output, session) {

  #Data import and manipulation functions
  source("getData.R",local=TRUE)
  source("mappedreadscatter.R",local=TRUE)
  source("views.R",local=TRUE)
  source("canalqc.R",local=TRUE)
  
  
 output$ui_heatmap <- renderUI({
		d3heatmapOutput("heatmap")  
  })
  
  output$matplot1<-renderPlot({
      mat=tbl2mat()
      mat.scaled=scale(mat)
      heatmap(head(mat.scaled,100),Rowv=FALSE,symm=FALSE)
    
  })
  
  
  #heatmap View
  output$heatmap<-renderD3heatmap({
    dat=filTable()
    topn=100
    rn=dat$GENE
    dat=subset(dat,select=-GENE)
    row.names(dat)=rn
   # saveRDS(dat,"sampledat.rds")
    gcount=length(rownames(dat))
    dat=dat[rowSums(dat)>input$mingexp,]
    if (gcount >100) {
      d3heatmap(head(dat,topn),labRow=head(rownames(dat),topn),color="Blues",dendrogram="row")
    }else {
      d3heatmap(dat,labRow=rn,dendrogram="row",color="Blues")
    }
    
   
  })
  
  output$tbl1<-renderDataTable({
    dat=filTable()
    
    if (1==1) {
      dat=dat    
    }else {
      gns=dat$GENE
      cc=subset(dat,select=-GENE)
      cc=cpm(cc)
      names(cc)=names(dat)
      rownames(cc)=row.names(dat)
      #cc$GENE=gns
      dat=cc
    }
    dat
  })
  
  #summary tavle
  output$sumtable<-renderDataTable({
    mat=tbl2mat()
    sm=rowSums(mat)
    gn=names(sm)
    sm=as.data.frame(sm)
    sm$gene=gn
    names(sm)=sub("sm","Sum",names(sm))
    sm=as.data.table(sm)
    sm[order(-Sum)]
  })
  
  #summary tavle
  output$tagtable<-renderDataTable({
    mat=tbl2mat()
    sm=colSums(mat)
    gn=names(sm)
    sm=as.data.frame(sm)
    sm$Tag=gn
    names(sm)=sub("sm","Sum",names(sm))
    sm
  })
  
  output$selui<-renderUI({
    dat=getTable()
    colnms= getColNames()
    fluidRow(
       selectInput("gene","Filter/Select Genes to show:",multiple=T,selected=NULL,as.character(dat$GENE)),
        selectInput("tags","Filter/Select Barcodes to show:",multiple=T,selected=NULL,colnms)
      )
  })
  
  
  
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
  
  
 #Bar plot above table
  output$statplot<-renderPlot({
	tagmax=50
     dat=statTable()
     var=input$vartoplot
	 tags=unique(dat$DNAtag)
	 #Max of 100 tags filter 
	 if ( length(dat$DNAtag) >= tagmax ) {
		dat=subset(dat,DNAtag %in% head(tags,tagmax) ) 
	 }
     p= ggplot(dat,aes_string(x="DNAtag",y=var,fill="ORG")) 
     p = p + geom_bar(stat="identity",position="dodge",aes(fill=ORG))
     filename=input$ifile
     p= p + labs(title=sprintf("Histogram plot for %s",filename))
     p + coord_flip()
  })
  
  
  mappedCounts<-reactive({
     dat=statTable()
	 dat$ORG=gsub("\\-","",dat$ORG)
	 newd=dcast.data.table(dat,DNAtag~ORG,value.var="sum",fun.aggregate=sum)
	 newd$total=newd$HUMAN+newd$NONHUMAN
	 newd$PERCENT_HUMAN=newd$HUMAN/newd$total
	 newd$PERCENT_NONHUMAN=newd$NONHUMAN/newd$total
	return(newd)
  })

  
  #Mapped reads scatter plot
  output$scatterdata<-renderDataTable({
	mappedCounts()
  })
  
    #Download Handler 
 output$scatter_download <- downloadHandler(
                 filename = function() {
                    paste('mappedReadsData.',input$ifile,".",Sys.Date(), '.csv', sep="")
                  },
                 content = function(file) {
						write.table(mappedCounts(),file,sep=",",row.names=F,quote=F)
				}
 )
  
  
  
   output$scatterplot<-renderPlot({
	newd= mappedCounts()
	 ll= length(newd$DNAtag)
	if (input$logit=="yes") {
		newd$HUMAN=log10(newd$HUMAN)
		newd$NONHUMAN=log10(newd$NONHUMAN)
	}
     if (ll < 50) {
		p= ggplot(newd,aes_string(x="HUMAN",y="NONHUMAN",color="DNAtag")) 
     }else {
		p= ggplot(newd,aes_string(x="HUMAN",y="NONHUMAN"))
	 }
	 p = p + geom_point()
     filename=input$ifile
     p= p + labs(title=sprintf("Dotplot plot for %s",filename))
     p + coord_flip() + theme_bw()
  })
  
  
  output$stattable<-renderDataTable({
    dat=statTable()
    dat
    })
  
  #Download Handler 
 output$stattable_download <- downloadHandler(
                 filename = function() {
                    paste('StatSummary.',input$ifile,".",Sys.Date(), '.csv', sep="")
                  },
                 content = function(file) {
						write.table(statTable(),file,sep=",",row.names=F,quote=F)
				}
 )
  

 
	shinyjs::onclick("toggleAdvanced",shinyjs::toggle(id = "cppanel", anim = TRUE)) 

	
#High level summary of numbers, what's going on e.g. no. genes, tags, etc
output$tagcount<-renderText({
		dat=filTable()
		if (length(dat)>0) {
		nonprobes=0
		humanprobes=0
		tagcount=length(names(dat))-1
		genecount=length(dat$GENE)
		humanprobes=length(grep("HUMAN",dat$GENE))
		nonprobes=length(grep("HUMAN",dat$GENE,invert=TRUE))
		if (tagcount)
			sprintf("%s DNA tags, %s Genes, %s Human Genes, %s Non-Human Genes",tagcount,genecount,humanprobes,nonprobes)
		}else {
			"0 Tags"
		}
	})
	
 })
