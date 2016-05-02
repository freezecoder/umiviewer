  
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
  