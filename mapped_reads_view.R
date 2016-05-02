

  
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
  
  
  #mapped reads scatter plot 
   output$scatterplot<-renderPlot({
	newd= mappedCounts()
	 ll= length(newd$DNAtag)
	 
	#saveRDS(newd,"pcin.rds")
	
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
     p= p + labs(title=sprintf("Human vs NonHuman %s, %s barcodes",filename,ll))
     p + coord_flip() + theme_bw()
  })
  
