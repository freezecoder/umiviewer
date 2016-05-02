
library(rCharts)
library(shinyjs)
 
#Top control panel
controlPanel<-function(){
  
	  div(id="cppanel",
	      wellPanel(
	  	  h3("Control Panel"),
		  h4("Set inputs:"),
		fluidRow(
			column(7,uiOutput("choosefolder"))#,
			#column(4,uiOutput("choosefile"))
		),
		hr(),
        p("Hint:Enter genes as a filter for the data, the search box will load genes in the file as you type"),
       hr(),
	   fluidRow( 
		column(6,uiOutput("selui")),
		column(1,h2("OR ")),
		column(4,fileInput("tagfile","Select file with Barcodes/tags"),h3(textOutput("validtags")))
		)
      )
	  )
}

#Stats view
statsTab<-function(){
   tabPanel("Stat Summary",
          wellPanel(
          fluidRow(
            column(6,
            checkboxGroupInput("svar","Summarize Count Stats By:",selected=c("ORG","DNAtag"),c("GENE","ORG","DNAtag"))
            ),
            column(6,
            radioButtons("vartoplot","plot variable:",
                         selected="mean",c("mean","min","sum","max","logsum","sd"),inline=TRUE)
            )
          )
          ),
          fluidRow(
              plotOutput("statplot",width=800,height=600) 
                ),
          hr(),
          fluidRow(
				p("Download this table in CSV"),
				downloadButton('stattable_download', class="btn btn-info btn-lg",icon("file-excel-o") ),
		        dataTableOutput("stattable")
          )
        )
}

heatmapTab<-function(){
  tabPanel("Matrix Heatmap",
               sliderInput("mingexp","Minimum Gene Count across all Samples:",0,5000,value=0)
              ,uiOutput("ui_heatmap")			   
      )
}

multiPanel<-function(){
      tabPanel("Multiple Files Metrics",
		      h3("Metrics Viewed across Multiple Samples"),
             #  p("This view plots mapped reads across multiple datasets"),
              # fileInput("umifiles",multiple=T,"Upload  multiple UMI files here"),
              #plotOutput("mappedreadscatter",width=800,height=600),
			  uiOutput("metsel"),
			  showOutput("metricsbarchart", "dimple"),
			  downloadButton('multimetricsdownload', class="btn btn-info btn-lg",icon("file-excel-o") ),
			  dataTableOutput("metricsfiles")
               )
}

boxplotPanel<-function(){
     tabPanel("BoxPlot",
			fluidRow(
				column(width=12,div(style = "height: 1000px;",
						plotOutput("boxplotcmp",width=1200,height=800) 
				)
               )
			 )
			 
			 
      )
}

scatterplotPanel<-function(){
  tabPanel("Mapped Reads",
	fluidRow(
				column(width=12,div(style = "height: 1000px;",
						radioButtons("logit","Show log axis",selected="yes",c("yes","no")), 
						plotOutput("scatterplot",width=1000,height=600),
							p("Download this table in CSV"),
						downloadButton('scatter_download', class="btn btn-info btn-lg",icon("file-excel-o") ),
						dataTableOutput("scatterdata")
				)
               )
			 )
	)
}

tablePanel<-function(){
   tabPanel("Raw Data",
                dataTableOutput("tbl1")
        )
}


metricsPanel<-function(){
tabPanel("Alignment Metrics",
			tabsetPanel("View",type="pills",selected="Sums",
			tabPanel("All Data",
			p("Table showing data read from metrics file matching selected UMI file. Note that the metrics file must be in the same folder as the UMI file and have the same base file name e.g.
				file.umi.txt, file.metrics.txt 
			"),
			dataTableOutput("metall")
			),
			tabPanel("Sums",
			p("Table showing data read from metrics file matching selected UMI file. Note that the metrics file must be in the same folder as the UMI file and have the same base file name e.g.
				file.umi.txt, file.metrics.txt 
			"),
				fluidRow(
				column(8,showOutput("schart", "dimple")),
				column(4,
				downloadButton('metricsum_download', class="btn btn-info btn-lg",icon("file-excel-o") ),
				dataTableOutput("metsum"))
				),
				hr(),
				fluidRow(
				)
				)
			)
	)
}

canalQCPanel<-function(){
	tabPanel("Canal QC",
		p("Code reference :"), a(href="https://github.com/gbritt/SatijaLab/blob/DropAnalysis/analysis%20script%20master.R",target="_new","Github SatijaLab"),
		plotOutput("canalplot",width=1200,height=1000),
		downloadButton('canal_download', class="btn btn-info btn-lg",icon("file-excel-o") ),
		dataTableOutput("canaldt")
	)
}


shinyUI(fluidPage(div(h1("Imane's UMI Viewer"),style="font-family : fantasy ;"),
  fluidPage("",
  	mainPanel(
	 useShinyjs(),
		fluidRow(
			column(2,actionButton("toggleAdvanced",class="btn  btn-info ","Show/Hide Control Panel",icon=NULL)),
			column(6,h3(textOutput("tagcount"))),
			column(4,uiOutput("choosefile"))
	),hr(),
		controlPanel()
	),
  mainPanel(
     navlistPanel("Views",widths=c(2,10),
    scatterplotPanel(), #mapped reads scatter plot
	canalQCPanel(),
	tablePanel(),
    heatmapTab(),
    statsTab(),
	boxplotPanel(),
	metricsPanel(),
	"-----------",
	multiPanel(),
	tabPanel("Multiple Files Mapped reads",
	
	selectInput("ocmetric","Measure",selected="PERCENT_HUMAN",c("HUMAN","NONHUMAN","PERCENT_HUMAN","PERCENT_NONHUMAN","total")),
	    showOutput("orgcountplot", "dimple"),
		downloadButton('ocdownload', class="btn btn-info btn-lg",icon("file-excel-o") ),
		dataTableOutput("merged_counts_table")	
	)
      )
  )
))
)
