#
# 
#
# 
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Select Some Values"),
   
   # Sidebar with...
   sidebarLayout(
      sidebarPanel(
				 sliderInput("xaxis","Number of Columns",1,10,5),
				 sliderInput("yaxis","Number of Rows",1,10,5),
      	 sliderInput("ptsize","Allele Size",min = 0.1,max=3,value = 2),
         sliderInput("offset","Allele Offset",0.14, min=0,max=0.5,0.01),
         actionButton("update","Pick Alleles"),
				 actionButton("nextgen","Next Generation"),
				 br(),
				 br(),
				 sliderInput("RFRR","Relative Fitness (RR)",min = 0,max= 1,value=1),
				 sliderInput("RFRB","Relative Fitnes  (RB)",0,1,1),
				 sliderInput("RFBB","Relative Fitness (BB)",0,1,1)
      ),
      # Show a plot 
      mainPanel(
         plotOutput("plot"),
         plotOutput("plot2")
      )
   )
)

# Define server logic 
server <- function(input, output) {
	
	rv<-reactiveValues()
	
	observe({ xy<-expand.grid(1:input$xaxis,input$yaxis:1)
						a1<-rep(2,input$xaxis*input$yaxis)
						a2<-rep(1,input$xaxis*input$yaxis)
		        rv$df = data.frame(xy,a1,a2)
					
		        rv$df2=data.frame(xy)
		        rv$index=1
		        rv$gen=1
		        })		

	observeEvent(input$update, 
							 {rv$rand = sample(seq_along(rv$df[,1]),2)
							  rv$which_allele<-sample(c(1,2),2,replace = T) # select one allele at random from each rand ind 
							  rv$allele1<-as.numeric(rv$df[rv$rand[1],3:4][rv$which_allele][1]) #kid allele 1, from rand and which allele above
							  rv$allele2<-as.numeric(rv$df[rv$rand[2],3:4][rv$which_allele][2]) #kid allele 2
							  rv$df2[rv$index,3]<-rv$allele1
							  rv$df2[rv$index,4]<-rv$allele2
							  rv$index<-rv$index+1
							  })
	
	observeEvent(input$nextgen,
							 {rv$df<-rv$df2
							 xy<-expand.grid(1:input$xaxis,input$yaxis:1)
							 rv$df2=data.frame(xy)
							 rv$index=1
							 rv$gen<-rv$gen+1
							 })
	
	output$plot <- renderPlot({

    rv$offset <- c(-input$offset,input$offset)  
   	
    par(mar=c(0,0,1,0))
    plot(NA,NA,xlim=c(1-input$offset,input$xaxis+input$offset),
    		 ylim=c(1-input$offset,input$yaxis+input$offset),
    		 xlab="",
    		 ylab="",
    		 axes = T,
    		# bty="l",
    		 main=paste("Gen ",rv$gen))
   	
   for (i in seq_along(rv$df[,1])){
     points(rv$df[i,1]+rv$offset[1], rv$df[i,2],col=rv$df[i,3],pch=19,cex=input$ptsize)
   	 points(rv$df[i,1]+rv$offset[2], rv$df[i,2],col=rv$df[i,4],pch=19,cex=input$ptsize)
   	}
   	
   	if (is.null(rv$rand)) return()
   	else{
    #text(rv$df[rv$rand,1]+offset[which_allele],rv$df[rv$rand,2],labels = "x",cex=input$ptsize*1.3) ## purely visual for students to see
    points(rv$df[rv$rand,1]+rv$offset[rv$which_allele],rv$df[rv$rand,2],cex=input$ptsize*1.5) ## purely visual for students to see
   	
        }
  })

	output$plot2 <- renderPlot({
		
		par(mar=c(0,0,1,0))
		plot(NA,NA,xlim=c(1-input$offset,input$xaxis+input$offset),
				 ylim=c(1-input$offset,input$yaxis+input$offset),
				 xlab="",
				 ylab="",
				 axes = T,
				 # bty="l",
				 main=paste("Gen ",rv$gen+1))
		
		for (i in seq_along(rv$df[,1])){
			points(rv$df2[i,1]+rv$offset[1], rv$df2[i,2],col=rv$df2[i,3],pch=19,cex=input$ptsize)
			points(rv$df2[i,1]+rv$offset[2], rv$df2[i,2],col=rv$df2[i,4],pch=19,cex=input$ptsize)
		}		
		
	})
	
}

# Run the application 
shinyApp(ui = ui, server = server)

