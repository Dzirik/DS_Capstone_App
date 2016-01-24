###############################################################################
## Knihovny ###################################################################
###############################################################################
library(shiny)
library(shinydashboard)
library(networkD3)
library(RCurl)

ui <- dashboardPage(  
  #############################################################################
	## Celkový vzhled ###########################################################
	#############################################################################
  skin="blue",
  
  #############################################################################
	## Header ###################################################################
	#############################################################################
  dashboardHeader(title = "Next Word Prediction"),
  
  #############################################################################
	## Sidebar ##################################################################
	#############################################################################
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", 
				tabName="About", 
				icon=icon("pencil")
      ),
			menuItem("Prediction",
				tabName="Prediction",
				icon = icon("share-alt")
			),
      menuItem("Milestone Report", 
				tabName = "Milestone",
				icon=icon("file")
      )
    )
  ),
  
  #############################################################################
	## Body #####################################################################
	#############################################################################
  dashboardBody(
    tabItems(
      #About content ##########################################################
      tabItem(tabName="About",
        box(
          title="About.",
          "Version: 1",
          br(),
					"Date: 24.01.2016",
					br(),
          "Desctiption: This is a final application for Coursera Data Science 
					Capstone. The text-prediction part is in the Prediction part. In the 
          Milestone part you can find report, which contains an introductory 
					analysis.",
          width=12
        )
      ),
			
			#Prediction content #####################################################
			tabItem(tabName="Prediction",
				box(
					title="Prediction",
					textInput(
					  inputId="text",
					  label="Enter the text for prediction the next word.",
					  value=""
					),
					actionButton(
					  inputId="tlacPredikt",
					  label="Predict"
					),
					br(),
					"And prediction is:",
					br(),
					verbatimTextOutput("vypisPredikci")
				)
			),
			
			#Milestone content ##################################################
      tabItem(tabName="Milestone",
        box(
          includeHTML("Milestone_Report_Technical_Version.html") ,
          width=12
        )
      )
    )
  )  
)    