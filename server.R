###############################################################################
## Knihovny ###################################################################
###############################################################################
library(shiny)
library(shinydashboard)
library(networkD3)
library(plyr)
library(RCurl)
library(stringr)

###############################################################################
## Data #######################################################################
###############################################################################
  predTaz2<-readRDS("data/predTaz2.rds")	
  predTaz3<-readRDS("data/predTaz3.rds")
  predOzn2<-readRDS("data/predOzn2.rds")	
  predOzn3<-readRDS("data/predOzn3.rds")	
  predRoz2<-readRDS("data/predRoz2.rds")	
  predRoz3<-readRDS("data/predRoz3.rds")
  
  predRozGrams<-readRDS("data/predRozGrams.rds")
  predTazGrams<-readRDS("data/predTazGrams.rds")
  predOznGrams<-readRDS("data/predOznGrams.rds")

###############################################################################
## Functions ##################################################################
###############################################################################

###############################################################################
## Server #####################################################################
###############################################################################

server <- function(input, output, session) {

	## --------------------------------------------------------------------------
	## Ovládací prvky založené na data framech-----------------------------------
	## --------------------------------------------------------------------------
	
  predikce <- eventReactive(input$tlacPredikt,{
    text<-input$text
    
    #initial value
    prediction<-"the"
    
    if(text!=""){
      #I have nonempty text
      words<-strsplit(text," ")[[1]]
      nOfWords<-length(words)
      
      if(nOfWords==1){
        #JUST ONE WORD ----------------------------------------------------------
        #for prediction
        before<-words[1]
        
        #initial values
        pr<-rep(0,3)
        w<-c("","","")
        
        #finding pattern
        if(!(is.na(match(before,predTaz2$zacatek)))){
          pr[1]<-predTaz2[predTaz2$zacatek==before,"pravd"]
          w[1]<-predTaz2[predTaz2$zacatek==before,"konec"]
        }
        if(!(is.na(match(before,predOzn2$zacatek)))){
          pr[2]<-predOzn2[predOzn2$zacatek==before,"pravd"]
          w[2]<-predOzn2[predOzn2$zacatek==before,"konec"]
        }
        if(!(is.na(match(before,predRoz2$zacatek)))){
          pr[3]<-predRoz2[predRoz2$zacatek==before,"pravd"]
          w[3]<-predRoz2[predRoz2$zacatek==before,"konec"]
        }
        
        #handling results
        if(sum(pr)==0){
          winner=0
          
          prediction<-"the"
        }else{
          winner<-order(pr,decreasing=TRUE)[1]
          
          prediction<-w[winner]
        }
      }else if(nOfWords==2){
        #JUST TWO WORDS ---------------------------------------------------------
        #for prediction
        before<-paste(words[1],words[2],sep=" ")
        
        #initial values
        pr<-rep(0,3)
        w<-c("","","")
        
        #finding pattern
        if(!(is.na(match(before,predTaz3$zacatek)))){
          pr[1]<-predTaz3[predTaz3$zacatek==before,"pravd"]
          w[1]<-predTaz3[predTaz3$zacatek==before,"konec"]
        }
        if(!(is.na(match(before,predOzn3$zacatek)))){
          pr[2]<-predOzn3[predOzn3$zacatek==before,"pravd"]
          w[2]<-predOzn3[predOzn3$zacatek==before,"konec"]
        }
        if(!(is.na(match(before,predRoz3$zacatek)))){
          pr[3]<-predRoz3[predRoz3$zacatek==before,"pravd"]
          w[3]<-predRoz3[predRoz3$zacatek==before,"konec"]
        }
        
        #handling results
        if(sum(pr)==0){
          winner=0
          
          prediction<-"the"
        }else{
          winner<-order(pr,decreasing=TRUE)[1]
          
          prediction<-w[winner]
        }
      }else if (nOfWords==3){
        #JUST THREE WORDS -------------------------------------------------------
        #for prediction
        before<-paste(words[1],words[2],words[3],sep=" ")
        
        #initial values
        pr<-rep(0,3)
        w<-c("","","")
        
        #finding pattern
        if(!(is.na(match(before,predTazGrams$zacatek)))){
          pr[1]<-predTazGrams[predTazGrams$zacatek==before,"pravd"]
          w[1]<-predTazGrams[predTazGrams$zacatek==before,"konec"]
        }
        if(!(is.na(match(before,predOznGrams$zacatek)))){
          pr[2]<-predOznGrams[predOznGrams$zacatek==before,"pravd"]
          w[2]<-predOznGrams[predOznGrams$zacatek==before,"konec"]
        }
        if(!(is.na(match(before,predRozGrams$zacatek)))){
          pr[3]<-predRozGrams[predRozGrams$zacatek==before,"pravd"]
          w[3]<-predRozGrams[predRozGrams$zacatek==before,"konec"]
        }
        
        #handling results
        if(sum(pr)==0){
          winner=0
          
          prediction<-"the"
        }else{
          winner<-order(pr,decreasing=TRUE)[1]
          
          prediction<-w[winner]
        }
      }else{
        #REST -------------------------------------------------------------------
        #what kind of sentence I have?
        #for prediction
        before<-paste(words[1],words[2],sep=" ")
        
        #initial values
        pr<-rep(0,3)
        
        #finding pattern
        if(!(is.na(match(before,predTaz3$zacatek)))){
          pr[1]<-predTaz3[predTaz3$zacatek==before,"pravd"]
        }
        if(!(is.na(match(before,predOzn3$zacatek)))){
          pr[2]<-predOzn3[predOzn3$zacatek==before,"pravd"]
        }
        if(!(is.na(match(before,predRoz3$zacatek)))){
          pr[3]<-predRoz3[predRoz3$zacatek==before,"pravd"]
        }
        
        #handling results
        if(sum(pr)==0){
          #no result, I take indicative sentence
          winner=2
        }else{
          winner<-order(pr,decreasing=TRUE)[1]
        }
        
        #prediction
        before<-paste(words[nOfWords-2],words[nOfWords-1],words[nOfWords],sep="_")
        if(winner==1){
          if(!(is.na(match(before,predTazGrams$zacatek)))){
            prediction<-predTazGrams[predTazGrams$zacatek==before,"konec"]
          }
        }
        if(winner==2){
          if(!(is.na(match(before,predOznGrams$zacatek)))){
            prediction<-predOznGrams[predOznGrams$zacatek==before,"konec"]
          }
        }
        if(winner==3){
          if(!(is.na(match(before,predRozGrams$zacatek)))){
            prediction<-predRozGrams[predRozGrams$zacatek==before,"konec"]
          }
        }
      }
    }
    
    prediction
  })
  output$vypisPredikci <- renderText({
    predikce()
  })
}