library(shiny)
shinyServer(function(input, output) {
  aaa=function(a,b){
    c=round((44.721*round(4*a)/4/round(b,1)-39.823),1)
    if( a>0&b>0&c>5&c<40){
      return(c)
    }
    if( a>0&b>0&c<=3){
      return("3      Warning:our estimation may be incorrect,please check your input")
    }   
    if( a>0&b>0&c>=40){
      return("40      Warning:our estimation may be incorrect,please check your input")
    }
    if( a<=0 | b<=0){
      return("Error: please check your input")
    }
  }
  output$B=renderPrint(cat(aaa(input$a,input$b)))
})