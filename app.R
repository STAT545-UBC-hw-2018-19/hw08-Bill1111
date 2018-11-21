library(shiny)

ui <- fluidPage(
  

  
  ##Inputs##
  
  wellPanel(h3("Inputs - Transition Probabilities"), tags$br(),
             
             #Transition Probabilities From Home Care to these other Venues of Care#
             fluidRow(column(6, h4(sliderInput(inputId = "p.in.H", label = "Going Home", value = 20, min = 0, max = 20, post  = "%"))),
                      column(6, h4(sliderInput(inputId = "p.in.ED", label = "Going to ED", value = 10, min = 0, max = 10, post  = "%")))),
                      
                      tags$hr(style="border-color: black;"),
                      
            fluidRow(column(6, h4(sliderInput(inputId = "p.in.Hosp", label = "Going to Hospital", value = 15, min = 0, max = 15, post  = "%"))),
             column(6, h4(sliderInput(inputId = "p.in.Rez", label = "Going to Rez Care", value = 10, min = 0, max = 10, post  = "%"))))),
                      
                      #column(4, h4(sliderInput(inputId = "p.in.Dead", label = "Dying", value = .1, min = 0, max = 1))))),
  
  
  
  ##Output##
  
  #Costs#
    
    wellPanel(h3("Expenditure"),
              #p(HTML("<a href='#rateS'>Go here</a>")),
              fluidRow(column(2, h4(" ")),
                       column(3, align="center",h4("Pre Intervention")),
                       column(3, align="center",h4("Post Intervention")),
                       column(4, align="center",h4("Cost Differential"))),
              fluidRow(column(2, align="center",h6("ED Cost")),
                       column(3, align="center",h6("$74,584.50")),
                       column(3, align="center",h6(textOutput("edCost"))),
                       column(4, align="center",h6(textOutput("edCostDiff")))),
              fluidRow(column(2, align="center",h6("Hosp Cost")),
                       column(3, align="center",h6("$8,740,225.70")),
                       column(3, align="center",h6(textOutput("hospCost"))),
                       column(4, align="center",h6(textOutput("hospCostDiff")))),    
              fluidRow(column(2, align="center",h6("Rez Cost")),
                       column(3, align="center",h6("$4,380,374.57")),
                       column(3, align="center",h6(textOutput("rezCost"))),
                       column(4, align="center",h6(textOutput("rezCostDiff"))))
              #fluidRow(column(5, h6(" ")),
              #         column(3, h6("Total:")),
              #         column(4, h6(textOutput("rezCostDiff"))))
              
              
              
    ),
    
    wellPanel(h3("Budget per Patient per Day"),
        fluidRow(
          column(4, align="center", h4(textOutput("costAvoid")))),
        fluidRow(
          column(4, h4(tags$hr(style="border-color: black;"))),
          column(1, h4("=")),
          column(7, h4(textOutput("budgetPerPatientDay")))),
        fluidRow(
          column(4,align="center", h4(textOutput("patientDays"))))         
        ),
        
              
               
              
              
    
    
  
  #Graphs#
  wellPanel(h3("Patient Transitions"),
  plotOutput(outputId = "stateGraph")
  )
  
  )

#Debugging#
    
  #tableOutput("mpTdeab"),
  #textOutput("popHC"),
  #textOutput("days")
  
  

  



##########################################################################################

##Server##
server <- function(input,output) {
  
  #Markov Input Parameters#
  v.n  <- c("home", "home care", "hospital", "residential", "dead") # variable/state names
  n.s  <- length(v.n)                                               # number of states
  n.t  <- 12                                                        # number of cycles
  cy.Dy <- 30                                                       # length of cycle in days
  
  ####TRANSITION PROBABILITIES####
  #These represent the probabilities of transitioning out of each state and into another#

  #Home Transitions - Hard Coded
  p.HHC  <- 0.10                        # probability of transitioning from home to home care
  p.HHS  <- 0.20                        # probability of transitioning from home to hospital
  p.HR   <- 0.10                        # probability of transitioning from home to residential care
  p.HD   <- 0.05                        # probability of transitioning from home to dead
  
  
  #Home Care Transitions - Hard Coded
  p.HCH   <- reactive({input$p.in.H/100})        # probability of transitioning from home care to home 
  p.HCHS  <- reactive({input$p.in.Hosp/100})     # probability of transitioning from home care to hosp
  p.HCR   <- reactive({input$p.in.Rez/100})      # probability of transitioning from home care to residential 
  p.HCD   <- .1 #reactive({input$p.in.Dead})     # probability of transitioning from home care to dead
  #Need to change this back if you want to make it reactive
  p.HCED  <- reactive({input$p.in.ED/100})       # probability of visiting ED when at home care
  
  #Hospital Transitions - Hard Coded
  p.HSH  <- 0.05                        # probability of transitioning from hospital to home
  p.HSHC <- 0.20                        # probability of transitioning from hospital to home care
  p.HSR  <- 0.20                        # probability of transitioning from hospital to rez
  p.HSD  <- 0.25                        # probability of transitioning from hospital to dead
  
  ##Residential Care Transitions - Hard Coded
  p.RH   <- 0                           # probability of transitioning from residential to home
  p.RHC  <- 0                           # probability of transitioning from residential to home care
  p.RHS  <- 0.20                        # probability of transitioning from residential to hospital
  p.RD   <- 0.25                        # probability of transitioning from residential to dead
  
  # Emergency Department (ED) Event Probabilities
  p.HED  <- 0.05                        # probability of visiting ED when at home
 #p.HCED <- 0.10                       # probability of visiting ED when in home care -> reactive based on inputs
  p.HSED <- 0                           # probability of visiting ED when in hospital
  p.RED  <- 0.20                        # probability of visiting ED when in residential care
  
  
  ####COSTS#### -> all hard coded
  
  #Emergency Department visit - Hard Coded
  c.ED <- 300                           # cost of ED visit
  
  ##Home - Hard Coded
  dcost.Home <- 0                                           # daily cost of patient at home
  c.H <- ((dcost.Home*cy.Dy) + (c.ED*p.HED))  # cost of one cycle at home (including ED visits)
  
  ##Home Care - Reactive
  dcost.HomeCare <- 90                                            # daily cost of patient in home care
  c.HCED <- reactive({ (c.ED*p.HCED()) })                         # cost of HC Patients having ED visits - reactive
  c.HCNOED <- (dcost.HomeCare*cy.Dy)                              # cost of HC Patients without ED visits
  c.HC <- reactive({ (dcost.HomeCare*cy.Dy) + (c.ED*p.HCED()) })  # cost of one cycle in home care (including ED visits)
  
  ##Hospital - Hard Coded
  dcost.Hospital <- 300                             # daily cost of patient in hospital
  c.HS <- (dcost.Hospital*cy.Dy) + (c.ED*p.HSED)    # cost of one cycle in hospital
  
  ##Residential Care - Hard Coded
  dcost.Residential <- 150                          # daily cost of patient in Residential Care
  c.R <- (dcost.Residential*cy.Dy) + (c.ED*p.RED)   # cost of one cycle in home care (no ED visits)
  
  ##Dead - Hard Coded
  c.D <- 0
  
  #####Discount Rate##### -> Hard Coded, but let's leave at 0 for now.
  d.r  <- 0                        # discount rate per cycle -> to make 3%, change 0 to 0.03
  v.dw <- 1 / (1 + d.r) ^ (0:n.t)  # calculate discount weight for each cycle based on discount rate d.r
  
  ####### INITIALIZATION ##########################################33
  #combine the rows to get a transition probability matrix
  m.P  <- reactive({
    transmatrix <- rbind(c(1-p.HHC-p.HHS-p.HR-p.HD, p.HHC, p.HHS, p.HR, p.HD),
                         c(p.HCH(), 1-p.HCH()-p.HCHS()-p.HCR()-p.HCD, p.HCHS(), p.HCR(), p.HCD),
                         c(p.HSH, p.HSHC, 1-p.HSH-p.HSHC-p.HSR-p.HSD, p.HSR, p.HSD),
                         c(p.RH, p.RHC, p.RHS, 1-p.RH-p.RHC-p.RHS-p.RD, p.RD),
                         c(0,0,0,0,1))
    colnames(transmatrix) <- rownames(transmatrix) <- v.n
    transmatrix
  })
  
  ##This code creates a structure for the Markov Trace - distributions for all the states in all the cycle rows#
  m.TR <- matrix(NA, nrow = n.t + 1 , ncol = n.s, 
                 dimnames = list(0:n.t, v.n))  # create Markov trace
  
  ##This is the initial distribution for the first cycle of the Markov Trace
  m.TR[1,] <- c(100,0,0,0,0)                         # initialize Markov trace
  
  ############## PROCESS ########################
  ##this will multiply the matrices together - right now, 100 ppl will be added every cycle to the home state#
  m.TRr <- reactive({
    for (t in 1:n.t){                                               # throughout the number of cycles
      m.TR[t + 1, ] <- m.TR[t, ] %*% m.P()+c(0,100,0,0,0)           # estimate the Markov trace for cycle t + 1
    }
  m.TR
      })
  
  
  ############ OUTPUT  #####################
  
  ##Debugging -> output tables##
  
  output$mpTab <- renderTable({ m.TRr() })
  
  
  
  
  ##Costs##
  #Cost of Home (including ED visits) -> so we don't actually need that. What we need is the costs of each state + ED going out of Home    Care, because that is what ppl will need#
  #c.home <- sum(m.TR[,1] * c.H())
  #c.home
  
  #Cost of ED vists from HC Patients
  c.hc.ed <- reactive({ sum(m.TRr()[,2] * c.HCED()) })
  output$edCost <- renderText({
    paste0("$", formatC(c.hc.ed(), format="f", digits=2, big.mark=','))
  }) 
  
  #Cost of HC Patients from HC -> this should be zero
  #c.hc.noed <- sum(m.TR()[,2] * c.HCNOED)
  #c.hc.noed
  
  #Cost of Hospital
  c.hosp <- reactive({ sum(m.TRr()[,3] *c.HS) })
  output$hospCost <- renderText({
    paste0("$", formatC(c.hosp(), format="f", digits=2, big.mark=','))
  }) 
  
  #Cost of Rez
  c.rez <- reactive({ sum(m.TRr()[,4] *c.R) }) 
  output$rezCost <- renderText({
    paste0("$", formatC(c.rez(), format="f", digits=2, big.mark=','))
  }) 
  
  
  #Cost of Death
  #c.dead <- sum(m.TRr()[,2] *c.D)
  #c.dead
  
  #Total Cost
  
  ####Budget per Patient per Day####
  
  #Baseline Costs for pHC->H = .2, pHED = .1, pH->HS = .15, pH->R = .1, pH->D = .1
  edCostFinal <- 74584.50
  hospCostFinal <- 8740225.70
  rezCostFinal <- 4380374.57
  
  #Number of People in Home Care State over Model Run#
  output$popHC <- renderText({ sum(m.TRr()[,2]) })
  
  #Number of days#
  cycleDays <- (n.t*cy.Dy)
  output$days <- renderText(cycleDays)
  
  #ED Cost Differential#
  edCostDif <- reactive({ 
    out <- (edCostFinal - c.hc.ed()) 
    if(round(out,2)==0){ out <- 0 }
    out
    })
  output$edCostDiff <- renderText({
    paste0("$", formatC(edCostDif(), format="f", digits=2, big.mark=','))
  }) 
  
  
  
  #Hosp Cost Differential#
  hospCostDif <- reactive({ 
  outH <- (hospCostFinal - c.hosp()) 
  if(round(outH,2)==0){ outH <- 0 }
  outH
  })
  
  output$hospCostDiff <- renderText({
    paste0("$", formatC(hospCostDif(), format="f", digits=2, big.mark=','))
  }) 
  
  #Rez Cost Differential#
  rezCostDif <- reactive({ (rezCostFinal - c.rez()) })
  output$rezCostDiff <- renderText({
    paste0("$", formatC(rezCostDif(), format="f", digits=2, big.mark=','))
  }) 
  
  #Total Cost Final#
  totalCostPre <- sum(edCostFinal,hospCostFinal,rezCostFinal)
  costAvoided <- reactive ({ (totalCostPre - c.hc.ed() - c.hosp() - c.rez()) })
  output$costAvoid <- renderText({
    paste0("$", formatC(costAvoided(), format="f", digits=2, big.mark=','), " Avoided")
  }) 
  
  #Patient Days#
  patDays <- reactive({ ((sum(m.TRr()[,2]))*cycleDays) })
  output$patientDays <- renderText({ 
    paste0(formatC(patDays(), format="f", digits=0, big.mark=','), " Patient Days")
    })
  
  #h6("Patient Days")
  
  
  #Budget per Patient per Day#
  budgetPPD <- reactive({ costAvoided()/patDays() })
  output$budgetPerPatientDay <- renderText({ 
    paste0("$", formatC(budgetPPD(), format="f", digits=2, big.mark=','), " per Patient Day")
  }) 

  
  
  
  #Budget per Patient per Day#
  #costDif/patDays
  
  
  
  ##Graph##
  output$stateGraph <- renderPlot({ 
    
    plotDat <- data.frame(n=c(m.TRr()),
                          State=rep(colnames(m.TRr()), each=nrow(m.TRr())),
                          time=rep(1:nrow(m.TRr()), ncol(m.TRr())))
    
    library(ggplot2)
    ggplot(plotDat) +
      aes(time, n, colour=State) +
      geom_line() + geom_point() +
      xlab("Time") +
      ylab("Number of patients") +
      scale_x_continuous(breaks=seq(1, 20, 2)) +
      theme_bw(base_size=14) +
      theme(legend.position=c(0, 1),
            legend.justification=c(0, 1))
    
    
  })
  
}
  
  
  




##########################################################################################

##Shiny Application##
shinyApp(ui=ui, server=server)

