library(shiny)

ui <- fluidPage(
  titlePanel("Μοντέλο εξέλιξης επιδημίας COVID-19 στην Ελλάδα"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("cR0", "Βασικός αναπαραγωγικός αριθμός (R0)", 
                  min=1, max=4, value=2.2, step=0.1),
      sliderInput("cR0min", "Ελάχιστος βασικός αναπαραγωγικός αριθμός (R0)", 
                  min=0.5, max=4, value=2.2, step=0.1),
      helpText("Η περιοδικότητα στη μεταδοτικότητα του ιού αναπαριστάται με ημιτονοειδή καμπύλη με μέγιστο στις 1 Φεβρουαρίου και ελάχιστο την 1 Αυγούστου"),
      sliderInput("cTg", "Χρόνος γενεάς", 
                  min=3, max=12, value=7.5, step=0.2),
      fluidRow(
      column(8, sliderInput("cIni", "Αριθμός μολυσματικών κατά την έναρξη", 
                  min=0, max=1000, value=100, step=10)),
      column(4, dateInput("cStartDate", "Έναρξη επιδημίας", value="2020-3-1", min="2020-2-1", max="2020-4-1", format="dd/mm/yyyy"))),
      sliderInput("cIntros", "Επιπλέον εισαγωγές μολυσματικών ανά ημέρα", 
                  min=0, max=20, value=0, step=1),
      dateInput("cStopDate", "Προβολή επιδημίας έως", value="2020-7-1", min="2020-5-1", max="2021-7-1", format="dd/mm/yyyy")
    ),
    mainPanel(
      plotOutput("SIRplot"),
      plotOutput("caseplot"),
      plotOutput("R0plot")
    )
  )
)

server <- function(input, output) {
  
  dat <- reactive({
    R0min <- min(input$cR0, input$cR0min)
    a <- data.frame(
      date = seq.Date(input$cStartDate, input$cStopDate, by="days"),
      S = 0, I = 0, R = 0, dI = 0, dR = 0, beta=input$cR0/input$cTg
    )
    a$S[1] <- 11000000
    a$I[1] <- input$cIni
    a$beta <- ((cos((as.integer(format(a$date, "%j"))-30)*2*pi/366)+1)/2*(input$cR0-R0min) + R0min)/input$cTg
    
    gamma <- 1/input$cTg
    for (t in 2:nrow(a)) {
      a$dI[t] <- round((1 - exp(-a$beta[t]*a$I[t-1]/a$S[1]))*a$S[t-1]) + input$cIntros
      a$dR[t] <- round((1 - exp(-gamma))*a$I[t-1])
      a$I[t] <- a$I[t-1] + a$dI[t] - a$dR[t]
      a$R[t] <- a$R[t-1] + a$dR[t]
      a$S[t] <- a$S[1] - a$I[t] - a$R[t]
    }
    return(a)
  })

  output$SIRplot <- renderPlot({
    with(dat(), {
      plot(date, S, type="l", lwd=2, col="green", ylim=c(0,S[1]),
        ylab="Αριθμός ατόμων", xlab="Ημερομηνία", bty="l", yaxt="n"
      )
      points(date, I, type="l", lwd=2, col="red")
      points(date, R, type="l", lwd=2, col="blue")
      legend("top", c("Susceptible","Infected","Removed"), bty="n", horiz=TRUE,
        col=c("green","red","blue"), lwd=2, xpd=NA, inset=c(0,-0.12))
      axis(2, at=(0:11)*10^6, labels=rep(NA, 12), tcl=-0.2)
      axis(2, at=c(0,5,10)*10^6, labels=c(0, "5.000.000", "10.000.000"))
      abline(h=(0:11)*10^6, lty="dotted", col="grey")
    })
  })
  
  output$caseplot <- renderPlot({
    with(dat(), {
      plot(date, I, type="h", col="red", lend=1, lwd=4, bty="l",
        main="Αριθμός νέών μολύνσεων",
        ylab="Αριθμός μολύνσεων", xlab="Ημερομηνία"
      )
      abline(h=seq(0,11000000,500000), lty="dotted", col="grey")
    })
  })
  
  output$R0plot <- renderPlot({
    with(dat(), {
      plot(date, beta*input$cTg, type="l", col="purple", lwd=3, bty="l",
        main="Βασικός αναπαραγωγικός αριθμός (ενσωμάτωση περιοδικότητας)",
        ylab="Βασικός αναπαραγωγικός αριθμός (R0)", xlab="Ημερομηνία"
      )
    })
  })

}

shinyApp(ui = ui, server = server)
