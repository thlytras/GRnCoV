library(shiny)

ui <- fluidPage(
  titlePanel("Μοντέλο εξέλιξης επιδημίας COVID-19 στην Ελλάδα"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("cR0", "Βασικός αναπαραγωγικός αριθμός (R0)", 
                min=0.5, max=4, value=2.2, step=0.1),
      sliderInput("cR0min", "Ελάχιστος βασικός αναπαραγωγικός αριθμός (R0)", 
                min=0.5, max=4, value=2.2, step=0.1),
      fluidRow(
        column(8, helpText("Η περιοδικότητα στη μεταδοτικότητα του ιού αναπαριστάται με ημιτονοειδή καμπύλη με ελάχιστο μέσα στο καλοκαίρι")),
        column(4, dateInput("cMinD", "Πότε ακριβώς?", value="2020-8-1", 
                min="2020-6-1", max="2020-8-31", format="dd/mm/yyyy"))
      ),
      sliderInput("cTg", "Χρόνος γενεάς", 
                min=3, max=12, value=7.5, step=0.2),
      sliderInput("cLat", "Μέση περίοδος επώασης", 
                min=3, max=12, value=5, step=0.2),
      fluidRow(
        column(8, sliderInput("cIni", "Αριθμός μολυσματικών κατά την έναρξη", 
                min=0, max=1000, value=100, step=10)),
        column(4, dateInput("cStartDate", "Έναρξη επιδημίας", value="2020-3-1", 
                min="2020-2-1", max="2020-4-1", format="dd/mm/yyyy"))
      ),
      sliderInput("cIntros", "Επιπλέον εισαγωγές μολυσματικών ανά ημέρα", 
                  min=0, max=20, value=0, step=1),
      dateInput("cStopDate", "Προβολή επιδημίας έως", value="2020-10-1", min="2020-5-1", max="2021-7-1", format="dd/mm/yyyy"),
      checkboxInput("sCntTrc", "Contact tracing"),
      conditionalPanel("input.sCntTrc", 
        wellPanel(
          sliderInput("cAsc", "Ascertainment ratio (%)", min=0, max=50, value=10),
          sliderInput("cPctCnt", "Ποσοστό (%) επαφών των ανωτέρω που διερευνώνται και απομονώνονται αποτελεσματικά",
            min=0, max=100, value=0, step=1),
          helpText("Θέσατε 0 για να σταματήσετε το contact tracing."),
          sliderInput("cMaxCnt", "Μέγιστος αριθμός επαφών που μπορούν να διερευνηθούν και να απομονωθούν αποτελεσματικά ανά ημέρα",
            min=0, max=1000, value=0, step=5),
          helpText("Θέσατε 0 αν νομίζετε οτι το contact tracing μπορεί να κλιμακωθεί επ'αόριστον.")
        )
      )
    ),
    mainPanel(
      plotOutput("SIRplot"),
      plotOutput("caseplot"),
      plotOutput("R0plot")
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    if (input$cLat>input$cTg-0.2) updateSliderInput(session, "cLat", value=input$cTg-0.2)
  })
  
  observe({
    if (input$cR0min>input$cR0) updateSliderInput(session, "cR0min", value=input$cR0)
  })

  
  dat <- reactive({
    R0min <- min(input$cR0, input$cR0min)
    R0 <- input$cR0
    L <- input$cLat
    V <- input$cTg
    D <- (V - L)*2
    a <- data.frame(
      date = seq.Date(input$cStartDate, input$cStopDate, by="days"),
      S = 0, E = 0, I = 0, R = 0, dE = 0, dI = 0, dR = 0, 
      beta=R0/D, D=D
    )
    a$S[1] <- 11000000
    a$I[1] <- input$cIni
    minD <- as.integer(format(input$cMinD, "%j"))
    a$beta <- ((cos((as.integer(format(a$date, "%j"))-(minD-183))*2*pi/366)+1)/2*(R0-R0min) + R0min)/D
    kappa <- 1/L
    gamma <- 1/D
    for (t in 2:nrow(a)) {
      a$dE[t] <- round((1 - exp(-a$beta[t]*a$I[t-1]/a$S[1]))*a$S[t-1])
      a$dI[t] <- round((1 - exp(-kappa))*a$E[t-1])
      a$dR[t] <- round((1 - exp(-gamma))*a$I[t-1])
        cntRemoved <- round(input$cPctCnt*input$cAsc/100/100*a$dE[t])
        if (input$cMaxCnt!=0 & input$cMaxCnt<cntRemoved) cntRemoved <- input$cMaxCnt
      a$E[t] <- a$E[t-1] + a$dE[t] - a$dI[t] - cntRemoved
      a$I[t] <- a$I[t-1] + a$dI[t] - a$dR[t] + input$cIntros
      a$R[t] <- a$R[t-1] + a$dR[t] + cntRemoved
      a$S[t] <- a$S[1] - a$E[t] - a$I[t] - a$R[t]
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
      legend("top", c("Susceptible","Infectious","Removed"), bty="n", horiz=TRUE,
        col=c("green","red","blue"), lwd=2, xpd=NA, inset=c(0,-0.12))
      axis(2, at=(0:11)*10^6, labels=rep(NA, 12), tcl=-0.2)
      axis(2, at=c(0,5,10)*10^6, labels=c(0, "5.000.000", "10.000.000"))
      abline(h=(0:11)*10^6, lty="dotted", col="grey")
    })
  })
  
  output$caseplot <- renderPlot({
    with(dat(), {
      plot(date, dE, type="h", col="red", lend=1, lwd=4, bty="l",
        main="Αριθμός νέών μολύνσεων",
        ylab="Αριθμός μολύνσεων", xlab="Ημερομηνία"
      )
      abline(h=seq(0,11000000,500000), lty="dotted", col="grey")
    })
  })
  
  output$R0plot <- renderPlot({
    with(dat(), {
      plot(date, beta*D, type="l", col="purple", lwd=3, bty="l",
        main="Βασικός αναπαραγωγικός αριθμός (ενσωμάτωση περιοδικότητας)",
        ylab="Βασικός αναπαραγωγικός αριθμός (R0)", xlab="Ημερομηνία"
      )
    })
  })

}

shinyApp(ui = ui, server = server)
