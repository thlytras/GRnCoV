library(shiny)
source("miniFileInput.R")


vecshift <- function(x, shift=0) {
  if (shift==0) return(x)
  if (shift>0) return(c(x[-(1:shift)], rep(NA,shift)))
  if (shift<0) return(c(rep(NA, -shift), x[1:(length(x)+shift)]))
}


ui <- fluidPage(
  titlePanel("Μοντέλο εξέλιξης επιδημίας COVID-19 στην Ελλάδα"),

  sidebarLayout(
    sidebarPanel(
      wellPanel(
        sliderInput("cR0", "Βασικός αναπαραγωγικός αριθμός (R0)", 
                  min=0.5, max=4, value=2.2, step=0.1),
        sliderInput("cTg", "Χρόνος γενεάς", 
                  min=3, max=12, value=7.5, step=0.2),
        sliderInput("cLat", "Μέση περίοδος επώασης", 
                  min=3, max=12, value=5, step=0.2)
      ),
      wellPanel(
        fluidRow(
          column(8, sliderInput("cIni", "Αριθμός μολυσματικών κατά την έναρξη", 
                  min=0, max=5000, value=500, step=100)),
          column(4, dateInput("cStartDate", "Έναρξη επιδημίας", value="2020-3-1", 
                  min="2020-2-1", max="2020-4-1", format="dd/mm/yyyy"))
        ),
        sliderInput("cIntros", "Επιπλέον εισαγωγές μολυσματικών ανά ημέρα", 
                    min=0, max=50, value=0, step=1),
        dateInput("cStopDate", "Προβολή επιδημίας έως", value="2020-10-1", min="2020-5-1", max="2021-7-1", format="dd/mm/yyyy")
      ),
      checkboxInput("sSeasonal", "Εποχικότητα"),
      conditionalPanel("input.sSeasonal", 
        wellPanel(
          sliderInput("cR0min", "Ελάχιστος βασικός αναπαραγωγικός αριθμός (R0)", 
                    min=0.5, max=4, value=2.2, step=0.1),
          fluidRow(
            column(8, helpText("Η εποχικότητα στη μεταδοτικότητα του ιού αναπαριστάται με ημιτονοειδή καμπύλη με ελάχιστο μέσα στο καλοκαίρι")),
            column(4, dateInput("cMinD", "Πότε ακριβώς?", value="2020-8-1", 
                    min="2020-6-1", max="2020-8-31", format="dd/mm/yyyy"))
          )
        )
      ),
      checkboxInput("sCntTrc", "Διερεύνηση επαφών"),
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
      ),
      checkboxInput("sSocDis", "Κοινωνική αποστασιοποίηση"),
      conditionalPanel("input.sSocDis", 
        wellPanel(
          sliderInput("cSympPct", "Ποσοστό (%) πλήρους απομόνωσης συμπτωματικών",
            min=0, max=100, value=0, step=1),
          sliderInput("cSympDelay", "Απομόνωση συμπτωματικών μετά από πόσες μέρες από εμφάνιση συμπτωμάτων?",
            min=0, max=3, value=1),
          fluidRow(
            column(8, sliderInput("cBetaDec", "Ποσοστιαία (%) μείωση contact rate", min=0, max=50, value=0)),
            column(4, dateInput("cBetaDecDate", "Από πότε?", value="2020-4-1", min="2020-3-1", max="2021-5-1", format="dd/mm/yyyy"))
          ),
          fluidRow(
            column(8, sliderInput("cSchoolEff", "Ποσοστό (%) \"ανοσοποίησης\" από κλείσιμο σχολείων", min=0, max=100, value=0)),
            column(4, dateInput("cSchoolDate", "Κλείσιμο σχολείων πότε?", value="2020-4-1", min="2020-3-1", max="2021-5-1", format="dd/mm/yyyy"))
          ),
          helpText("Θεωρούμε (πολύ απλουστευτικά) οτι από τα ~2 εκατ. μαθητών/φοιτητών ένα ποσοστό αφαιρείται από το pool των επίνοσων.")
        )
      ),
      checkboxInput("sImpact", "Αντίκτυπος (Impact)"),
      conditionalPanel("input.sImpact", 
        wellPanel(
          sliderInput("cIFR", "Infection Fatality Rate (%)", 
            min=0, max=2, value=0.05, step=0.01),
          sliderInput("cICUrate", "% μολύνσεων που θα χρειαστούν Μονάδα Εντατικής Θεραπείας (ΜΕΘ)", 
            min=0, max=5, value=0.20, step=0.05),
          sliderInput("cICUlen", "Μέση διάρκεια παραμονής στη ΜΕΘ (ημέρες)", 
            min=1, max=20, value=10, step=1)
        )
      ),
      fluidRow(
        column(6, downloadButton("cSave", "Αποθήκευση σεναρίου", style="width:100%")),
        column(6, miniFileInput("cLoad", "Άνοιγμα σεναρίου", accept = c('application/octet-stream')))
      )
    ),
    mainPanel(
      plotOutput("SIRplot"),
      plotOutput("caseplot"),
      plotOutput("R0plot"),
      plotOutput("deadplot"),
      plotOutput("ICUplot"),
      verbatimTextOutput("outConsole")
    )
  )
)

server <- function(input, output, session) {

  slidersC <- c("cR0", "cTg", "cLat", "cIni", "cIntros", "cR0min", 
    "cAsc", "cPctCnt", "cMaxCnt", "cSympPct", "cSympDelay", 
    "cBetaDec", "cSchoolEff", "cIFR", "cICUrate", "cICUlen")
  datesC <- c("cStartDate", "cStopDate", "cMinD", "cBetaDecDate", "cSchoolDate")

  maxICU <- reactiveVal(0)

  observe({
    if (input$cLat>input$cTg-0.2) updateSliderInput(session, "cLat", value=input$cTg-0.2)
  })
  
  observe({
    if (input$cR0min>input$cR0 || !isolate(input$sSeasonal)) updateSliderInput(session, "cR0min", value=input$cR0)
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
    a$beta[which(a$date >= input$cBetaDecDate)] <- a$beta[which(a$date >= input$cBetaDecDate)] * (100 - input$cBetaDec)/100
    kappa <- 1/L
    gamma <- 1/(D - max(0, D-input$cSympDelay)*input$cSympPct/100)
    for (t in 2:nrow(a)) {
      a$dE[t] <- round((1 - exp(-a$beta[t]*a$I[t-1]/a$S[1]))*a$S[t-1])
      a$dI[t] <- round((1 - exp(-kappa))*a$E[t-1])
      a$dR[t] <- round((1 - exp(-gamma))*a$I[t-1])
      # Control measures
        # How many exposed contacts we trace and isolate (remove)
        cntRemoved <- round(input$cPctCnt*input$cAsc/100/100*a$dE[t])
        if (input$cMaxCnt!=0 & input$cMaxCnt<cntRemoved) cntRemoved <- input$cMaxCnt
      a$E[t] <- a$E[t-1] + a$dE[t] - a$dI[t] - cntRemoved
      a$I[t] <- a$I[t-1] + a$dI[t] - a$dR[t] + input$cIntros 
      a$R[t] <- a$R[t-1] + a$dR[t] + cntRemoved 
      a$S[t] <- a$S[1] - a$E[t] - a$I[t] - a$R[t]
      if (a$date[t]==input$cSchoolDate) {
        mv <- round(a$S[t]*2/11*input$cSchoolEff/100)
        a$S[t] <- a$S[t] - mv
        a$R[t] <- a$R[t] + mv
      }
    }
    return(a)
  })

  output$SIRplot <- renderPlot({
    with(dat(), {
      plot(date, S, type="l", lwd=2, col="green", ylim=c(0,S[1]),
        ylab="Αριθμός ατόμων", xlab="Ημερομηνία", bty="l", yaxt="n", xaxt="n"
      )
      points(date, I, type="l", lwd=2, col="red")
      points(date, R, type="l", lwd=2, col="blue")
      legend("top", c("Susceptible","Infectious","Removed"), bty="n", horiz=TRUE,
        col=c("green","red","blue"), lwd=2, xpd=NA, inset=c(0,-0.12))
      axis(2, at=(0:11)*10^6, labels=rep(NA, 12), tcl=-0.2)
      axis(2, at=c(0,5,10)*10^6, labels=c(0, "5.000.000", "10.000.000"))
      abline(h=(0:11)*10^6, lty="dotted", col="grey")
      axis(1, at=date[which(as.integer(format(date, "%d"))==1)],
        labels=format(date[which(as.integer(format(date, "%d"))==1)], "%b"))
    })
  })
  
  output$caseplot <- renderPlot({
    with(dat(), {
      plot(date, dE, type="h", col="red", lend=1, lwd=4, bty="l",
        main="Αριθμός νέών μολύνσεων",
        ylab="Αριθμός μολύνσεων", xlab="Ημερομηνία", xaxt="n"
      )
      abline(h=seq(0,11000000,500000), lty="dotted", col="grey")
      axis(1, at=date[which(as.integer(format(date, "%d"))==1)],
        labels=format(date[which(as.integer(format(date, "%d"))==1)], "%b"))
    })
  })
  
  output$R0plot <- renderPlot({
    with(dat(), {
      plot(date, beta*D, type="l", col="orange", lwd=3, bty="l",
        main="Βασικός αναπαραγωγικός αριθμός (ενσωμάτωση εποχικότητας)",
        ylab="Βασικός αναπαραγωγικός αριθμός (R0)", xlab="Ημερομηνία", xaxt="n"
      )
      axis(1, at=date[which(as.integer(format(date, "%d"))==1)],
        labels=format(date[which(as.integer(format(date, "%d"))==1)], "%b"))
    })
  })

  output$deadplot <- renderPlot({
    with(dat(), {
      a <- which(as.integer(format(date, "%d"))==1)
      a <- c(a[2:5], rev(a)[1])
      plot(date, cumsum(round(dI*input$cIFR/100)), type="l", lwd=2, bty="l",
        main="Συνολικός αριθμός νεκρών", col="purple",
        ylab="Αριθμός ατόμων", xlab="Ημερομηνία", xaxt="n"
      )
      legend("topleft", sprintf("Στις %s:  %s", format(date[a], "%d %B %Y"), 
        cumsum(round(dI*input$cIFR/100))[a]), bty="n")
      axis(1, at=date[which(as.integer(format(date, "%d"))==1)],
        labels=format(date[which(as.integer(format(date, "%d"))==1)], "%b"))
    })
  })

  output$ICUplot <- renderPlot({
    with(dat(), {
      icu <- round(dI*input$cICUrate/100)
      icu <- sapply(0:(input$cICUlen-1), function(i) vecshift(icu, -i))
      icu[is.na(icu)] <- 0
      icu <- rowSums(icu)
      maxICU(max(icu))
      plot(date, icu, type="l", lwd=2, bty="l",
        main="Ανάγκες σε κλίνες ΜΕΘ", col="darkred",
        ylab="Αριθμός Κλινών", xlab="Ημερομηνία", xaxt="n"
      )
      axis(1, at=date[which(as.integer(format(date, "%d"))==1)],
        labels=format(date[which(as.integer(format(date, "%d"))==1)], "%b"))
    })
  })

  output$outConsole <- renderText({
    with(dat(), {
      res <- c(
        "Στο τέλος της προβολής:",
        sprintf("-- Συνολικός αριθμός προσβληθέντων = %s", S[1] - rev(E)[1] - rev(R)[1] - rev(I)[1]),
        sprintf("-- Συνολικός αριθμός νεκρών = %s", sum(round(dI*input$cIFR/100))),
        "",
        sprintf("Μέγιστος αριθμός μολυσματικών = %s", max(I)), 
        sprintf("Μέγιστη χρήση κλινών ΜΕΘ = %s", maxICU())
      )
      paste(res, collapse="\n")
    })
  })

  output$cSave <- downloadHandler(
    filename = function() {
      "scenario.rds"
    },
    content = function(file) {
      res <- lapply(c(slidersC, datesC), function(x) input[[x]])
      names(res) <- c(slidersC, datesC)
      saveRDS(res, file=file)
    }
  )

  observeEvent(input$cLoad, {
    if (is.null(input$cLoad)) return()
    inFile <- input$cLoad
    v <- try(readRDS(inFile$datapath), silent=TRUE)
      # Has the file been read successfully?
    if (length(v)==1 && class(m)=="try-error") {
      return()
    }
    if (v$cR0 != v$cR0min) updateCheckboxInput(session, "sSeasonal", value=TRUE)
    for (n in slidersC) updateSliderInput(session, n, value = v[[n]])
    for (n in datesC) updateDateInput(session, n, value = v[[n]])
  }, ignoreInit=TRUE)

}

shinyApp(ui = ui, server = server)
