# This R script is a Shiny app.
#
# Author: Chenfeng Chen
# Created on 2020-01-29

source("2019_nCoV.R")


# Define the UI.
ui <- fluidPage(
    # Application title.
    titlePanel("The 2019-nCoV epidemic data analysis and prediction"),
    
    # Sidebar for input.
    sidebarLayout(
        sidebarPanel(
            width = 4,    
            selectInput("group", 
                        "Choose a group for Figure 2:",
                        group.names
            ),
            sliderInput(inputId = "n.seg",
                        label = "Select number of segments for Figure 2:",
                        min = 1,
                        max = floor(n/5),
                        value = 1,
                        step = 1),
            sliderInput(inputId = "m",
                        label = "Select number of days to predict from 
                        the most recent date for Figure 3:",
                        min = 1,
                        max = 5,
                        value = 2)
        ),
        
        # Show results.
        mainPanel(
            width = 8,
            p("The data used in this analysis is fetched from the", 
              a(href = "http://www.nhc.gov.cn/",
                "the Nathinal Health Commission of 
                the People's Republic of China (NHC)"), ".", 
              "The data is update daily on NHC's website."),
            tabsetPanel(
                tabPanel("Plot", plotOutput("distPlot1")),
                tabPanel("Data", tableOutput("data"))
            ),
            strong("Figure 1. Display the epidemic data of 2019-nCoV in 
                   China."),
            p("The upper plot displays the number of cases in each group
              labeled as confirmed 2019-nCoV infection patients (confirmed), 
              inpatients with severe conditions (inpatient), dead patients due
              to 2019-nCoV infection (dead), patients recovered from the
              infection (healed), people who were suspected to be infected
              but were not yet confirmed (suspected), people who might have
              recent close contact with the confirmed infected patients
              (contacted), people who are not considered as at risk although
              they might have contacted wit the confirmed patients (suspended),
              people who are still under monitor as they have ecent close 
              contact with the confirmed infected patients (watched). 
              The lower plot displays the log2-transformed results. Please
              click the 'Data' tab for detailed raw data"),
            br(),
            br(),
            plotOutput("distPlot2"),
            strong("Figure 2. Analysis of growth of the number in each group
                   as shown in Figure 1."),
            p("The balck points are the log2-transformed reported number of the
            cases in the selected group during 2019-nCoV outbreak in China. 
            The red lines are the best piecewise linear regression fitting
            curves for the numbers. The intercept, slope, and the standard error
            for each segment is calculated and display at the table below with
            ranges of days(x) for each segment. The R-squared is calculated for 
            the piecewise linear regression. The p-value is calculated by
            the pscore.test for tests for a non-zero difference-in-slope 
            parameter of a segmented relationship, a p < 0.05 indicates 
            introducing one or more breakpoint is significantly better than 
            not introducing a breakpoint."),
            tableOutput('df.seg'),
            br(),
            br(),
            plotOutput("distPlot3"),
            strong("Figure 3. Predict the number in each group as shown in
                   Figure 1 in the next few days."),
            p("Please indicate how many days in the future to predict using
              the slider bar. The black points are the reported number of the
              cases in the selected group during 2019-nCoV outbreak in China.
              The red line shows the predicted numbers of cases using the 
              formula parameters calculated in the table above, and the dotted
              red lines show its 95% confidence intervals (CI)."),
            br(),
            br(),
            
            p("For more details, please refer to the source code of this app
              on GitHub,", a(href = "https://github.com/wooii/2019-nCoV", 
                             "https://github.com/wooii/2019-nCoV"), ".")
            
        )
    )
)


# Define the server logic.
server <- function(input, output) {
    aa <- reactive({       
        g <- input$group # Get the group name from ui selectInput.
        n.seg <- input$n.seg
        m <- input$m })
    output$distPlot1 <- renderPlot(plot1)
    output$data <- renderTable(d0,
                               hover = T,
                               digits = 0)
    output$distPlot2 <- renderPlot({
        g <- input$group # Get the group name from ui selectInput.
        n.seg <- input$n.seg
        # g <- "confirmed"; n.seg <- 2 # for local test.
        
        v <- d.log2[[g]]
        v.max <- max(v, na.rm = T)
        v.min <- min(v, na.rm = T)
        
        if (n.seg == 0) {
            ggplot(d, aes(days, v)) +
                geom_point() +
                stat_smooth(formula = y ~ x, method = "lm", level = 0.99) +
                stat_regline_equation(
                    aes(label =  paste(..eq.label.., ..adj.rr.label.., 
                                       sep = "~~")),
                    formula = y ~ x,
                    label.x = 1, label.y = v.max) +
                stat_cor(label.x = 1, label.y = v.min + 0.8*(v.max - v.min)) +
                labs(title = paste("2019-nCoV ",
                                   g, " cases in China", sep = ""),
                     subtitle = paste(dates[1], " - ", tail(dates, 1), 
                                      sep = ""),
                     x = "Days",
                     y = "y = log2(Number of cases)") +
                scale_x_continuous(breaks = seq(from = 1, to = n, by = 1)) +
                theme(legend.position = "right")
        } else {
            x <- d$days
            model <- lm(v ~ x)
            seg <- segmented(obj = model, seg.Z = ~x, npsi = n.seg)
            
            yp <- rep(NA, n)
            yp[!is.na(v)] <- seg$fitted.values
            bp <- confint.segmented(seg)[, 1] # Breakpoints.
            
            df.seg <- data.frame(intercept(seg)$x, slope(seg)$x[, 1:2])
            colnames(df.seg) <- c("intercept", "slope", "slope.se")
            p <- pscore.test(obj = model, seg.Z = ~x)$p.value
            R2 <- summary(seg)$adj.r.squared
            r2.p <- R2_p(r2 = R2, p = p)
            
            ggplot(d, aes(days, v)) +
                geom_point() +
                geom_line(aes(y = yp, colour = "fitted")) +
                geom_vline(xintercept = bp, linetype = "dashed") +
                geom_text(x = n/2, y = v.max, label = r2.p, parse = T) +
                labs(title = paste("2019-nCoV ",
                                   g, " cases in China", sep = ""),
                     subtitle = paste(dates[1], " - ", tail(dates, 1),
                                      sep = ""),
                     x = "Days",
                     y = "y = log2(Number of cases)") +
                scale_x_continuous(breaks = seq(from = 1, to = n, by = 1)) +
                theme(legend.position = "right")
            
        }
    })
    output$df.seg <- renderTable({       
        g <- input$group # Get the group name from ui selectInput.
        n.seg <- input$n.seg
        # g <- "confirmed"; n.seg <- 2 # for local test.
        v <- d.log2[[g]]
        x <- d$days
        model <- lm(v ~ x)
        seg <- segmented(obj = model, seg.Z = ~x, npsi = n.seg)
        bp <- confint.segmented(seg)[, 1] # Breakpoints.
        x_range <- function(x) {
            pp <- c(1, round(x, 1), Inf)
            y <- vector()
            for (i in 2:length(pp)) {
                y[i - 1] <- paste(pp[i - 1], "<=", "x", "<=", pp[i], sep = " ")
            }
             return(y)
        }
        df.seg <- data.frame(segment = 1:(n.seg + 1),
                             intercept(seg)$x, slope(seg)$x[, 1:2],
                             x_range(bp))
        colnames(df.seg) <- c("segment", "intercept", "slope", "slope.se",
                              "x.range")
        df.seg
    }) 
    
    # Make a prediction.
    output$distPlot3 <- renderPlot({
        g <- input$group # Get the group name from ui selectInput.
        n.seg <- input$n.seg
        m <- input$m # Get number of days to predict from ui sliderInput.
        
        # g <- "confirmed"; n.seg <- 1; m <- 3 # for local test.
        
        v <- d.log2[[g]]
        
        x <- d$days
        model <- lm(v ~ x)
        
        
        seg <- segmented(obj = model, seg.Z = ~x, npsi = n.seg)
        bp <- confint.segmented(seg)[, 1] # Breakpoints.
        v <- d.log2[[g]][x > tail(bp, 1)]
        
        x <- x[x > tail(bp, 1)]
        model <- lm(v ~ x)
        
        
        
        
        # number of days to predict from the most recent date.
        predicted <- log2_lm_predict(lm.model = model,
                                     x.predict = 1:(n + m), 
                                     ci.level = 0.95)
        predicted[1:max(x - 1), ] <- NA
        predicted <- data.frame(predicted, 
                                days = 1:(n + m),
                                group = c(d[[g]], rep(NA, m)))
        dates.m <- c(dates, tail(dates, 1) + 1:m)
        
        
        #predicted.melt <- reshape2::melt(predicted, id = "days")
        
        ggplot(predicted, aes(dates.m, group, colour = paste(" ", g, 
                                                             sep = ""))) +
            geom_point() +
            geom_line(aes(y = y, colour = "predicted")) +
            geom_line(aes(y = y.left, colour = "predicted 95% CI"),
                      linetype = "dotted") +
            geom_line(aes(y = y.right, colour = "predicted 95% CI"),
                      linetype = "dotted") +
            scale_color_manual(values = c( "black", "red", "red")) +
            scale_x_date(date_breaks = "1 day", date_labels = "%m-%d",
                         minor_breaks = NULL) +
            labs(title = paste("Predict 2019-nCoV ",
                               g, " cases in China", sep = ""),
                 y = "Number of cases",
                 x = "Date",
                 color = "Class") +
            theme(legend.position = "right",
                  axis.text.x = element_text(angle = 90, hjust = 0.5))
    })
    
}


# Run the application.
shinyApp(ui = ui, server = server)
