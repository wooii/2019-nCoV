# This R script is a Shiny app.
#
# Author: Chenfeng Chen
# Created on 2020-01-29

source("2019_nCoV.R")


ui <- fluidPage(
    # Application title.
    titlePanel("The 2019-nCoV epidemic data analysis and prediction"),
    
    # Sidebar with a slider input.
    sidebarLayout(
        sidebarPanel(
            sliderInput("m",
                        "Number of days to predict from the most recent date:",
                        min = 1,
                        max = 5,
                        value = 2)
        ),
        
        # Show results.
        mainPanel(
            plotOutput("distPlot1"),
            strong("Figure 1. Display the epidemic data of 2019-nCoV in 
            China (up), and in its log2 form."),
            p("The upper plot displays the number of cases in each group
              labeled as confirmed 2019-nCoV infection patients (confirmed), 
              inpatients with severe conditions (hospital), dead patients due
              to 2019-nCoV infection (dead), patients recovered from the
              infection (healed), people who were suspected to be infected
              but not yet confirmed (suspected), people who might have recent
              close contact with the confirmed infected patients (contacted). 
              The lower plot displays the same result after log2 treatment."),
            br(),
            br(),
            plotOutput("distPlot2"),
            strong("Figure 2. The growth of the confirmed number of infection
            cases of 2019-nCoV in China is exponential so far."),
            p("The blue line is the best fitting curve for confirmed number of
            infection cases of 2019-nCoV in China and the grey range is its
            95% confidence interval. The upper plot displays the number of 
            confirmed 2019-nCoV infection cases. The lower plot displays the 
            same result after its log2 treatment, and the formula is calculated
            by using linear regression. "),
            br(),
            br(),
            plotOutput("distPlot3"),
            strong("Figure 3. To predict the confirmed infection cases of
            2019-nCoV in China in the next few days."),
            p("Please indicate how many days in the future to predict using
              the slider bar. The blue point is the confirmed number of 
              infection cases, the blue line is its best fitting and its 95% 
              confidence intervals (grey area). The red line is the predicted
              number of infection cases using the formula derived from 
              Figure 2, and the dotted red line is its 95% confidence 
              intervals (CI)."),
            br(),
            br(),
            p("For more details, please refer to the source code of this app
              below."),
            a(href = "https://github.com/wooii/2019-nCoV", 
              "https://github.com/wooii/2019-nCoV")
        )
    )
)


# Define server logic.
server <- function(input, output) {
    
    output$distPlot1 <- renderPlot(p1)
    output$distPlot2 <- renderPlot(p2)
    
    # Make prediction.
    output$distPlot3 <- renderPlot({
        model <- lm(log2(confirmed) ~ days, data = d0)
        # number of days to predict from the most recent date.
        m <- input$m # Get this variable from ui sliderInput.
        predicted <- log2_lm_predict(lm.model = model,
                                     x.predict = 1:(n + m), 
                                     ci.level = 0.95)
        predicted <- data.frame(predicted, 
                                days = 1:(n + m),
                                confirmed = c(d0$confirmed, rep(NA, m)))
        dates <- c(dates, tail(dates, 1) + 1:m)
        ggplot(predicted, aes(dates, confirmed, colour = "confirmed")) +
            geom_point() +
            stat_smooth() +
            geom_line(aes(y = y, colour = "predicted")) +
            geom_line(aes(y = y.left, colour = "predicted 95% CI"),
                      linetype = "dotted") +
            geom_line(aes(y = y.right, colour = "predicted 95% CI"),
                      linetype = "dotted") +
            scale_color_manual(values = c( "blue", "red", "red")) +
            scale_x_date(date_breaks = "1 day", date_labels = "%m-%d",
                         minor_breaks = NULL) +
            labs(title = paste("Predict for 2019-nCoV infection cases (", 
                               dates[1], " - ", tail(dates, 1) + m, ")",
                               sep = ""),
                 y = "Number of cases",
                 x = "Date",
                 color = "Class") +
            theme(legend.position = "right",
                  axis.text.x = element_text(angle = 90, hjust = 0.5))
    })
}


# Run the application.
shinyApp(ui = ui, server = server)
