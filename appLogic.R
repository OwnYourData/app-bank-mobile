# application specific logic
# last update: 2017-03-01

getRepoStruct <- function(repo){
        appStruct[[repo]]
}

repoData <- function(repo){
        data <- data.frame()
        app <- currApp()
        if(length(app) > 0){
                url <- itemsUrl(app[['url']],
                                repo)
                data <- readItems(app, url)
        }
        data
}

# anything that should run only once during startup
appStart <- function(){
        
}

output$bankGraph <- renderPlotly({
        pdf(NULL)
        outputPlot <- plotly_empty()
        data <- currData()
        if(nrow(data) > 0){
                app <- currApp()
                url <- itemsUrl(app[['url']], paste0(app[['app_key']],
                                                     '.reference'))
                refData <- readItems(app, url)
                if(nrow(refData) == 1){
                        subData <- data[as.Date(as.character(data$date)) < 
                                                as.Date(refData$date), ]
                        startValue <- refData$value - sum(subData$value)
                        minDate <- min(data$dat)
                        refRow <- data.frame(as.character(as.Date(minDate)-1),
                                             'Startbetrag',
                                             -1,
                                             startValue,
                                             as.Date(minDate)-1)
                        data$dat <- as.Date(data$date)
                        data <- data[, c('date', 'description', 'id', 'value', 'dat')]
                        colnames(refRow) <- colnames(data)
                        data <- rbind(refRow, data)
                }
                data <- data %>%
                        group_by(date) %>%
                        summarise(description = 
                                          paste0('\u20ac ',
                                                 formatC(as.numeric(value), 
                                                         format='f', 
                                                         digits=2, 
                                                         big.mark=','), 
                                                 ': ',
                                                 description, 
                                                 collapse = '<br>'),
                                  value = sum(value))
                data$dat <- as.Date(data$date)
                data <- as.data.frame(data)
                data <- data[order(data[, 'dat']),]
                data$cumsum <- cumsum(data$value)
                myRange <- c(mymin = as.Date(Sys.Date()),
                             mymax = as.Date(Sys.Date()))
                switch(input$dateSelect,
                       '1' = { myRange <- c(mymin = as.Date(Sys.Date()-7),
                                            mymax = as.Date(Sys.Date())) },
                       '2' = { myRange <- c(mymin = as.Date(Sys.Date() - months(1)),
                                            mymax = as.Date(Sys.Date())) },
                       '3' = { myRange <- c(mymin = as.Date(Sys.Date() - months(2)),
                                            mymax = as.Date(Sys.Date())) },
                       '4' = { myRange <- c(mymin = as.Date(Sys.Date() - months(6)),
                                            mymax = as.Date(Sys.Date())) },
                       '5' = { myRange <- c(mymin = as.Date(paste(year(Sys.Date()),'1','1',sep='-')),
                                            mymax = as.Date(paste(year(Sys.Date()),'12','31',sep='-'))) },
                       '6' = { myRange <- c(mymin = as.Date(Sys.Date() - months(12)),
                                            mymax = as.Date(Sys.Date())) },
                       '10'= { myRange <- c(mymin = as.Date('1970-01-01'),
                                            mymax = as.Date('2070-01-01')) },
                       {})
                
                mymin <- myRange['mymin']
                mymax <- myRange['mymax']
                daterange <- seq(mymin, mymax, 'days')
                data <- data[data$dat %in% daterange, ]
                if(nrow(data) > 0){
                        outputPlot <- plot_ly(
                                        data = data,
                                        x = ~dat,
                                        y = ~cumsum,
                                        hoverinfo = 'text',
                                        text = paste0(format(data$dat, '%d.%m.%Y'),
                                                      ': \u20ac ',
                                                      formatC(as.numeric(data$cumsum), 
                                                              format='f', 
                                                              digits=2, 
                                                              big.mark=',')),
                                        type = 'scatter',
                                        mode = 'lines+markers',
                                        line = list(width = 3),
                                        marker = list(size = 8)) %>%
                                layout(title = '',
                                       xaxis = list(title = ''),
                                       yaxis = list(title = '',
                                                    tickformat = ':04,2f',
                                                    tickprefix = '\u20ac '),
                                       showlegend = FALSE,
                                       margin = list(l = 80, r = 80))
                }
        }
        dev.off()
        outputPlot
})

output$bankList <- DT::renderDataTable(datatable({
        data <- currDataDateSelect(input$dateListSelect)
        if(nrow(data) > 0){
                data <- data[, c('dat', 'value', 'description'), drop = FALSE]
                colnames(data) <- c('Datum', 'Betrag', 'Text')
                data
        } else {
                data.frame()
        }
}, options = list(
        language = list(
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/German.json'),
        searching = FALSE,
        lengthChange = FALSE,
        pageLength = 10)
))