# mobile UI to collect data
# last update: 2017-03-01

source('appSelect.R')

appGraph <- function(){
        tabPanel('Verlauf',
                 appSelect(),
                 bsAlert('dataStatus'),
                 plotlyOutput('bankGraph')
        )
}
