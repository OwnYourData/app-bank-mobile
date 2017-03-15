# layout for listing records
# last update: 2017-03-01

source('appListSelect.R')

appList <- function(){
            tabPanel('Buchungszeilen',
                     appListSelect(),
                     bsAlert('dataStatus'),
                     DT::dataTableOutput('bankList')
            )
}
