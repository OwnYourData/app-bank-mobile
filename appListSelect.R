# UI for selecting a date-range
# last update: 2017-03-01

appListSelect <- function(){
       selectInput('dateListSelect',
                   label = 'Auswahl',
                   choices = c('letzte Woche'='1',
                               'letztes Monat'='2',
                               'letzten 2 Monate'='3',
                               'letzten 6 Monate'='4',
                               'aktuelles Jahr'='5',
                               'letztes Jahr'='6',
                               'alle Daten'='10'),
                   selected = 3
       )
}
