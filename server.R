library(shiny)
library(shinyapps)
library(ggplot2)
library(googleVis)
library(graphics)
library(grDevices)
library(XLConnect)
wb <- loadWorkbook("debt.xlsx")
logfreq <- readWorksheet(wb, sheet = "debt")
logfreq$date <- as.character(logfreq$date)
logfreq$date <- factor(logfreq$date, levels=unique(logfreq$date))

shinyServer(function(input, output) {
    output$freqPlot <- renderPlot({
        if (is.null(input$errors_shown)) {
            
            print(ggplot(data.frame(x=c(0),y=c(0))) + geom_point(aes(x,y), alpha=0))
            
        } else {
            lf.filtered <- subset(logfreq, region %in% input$errors_shown)
            lf.filtered <- subset(lf.filtered, (year >= min(input$range)))
            lf.filtered <- subset(lf.filtered, (year <= max(input$range)))
            p <- ggplot(lf.filtered) +
                geom_point(aes(date, vol., color=region), size=2) +
                geom_line(aes(date, vol., color=region, group=region), size=1) +
                expand_limits(ymin=0) +
                theme(legend.position='left') +
                ggtitle('International debt securities by residence of issuer') +
                ylab('Volume in Billion of Dollars') +
                xlab('Date')
            print(p)
        }
    })
    output$summary <- renderPrint({
        #dataset <- datasetInput()
        if (is.null(input$errors_shown)) {
            # If no checkboxes are checked, dislay an empty plot.
            summary(logfreq)
        } else {      
            lf.filtered <- subset(logfreq, region %in% input$errors_shown)
            lf.filtered <- subset(lf.filtered, (year >= min(input$range)))
            lf.filtered <- subset(lf.filtered, (year <= max(input$range)))
            summary(lf.filtered)
        }
    })
    
    output$mytable1 = renderDataTable({
        library(ggplot2)
        if (is.null(input$errors_shown)) {
            # If no checkboxes are checked, dislay an empty plot.
            logfreq
        } else {      
            lf.filtered <- subset(logfreq, region %in% input$errors_shown)
            lf.filtered <- subset(lf.filtered, (year >= min(input$range)))
            lf.filtered <- subset(lf.filtered, (year <= max(input$range)))
            lf.filtered
        }
    }, options = list(bSortClasses = TRUE))
    
    output$gvis <- renderGvis({
        if (is.null(input$errors_shown) || input$errors_shown=="All countries"){
            dat <- aggregate(logfreq$vol., list(gp=logfreq$region), mean)
            colnames(dat) <- c('region','mean volume across the period')
            gvisGeoChart(dat,locationvar="region",colorvar="mean volume across the period",
                         options=list(width=1000, height=800,
                                      colorAxis="{colors:['#FFFFFF', '#0000FF']}"))
        }else { 
            if (input$errors_shown=="Developed countries"){
                lf.filtered <- subset(logfreq, region %in% c(
                    'Australia '='Australia ',
                    'Austria'='Austria',
                    'Belgium'='Belgium',
                    'Canada'='Canada',
                    'Cyprus'='Cyprus',
                    'Denmark '='Denmark ',
                    'Estonia'='Estonia',
                    'Finland '='Finland ',
                    'France'='France',
                    'Germany '='Germany ',
                    'Greece'='Greece',
                    'Iceland'='Iceland',
                    'Ireland'='Ireland',
                    'Italy'='Italy',
                    'Japan'='Japan',
                    'Liechtenstein'='Liechtenstein',
                    'Luxembourg'='Luxembourg',
                    'Malta'='Malta',
                    'Netherlands'='Netherlands',
                    'New Zealand '='New Zealand ',
                    'Norway'='Norway',
                    'Portugal '='Portugal ',
                    'Slovakia'='Slovakia',
                    'Slovenia '='Slovenia ',
                    'Spain '='Spain ',
                    'Sweden'='Sweden',
                    'Switzerland '='Switzerland ',
                    'United Kingdom'='United Kingdom',
                    'United States '='United States ',
                    'Hong Kong SAR'='Hong Kong SAR',
                    'Singapore'='Singapore',
                    'Israel'='Israel',
                    'United Arab Emirates'='United Arab Emirates',
                    'Korea'='Korea',
                    'Croatia '='Croatia ',
                    'Czech Republic'='Czech Republic'
                ))
                lf.filtered <- subset(lf.filtered, (year >= min(input$range)))
                lf.filtered <- subset(lf.filtered, (year <= max(input$range)))
                dat <- aggregate(lf.filtered$vol., list(gp=lf.filtered$region), mean)
                colnames(dat) <- c('region','mean volume across the period')
                gvisGeoChart(dat,locationvar="region",colorvar="mean volume across the period",
                             options=list(width=1000, height=800,
                                          colorAxis="{colors:['#FFFFFF', '#0000FF']}"))
            }else {
                lf.filtered <- subset(logfreq, region %in% input$errors_shown)
                lf.filtered <- subset(lf.filtered, (year >= min(input$range)))
                lf.filtered <- subset(lf.filtered, (year <= max(input$range)))
                dat <- aggregate(lf.filtered$vol., list(gp=lf.filtered$region), mean)
                colnames(dat) <- c('region','mean volume across the period')
                gvisGeoChart(dat,locationvar="region",colorvar="mean volume across the period",
                             options=list(width=1000, height=800,
                                          colorAxis="{colors:['#FFFFFF', '#0000FF']}"))
            }}
        
    })

})