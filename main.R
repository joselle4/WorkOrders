
# load library
library(devtools)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(plotly)
library(anytime)
library(lubridate)
library(scales)
library(formattable)
library(RColorBrewer)
library(DT)


getData <- function(fileDir) {
  
  dt <- fread(fileDir)
  
  # remove spaces on colnames
  names(dt) <- gsub(" ", "", names(dt))
  
  dt <- convertDates(dt)
  
  
  
  return(dt)
}

getwoData <- function(fileDir) {
  
  dt <- getData(fileDir)
  
  #separate work order status
  dt <- separate(data = dt, 
                 col = `WorkOrderStatus`,
                 into = c("Status", "StatusDescription"),
                 sep = " - ")
  
  dt <- separate(data = dt, 
                 col = `WorkOrderPriority`,
                 into = c("Priority", "PriorityDescription"),
                 sep = " - ")
  
  dt$OpenClosed[dt$Status == "CLOSE" | dt$Status == "COMP" | dt$Status == "PRECLOSE"] <- "Closed"
  dt$OpenClosed[dt$Status != "CLOSE" & dt$Status != "COMP" & dt$Status != "PRECLOSE"] <- "Open"
  
  #add first of day of each month
  dt$firstOfReportedDate <- dt$WorkOrderReportedDate - day(dt$WorkOrderReportedDate) + 1
  #add period
  dt$Period <- addPeriod(dt$WorkOrderReportedDate)
  
  #identify GenStations
  dt$GenStation <- grepl("GS$", dt$WorkOrderSite)
  
  return(dt)
}

convertDates <- function(dt) {
  
  if("WorkOrderReportedDate" %in% colnames(dt)) {
    dt$WorkOrderReportedDate <- sapply(dt$WorkOrderReportedDate, function(dateStamp){
      if(grepl("/", dateStamp) == TRUE) {
        dateStamp <- as.Date(dateStamp, format = "%m/%d/%Y")}
      else if(grepl("-", dateStamp) == TRUE) {
        dateStamp <- as.Date(dateStamp , format = "%Y-%m-%d")}
    })
    dt$WorkOrderReportedDate <- as.Date(dt$WorkOrderReportedDate, origin = "1970-01-01")
  }
  
  return(dt)
}

addPeriod <- function(dateColumn, separator = NULL) {
  return(sapply(dateColumn, function(dateColumn){
    ifelse(nchar(month(dateColumn)) == 1, 
           paste0(year(dateColumn), separator, 0, month(dateColumn)), 
           paste0(year(dateColumn), separator, month(dateColumn)))
    
  }))
}

calcWOGSThresholds <- function(dt) {
  
}

plotOpenClosedGSWOs <- function(dt, OpenClosedValue = "Closed", yearValue = NULL) {
  
  dt <- dt[GenStation == TRUE & OpenClosed == OpenClosedValue & 
             WorkOrderReportedYear >= yearValue & Priority != "?", 
           sum(WorkOrderCount), 
           keyby = c("WorkOrderSite", "Period")]
  
  p <- plot_ly(dt, x = ~Period, y = ~V1, color = ~WorkOrderSite, type = "bar", textposition = "auto") %>%
    layout(barmode = "stack", xaxis = list(title = ""), yaxis = list(title = ""))
  
  return(p)
}

plotOpenClosedWOsByPriority <- function(dt, OpenClosedValue = "Closed", yearValue = NULL) {
  
  dt <- dt[GenStation == TRUE & OpenClosed == OpenClosedValue & 
             WorkOrderReportedYear >= yearValue & Priority != "?", 
           sum(WorkOrderCount), 
           keyby = c("Priority", "Period")]
  plotColor <- "aliceblue"
  barColors <- scale_color_brewer(palette = "Reds", direction = -1)
  
  p <- plot_ly(dt, x = ~Period, y = ~V1, color = ~Priority, type = "bar", textposition = "auto", 
               colors = c("forestgreen", "lightgoldenrod", "orange", "red")) %>%
    layout(barmode = "stack", xaxis = list(title = ""), yaxis = list(title = ""))
  
  #plot_bgcolor = plotColor, paper_bgcolor = plotColor
  return(p)
}

plotOpenClosedWOsByPriorityPie <- function(dt, OpenClosedValue = "Closed", yearValue = NULL) {
  
  dt <- dt[GenStation == TRUE & OpenClosed == OpenClosedValue & 
             WorkOrderReportedYear == yearValue & Priority != "?", 
           sum(WorkOrderCount), keyby = c("Priority", "PriorityDescription")]
  dt$Label <- paste(dt$Priority, "-", dt$PriorityDescription)
  
  p <- plot_ly(dt, labels = ~Label, values = ~V1, type = "pie", 
               textinfo = "label+value+percent", insidetextfont = list(color = "white")) %>% 
    layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), 
           showlegend = FALSE, margin = list(l = 170, t = 20, b = 70))
  
  return(p)
}

plotOpenClosedWOsLaborHours <- function(dt, OpenClosedValue = "Closed", yearValue = NULL) {

  dt <- dt[GenStation == TRUE & OpenClosed == OpenClosedValue & 
             WorkOrderReportedYear >= yearValue & Priority != "?", 
           sum(ActualLaborHours), 
           keyby = c("WorkOrderSite", "Period")]

  p <- plot_ly(dt, x = ~Period, y = ~V1, color = ~WorkOrderSite, type = "bar", textposition = "auto") %>%
    layout(barmode = "stack", xaxis = list(title = ""), yaxis = list(title = ""))
  
  return(p)
}

plotOpenClosedWOsLaborHoursPie <- function(dt, OpenClosedValue = "Closed", yearValue = NULL) {
  
  dtAll <- dt[GenStation == TRUE & OpenClosed == OpenClosedValue & 
                WorkOrderReportedYear == yearValue & Priority != "?",
              .(sum(TotalLaborRegularHours), sum(TotalLaborOvertimeHours))]
  colnames(dtAll) <- c("Regular", "Overtime")
  dtAll <- melt(dtAll, measure.vars = c("Regular", "Overtime"))
    
  dt <- dt[GenStation == TRUE & OpenClosed == OpenClosedValue & 
             WorkOrderReportedYear == yearValue & Priority != "?",
             .(sum(TotalLaborRegularHours), sum(TotalLaborOvertimeHours)), 
             keyby = c("WorkOrderSite", "Priority")]
  colnames(dt) <- c("WorkOrderSite", "Priority", "Regular", "Overtime")
  dt <- melt(dt, measure.vars = c("Regular", "Overtime"))
    
  p <- plot_ly(data = dtAll, labels = ~variable, values = ~value, type = "pie", visible = TRUE, 
               textinfo = "label+value+percent", insidetextfont = list(color = "white"), 
               name = "All GS & Priorities") %>%
    layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           showlegend = FALSE, margin = list(l = 100, t = 50, b = 50))
  # get unique combo of gen stations and priorities
  
  uniqueGS <- unique(dt$WorkOrderSite)
  uniquePriority <- unique(dt$Priority)
  
  for (i in 1:length(uniqueGS)) {
    for (j in 1:length(uniquePriority)) {
      p <- add_trace(p, data = dt[WorkOrderSite == uniqueGS[i] & Priority == uniquePriority[j]], 
                     labels = ~variable, values = ~value, type = "pie", visible = FALSE, 
                     textinfo = "label+value+percent", insidetextfont = list(color = "white"), 
                     name = paste(uniqueGS[i], "&", uniquePriority[j])) %>% 
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               showlegend = FALSE, margin = list(l = -100, t = 50, b = 50))
    }
  }
  
  p <- p %>% layout(updatemenus = list(
    list( 
      buttons = list(
        list(label = "All GS and Priorities", 
             method = "restyle", 
             args = list("visible", list(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                                         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))), 
        list(label = "CGS - Priority 1", 
             method = "restyle", 
             args = list("visible", list(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                                         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))),  
        list(label = "CGS - Priority 2", 
             method = "restyle", 
             args = list("visible", list(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                                         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))),  
        list(label = "CGS - Priority 3", 
             method = "restyle", 
             args = list("visible", list(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                                         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))),  
        list(label = "CGS - Priority 4", 
             method = "restyle", 
             args = list("visible", list(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                                         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))),  
        list(label = "GGS - Priority 1", 
             method = "restyle", 
             args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                                         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))),  
        list(label = "GGS - Priority 2", 
             method = "restyle", 
             args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, 
                                         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))),  
        list(label = "GGS - Priority 3", 
             method = "restyle", 
             args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, 
                                         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))),  
        list(label = "GGS - Priority 4", 
             method = "restyle", 
             args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, 
                                         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))),  
        list(label = "JGS - Priority 1", 
             method = "restyle", 
             args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, 
                                         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))),  
        list(label = "JGS - Priority 2", 
             method = "restyle", 
             args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, 
                                         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))),  
        list(label = "JGS - Priority 3", 
             method = "restyle", 
             args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                                         TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))),  
        list(label = "JGS - Priority 4", 
             method = "restyle", 
             args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                                         FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))),  
        list(label = "RGS - Priority 1", 
             method = "restyle", 
             args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                                         FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))),  
        list(label = "RGS - Priority 2", 
             method = "restyle", 
             args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                                         FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))),  
        list(label = "RGS - Priority 3", 
             method = "restyle", 
             args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                                         FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))),  
        list(label = "RGS - Priority 4", 
             method = "restyle", 
             args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                                         FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE))),  
        list(label = "WGS - Priority 1", 
             method = "restyle", 
             args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                                         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))), 
        list(label = "WGS - Priority 2", 
             method = "restyle", 
             args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                                         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))),  
        list(label = "WGS - Priority 3", 
             method = "restyle", 
             args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                                         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE))),  
        list(label = "WGS - Priority 4", 
             method = "restyle", 
             args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                                         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))) 
      )
    )
  ))
  
  return(p)
}

# p <- p %>% layout(
#   xaxis = list(domain = c(0.1, 1)),
#   updatemenus = list(
#     # first dropdown
#     list(
#       y = 1,
#       buttons = list (
#         
#         list(method = "restyle", 
#              args = list("visible", list(TRUE, FALSE, FALSE, FALSE, FALSE)),
#              label = "CGS"), 
#         list(method = "restyle", 
#              args = list("visible", list(FALSE, TRUE, FALSE, FALSE, FALSE)),
#              label = "GGS"),
#         list(method = "restyle", 
#              args = list("visible", list(FALSE, FALSE, TRUE, FALSE, FALSE)),
#              label = "JGS"),
#         list(method = "restyle", 
#              args = list("visible", list(FALSE, FALSE, FALSE, TRUE, FALSE)),
#              label = "RGS"),
#         list(method = "restyle", 
#              args = list("visible", list(FALSE, FALSE, FALSE, FALSE, TRUE)),
#              label = "WGS")
#       )
#     ),
#     
#     # second dropdown
#     list(
#       y = .8,
#       buttons = list (
#         
#         list(method = "restyle", 
#              args = list("visible", list(TRUE, FALSE, FALSE, FALSE, FALSE)),
#              label = "?"), 
#         list(method = "restyle", 
#              args = list("visible", list(FALSE, TRUE, FALSE, FALSE, FALSE)),
#              label = "1"),
#         list(method = "restyle", 
#              args = list("visible", list(FALSE, FALSE, TRUE, FALSE, FALSE)),
#              label = "2"),
#         list(method = "restyle", 
#              args = list("visible", list(FALSE, FALSE, FALSE, TRUE, FALSE)),
#              label = "3"),
#         list(method = "restyle", 
#              args = list("visible", list(FALSE, FALSE, FALSE, FALSE, TRUE)),
#              label = "4")
#       )
#     )
#   ))

# p <- add_trace(p, data = dt[WorkOrderSite == "CGS" & Priority == 2], 
#                labels = ~variable, values = ~value, type = "pie", visible = FALSE, 
#                textinfo = "label+value+percent", insidetextfont = list(color = "white")) %>%
#   layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#          showlegend = FALSE)