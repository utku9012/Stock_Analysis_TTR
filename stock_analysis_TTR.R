install.packages("tidyquant")
install.packages("tidyverse")
install.packages("janitor")
install.packages("TTR")
install.packages("plotly")

library(tidyquant)
library(tidyverse)
library(janitor)
library(TTR)
library(plotly)

#rawData <- tq_get("APPL" from="2020-01-05")
rawData <- tq_get(c("SASA.IS","TUPRS.IS","ASELS.IS","FROTO.IS","THYAO.IS"), from="2020-01-01")

#checking the table
rawData %>% count(symbol)

#checking for NA values
rawData %>% is.na() %>% colSums()
rawData %>% filter(is.na(open))

#filling the NA values with the value above it
Data <- rawData %>% 
  fill(c("open","close","high","low","volume","adjusted"), .direction ="down")

#checking if any NA values left
any(is.na(Data))

#making colums upper case using library janitor
Data <- rawData %>% 
  clean_names("screaming_snake") 

#selecting columns
Data %>% 
  select(SYMBOL,DATE,OPEN,CLOSE)

#optimal to work with multiple stocks, filtering by the stock name, filling the NA values, renaming the columns, and only selecting the needed columns for later use
a <- Data %>% 
  nest(data = -SYMBOL) %>% 
  mutate(data = map(data,
                    ~fill(.,
                          c("OPEN","CLOSE","HIGH","LOW","VOLUME","ADJUSTED"),
                          .direction ="down") %>% 
                      clean_names("screaming_snake") %>%
                      select(DATE, OPEN, CLOSE, HIGH, LOW)))


a %>% is.na() %>% colSums()

#simple visualization showing last 5 years
ggplot(data = a$data[[1]], aes(x= DATE, y = CLOSE))+
  geom_line(color = "blue", size = 0.5)+
  labs(title = paste("Stock Price of", (a$SYMBOL[[1]])),
       x = "Date",
       y = "Price")+
  theme_minimal()

#simple visualization showing only last year
ggplot(a$data[[1]] %>% filter(DATE >= as.Date("2024-01-01")), aes(x= DATE, y = CLOSE))+
  geom_line(color = "blue", size = 0.5)+
  labs(title = paste("Stock Price of", (a$SYMBOL[[1]])),
       x = "Date",
       y = "Price")+
  theme_minimal()

#calculating indicators for each stock
new_sasa <- a$data[[1]] %>% drop_na() %>% 
  mutate(SMA = SMA(CLOSE, n=30),
         RSI = RSI(CLOSE, n=14),
         ADX = ADX(HLC = select(., HIGH, LOW, CLOSE), n = 14)[,"ADX"],
         MACD_diff = MACD(CLOSE, nFast = 12, nSlow = 26, nSig = 9)[,"macd"],
         MACD_signal = MACD(CLOSE, nFast = 12, nSlow = 26, nSig = 9)[,"signal"],
         BB_upper = BBands(CLOSE)[,"up"],
         BB_lower = BBands(CLOSE)[,"dn"])

new_tuprs <- a$data[[2]] %>% drop_na() %>% 
  mutate(SMA = SMA(CLOSE, n=30),
         RSI = RSI(CLOSE, n=14),
         ADX = ADX(HLC = select(., HIGH, LOW, CLOSE), n = 14)[,"ADX"],
         MACD_diff = MACD(CLOSE, nFast = 12, nSlow = 26, nSig = 9)[,"macd"],
         MACD_signal = MACD(CLOSE, nFast = 12, nSlow = 26, nSig = 9)[,"signal"],
         BB_upper = BBands(CLOSE)[,"up"],
         BB_lower = BBands(CLOSE)[,"dn"])

new_asels <- a$data[[3]] %>% drop_na() %>% 
  mutate(SMA = SMA(CLOSE, n=30),
         RSI = RSI(CLOSE, n=14),
         ADX = ADX(HLC = select(., HIGH, LOW, CLOSE), n = 14)[,"ADX"],
         MACD_diff = MACD(CLOSE, nFast = 12, nSlow = 26, nSig = 9)[,"macd"],
         MACD_signal = MACD(CLOSE, nFast = 12, nSlow = 26, nSig = 9)[,"signal"],
         BB_upper = BBands(CLOSE)[,"up"],
         BB_lower = BBands(CLOSE)[,"dn"])

new_froto <- a$data[[4]] %>% drop_na() %>% 
  mutate(SMA = SMA(CLOSE, n=30),
         RSI = RSI(CLOSE, n=14),
         ADX = ADX(HLC = select(., HIGH, LOW, CLOSE), n = 14)[,"ADX"],
         MACD_diff = MACD(CLOSE, nFast = 12, nSlow = 26, nSig = 9)[,"macd"],
         MACD_signal = MACD(CLOSE, nFast = 12, nSlow = 26, nSig = 9)[,"signal"],
         BB_upper = BBands(CLOSE)[,"up"],
         BB_lower = BBands(CLOSE)[,"dn"])

new_thyao <- a$data[[5]] %>% drop_na() %>% 
  mutate(SMA = SMA(CLOSE, n=30),
         RSI = RSI(CLOSE, n=14),
         ADX = ADX(HLC = select(., HIGH, LOW, CLOSE), n = 14)[,"ADX"],
         MACD_diff = MACD(CLOSE, nFast = 12, nSlow = 26, nSig = 9)[,"macd"],
         MACD_signal = MACD(CLOSE, nFast = 12, nSlow = 26, nSig = 9)[,"signal"],
         BB_upper = BBands(CLOSE)[,"up"],
         BB_lower = BBands(CLOSE)[,"dn"])

#30 day simple moving average for SASA.IS 
plot_ly(new_sasa, 
        x = ~DATE, y = ~CLOSE, 
        name = "Price", type = "scatter", 
        mode = "lines", line = list(color = "#1f77b4"), 
        hoverinfo = "x+y+name", showlegend = FALSE) %>% 
  add_trace(y =~ SMA, name="SMA", mode="lines", line = list(color = "red"),hoverinfo = "y+name", showlegend = FALSE) %>% 
  layout(
    title = "30 Day Simple Moving Average",
    xaxis = list(title = "Date"),
    yaxis = list(title = "Price")
  )

#RSI
plot_ly(new_sasa, 
        x = ~DATE, y = ~RSI, 
        name = "RSI", type = "scatter", 
        mode = "lines", line = list(color = "#ff7f0e"), 
        hoverinfo = "y+name", showlegend = FALSE) %>%
  add_segments(x = ~first(DATE), xend = ~last(DATE), y = 30, yend = 30, line=list(color="black",dash="dot"),showlegend= FALSE) %>%
  add_segments(x = ~first(DATE), xend = ~last(DATE), y = 70, yend = 70, line=list(color="black",dash="dot"),showlegend= FALSE) %>% 
  layout(
    title = "RSI",
    xaxis = list(title = "Date")
  )

#MACD
plot_ly(new_sasa, 
        x = ~DATE, y = ~MACD_diff, 
        name = "MACD_Diff", type = "scatter", 
        mode = "lines", line = list(color = "green"), 
        hoverinfo = "x+y+name", showlegend = FALSE) %>% 
  add_trace(y =~ MACD_signal, name="MACD_Signal", mode="lines", line = list(color = "red"),hoverinfo = "y+name", showlegend = FALSE) %>% 
  layout(
    title = "MACD",
    xaxis = list(title = "Date")
    )

#ADX
plot_ly(new_sasa, 
        x = ~DATE, y = ~ADX, 
        name = "ADX", type = "scatter", 
        mode = "lines", line = list(color = "#2ca02c"), 
        hoverinfo = "x+y+name", showlegend = FALSE) %>% 
  add_segments(x = ~first(DATE), xend = ~last(DATE), y = 25, yend = 25, line=list(color="black",dash="dot"),showlegend= FALSE) %>% 
  layout(
    title = "ADX",
    xaxis = list(title = "Date")
  )

#BB
plot_ly(new_sasa, x = ~DATE) %>%
  add_lines(y = ~CLOSE, name = "Price", line = list(color = "blue")) %>%
  add_lines(y = ~SMA, name = "Moving Avg", line = list(color = "black", dash = "dash")) %>%
  add_lines(y = ~BB_upper, name = "Upper Band", line = list(color = "red", dash = "dot")) %>%
  add_lines(y = ~BB_lower, name = "Lower Band", line = list(color = "red", dash = "dot")) %>%
  add_ribbons(
    ymin = ~BB_lower,
    ymax = ~BB_upper,
    name = "Bollinger Bands",
    fillcolor = "rgba(255, 0, 0, 0.2)",
    line = list(color = "transparent")
  ) %>%
  layout(
    title = "Bollinger Bands",
    xaxis = list(title = "Date"),
    yaxis = list(title = "Price")
  )

## Subplot
subplot(plot_ly(new_sasa, 
                x = ~DATE, y = ~CLOSE, 
                name = "SMA", type = "scatter", 
                mode = "lines", line = list(color = "#1f77b4"), 
                hoverinfo = "x+y+name", showlegend = FALSE) %>% 
          add_trace(y =~ SMA, name="SMA", mode="lines", line = list(color = "red"),hoverinfo = "y+name", showlegend = FALSE) %>% 
          layout(
            title = "Simple Moving Average",
            xaxis = list(title = "Date"),
            yaxis = list(title = "Price")
          ),

        plot_ly(new_sasa, 
                x = ~DATE, y = ~RSI, 
                name = "RSI", type = "scatter", 
                mode = "lines", line = list(color = "#ff7f0e"), 
                hoverinfo = "y+name", showlegend = FALSE) %>% 
          add_segments(x = ~first(DATE), xend = ~last(DATE), y = 30, yend = 30, line=list(color="black",dash="dot"),showlegend= FALSE) %>%
          add_segments(x = ~first(DATE), xend = ~last(DATE), y = 70, yend = 70, line=list(color="black",dash="dot"),showlegend= FALSE) %>% 
          layout(
            title = "RSI",
            xaxis = list(title = "Date")
          ),
        
        plot_ly(new_sasa, 
                x = ~DATE, y = ~MACD_diff, 
                name = "MACD_Diff", type = "scatter", 
                mode = "lines", line = list(color = "green"), 
                hoverinfo = "x+y+name", showlegend = FALSE) %>% 
          add_trace(y =~ MACD_signal, name="MACD_Signal", mode="lines", line = list(color = "red"),hoverinfo = "y+name", showlegend = FALSE) %>% 
          layout(
            title = "MACD",
            xaxis = list(title = "Date")
          ),
        
        plot_ly(new_sasa, 
                x = ~DATE, y = ~ADX, 
                name = "ADX", type = "scatter", 
                mode = "lines", line = list(color = "#2ca02c"), 
                hoverinfo = "x+y+name", showlegend = FALSE) %>% 
          add_segments(x = ~first(DATE), xend = ~last(DATE), y = 25, yend = 25, line=list(color="black",dash="dot"),showlegend= FALSE) %>% 
          layout(
            title = "",
            xaxis = list(title = "Date")
          ),
        nrows = 4, shareX = TRUE)

#BUY SELL and HOLD decisions
all_signals_sasa <- new_sasa %>% 
  transmute(DATE,CLOSE,
            signal_SMA = case_when(CLOSE > SMA ~ "Buy", 
                                   CLOSE < SMA ~ "Sell",
                                   TRUE ~ "Hold"),
            signal_RSI = case_when(RSI < 30 ~ "Buy",
                                   RSI > 70 ~ "Sell",
                                   TRUE ~ "Hold"),
            signal_MACD = case_when(MACD_diff > MACD_signal ~ "Buy",
                                    MACD_diff < MACD_signal ~ "Sell",
                                   TRUE ~ "Hold"),
            signal_ADX = case_when(ADX > 25 ~ "Strong Trend",
                                    TRUE ~ "Weak Trend")) 

            
# I assigned the decisions to four different variables to display them simultaneously on four different graphs.           
decision_SMA_sasa <- new_sasa %>% 
  transmute(DATE,OPEN,CLOSE,SMA,
            signal_SMA = case_when(CLOSE > SMA ~ "Buy", 
                                   CLOSE < SMA ~ "Sell",
                                   TRUE ~ "Hold")) %>%
            filter(signal_SMA != lag(signal_SMA)) %>% drop_na() # It filters out the data that is different from the one below it.

decision_RSI_sasa <- new_sasa %>% 
  transmute(DATE,OPEN,CLOSE,RSI,
            signal_RSI = case_when(RSI < 30 ~ "Buy",
                                   RSI > 70 ~ "Sell",
                                   TRUE ~ "Hold")) %>%
            filter(signal_RSI != lag(signal_RSI)) %>% drop_na() # It filters out the data that is different from the one below it.
                        
decision_MACD_sasa <- new_sasa %>% 
  transmute(DATE,CLOSE,MACD_diff,MACD_signal,
            signal_MACD = case_when(MACD_diff > MACD_signal ~ "Buy",
                                    MACD_diff < MACD_signal ~ "Sell",
                                    TRUE ~ "Hold")) %>%
            filter(signal_MACD != lag(signal_MACD)) %>% drop_na() # It filters out the data that is different from the one below it.

decision_ADX_sasa <- new_sasa %>% 
  transmute(DATE,CLOSE,ADX,
            signal_ADX = case_when(ADX > 25 ~ "Strong Trend",
                                   TRUE ~ "Weak Trend")) %>%
            filter(signal_ADX != lag(signal_ADX)) %>% drop_na() # It filters out the data that is different from the one below it.


view(decision_SMA_sasa)
      
#Buy/Sell signals using SMA
plot1 <- plot_ly() %>% 
  add_lines(data = new_sasa, x = ~DATE, y = ~CLOSE, name ="Price", line=list(color="black"), showlegend =FALSE) %>% 
  add_lines(data = new_sasa, x = ~DATE, y = ~SMA, name ="SMA", line=list(color="grey"), showlegend =FALSE) %>% 
  add_markers(data = decision_SMA_sasa %>% filter(signal_SMA == "Buy"),  x = ~DATE, y = ~CLOSE, text = ~"Buy",
              marker = list(color = 'green', size = 10, symbol = 'triangle-up'), name = "Buy", showlegend = FALSE) %>%
  add_markers(data = decision_SMA_sasa %>% filter(signal_SMA == "Sell"),  x = ~DATE, y = ~CLOSE, text = ~"Sell",
              marker = list(color = 'red', size = 10, symbol = 'triangle-down'), name = "Sell", showlegend = FALSE) %>% 
  layout(title = paste("Buy/Sell signals using SMA on", (a$SYMBOL[[1]])),
         yaxis = list(title ='Price'),
         xaxis = list(title ='Date'))

plot1

#Buy/Sell signals using RSI 
plot2 <- plot_ly() %>% 
  add_lines(data = new_sasa, x = ~DATE, xend = ~DATE, y = ~RSI, yend = ~RSI, name ="RSI", mode="lines", line=list(color="orange"), showlegend =FALSE) %>%
  add_segments(data = new_sasa, x = ~first(DATE), xend = ~last(DATE), y = 30, yend = 30,line=list(color = "black", dash = "dot"), showlegend = FALSE) %>%
  add_segments(data = new_sasa, x = ~first(DATE), xend = ~last(DATE), y = 70, yend = 70,line=list(color = "black", dash = "dot"), showlegend = FALSE) %>%
  add_markers(data = decision_RSI_sasa %>% filter(signal_RSI == "Buy"),  x = ~DATE, y = ~RSI, text = ~"Buy",
              marker = list(color = 'green', size = 10, symbol = 'triangle-up'), name = "Buy", showlegend = FALSE) %>%
  add_markers(data = decision_RSI_sasa %>% filter(signal_RSI == "Sell" & RSI > 70 & RSI <= 80),  x = ~DATE, y = ~RSI, text = ~"Sell",
              marker = list(color = 'red', size = 10, symbol = 'triangle-down'), name = "Sell", showlegend = FALSE) %>%
  layout(title = paste("Buy/Sell signals using RSI on", (a$SYMBOL[[1]])),
         yaxis = list(title ='RSI'),
         xaxis = list(title ='Date'))


plot2

#Buy/Sell signals using MACD
plot3 <- plot_ly(new_sasa, x = ~DATE, y = ~MACD_diff, name = "MACD_Diff", type = "scatter", mode = "lines", hoverinfo = "x+y+name", showlegend = FALSE) %>% 
  add_trace(y =~ MACD_signal, name="MACD_Signal", mode="lines", line = list(color = "orange"), hoverinfo = "y+name", showlegend = FALSE) %>% 
  add_markers(data = decision_MACD_sasa %>% filter(signal_MACD == "Buy"),  x = ~DATE, y = ~MACD_signal, text = ~"Buy",
              marker = list(color = 'green', size = 10, symbol = 'triangle-up'), name = "Buy", showlegend = FALSE) %>%
  add_markers(data = decision_MACD_sasa %>% filter(signal_MACD == "Sell"),  x = ~DATE, y = ~MACD_signal, text = ~"Sell",
              marker = list(color = 'red', size = 10, symbol = 'triangle-down'), name = "Sell", showlegend = FALSE) %>%
  layout(
    title = paste("Buy/Sell signals using MACD on", (a$SYMBOL[[1]])),
    xaxis = list(title = "Date")
  )

plot3 

#ADX Trend References (NOT WORKING??)
plot_ly(new_sasa, 
        x = ~DATE, y = ~ADX, 
        name = "ADX", type = "scatter", 
        mode = "lines", line = list(color = "#2ca02c"), 
        hoverinfo = "x+y+name", showlegend = FALSE) %>% 
  add_segments(x = ~first(DATE), xend = ~last(DATE), y = 25, yend = 25, line=list(color="black",dash="dot"),showlegend= FALSE) %>% 
  layout(
    title = "ADX",
    xaxis = list(title = "Date")
  )

#Subplot all of decisions together
subplot(plot1,plot2,plot3 %>% 
          layout(
            title = "Stock Anaylsis with Buy/Sell Indicators",
            xaxis = list(title = "Date")
          ),
        nrows = 3, shareX = TRUE)
