library(data.table)
library(PMwR)
library(ggplot2)
library(reshape2)
library(grid)
library(scales)
library(quantmod)
library(xts)
library(highfrequency)

setDTthreads(percent = 100)

## ##
setwd("~/Documents/QuantLet/plot_crypto_ts")
Sys.setenv(LANG = "en") # set environment language to English
Sys.setlocale("LC_TIME", "en_US.UTF-8") # set timestamp language to English
## ##

## theme
plot_theme <- theme(panel.border = element_blank(), panel.background = element_blank(),
                  axis.text = element_text(size = 14, face = "bold"),
                  axis.title = element_text(size = 24, face = "bold"),
                  strip.text = element_text(size = 14, face = "bold"),
                  axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0),
                  legend.position="top",
                  legend.box = "horizontal",
                  legend.text = element_text(colour="black", size=18, face="bold"),
                  legend.title = element_text(colour="black", size=14, face="bold"),
                  plot.title = element_text(size = 24, face = "bold", hjust = .5)
)

plot_theme_notext <- theme(panel.border = element_blank(), panel.background = element_blank(),
                    axis.text = element_text(size = 14, face = "bold"),
                    axis.title = element_blank(),
                    strip.text = element_text(size = 14, face = "bold"),
                    axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0),
                    legend.position="none",
                    plot.title = element_blank()
)

### 

load("./data/agg_files/DT_list.RData")
DT_subs <- rbindlist(lapply(seq_along(DT_list), function(x) rbindlist(DT_list[[x]], idcol = "s")))
DT_subs[, "Symbol" := s]
DT_subs[Symbol %in% "btcusd", Symbol := "BTC / USD"]
DT_subs[Symbol %in% "ethusd", Symbol := "ETH / USD"]
DT_subs[Symbol %in% "ltcusd", Symbol := "LTC / USD"]
DT_subs[Symbol %in% "dshusd", Symbol := "DSH / USD"]
DT_subs[Symbol %in% "xmrusd", Symbol := "XMR / USD"]
DT_subs[Symbol %in% "xrpusd", Symbol := "XRP / USD"]
DT_subs <- DT_subs[,2:length(DT_subs)]

DT_subs[, t :=as.POSIXct(t, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")]
#DT_subs[, p := scale(p, scale = T, center = T), by = "s"]

cryptos <- ggplot(data = DT_subs, mapping = aes(y = p, x = t, group = Symbol, colour = Symbol)) +
  geom_line(size = 1) +
  plot_theme_notext +
  scale_x_datetime(limits = c(min = min(DT_subs$t), max = max(DT_subs$t)), expand=c(0.01,0.01),  labels = date_format("%B %Y")) +
  theme(legend.position = "none",
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    plot.margin = margin(10, 10, 10, 85)
  ) 
cryptos
ggsave(plot = cryptos, file = "./plot/cryptos.png",  bg = "transparent", 
       width = 11.69, height = 8.27, dpi = 300)