setwd("~/Documents/blog/dispersione scolastica")
library(ggplot2)
library(readr)
library(tidyr)
library(viridis)
library(stringr)

dati <- read_csv2("dati.csv")

dati$quinquennio_ok <- str_replace(dati$quinquennio, "-", "\n")
dati$primo_anno_label <- paste(substr(dati$primo_anno, 1,3), sep="", "k")
dati$quinto_anno_label <- paste(substr(dati$quinto_anno, 1,3), sep="", "k")

dati_reordered <- dati %>% 
  gather("anno", "studenti", 2:3)
  
ggplot(dati) +
  geom_text(aes(x=quinquennio_ok, y=primo_anno, label=primo_anno_label, vjust=-1.1), family="Montserrat", color="dark blue") +
  geom_text(aes(x=quinquennio_ok, y=quinto_anno, label=quinto_anno_label, vjust=2), family="Montserrat", color="dark red") +
  geom_segment(
    aes(x=quinquennio_ok,
        y=primo_anno-4000,
        xend=quinquennio_ok,
        yend=quinto_anno+10000),
    colour="dark blue", size=1, alpha=.4) +
  geom_point(aes(x=quinquennio_ok, 
                 y=primo_anno,
                 color="Studenti iscritti al I anno"),
             size=4, alpha=.5, show.legend = TRUE) +
  geom_point(aes(x=quinquennio_ok,
                 y=quinto_anno+8000),
             size=3, alpha=.5, fill="dark blue", colour="dark blue", shape=25) +
  geom_point(aes(x=quinquennio_ok,
                 y=quinto_anno,
                 color="Studenti iscritti al V anno"),
             size=3, alpha=.5, show.legend = TRUE) +
  scale_color_manual("", values=c("dark blue", "dark red")) +
  theme_minimal() +
  theme(plot.title = element_text(family="Montserrat", size=20),
        axis.title = element_text(family="Montserrat", size=15),
        axis.text = element_text(family="Montserrat", size=10),
        legend.text = element_text(family="Montserrat", size=12),
        plot.caption = element_text(family="Montserrat"),
        legend.position = c(.85,.14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(color="light grey"),
        panel.grid.minor.y = element_line(color="light grey")) +
  scale_y_continuous(breaks=seq(300000,700000,50000), limits=c(350000,650000),
                     labels = scales::unit_format(unit="k",scale=.001,sep="")) +
  labs(x="Quinquenni",
       y="Studenti iscritti",
       title="Dispersione scolastica nella scuola secondaria superiore in Italia dal 1995 al 2018",
       caption="Fonte: Report TuttoScuola 2018")

#ggsave("Dispersione.jpg", height=20, width=40, units="cm")
#ggsave("Dispersione.pdf", height=20, width=40, units="cm")