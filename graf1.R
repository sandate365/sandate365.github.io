D_partic %>% ggplot(mapping = aes(x = Partic, y = Prom_Part))+
  geom_bar(stat = "identity",fill="blue")+
  labs(title = "Nota promedio de las participaciones",
       x = "Participación",
       y = "Nota promedio de participación",
       subtitle = "Ciclo 20-02",
       caption = "Fuente: UPC")+
  geom_text(aes(y = Prom_Part,label=round(Prom_Part,2)),
            vjust = -0.8,size=5, col="blue")