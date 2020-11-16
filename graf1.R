D_partic %>% ggplot(mapping = aes(x = Partic, y = Prom_Part))+
  geom_bar(stat = "identity",fill="blue")+
  labs(title = "Nota promedio de las participaciones",
       x = "Participación",
       y = "Nota promedio de participación",
       subtitle = "Ciclo 20-02",
       caption = "Fuente: UPC")+
  geom_text(aes(y = Prom_Part,label=round(Prom_Part,2)),
            vjust = -0.8,size=5, col="blue")
file.choose()
descr="C:\\Users\\HP\\Documents\\Clases de R\\Clases UPC\\Clases UPC 2020\\MA460.xlsx"
MA460=read_excel(descr)

cajas=MA460 %>% select(2:3)
cajas1= na.omit(cajas)

ggplot(cajas1, mapping = aes(x=Turno,y=PC2,fill=Turno))+
                      geom_boxplot()+
                      labs(title = "Notas de la PC2 para ambos turnos",
                      x = "Turnos de la PC2",
                      y = "Nota de la PC2",
                      subtitle = "MA460 - Ciclo 20-02",
                      caption = "Fuente: ---")

#creando variable condicón aprobado o desaprobado

MA460_1 = MA460 %>% mutate(Cond = if_else (PC2 >= 13,"aprobado", "desaprobado"))

MA460_2 = MA460_1 %>% select(c(3,6)) %>% na.omit()

library(RColorBrewer) # trae una paleta de colores 
display.brewer.all() 


MA460_2 %>% ggplot(mapping = aes(x=Turno, fill=Cond))+
                             geom_bar(position = "dodge")+
                             labs(title = "Distribución de alumnos según turno y condición",
                             x = "Turnos de la PC2",
                             y = "Número de alumos",
                             subtitle = "MA460 - Ciclo 20-02",
                             caption = "Fuente: ---")+
                             geom_text(aes(y = count(),label=count()),
                             vjust = -0.5,size=4, col="blue")
