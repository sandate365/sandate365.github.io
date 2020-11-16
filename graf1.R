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

######################## Diagrama de cajas #####################################

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
                             caption = "Fuente: ---") +
                             geom_text(aes(label=comma(n())),
                             vjust = -0.5,size=4, col="blue")

################################################################################
################Grafico de barras agrupadas Turno/Condición ####################
##########

t_c_1=table(MA460_2$Turno,MA460_2$Cond)

Turno=c("Turno 1","Turno 1","Turno 2","Turno 2")
Condición=c("aprobado","desaprobado","aprobado","desaprobado")
freq= c(439,64,494,78)

MA460_3 = data.frame(Turno,Condición,freq)

MA460_3 %>% ggplot(mapping = aes(x=Turno,y = freq, fill = Condición))+
                             geom_bar(stat = "identity", position = "dodge")+
                             labs(title = "Distribución de alumnos según turno y condición",
                             x = "Turnos de la PC2",
                             y = "Número de alumos",
                             subtitle = "MA460 - Ciclo 20-02",
                             caption = "Fuente: ---") +
                             geom_text(aes(y = freq,label = freq),
                             position = position_dodge(width = 0.9),
                             vjust = -0.5,size=4, col="black")+
                             theme_classic()


################################################################################
################Grafico de barras promedio en los dos turnos ####################
##########

MA460 %>% group_by(Turno) %>% 
                             summarise(promedio=mean(PC2,na.rm = TRUE))


D_turno = c("Turno 1", "Turno 2")
D_prom_PC2 = c(16.9,16.6)

#creando el data frame (que en realidad es una tabla)
DF_prom_PC2 = data.frame(D_turno,D_prom_PC2)

### Creando gráfica, muy importante la función stat= "identity" ya que en realidad
# D_partic no es una data frame.

# geom_text sirve para agregar las etiquetas

DF_prom_PC2 %>% ggplot(mapping = aes(x = D_turno, y = D_prom_PC2))+
  geom_bar(stat = "identity",fill="blue")+
  labs(title = "Nota promedio de la PC2 por turno",
       x = "Turno",
       y = "Nota promedio de la PC2",
       subtitle = "MA460 - Ciclo 20-02",
       caption = "Fuente: ---")+
  geom_text(aes(y = D_prom_PC2,label=round(D_prom_PC2,1)),
            vjust = -0.5,size=4, col="blue")+
  #theme_minimal()    
  theme_classic()

###############################################################################

MA460_4 = MA460 %>% filter(Auto_PC2=NA)

Control=is.na(MA460$Auto_PC2)

MA460_4 = MA460 %>% mutate(Control2 = is.na(Auto_PC2))

# creando tabla

MA460_4 %>% group_by(Control2) %>% 
  summarise(Cuantos=n(), promedio=mean(PC2,na.rm = TRUE))

Hicieron = c("SI","NO")
frec1=c(399,731)

Auto_ev= data.frame(Hicieron,frec1)

####### graficos auto evaluacion  ######

Auto_ev %>% ggplot(mapping = aes(x = Hicieron, y = frec1))+
  geom_bar(stat = "identity",fill="red")+
  labs(title = "Distribución de alumnos según participación en la autoevaluación de la PC2",
       x = "Hicieron la Auto-evaluación",
       y = "Número de alumnos",
       subtitle = "MA460 - Ciclo 20-02",
       caption = "Fuente: ---")+
  geom_text(aes(y = frec1,label=round(frec1,1)), 
            vjust = -0.5,size=4, col="red")+
  #theme_minimal()    
  theme_classic()

#################

Prom_dieron=c(17.8,16.1)
Prom_auto = data.frame(Hicieron,Prom_dieron)


Prom_auto %>% ggplot(mapping = aes(x = Hicieron, y = Prom_dieron))+
  geom_bar(stat = "identity",fill="red")+
  labs(title = "Promedio de la PC2 según participación en la autoevaluación de la PC2",
       x = "Hicieron la Auto-evaluación",
       y = "Promedio de la PC2",
       subtitle = "MA460 - Ciclo 20-02",
       caption = "Fuente: ---")+
  geom_text(aes(y = Prom_dieron,label=round(Prom_dieron,1)), 
            vjust = -0.5,size=4, col="red")+
  #theme_minimal()    
  theme_classic()


