#Learning ggplot--------------------------------------------------------------

ggplot(BOD,aes(Time,demand))+
  geom_point(size = 3)+
  geom_line(colour = "red")
data()

CO2
?CO2
View(CO2)

ggplot(CO2,aes(conc,uptake,
               colour = Treatment))+
  geom_point(size = 3,alpha = 0.5)+
  geom_line()+
  geom_smooth(method = lm,se=F)+
  facet_wrap(~Type)+
  labs(title = "concentration of CO2")+
  theme_bw()

ggplot(CO2,aes(Treatment,uptake))+
  geom_boxplot()+
  geom_point(alpha = 0.5,
             aes(size = conc,
                 colour =Plant ))+
  facet_wrap(~Type)+
  coord_flip()+
  theme_bw()+
  labs(title = "Chilled vs Non-Chilled")

# Another Dataset of mpg-------------------------------------------------------

View(mpg)

ggplot(mpg,aes(displ,cty))+
  geom_point(aes(colour = drv,size = trans),
             alpha = 0.5)+
  geom_smooth(method = lm)+
  facet_wrap(~year)+
  theme_bw()+
  labs(x = "Engine Size",
       y = "MPG in the city",
       title = "Fuel Efficiency")







