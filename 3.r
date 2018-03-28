mtcars#пакет машин
diamonds#пакет бриллиантов
#aes-эстетика
ggplot(mtcars,aes(x=cyl,y=mpg,col=disp,size=disp))+geom_point() #отобразить точками,выбрать цвет точек,размер точек 
ggplot(mtcars,aes(x=disp,y=vs))+geom_point()#выбрать другие параметры и колонки графика
ggplot(diamonds,aes(x=color,y=price))+geom_point()
ggplot(diamonds,aes(x=carat,y=price,col=clarity)) +geom_point()+geom_smooth()
#geom_smooth-геометрия сглаженная кривая,категории яркости






