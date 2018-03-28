names(i)=виды
i=iris
names(i$"Species")=виды
i=iris
x=5:5
x=x*2
x=(x>0)=x(x>0)*2

my_function=function(fx){
if(fx>0){
print("positiv")
}else{
print("negativ")}
return(NULL)}
my_function(-2)
y=my_function(5)

my_function=function(fx){
if(fx>0){
 return("positiv")
}else{
if(fx<0){}
return("negativ")}
return("zero")}
my_function(2)
y=my_function(-5)

log(-1)
my_log=function(x){
if(x<0)}
return(NA)
}else{
return(my_log(x)}

for(i in 1:10){ #итерация
print(i)}#печатать 10 единичных векторов

x=-5:5
for(i in 1:length(x)){    #цикл от 1 до длины х
if(x[i]>0){
x[i]=x[i]*2}
}
apply(dt,1,sum)
apply(iris,2,summary)#таблица,2 колонка,суммировать
apply(iris[1:4],2,sum)#таблица,с 1 по 4 колонку,суммировать
apply(iris,2,function(x){
if(typeof(x)="double"){returne(sum(x))}
else{return("NA")}
})

plot (x,y)
x=1:100
y=sin(x)
plot(x,y)#строим график
x=seq(1,100,by=1)
plot(iris$Species,iris$Sepal.Length)
x=seq(1,100,by=1)
y-sin(x)
plot(x,y,type="b",col=rgb(0.5,0.8,1,1),lwd=(2),lty=(5),pch=(11),xlim=c(1,10),ylim=c(0.5,0.8),main = "график")
#тип графика=точки и линии,цвет=ргб,толщина линии,соединение точек,вид точки,приближение графика,выбор значений,название графика



