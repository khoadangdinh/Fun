#write a nice message, get creative
library(ggplot2)
library(extrafont)
library(reshape2)

loadfonts(device = "all")
#put the name of the person you want to make this card for in display_text
display_text="FRIEND" #change this no any text
n=200 #number of words
#design the christmas tree
upper_color=rgb(0.2,0.7,0.2)
lower_color=rgb(0.2,0.5,0.2)
bgColor=rgb(0.1,0.1,0.25)
myY=runif(n,3,11)
dat <- data.frame(
  y = sort(myY),
  x = runif(n,-4,4),
  a = sort(myY)/13,
  textangle = 45*(runif(n,0,2)-1),
  textsize=runif(n,4,5)) #random size
text = rep(display_text,n)
b<-ggplot(dat, aes(x=x, y=y)) + 
  scale_y_continuous(limits=c(3, 11), breaks=NULL) +
  scale_x_continuous(breaks=NULL)+
  geom_text(aes(label=text,family="Courier",fontface='bold'),
            size=dat$textsize,color="white",alpha=dat$a,angle=dat$textangle)+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),  #remove y axis ticks
        legend.position = 'none',
        panel.background = element_rect(fill = bgColor, color = bgColor),
        plot.background = element_rect(fill = bgColor, color = bgColor),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),)

x=seq(-1,0.02,1/40)
y=(x+1)^2
x1=seq(-0.02,1,1/40)
y1=(-x1+1)^2
datTree<-data.frame(x,x1,y,y1
)


card<-b+geom_line(data=datTree,aes(x=x,y=y+9),color=upper_color,linewidth=3)+
  geom_line(data=datTree,aes(x=x,y=y+8.9),color=lower_color,linewidth=3)+
  geom_line(data=datTree,aes(x=x1,y=y1+9),color=upper_color,linewidth=3)+
  geom_line(data=datTree,aes(x=x1,y=y1+8.9),color=lower_color,linewidth=3)+
  geom_line(data=datTree,aes(x=x*1.5,y=y+8),color=upper_color,linewidth=3.5)+
  geom_line(data=datTree,aes(x=x*1.5,y=y+7.9),color=lower_color,linewidth=3.5)+
  geom_line(data=datTree,aes(x=x1*1.5,y=y1+8),color=upper_color,linewidth=3.5)+
  geom_line(data=datTree,aes(x=x1*1.5,y=y1+7.9),color=lower_color,linewidth=3.5)+
  geom_line(data=datTree,aes(x=x*2,y=y+7),color=upper_color,linewidth=4.5)+
  geom_line(data=datTree,aes(x=x*2,y=y+6.9),color=lower_color,linewidth=3)+
  geom_line(data=datTree,aes(x=x1*2,y=y1+7),color=upper_color,linewidth=2.5)+
  geom_line(data=datTree,aes(x=x1*2,y=y1+6.9),color=lower_color,linewidth=3)+
  geom_line(data=datTree,aes(x=x*2.5,y=y+6),color=upper_color,linewidth=5.5)+
  geom_line(data=datTree,aes(x=x*2.5,y=y+5.9),color=lower_color,linewidth=3.3)+
  geom_line(data=datTree,aes(x=x1*2.5,y=y1+6),color=upper_color,linewidth=5.3)+
  geom_line(data=datTree,aes(x=x1*2.5,y=y1+5.9),color=lower_color,linewidth=2.5)+
  geom_line(data=datTree,aes(x=x*3,y=y+5),color=upper_color,linewidth=5)+
  geom_line(data=datTree,aes(x=x*3,y=y+4.9),color=lower_color,linewidth=5.3)+
  geom_line(data=datTree,aes(x=x1*3,y=y1+5),color=upper_color,linewidth=5.5)+
  geom_line(data=datTree,aes(x=x1*3,y=y1+4.9),color=lower_color,linewidth=3.5)+
  geom_text(aes(x=0,y=4.5,label="HAPPY\nHOLIDAY SEASON",family="Bookman",fontface='bold'),
            size=12,color=rgb(1,0.1,0.1),alpha=1)


card
file_name="friend.pdf"
pdf(file_name,width = 8,height = 10)
print(card)
dev.off()
file_name="friend.png"
png(file_name,width = 800,height = 1000)
print(card)
dev.off()