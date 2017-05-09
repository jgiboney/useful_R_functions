cross.plot <- function (data, x, y, cross, x.lab, y.lab, cross.lab, theme, 
                        legend.pos, pos.dodge, output.path, output.h, output.w) {
  pdf(output.path, height=output.h, width=output.w)
  cross.plot <- eval(parse(text=paste("ggplot(data, aes(x=",x,", y=",y,", linetype=",cross,"))",sep="")))
  cross.plot + stat_summary(fun.y=mean, geom="point", position=pos.dodge) +
    stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.2, position=pos.dodge) +
    stat_summary(fun.y=mean, geom="line", aes(group=eval(parse(text=paste(cross,sep="")))), position=pos.dodge) +
    labs(x=x.lab, y=y.lab) + 
    scale_linetype_manual(values=c('solid', 'dashed'), name="Face") +
    theme_bw(base_size=12) + theme +
    theme(legend.position=legend.pos)
  dev.off()
}
