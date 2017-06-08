cross.plot <- function (data, x, y, cross, x.lab, x.labels, y.lab, y.labels, cross.lab, cross.labels, 
                        theme, legend.pos, pos.dodge, output.path, output.h, output.w) {
  x.labels <- if(is.null(x.labels)) waiver() else x.labels
  y.labels <- if(is.null(y.labels)) waiver() else y.labels
  cross.labels <- if(is.null(cross.labels)) waiver() else cross.labels
  cross.plot <- eval(parse(text=paste("ggplot(data, aes(x=",x,", y=",y,", linetype=",cross,"))",sep="")))
  print(cross.plot + stat_summary(fun.y=mean, geom="point", position=pos.dodge) +
    stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.2, position=pos.dodge) +
    stat_summary(fun.y=mean, geom="line", aes(group=eval(parse(text=paste(cross,sep="")))), position=pos.dodge) +
    labs(x=x.lab, y=y.lab) + 
    scale_x_discrete(labels=x.labels) +
    scale_y_continuous(labels=y.labels) +
    scale_linetype_manual(values=c('solid', 'dashed'), name=cross.lab, labels=cross.labels) +
    theme_bw(base_size=12) + theme +
    theme(legend.position=legend.pos))
  ggsave(output.path, plot=last_plot(), width=output.w, height=output.h)
}
