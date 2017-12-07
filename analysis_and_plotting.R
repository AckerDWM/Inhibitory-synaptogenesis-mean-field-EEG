library(ggplot2); library(ggthemes); library(matlab); library(reshape2); library(gridExtra)

power = function(x) apply(x, 2, function(xi) log(sum(xi^2)) )

df = param_grid %>% 
  mutate(
    Control = power(sim_result),
    IE = power(sim_result_ie),
    II = power(sim_result_ii)
  ) %>%
  melt(
    id.vars=c("A", "B", "G"), 
    variable.name="Condition", 
    value.name="Power"
  ) %>%
  mutate(A = factor(A))

# themes
theme_set(theme_classic()+theme(text=element_text(family="sans", face=2)))

# representative grid plots
g = ggplot(df, aes(B, G, fill=Power))
g = g + facet_grid(Condition~A, 
                   labeller=labeller(.rows=label_value, .cols=label_both))
g = g + geom_raster()
#g = g + theme_tufte()
g = g + scale_fill_gradientn(colors=jet.colors(255))
g = g + scale_x_continuous(breaks=c(0, 25, 50))
g = g + scale_y_continuous(breaks=c(0, 15, 30))
g = g + labs(fill="Log power")
g_grid = g

# boxplots
g = ggplot(df, aes(Condition, Power))
g = g + facet_grid(~A, labeller=label_both)
g = g + geom_violin(aes(fill=Condition), color=NA)
g = g + geom_boxplot(fill=NA, outlier.shape=NA)
#g = g + theme_tufte()
g = g + ylim(NA, 20)
g = g + theme(axis.text.x=element_text(angle=90, hjust=0.95, vjust=0.5))
g = g + theme(legend.position="none", axis.title.x=element_blank())
g = g + ylab("Log power")
g = g + scale_fill_ptol()
g_boxplots = g

# representative traces
get_trace = function(x) with(param_grid, x[, A==5 & B==35 & G==28])

df_traces = data.frame(
    Time = seq(0, 5, length=1000),
    Control = get_trace(sim_result),
    IE = get_trace(sim_result_ie),
    II = get_trace(sim_result_ii)
  ) %>%
  melt(id.vars="Time")

g = ggplot(df_traces, aes(Time, value, color=variable))
g = g + facet_grid(variable~.)
g = g + geom_line()
#g = g + theme_tufte()
g = g + theme(legend.position="none")
g = g + labs(x="Time [sec]", y=expression("Amplitude ["*mu*"V]"))
g = g + scale_color_ptol()
g_traces = g

# figure
grid.arrange(
  g_grid, g_boxplots, g_traces, 
  layout_matrix=rbind(c(1, 1), c(2, 3)))
