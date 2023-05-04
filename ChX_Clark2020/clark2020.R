# fig 2 ----
# Plotting
dat<-read_excel("C:/Users/wb383351/WBG/Ilyun Koh - data/clark2020/aba7357_datas1.xlsx",
                        sheet = "Figure2ab")

dat <- dat %>% mutate(Label=ifelse(Label=="BAU non food emissions by 2075",
                                   str_wrap("BAU food emissions, 100% non-food mitigation by 2075",30),Label))
dat <- dat %>% mutate(Label=ifelse(Label=="BAU non food emissions by 2050",
                                   str_wrap("BAU food emissions, 100% non-food mitigation by 2050",30),Label))

ggplot(dat, aes(x = Year, y = cumulative_gwpstar, colour = factor(Label))) +
  geom_line(size=2) + # lines for food system scenario
  # geom_line(dat = ff.annual.2050, aes(x = Year, y = cumgwpstar), colour = 'black', linetype = 2) +
  scale_colour_manual(values = c('#efedf5','#fee6ce','#bcbddc','#fdae6b','#756bb1','#e6550d'))+ 
  scale_x_continuous(limits = c(2010,2130), breaks = c(2020,2040,2060,2080,2100)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,2250)) +
  geom_text(dat = dat[dat$Year == 2100,],aes(x = 2102, y = cumulative_gwpstar, label = Label), hjust = 0, size = 3.5,family = 'Helvetica',colour = 'black') +
  # geom_text(dat = ff.annual.2050[ff.annual.2050$Year == 2100,],aes(x = 2102, y = cumgwpstar, label = 'Non-Food Emissions'), hjust = 0, size = 2.5, colour = 'black',family = 'Helvetica') +
  theme_classic() +
  annotate(geom = 'text',x = 2010, y = 1412,label = '67% Chance\nfor 2C',hjust=0,vjust=0.5, size = 3.5,family = 'Helvetica', alpha = .5) +
  annotate(geom = 'text',x = 2010, y = 508,label = '67% Chance\nfor 1.5C',hjust=0,vjust=.5, size = 3.5,family = 'Helvetica', alpha = .5) +
  annotate(geom = 'text',x = 2010, y = 1822,label = '50% Chance\nfor 2C',hjust=0,vjust=.5, size = 3.5,family = 'Helvetica', alpha = .5) +
  annotate(geom = 'text',x = 2010, y = 713,label = '50% Chance\nfor 1.5C',hjust=0,vjust=.5, size = 3.5,family = 'Helvetica', alpha = .5) +
  theme(legend.position = 'none') +
  geom_hline(yintercept = 0, colour = 'black') +
  geom_segment(aes(x = 2010, xend = 2110, y = 500, yend = 500),colour = 'orangered1',linetype=2)+
  geom_segment(aes(x = 2010, xend = 2110, y = 705, yend = 705),colour = 'orangered1',linetype=2)+
  geom_segment(aes(x = 2010, xend = 2110, y = 1400, yend = 1400),colour = 'orangered1',linetype=2)+
  geom_segment(aes(x = 2010, xend = 2110, y = 1816, yend = 1816),colour = 'orangered1',linetype=2)+
  
  #geom_hline(yintercept = 500, colour = 'orange', linetype = 2) +
  #geom_hline(yintercept = 1405, colour = 'red', linetype = 2) +
  # geom_hline(yintercept = 500 - 61.5, colour = 'orange', linetype = 2, alpha = .5) +
  #geom_hline(yintercept = 705, colour = 'orange', linetype = 2, alpha = .5) +
  # geom_hline(yintercept = 1405 + 152, colour = 'red', linetype = 2, alpha = .5) +
  #geom_hline(yintercept = 1816, colour = 'red', linetype = 2, alpha = .5) +
  labs(y = bquote('Cumulative GHG Emissions(Gt '~CO[2]~ '-w.e.)'),x = '') +
  theme(axis.text = element_text(size = 8,family = 'Helvetica'),
        axis.title = element_text(size = 10,family = 'Helvetica')) +
  annotate(geom = 'text', x = 2017, y = 2000, label = '', size = 3, family = 'Helvetica', fontface = 'bold')

fig2a
write.csv(dat,"C:/Users/wb565654/OneDrive - WBG/project/DARL/data/clark2020/clark_figure.csv")
