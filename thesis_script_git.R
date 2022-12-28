#Loading Packages and Data####
setwd(path.expand("~"))
lib<-c("lme4", "readr", "tidyr", "effects", "ggplot2","broom","purrr",
       "psych", "MASS", "Rmisc", "plyr", "dplyr", "lmerTest", "ggthemes",
       "lsmeans", "pastecs", "sjstats", "car","irr", "reshape2", "gridExtra",
       "Hmisc", "corrplot", "modelr", "viridis", "randomizr", "apaTables")

lapply(lib,require,character.only=TRUE)

raw_sas <- read.csv('D://Downloads//SAS_data.csv', header=T)

#Cleaning####
colnames(raw_sas)[1] <- c("PID")

sas <- raw_sas %>% #filter(p.age >= 5 & p.age < 11) %>%
                filter((!is.na(in.grp) | !is.na(out.grp)) | #completed at least one measure
                         (rowSums(is.na(raw_sas[,c(15:24)]))< 3) |
                         (rowSums(is.na(raw_sas[,c(9:11)]))< 3)) %>%
                #select(-sdo.mean, -emp.mean, -sdo2, -sdo4, -sdo5, -sdo6, -sdo10) %>% #remove Excel-calculated  columns
                #filter(chk1 == 1 | chk2 == 1) %>%  #passed at least one comp check
        mutate(vid = as.integer(as.character(vid)),
               p.gen = as.integer(as.character(p.gen)),
               p.famchildnum = as.numeric(as.character(p.famchildnum)),
               par.polecon = as.numeric(as.character(par.polecon)),
               p.ses = as.numeric(as.character(p.ses)),
               par.polsocl = as.numeric(as.character(par.polsocl)),
               par1.edu.num = as.numeric(as.character(recode(par1.edu, '"High School Graduate"=1; "High school graduate"=1;
                                     "Partial College"=2; "Partial College (at least one year)"=2;
                                     "College"=3; "College Education" = 3; "Graduate" = 4;
                                     "Graduate Education" = 4'))),
               par2.edu.num = as.numeric(as.character(recode(par2.edu, '"High School Graduate"=1;
                                     "Partial College"=2; "Partial College (at least one year)"=2;
                                     "College"=3; "College Education" = 3;
                                     "Graduate Education" = 4'))),
               p.color.num = as.integer(as.character(recode(p.color, '"G"=1; "O"=0')))) %>%
        mutate(sdo2 = recode(sdo2.r, '1=4; 2=3; 3=2; 4=1'), sdo4 = recode(sdo4.r, '1=4; 2=3; 3=2; 4=1'),
               sdo5 = recode(sdo5.r, '1=4; 2=3; 3=2; 4=1'), sdo6 = recode(sdo6.r, '1=4; 2=3; 3=2; 4=1'),
               sdo10 = recode(sdo10.r,'1=4; 2=3; 3=2; 4=1'))%>%   #reverse score SDO items
        rowwise() %>%
        mutate(in.out.bias = (in.grp - out.grp),  #calculate ingroup/outgroup difference score 
               status = ifelse(low.group == pcp.group, 0, 1)) %>%  #mark participant's assigned status (hi/lo)
        mutate(sdo.mean = mean(c(sdo1, sdo2, sdo3, sdo4,  #rowwise means
                                  sdo5, sdo6, sdo7, sdo8, 
                                  sdo9, sdo10), na.rm=T),
               sdoD.mean = mean(c(sdo1, sdo3, sdo7, sdo8,
                                  sdo9), na.rm=T),
               sdoE.mean = mean(c(sdo2, sdo4,sdo5,
                                  sdo6,sdo10), na.rm=T),
               emp.mean = as.numeric(mean(c(emp1, emp2, emp3), na.rm=T)),
               mean.grp.lik = mean(c(in.grp, out.grp), na.rm=T),
               age.round = trunc(p.age),
               par.edu.num = sum(par1.edu.num, par2.edu.num),
               logsdo.mean = log(sdo.mean),
               logsdoD.mean = log(sdoD.mean),
               logsdoE.mean = log(sdoE.mean),
               memjust.in = ifelse(memjust.num == 2, 1,0),
               memjust.ex = ifelse(memjust.num == 1, 1,0),
               memjust.idk = ifelse(memjust.num == 0, 1,0),
               statjust.in = ifelse(statjust.num == 2, 1,0),
               statjust.ex = ifelse(statjust.num == 1, 1,0),
               statjust.idk = ifelse(statjust.num == 0, 1,0),
               grp.ex.si = ifelse(grp.ex.num == 2, 1,0),
               grp.ex.mer = ifelse(grp.ex.num == 1, 1,0),
               grp.ex.idk = ifelse(grp.ex.num == 0, 1,0)) %>%
        as.data.frame()


#sas <- sas_sdomiss %>% drop_na("sdo1":"sdo10")   #no SDO items missing

sas.compl <- sas %>% filter((!is.na(in.grp) | !is.na(out.grp)) & #completed all measures
                              (rowSums(is.na(sas[,c(15:24)]))< 3) &
                              (rowSums(is.na(sas[,c(9:11)]))< 3))

sas.fac <- sas
sas.fac[,c(2:4)] <- lapply(sas.fac[,c(2:4)], as.factor)
sas.fac[,c(28:36)] <- lapply(sas.fac[,c(28:36)], as.factor)
sas.fac[,c(57)] <- as.factor(as.character(sas.fac[,c(57)]))
sas.fac$status <- ifelse(sas.fac$status==0, "Low", "High")
sas.fac$status <- factor(sas.fac$status, levels=c("Low", "High"))
sas.fac$p.gen <- ifelse(sas.fac$p.gen==0, "M", "F")
sas.fac$p.gen <- factor(sas.fac$p.gen, levels=c("F", "M"))


sas.fac.compl <- sas.compl
sas.fac.compl[,c(2:4)] <- lapply(sas.fac.compl[,c(2:4)], as.factor)
sas.fac.compl[,c(28:36)] <- lapply(sas.fac.compl[,c(28:36)], as.factor)
sas.fac.compl[,c(57)] <- as.factor(as.character(sas.fac.compl[,c(57)]))
sas.fac.compl$status <- ifelse(sas.fac.compl$status==0, "Low", "High")
sas.fac.compl$status <- factor(sas.fac.compl$status, levels=c("Low", "High"))
sas.fac.compl$p.gen <- ifelse(sas.fac.compl$p.gen==0, "M", "F")
sas.fac.compl$p.gen <- factor(sas.fac.compl$p.gen, levels=c("F", "M"))


sas.male <- sas %>% filter(p.gen ==0)
sas.fem <-  sas %>% filter(p.gen ==1)

sas.male.compl <- sas.compl %>% filter(p.gen ==0)
sas.fem.compl <-  sas.compl %>% filter(p.gen ==1)

par.edu <- data.frame(par.edu = (c(sas$par1.edu.num, sas$par2.edu.num)))

#Condition summaries
status.summary <- sas.fac %>% group_by(status) %>%
  summarise(emp.mean2 = mean(emp.mean),
            emp.ci.upper = (as.vector(CI(emp.mean)))[1],
            emp.ci.lower = (as.vector(CI(emp.mean))[3]),
            in.out.bias2 = mean(in.out.bias))

status.summary.compl <- sas.fac.compl %>% group_by(status) %>%
                              summarise(sdo.mean2 = mean(sdo.mean),
                                        sdo.max = max(sdo.mean),
                                        sdo.min = min(sdo.mean),
                                        sdo.ci.upper = (as.vector(CI(sdo.mean))[1]),
                                        sdo.ci.lower = (as.vector(CI(sdo.mean))[3]),
                                        emp.mean2 = mean(emp.mean),
                                        emp.ci.upper = (as.vector(CI(emp.mean))[1]),
                                        emp.ci.lower = (as.vector(CI(emp.mean))[3]),
                                        in.out.bias2 = mean(in.out.bias))
                              

gender.summary <- sas.fac %>% group_by(p.gen) %>%
  summarise(emp.mean2 = mean(emp.mean),
            emp.ci.upper = (as.vector(CI(emp.mean))[1]),
            emp.ci.lower = (as.vector(CI(emp.mean))[3]),
            in.out.bias2 = mean(in.out.bias)) %>%
  filter(!is.na(p.gen))

gender.summary.compl <- sas.fac.compl %>% group_by(p.gen) %>%
  summarise(sdo.mean2 = mean(sdo.mean),
            sdo.max = max(sdo.mean),
            sdo.min = min(sdo.mean),
            sdo.ci.upper = (as.vector(CI(sdo.mean))[1]),
            sdo.ci.lower = (as.vector(CI(sdo.mean))[3]),
            emp.mean2 = mean(emp.mean),
            emp.ci.upper = (as.vector(CI(emp.mean))[1]),
            emp.ci.lower = (as.vector(CI(emp.mean))[3]),
            in.out.bias2 = mean(in.out.bias)) %>%
  filter(!is.na(p.gen))

statgen.summary <- sas.fac %>% group_by(status, p.gen) %>%
  summarise(emp.mean2 = mean(emp.mean),
            emp.ci.upper = (as.vector(CI(emp.mean))[1]),
            emp.ci.lower = (as.vector(CI(emp.mean))[3]),
            in.out.bias2 = mean(in.out.bias)) %>%
  filter(!is.na(p.gen))

statgen.summary.compl <- sas.fac.compl %>% group_by(status, p.gen) %>%
  summarise(sdo.mean2 = mean(sdo.mean),
            sdo.max = max(sdo.mean),
            sdo.min = min(sdo.mean),
            sdo.ci.upper = (as.vector(CI(sdo.mean))[1]),
            sdo.ci.lower = (as.vector(CI(sdo.mean))[3]),
            emp.mean2 = mean(emp.mean),
            emp.ci.upper = (as.vector(CI(emp.mean))[1]),
            emp.ci.lower = (as.vector(CI(emp.mean))[3]),
            in.out.bias2 = mean(in.out.bias)) %>%
  filter(!is.na(p.gen))

#Demographics and Descriptives########
sink("demo.txt", type = c("output", "message"))
table(sas.fac$status)
table(sas.fac$low.group)
table(sas.fac$pcp.group)
table(sas.fac$p.gen)
table(sas.fac$p.gen, sas$age.round)
table(sas.fac$p.race)
table(sas.fac$p.primlang)
table(sas.fac$p.langspok)
describe(sas.fac$p.age)
sd(sas.fac$p.age)
describe(sas.fem$p.age)
sd(sas.fem$p.age, na.rm = T)
describe(sas.male$p.age)
sd(sas.male$p.age, na.rm = T)
describe(sas.fac$p.famchildnum)
describe(sas.fac$par.edu.num)
describe(par.edu$par.edu)
median(par.edu$par.edu,na.rm = T)
describe(sas.fac$p.ses)
sd(sas.fac$p.ses, na.rm = T)
sink(type = c("output", "message"))
sink()
file.show(path.expand("~//demo.txt"))

sink("descriptives.txt", type = c("output", "message"))
describe(sas$in.grp)
sd(sas$in.grp)
describe(sas$out.grp)
sd(sas$out.grp)
describe(sas$in.out.bias)
sd(sas$in.out.bias)
describe(sas$emp.mean)
sd(sas$emp.mean)
describe(sas.compl$sdo.mean)
sd(sas.compl$sdo.mean)
describe(sas.compl$sdoE.mean)
sd(sas.compl$sdoE.mean)
describe(sas.compl$sdoD.mean)
sd(sas.compl$sdoD.mean)
describe(sas.compl$logsdo.mean)
sd(sas.compl$logsdo.mean)
describe(sas.compl$logsdoE.mean)
sd(sas.compl$logsdoE.mean)
describe(sas.compl$logsdoD.mean)
sd(sas.compl$logsdoD.mean)

sink(type = c("output", "message"))
sink()
file.show(path.expand("~//descriptives.txt"))

#Internal Reliability Tests#######
sdos <- sas %>% select(sdo1, sdo2, sdo3, sdo4,       
                       sdo5, sdo6, sdo7, sdo8,
                       sdo9, sdo10)
sdo_d <- sas %>% select(sdo1, sdo3, sdo7, sdo8, sdo9)
sdo_e <- sas %>% select(sdo2, sdo4, sdo5, sdo6, sdo10)
sdo_young <- sas %>% filter(p.age < 8) %>% 
                         select(sdo1, sdo2, sdo3, sdo4, sdo5, sdo6, sdo7, sdo8, sdo10)
sdo_old <- sas %>% filter(p.age >= 8) %>% 
                       select(sdo1, sdo2, sdo3, sdo4, sdo5, sdo6, sdo7, sdo8, sdo10)
emp <- sas %>% select(emp1, emp2,emp3)

sink("alphas_sdo.txt", type = c("output", "message"))
alpha(sdos, check.keys = F)  #scale reliability for sdo
alpha(sdo_d, check.keys = F) #scale reliability for sdo-d
alpha(sdo_e, check.keys = F) #scale reliability for sdo-e
alpha(sdo_young, check.keys = F) #scale reliability for sdo in younger kids
alpha(sdo_old, check.keys = F) #scale reliability for sdo in older kids
alpha(emp, check.keys = F) #scale reliability for sdo in older kids
sink(type = c("output", "message"))
sink()
file.show(path.expand("~//alphas_sdo.txt"))

#T-tests####
sink("ttests.txt", type = c("output", "message"))
#Group names and colors
t.test(sas.compl$sdo.mean[sas.compl$p.color=="G"], sas.compl$sdo.mean[sas.compl$p.color=="O"]) #differences in sdo by group color
t.test(sas.compl$sdo.mean[sas.compl$pcp.group=="G"], sas.compl$sdo.mean[sas.compl$pcp.group=="Z"]) #differences in sdo by group name
t.test(sas$in.out.bias[sas$p.color=="G"], sas$in.out.bias[sas$p.color=="O"]) #differences in bias by group color
t.test(sas$in.out.bias[sas$pcp.group=="G"], sas$in.out.bias[sas$pcp.group=="Z"]) #differences in bias by group name
t.test(sas$emp.mean[sas$p.color=="G"], sas$emp.mean[sas$p.color=="O"]) #differences in empathy by group color
t.test(sas$emp.mean[sas$pcp.group=="G"], sas$emp.mean[sas$pcp.group=="Z"]) #differences in empathy by group name

#Gender
t.test(sas.compl$sdo.mean[sas.compl$p.gen==0], sas.compl$sdo.mean[sas.compl$p.gen==1]) #differences in sdo by gender
t.test(sas$in.out.bias[sas$p.gen==0], sas$in.out.bias[sas$p.gen==1]) #differences in in-group preference by gender
t.test(sas$emp.mean[sas$p.gen==0], sas$emp.mean[sas$p.gen==1]) #differences in empathy by gender
t.test(sas$mean.grp.lik[sas$p.gen==0], sas$mean.grp.lik[sas$p.gen==1]) # mean group liking by gender

#Bias
t.test(sas$in.out.bias, alternative = "greater") #significance of in-group preference
t.test(sas$in.grp, sas$out.grp, alternative = "greater", paired = T) #differences in liking scores for in/out-groups
t.test(sas$in.out.bias[sas$status==1], sas$in.out.bias[sas$status==0]) #differences in bias by condition

#SDO-E, SDO-D
t.test(sas.compl$sdoE.mean, sas.compl$sdoD.mean, paired = T) #differences in liking scores for in/out-groups

sink()
file.show(path.expand("~//ttests.txt"))


#Correlation Matrices########
#Matrices
sas_num <- sas.compl %>% select_if(is.numeric) %>%
  select(-chk1, -chk2, -sdo2.r, -sdo4.r, -sdo5.r, -sdo6.r, -sdo10.r)
apa.cor.table(sas_num, filename = "cortable.apa.doc")
sas_cor <- rcorr(as.matrix(sas_num[, -1]))    #rcorr matrix
sas_cor.r <- as.matrix(round(sas_cor$r, 2))
sas_cor.p <- as.matrix(round(sas_cor$P,3))
melt.sas_cor.r <- melt(replace(sas_cor.r, lower.tri(sas_cor.r, TRUE), NA))
melt.sas_cor.p <- melt(replace(sas_cor.p, lower.tri(sas_cor.p, TRUE), NA))

#ggplot matrices
#reordering
#cor.tbl2 <- sas_cor.r
#cor.tbl2.p <- sas_cor.p

#reorder_cor.tbl2 <- function(cor.tbl2){
#  # Use correlation between variables as distance
#  dd <- as.dist((1-cor.tbl2)/2)
#  hc <- hclust(dd)
#  cor.tbl2 <-cor.tbl2[hc$order, hc$order]
#}
#cor.tbl2 <- reorder_cor.tbl2(cor.tbl2)

#reorder_cor.tbl2.p <- function(cor.tbl2.p){
  # Use correlation between variables as distance
#  dd <- as.dist((1-cor.tbl2.p)/2)
#  hc <- hclust(dd)
#  cor.tbl2.p <-cor.tbl2.p[hc$order, hc$order]
#}
#cor.tbl2.p <- reorder_cor.tbl2.p(cor.tbl2.p)


#r visual
png(filename = "cor.tbl.sas.all.png", width = 1920, height = 1080, units = "px", pointsize = 18, res = 160)
plot.cor.tbl1 <- ggplot(melt.sas_cor.r, aes(Var1, Var2, fill = value, alpha = abs(value)))+
  geom_tile(color = "white")+
  scale_fill_gradientn(colors = c("#c90000","#ffffff" ,"#3c00c9"),
                       na.value = "gray90",
                       values = scales::rescale(c(-0.85,-0.1, 0,0.1, 1)),
                       space = "Lab", name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = -60, vjust = 0.2, 
                                   size = 10, hjust = 0))+
  coord_fixed()+
  guides(alpha = "none")

plot.cor.tbl1 + 
  geom_text(aes(Var1, Var2, label = value,  color = value, alpha = abs(value)),  
            size = 2.5, fontface = 2) +
  scale_color_gradientn(colors = c("black","black", "white", "white"), 
                        values = c(-0.5, 0.76999, 0.77,1)) +
  scale_alpha_continuous(range = c(0.2, 1)) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(0.1, 0),
    legend.position = c(0.6, 0.1),
    legend.direction = "horizontal")+
  guides(alpha = "none",
         color = "none",
         fill = guide_colorbar(barwidth = 5, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
dev.off()
file.show("~//cor.tbl.sas.all.png")

#p visual
png(filename = "cor.tbl.sas.all.p.png", width = 1920, height = 1080, units = "px", pointsize = 18, res = 160)
plot.cor.tbl1.p <- ggplot(melt.sas_cor.p, aes(Var1, Var2, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradientn(colors = c("gray2","gray95","gray95"),
                       na.value = "gray95",
                       values = c(0,0.2,1),
                       space = "Lab", name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = -60, vjust = 0.2, 
                                   size = 10, hjust = 0))+
  coord_fixed()
  
plot.cor.tbl1.p + 
  geom_text(aes(Var1, Var2, label = value,  color = value),  
            size = 2, fontface = 2) +
  scale_color_gradientn(colors = c("gray99", "gray99"), 
                        values = c(0,1)) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(0.1, 0),
    legend.position = c(0.6, 0.1),
    legend.direction = "horizontal")+
  guides(color = "none",
         fill = guide_colorbar(barwidth = 5, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
dev.off()
file.show("~//cor.tbl.sas.all.p.png")


sas_numsparse2 <- sas.compl %>% select_if(is.numeric) %>%
  select(emp.mean, in.out.bias, logsdo.mean, logsdoD.mean, logsdoE.mean, status,
         p.gen, p.age, p.famchildnum, par.polsocl, par.polecon, p.ses, par.edu.num, 
         p.color.num, memjust.ex, memjust.idk, memjust.in, statjust.ex, statjust.idk,
         statjust.in, grp.ex.si, grp.ex.mer, grp.ex.idk)
sas_numsparse <- sas.compl %>% select_if(is.numeric) %>%
  select(emp.mean, in.out.bias, logsdo.mean, logsdoD.mean, logsdoE.mean, status,
         p.gen, p.age, p.famchildnum, par.polsocl, par.polecon, p.ses, par.edu.num, 
         p.color.num)

apa.cor.table(sas_numsparse, table.number = 1, show.conf.interval = F, filename = "cortable.apa.sparse2.doc")
sas_corsparse <- rcorr(as.matrix(sas_numsparse))    #rcorr matrix
sas_corsparse.r <- as.matrix(round(sas_corsparse$r,2))
sas_corsparse.p <- as.matrix(round(sas_corsparse$P, 3))
sas_corsparse.n <- as.matrix(sas_corsparse$n)
melt.sas_corsparse.r <- melt(replace(sas_corsparse.r, lower.tri(sas_corsparse.r, TRUE), NA))
melt.sas_corsparse.p <- melt(replace(sas_corsparse.p, lower.tri(sas_corsparse.p, TRUE), NA))

sas_numtiny <- sas.compl %>% select_if(is.numeric) %>%
  select(emp.mean, in.out.bias, logsdo.mean, logsdoD.mean, logsdoE.mean, status,
         p.gen, p.age, par.polsocl, p.ses, par.edu.num)
#apa.cor.table(sas_numtiny, table.number = 1, show.conf.interval = F, filename = "cortable.apa.sparse3.doc")

#cor.tbl3 <- as.matrix(round((rcorr(as.matrix(sas_numsparse[,-1]))$r), 2))
#cor.tbl3.p <- as.matrix((rcorr(as.matrix(sas_numsparse[,-1]))$P))

#reorder_cor.tbl3 <- function(cor.tbl3){
  # Use correlation between variables as distance
  #dd <- as.dist((1-cor.tbl3)/2)
  #hc <- hclust(dd)
  #cor.tbl3 <-cor.tbl3[hc$order, hc$order]
#}
#cor.tbl3 <- reorder_cor.tbl3(cor.tbl3)
#cor.tbl3.p <- reorder_cor.tbl3(cor.tbl3.p)


#r visual (sparse)
png(filename = "cor.tbl.sas.sparse.png", width = 1920, height = 1080, units = "px", pointsize = 18, res = 160)
plot.cor.tbl.sparse <- ggplot(melt.sas_corsparse.r, aes(Var1, Var2, fill = value, alpha = abs(value)))+
  geom_tile(color = "white")+
  scale_fill_gradientn(colors = c("#c90000","#ffffff" ,"#3c00c9"),
                       na.value = "gray90",
                       values = scales::rescale(c(-0.85,-0.1, 0,0.1, 1)),
                       space = "Lab", name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = -60, vjust = 0.2, 
                                   size = 10, hjust = 0))+
  coord_fixed()+
  guides(alpha = "none")
plot.cor.tbl.sparse + 
  geom_text(aes(Var1, Var2, label = value,  color = value, alpha = abs(value)),  
            size = 2, fontface = 2) +
  scale_color_gradientn(colors = c("black","black", "white", "white"), 
                        values = c(-0.5, 0.76999, 0.77,1)) +
  scale_alpha_continuous(range = c(0.2, 1)) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(0.1, 0),
    legend.position = c(0.6, 0.1),
    legend.direction = "horizontal")+
  guides(alpha = "none",
         color = "none",
         fill = guide_colorbar(barwidth = 5, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
dev.off()
file.show("~//cor.tbl.sas.sparse.png")

#p visual (sparse)
png(filename = "cor.tbl.sas.sparse.p.png", width = 1920, height = 1080, units = "px", pointsize = 18, res = 160)
plot.cor.tbl.sparse.p <- ggplot(melt.sas_corsparse.p, aes(Var1, Var2, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradientn(colors = c("gray2","gray95","gray95"),
                       na.value = "gray95",
                       values = c(0,0.2,1),
                       space = "Lab", name="Pearson\nCorrelation\nP-Value") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = -60, vjust = 0.2, 
                                   size = 10, hjust = 0))+
  coord_fixed()

plot.cor.tbl.sparse.p + 
  geom_text(aes(Var1, Var2, label = value,  color = value),  
            size = 2, fontface = 2) +
  scale_color_gradientn(colors = c("gray99", "gray99"), 
                        values = c(0,1)) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(0.1, 0),
    legend.position = c(0.6, 0.1),
    legend.direction = "horizontal")+
  guides(color = "none",
         fill = guide_colorbar(barwidth = 5, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
dev.off()
file.show("~//cor.tbl.sas.sparse.p.png")

#Linear Models####
#Simple Regressions
#"lm.y.x <-"#
#Demographics
#Age
lm.logsdo.age <- summary(lm(logsdo.mean ~ p.age, data = sas.compl))  #logsdo on age
lm.bias.age <- summary(lm(in.out.bias ~ p.age, data = sas))  #in-group preference on age
lm.empathy.age <- summary(lm(emp.mean ~ p.age, data = sas))  #empathy on age
lm.grplik.age <- summary(lm(mean.grp.lik ~ p.age, data = sas))  #overall group liking on age

lm.logsdoE.age <- summary(lm(logsdoE.mean ~ p.age, data = sas.compl))  #logsdo on age
lm.logsdoD.age <- summary(lm(logsdoD.mean ~ p.age, data = sas.compl))  #logsdo on age

#Gender
lm.logsdo.gender <- summary(lm(logsdo.mean ~ p.gen, data = sas.compl))  #logsdo on gender
lm.logsdo.gender.color <- summary(lm(logsdo.mean ~ p.gen+p.color.num, data = sas.compl))  #logsdo on gender (color as control)
lm.bias.gender <- summary(lm(in.out.bias ~ p.gen, data = sas))  #in-group preference on gender
lm.empathy.gender <- summary(lm(emp.mean ~ p.gen, data = sas))  #empathy on gender
lm.grplik.gender <- summary(lm(mean.grp.lik ~ p.gen, data = sas))  #overall group liking on gender
lm.logsdoE.gender <- summary(lm(logsdoE.mean ~ p.gen, data = sas.compl))  #logsdo on gender
lm.logsdoE.gender.color <- summary(lm(logsdoE.mean ~ p.gen+p.color.num, data = sas.compl))  #logsdo on gender (color as control)
lm.logsdoD.gender <- summary(lm(logsdoD.mean ~ p.gen, data = sas.compl))  #logsdo on gender
lm.logsdoD.gender.color <- summary(lm(logsdoD.mean ~ p.gen+p.color.num, data = sas.compl))  #logsdo on gender (color as control)


#SES, Ed, & Political Views
lm.logsdo.ses <- summary(lm(logsdo.mean ~ p.ses, data = sas.compl))  #logsdo on SES
lm.logsdoE.ses <- summary(lm(logsdoE.mean ~ p.ses, data = sas.compl))  #logsdoE on SES
lm.logsdoD.ses <- summary(lm(logsdoD.mean ~ p.ses, data = sas.compl))  #logsdoD on SES

lm.bias.ses <- summary(lm(in.out.bias ~ p.ses, data = sas))  #in-group preference on SES
lm.empathy.ses <- summary(lm(emp.mean ~ p.ses, data = sas))  #empathy on SES
lm.grplik.ses <- summary(lm(mean.grp.lik ~ p.ses, data = sas))  #overall group liking on SES

lm.logsdo.edu <- summary(lm(logsdo.mean ~ par.edu.num, data = sas.compl))  #overall group liking on SES
lm.logsdoE.edu <- summary(lm(logsdoE.mean ~ par.edu.num, data = sas.compl))  #overall group liking on SES
lm.logsdoD.edu <- summary(lm(logsdoD.mean ~ par.edu.num, data = sas.compl))  #overall group liking on SES

lm.logsdo.parpolsocl <- summary(lm(logsdo.mean ~ par.polsocl, data = sas.compl))  #logsdo on SES
lm.logsdoE.parpolsocl <- summary(lm(logsdoE.mean ~ par.polsocl, data = sas.compl))  #logsdo on SES
lm.logsdoD.parpolsocl <- summary(lm(logsdoD.mean ~ par.polsocl, data = sas.compl))  #logsdo on SES

lm.logsdo.parpolecon <- summary(lm(logsdo.mean ~ par.polecon, data = sas.compl))  #logsdo on SES
lm.logsdoE.parpolecon <- summary(lm(logsdoE.mean ~ par.polecon, data = sas.compl))  #logsdo on SES
lm.logsdoD.parpolecon <- summary(lm(logsdoD.mean ~ par.polecon, data = sas.compl))  #logsdo on SES

lm.bias.parpolsocl <- summary(lm(in.out.bias ~ par.polsocl, data = sas.compl))  #logsdo on SES
lm.emp.parpolsocl <- summary(lm(emp.mean ~ par.polsocl, data = sas.compl))  #logsdo on SES

lm.emp.ses <- summary(lm(emp.mean ~ p.ses, data = sas))
lm.bias.ses <- summary(lm(in.out.bias ~ p.ses, data = sas))

#Main DVs
lm.logsdo.empathy <- summary(lm(logsdo.mean ~ emp.mean, data = sas.compl))  #logsdo on empathy
lm.empathy.logsdo <- summary(lm(emp.mean ~ logsdo.mean, data = sas.compl)) #empathy on logsdo

lm.logsdo.bias <- summary(lm(logsdo.mean ~ in.out.bias, data = sas.compl))  #logsdo on bias
lm.bias.logsdo <- summary(lm(in.out.bias ~ logsdo.mean, data = sas.compl)) #bias on logsdo

lm.bias.empathy <- summary(lm(in.out.bias ~ emp.mean, data = sas))  #in-group preference on empathy
lm.empathy.bias <- summary(lm(emp.mean ~ in.out.bias, data = sas)) #empathy on in-group preference

lm.bias.logsdoE <- summary(lm(in.out.bias ~ logsdoE.mean, data = sas.compl))  #logsdoE on bias
lm.bias.logsdoD <- summary(lm(in.out.bias ~ logsdoD.mean, data = sas.compl))  #logsdoD on bias

lm.empathy.logsdoE <- summary(lm(emp.mean ~ logsdoE.mean, data = sas.compl))  #logsdoE on bias
lm.empathy.logsdoD <- summary(lm(emp.mean ~ logsdoD.mean, data = sas.compl))  #logsdoD on bias

#Experimental Effects
lm.logsdo.status <- summary(lm(logsdo.mean ~ status, data = sas.compl))  #logsdo on group status assignment
lm.bias.status <- summary(lm(in.out.bias ~ status, data = sas))  #bias on group status assignment 
lm.empathy.status <- summary(lm(emp.mean ~ status, data = sas))  #empathy on group status assignment

lm.logsdoD.status <- summary(lm(logsdoD.mean ~ status, data = sas.compl))  #logsdo on group status assignment
lm.logsdoE.status <- summary(lm(logsdoE.mean ~ status, data = sas.compl))  #logsdo on group status assignment

lm.logsdo.status.color <- summary(lm(logsdo.mean ~ status+p.color.num, data = sas.compl))  #logsdo on group status assignment
lm.bias.status.color <- summary(lm(in.out.bias ~ status+p.color.num, data = sas))  #bias on group status assignment 
lm.empathy.status.color <- summary(lm(emp.mean ~ status+p.color.num, data = sas))  #empathy on group status assignment

lm.logsdoD.status.color <- summary(lm(logsdoD.mean ~ status+p.color.num, data = sas.compl))  #logsdo on group status assignment
lm.logsdoE.status.color <- summary(lm(logsdoE.mean ~ status+p.color.num, data = sas.compl))  #logsdo on group status assignment

#Open-ended Responses
#Predicting SDO
lm.logsdoD.statjust.in <- summary(lm(logsdoD.mean ~ statjust.in, data = sas.compl))  #logsdoD on group status assignment
lm.logsdoE.statjust.in <- summary(lm(logsdoE.mean ~ statjust.in, data = sas.compl))  #logsdoE on group status assignment
lm.logsdo.statjust.in <- summary(lm(logsdo.mean ~ statjust.in, data = sas.compl))  #logsdo on group status assignment
lm.logsdo.statjust.idk <- summary(lm(logsdo.mean ~ statjust.idk, data = sas.compl))  #logsdo on group status assignment

lm.logsdoD.statjust.ex <- summary(lm(logsdoD.mean ~ statjust.ex, data = sas.compl))  #logsdoD on group status assignment
lm.logsdoE.statjust.ex <- summary(lm(logsdoE.mean ~ statjust.ex, data = sas.compl))  #logsdoE on group status assignment
lm.logsdo.statjust.ex <- summary(lm(logsdo.mean ~ statjust.ex, data = sas.compl))  #logsdo on group status assignment

lm.logsdoD.memjust.in <- summary(lm(logsdoD.mean ~ memjust.in, data = sas.compl))  #logsdoD on group status assignment
lm.logsdoE.memjust.in <- summary(lm(logsdoE.mean ~ memjust.in, data = sas.compl))  #logsdoE on group status assignment
lm.logsdo.memjust.in <- summary(lm(logsdo.mean ~ memjust.in, data = sas.compl))  #logsdo on group status assignment
lm.logsdo.memjust.idk <- summary(lm(logsdo.mean ~ memjust.idk, data = sas.compl))  #logsdo on group status assignment

lm.logsdoD.grp.ex.si <- summary(lm(logsdoD.mean ~ grp.ex.si, data = sas.compl))  #logsdoD on group status assignment
lm.logsdoE.grp.ex.si <- summary(lm(logsdoE.mean ~ grp.ex.si, data = sas.compl))  #logsdoE on group status assignment
lm.logsdo.grp.ex.si <- summary(lm(logsdo.mean ~ grp.ex.si, data = sas.compl))  #logsdo on group status assignment
lm.logsdo.grp.ex.idk <- summary(lm(logsdo.mean ~ grp.ex.idk, data = sas.compl))  #logsdo on group status assignment

lm.logsdoD.grp.ex.mer <- summary(lm(logsdoD.mean ~ grp.ex.mer, data = sas.compl))  #logsdoD on group status assignment
lm.logsdoE.grp.ex.mer <- summary(lm(logsdoE.mean ~ grp.ex.mer, data = sas.compl))  #logsdoE on group status assignment
lm.logsdo.grp.ex.mer <- summary(lm(logsdo.mean ~ grp.ex.mer, data = sas.compl))  #logsdo on group status assignment

lm.memjust.in.status <- summary(lm(memjust.in ~ status, data = sas.compl))  #logsdo on group status assignment
lm.memjust.ex.status <- summary(lm(memjust.ex ~ status, data = sas.compl))  #logsdo on group status assignment
lm.memjust.idk.status <- summary(lm(memjust.idk ~ status, data = sas.compl))  #logsdo on group status assignment

lm.statjust.in.status <- summary(lm(statjust.in ~ status, data = sas.compl))  #logsdo on group status assignment
lm.statjust.ex.status <- summary(lm(statjust.ex ~ status, data = sas.compl))  #logsdo on group status assignment
lm.statjust.idk.status <- summary(lm(statjust.idk ~ status, data = sas.compl))  #logsdo on group status assignment

lm.grp.ex.mer.status <- summary(lm(grp.ex.mer ~ status, data = sas.compl))  #logsdo on group status assignment
lm.grp.ex.si.status <- summary(lm(grp.ex.si ~ status, data = sas.compl))  #logsdo on group status assignment
lm.grp.ex.idk.status <- summary(lm(grp.ex.idk ~ status, data = sas.compl))  #logsdo on group status assignment

lm.memjust.in.gender.age <- summary(lm(memjust.in ~ p.gen+p.age, data = sas.compl))  #logsdo on group status assignment
lm.memjust.ex.gender.age <- summary(lm(memjust.ex ~ p.gen+p.age, data = sas.compl))  #logsdo on group status assignment
lm.memjust.idk.gender.age <- summary(lm(memjust.idk ~ p.gen+p.age, data = sas.compl))  #logsdo on group status assignment

lm.statjust.in.gender.age <- summary(lm(statjust.in ~ p.gen+p.age, data = sas.compl))  #logsdo on group status assignment
lm.statjust.ex.gender.age <- summary(lm(statjust.ex ~ p.gen+p.age, data = sas.compl))  #logsdo on group status assignment
lm.statjust.idk.gender.age <- summary(lm(statjust.idk ~ p.gen+p.age, data = sas.compl))  #logsdo on group status assignment

lm.grp.ex.mer.gender.age <- summary(lm(grp.ex.mer ~ p.gen+p.age, data = sas.compl))  #logsdo on group status assignment
lm.grp.ex.si.gender.age <- summary(lm(grp.ex.si ~ p.gen+p.age, data = sas.compl))  #logsdo on group status assignment
lm.grp.ex.idk.gender.age <- summary(lm(grp.ex.idk ~ p.gen+p.age, data = sas.compl))  #logsdo on group status assignment

#Multivariate Models
#"lm.y.x1.x2"
#Effects of condition and gender
lm.logsdo.status.gender <- summary(lm(logsdo.mean ~ status*p.gen, data = sas.compl))  #logsdo on group status assignment and gender
lm.logsdo.status.gender2 <- summary(lm(logsdo.mean ~ status+p.gen, data = sas.compl))  #logsdo on group status assignment and gender

lm.logsdo.status.gender.color <- summary(lm(logsdo.mean ~ status*p.gen+p.color.num, data = sas.compl))  #logsdo on group status assignment and gender
lm.logsdo.status.gender.color2 <- summary(lm(logsdo.mean ~ status+p.gen+p.color.num, data = sas.compl))  #logsdo on group status assignment and gender
lm.logsdo.status.gender.color.age <- summary(lm(logsdo.mean ~ status+p.gen+p.color.num+p.age, data = sas.compl))  #logsdo on group status assignment and gender


lm.logsdoE.status.gender <- summary(lm(logsdoE.mean ~ status*p.gen, data = sas.compl))  #logsdoE on group status assignment and gender
lm.logsdoE.status.gender2 <- summary(lm(logsdoE.mean ~ status+p.gen, data = sas.compl))  #logsdoE on group status assignment and gender
lm.logsdoE.status.gender.color <- summary(lm(logsdoE.mean ~ status*p.gen+p.color.num, data = sas.compl))  #logsdo on group status assignment and gender
lm.logsdoE.status.gender.color2 <- summary(lm(logsdoE.mean ~ status+p.gen+p.color.num, data = sas.compl))  #logsdo on group status assignment and gender
lm.logsdoE.status.gender.color.age <- summary(lm(logsdoE.mean ~ status+p.gen+p.color.num+p.age, data = sas.compl))  #logsdo on group status assignment and gender

lm.logsdoD.status.gender <- summary(lm(logsdoD.mean ~ status*p.gen, data = sas.compl))  #logsdoD on group status assignment and gender
lm.logsdoD.status.gender2 <- summary(lm(logsdoD.mean ~ status+p.gen, data = sas.compl))  #logsdoD on group status assignment and gender
lm.logsdoD.status.gender.color <- summary(lm(logsdoD.mean ~ status*p.gen+p.color.num, data = sas.compl))  #logsdo on group status assignment and gender
lm.logsdoD.status.gender.color2 <- summary(lm(logsdoD.mean ~ status+p.gen+p.color.num, data = sas.compl))  #logsdo on group status assignment and gender
lm.logsdoD.status.gender.color.age <- summary(lm(logsdoD.mean ~ status+p.gen+p.color.num+p.age, data = sas.compl))  #logsdo on group status assignment and gender

lm.logsdo.status.fem <-  summary(lm(logsdo.mean~status, data = sas.fem.compl))
lm.logsdo.status.male <-  summary(lm(logsdo.mean~status, data = sas.male.compl))

lm.emp.status.gender <- summary(lm(emp.mean ~ status*p.gen, data = sas))  #empathy on group status assignment and gender

#(also age)
lm.logsdo.age.gender <- summary(lm(logsdo.mean ~ p.gen+p.age, data = sas.compl))  #logsdo on group status assignment and gender
lm.logsdoE.age.gender <- summary(lm(logsdoE.mean ~ p.gen+p.age, data = sas.compl))  #logsdo on group status assignment and gender
lm.logsdoD.age.gender <- summary(lm(logsdoD.mean ~ p.gen+p.age, data = sas.compl))  #logsdo on group status assignment and gender
lm.logsdo.age.gender.color <- summary(lm(logsdo.mean ~ p.gen+p.age+p.color.num, data = sas.compl))  #logsdo on group status assignment and gender
lm.logsdoE.age.gender.color <- summary(lm(logsdoE.mean ~ p.gen+p.age+p.color.num, data = sas.compl))  #logsdo on group status assignment and gender
lm.logsdoD.age.gender.color <- summary(lm(logsdoD.mean ~ p.gen+p.age+p.color.num, data = sas.compl))  #logsdo on group status assignment and gender


lm.emp.status.gender.age1 <- summary(lm(emp.mean ~ status*p.gen*p.age, data = sas))  #empathy on group status assignment and gender
lm.emp.status.gender.age2 <- summary(lm(emp.mean ~ status+p.gen+p.age, data = sas))  #empathy on group status assignment and gender
lm.emp.status.age.male <- summary(lm(emp.mean ~ status*p.age, data = sas.male))  #empathy on group status assignment (male only))
lm.emp.status.age.fem <- summary(lm(emp.mean ~ status*p.age, data = sas.fem))  #empathy on group status assignment (female only))

lm.emp.age.gender <- summary(lm(emp.mean ~ p.age*p.gen, data = sas))  #empathy on age and gender

#Effects of parent political beliefs and SES
lm.logsdo.status.parpolsocl <- summary(lm(logsdo.mean ~ status*par.polsocl, data = sas.compl))  #logsdo on group status assignment and parents' political beliefs
lm.logsdoE.status.parpolsocl <- summary(lm(logsdoE.mean ~ status*par.polsocl, data = sas.compl))  #logsdo D on group status assignment and parents' political beliefs
lm.emp.status.parpolsocl <- summary(lm(emp.mean ~ status*par.polsocl, data = sas))  #empathy on group status assignment and parents' political beliefs

lm.logsdoE.status.logparpolsocl <- summary(lm(logsdoE.mean ~ status*log(par.polsocl), data = sas.compl))  #logsdo D on group status assignment and parents' political beliefs

lm.logsdo.status.ses <- summary(lm(emp.mean ~ status*p.ses, data = sas.compl))  #logsdo on group status assignment and family SES
lm.logsdoE.status.ses <- summary(lm(logsdoE.mean ~ status*p.ses, data = sas.compl))  #logsdo D on group status assignment and family SES
lm.emp.status.ses <- summary(lm(emp.mean ~ status*p.ses, data = sas))  #empathy on group status assignment and family SES

lm.logsdo.parpolsocl <- summary(lm(logsdo.mean ~ par.polsocl, data = sas.compl))
lm.logsdo.ses.parpolsocl <- summary(lm(logsdo.mean ~ p.ses*par.polsocl, data = sas.compl))
lm.logsdo.ses.parpolsocl2 <- summary(lm(logsdo.mean ~ p.ses+par.polsocl, data = sas.compl))
lm.logsdo.ses.par.edu <- summary(lm(logsdo.mean ~ p.ses+par.edu.num, data = sas.compl))
lm.logsdo.ses.parpolsocl.par.edu <- summary(lm(logsdo.mean ~ p.ses+par.polsocl+par.edu.num, data = sas.compl))

lm.logsdoE.parpolsocl <- summary(lm(logsdoE.mean ~ par.polsocl, data = sas.compl))
lm.logsdoE.ses.parpolsocl <- summary(lm(logsdoE.mean ~ p.ses*par.polsocl, data = sas.compl))
lm.logsdoE.ses.parpolsocl2 <- summary(lm(logsdoE.mean ~ p.ses+par.polsocl, data = sas.compl))
lm.logsdoE.ses.par.edu <- summary(lm(logsdoE.mean ~ p.ses+par.edu.num, data = sas.compl))
lm.logsdoE.ses.parpolsocl.par.edu <- summary(lm(logsdoE.mean ~ p.ses+par.polsocl+par.edu.num, data = sas.compl))

#Best R^2 for SDO
lm.logsdo.total <- glance(lm(logsdo.mean ~ in.out.bias + p.age + p.ses, data = sas.compl)) #starter based on corr matrix
lm.logsdo.total2 <- glance(lm(logsdo.mean ~ in.out.bias + emp.mean + p.age + p.ses, data = sas.compl)) #not great
lm.logsdo.total3 <- glance(lm(logsdo.mean ~ in.out.bias + p.age + p.ses + par.polsocl, data = sas.compl)) #best so far
lm.logsdo.total4 <- glance(lm(logsdo.mean ~ in.out.bias + p.age + p.ses + par.polecon, data = sas.compl)) #almost as good
lm.logsdo.total5 <- glance(lm(logsdo.mean ~ in.out.bias + p.age + p.ses + par.polecon, data = sas.compl)) #worse
lm.logsdo.total6 <- glance(lm(logsdo.mean ~ in.out.bias + p.age + p.ses + par.polsocl + status, data = sas.compl)) #worse
lm.logsdo.total7 <- glance(lm(logsdo.mean ~ in.out.bias + p.age + p.ses + par.polsocl + status + emp.mean, data = sas.compl)) #worse
lm.logsdo.total8 <- glance(lm(logsdo.mean ~ in.out.bias + p.age + p.ses + log(par.polsocl), data = sas.compl)) #still no better

sdo.mod <- rbind(lm.logsdo.total, lm.logsdo.total2,
                 lm.logsdo.total3, lm.logsdo.total4, lm.logsdo.total5, lm.logsdo.total6,
                 lm.logsdo.total7, lm.logsdo.total8)
summary(lm.logsdo.total8)

lm.logsdoE.total <- lm(logsdoE.mean ~ in.out.bias + p.age + p.ses, data = sas.compl) #starter based on corr matrix
lm.logsdoE.total2 <- lm(logsdoE.mean ~ in.out.bias + emp.mean + p.age + p.ses, data = sas.compl) #not great
lm.logsdoE.total3 <- lm(logsdoE.mean ~ in.out.bias + p.age + p.ses + par.polsocl, data = sas.compl) #best so far
lm.logsdoE.total4 <- lm(logsdoE.mean ~ in.out.bias + p.age + p.ses + par.polecon, data = sas.compl) #almost as good
lm.logsdoE.total5 <- lm(logsdoE.mean ~ in.out.bias + p.age + p.ses + par.polecon, data = sas.compl) #worse
lm.logsdoE.total6 <- lm(logsdoE.mean ~ in.out.bias + p.age + p.ses + par.polsocl + status, data = sas.compl) #worse
lm.logsdoE.total7 <- lm(logsdoE.mean ~ in.out.bias + p.age + p.ses + par.polsocl + status + emp.mean, data = sas.compl) #worse
lm.logsdoE.total8 <- lm(logsdoE.mean ~ in.out.bias + p.age + p.ses + log(par.polsocl), data = sas.compl) #still no better

summary(lm.logsdoE.total.29)

lm.logsdo.total3 <- lm(logsdo.mean ~ in.out.bias + p.age + p.ses + par.polsocl, data = sas.compl) #only SES significant
lm.logsdoE.total3 <- lm(logsdoE.mean ~ in.out.bias + p.age + p.ses + par.polsocl, data = sas.compl) #only SES significant

lm.logsdoE.total.29 <- lm(logsdoE.mean ~ p.age + p.ses + p.color, data = sas.compl) #starter based on corr matrix


sink("lin_models.txt", type = c("output", "message"))
#Simple Regressions
#"lm.y.x <-"#
#Demographics
#Age

lm.logsdo.age #logsdo on age
lm.bias.age #bias on age
lm.empathy.age #empathy on age
lm.grplik.age #group liking on age
lm.logsdoE.age #logsdo-E on age
lm.logsdoD.age #logsdo-D on age

#Gender
lm.logsdo.gender #logsdo on gender
lm.logsdo.gender.color #logsdo on gender (color as control)
lm.logsdoE.gender #logsdo on gender
lm.logsdoE.gender.color #logsdo on gender (color as control)
lm.logsdoD.gender #logsdo on gender
lm.logsdoD.gender.color #logsdo on gender (color as control)
lm.bias.gender #bias on gender
lm.empathy.gender #empathy on gender
lm.grplik.gender #group liking on gender
lm.logsdo.age.gender #SDO on age and gender
lm.logsdoE.age.gender #SDO on age and gender
lm.logsdoD.age.gender #SDO on age and gender
lm.logsdo.age.gender.color #SDO on age and gender
lm.logsdoE.age.gender.color #SDO on age and gender
lm.logsdoD.age.gender.color #SDO on age and gender

#SES
lm.logsdo.ses
lm.bias.ses
lm.empathy.ses
lm.grplik.ses

#Main DVs
lm.logsdo.empathy #logsdo on empathy
lm.empathy.logsdo #empathy on logsdo
lm.logsdo.bias #logsdo on bias
lm.bias.logsdo #bias on logsdo
lm.bias.empathy #bias on empathy
lm.empathy.bias #empathy on bias
lm.bias.logsdoE #bias on logsdoE
lm.bias.logsdoD #bias on logsdod
lm.empathy.logsdoE #bias on logsdoE
lm.empathy.logsdoD #bias on logsdod

#Experimental Effects
lm.logsdo.status #logsdo on status condition
lm.bias.status #bias on status condition
lm.empathy.status #empathy on status condition
lm.logsdoD.status #logsdo D on status condition
lm.logsdoE.status #logsdo E on status condition

lm.logsdo.status.color #logsdo on status condition controlling for color
lm.bias.status.color #bias on status condition controlling for color
lm.empathy.status.color #empathy on status condition controlling for color
lm.logsdoD.status.color #logsdo D on status condition controlling for color
lm.logsdoE.status.color #logsdo E on status condition controlling for color

#Multivariate Regressions
lm.logsdo.status.gender #logsdo on status and gender (interaction
lm.logsdo.status.gender2 #logsdo on status and gender (main effects
lm.logsdo.status.gender.color#logsdo on status and gender (interaction) controlling for color
lm.logsdo.status.gender.color2 #logsdo on status and gender (main effects) controlling for color
lm.logsdo.status.fem #logsdo on status (girls only
lm.logsdo.status.male #logsdo on status (boys only
lm.logsdoE.status.gender #logsdoE on status and gender (interaction
lm.logsdoE.status.gender2
lm.logsdoE.status.gender.color#logsdo on status and gender (interaction) controlling for color
lm.logsdoE.status.gender.color2 #logsdo on status and gender (main effects) controlling for color
lm.logsdoD.status.gender #logsdoE on status and gender (interaction
lm.logsdoD.status.gender2
lm.logsdoD.status.gender.color#logsdo on status and gender (interaction) controlling for color
lm.logsdoD.status.gender.color2 #logsdo on status and gender (main effects) controlling for color
lm.emp.status.gender #empathy on status and gender
lm.emp.status.gender.age1  #empathy on status, gender, and age (interaction
lm.emp.status.gender.age2 #empathy on status, gender, and age (main effects
lm.emp.status.parpolsocl #empathy on status and parent social political views
lm.logsdo.status.parpolsocl #logsdo on status and parent social political views
lm.logsdoE.status.parpolsocl #logsdo D on status and parent social political views
lm.emp.status.ses #empathy on status and family SES
lm.logsdo.status.ses #logsdo on status and family SES
lm.logsdoE.status.ses #logsdo D on status and family SES
lm.logsdo.ses.parpolsocl #logsdo on SES and political views (interaction)
lm.logsdo.ses.parpolsocl2 #logsdo on SES and political views (controlling)

#Demographics
#Age
glance.df <- rbind.data.frame(
glance(lm.logsdo.age), #logsdo on age
glance(lm.bias.age), #bias on age
glance(lm.empathy.age), #empathy on age
glance(lm.grplik.age), #group liking on age
glance(lm.logsdoE.age), #logsdo-D on age
#Gender
glance(lm.logsdo.gender), #logsdo on gender
glance(lm.bias.gender), #bias on gender
glance(lm.empathy.gender), #empathy on gender
glance(lm.grplik.gender), #group liking on gender
glance(lm.logsdo.age.gender), #SDO on age and gender
glance(lm.logsdoE.age.gender), #SDO on age and gender
glance(lm.logsdoD.age.gender), #SDO on age and gender

#SES
glance(lm.logsdo.ses),
glance(lm.bias.ses),
glance(lm.empathy.ses),
glance(lm.grplik.ses),

#Main DVs
glance(lm.logsdo.empathy), #logsdo on empathy
glance(lm.empathy.logsdo), #empathy on logsdo
glance(lm.logsdo.bias), #logsdo on bias
glance(lm.bias.logsdo), #bias on logsdo
glance(lm.bias.empathy), #bias on empathy
glance(lm.empathy.bias), #empathy on bias
glance(lm.bias.logsdoE), #bias on logsdoE
#Experimental Effects
glance(lm.logsdo.status), #logsdo on status condition
glance(lm.bias.status), #bias on status condition
glance(lm.empathy.status), #empathy on status condition
glance(lm.logsdoD.status), #logsdo D on status condition
glance(lm.logsdoE.status), #logsdo E on status condition

#Multivariate Regressions
glance(lm.logsdo.status.gender), #logsdo on status and gender (interaction),
glance(lm.logsdo.status.gender2), #logsdo on status and gender (main effects),
glance(lm.logsdo.status.fem), #logsdo on status (girls only),
glance(lm.logsdo.status.male), #logsdo on status (boys only),
glance(lm.emp.status.gender), #empathy on status and gender
glance(lm.emp.status.gender.age1),  #empathy on status, gender, and age (interaction),
glance(lm.emp.status.gender.age2), #empathy on status, gender, and age (main effects),
glance(lm.emp.status.parpolsocl), #empathy on status and parent social political views
glance(lm.logsdo.status.parpolsocl), #logsdo on status and parent social political views
glance(lm.logsdoE.status.parpolsocl), #logsdo D on status and parent social political views
glance(lm.emp.status.ses), #empathy on status and family SES
glance(lm.logsdo.status.ses), #logsdo on status and family SES
glance(lm.logsdoE.status.ses)) #logsdo D on status and family SES
glance.df

models <- rbind(tidy(lm.logsdo.age), #logsdo on age
tidy(lm.bias.age), #bias on age
tidy(lm.empathy.age), #empathy on age
tidy(lm.grplik.age), #group liking on age
tidy(lm.logsdoE.age), #logsdo-D on age
#Gender
tidy(lm.logsdo.gender), #logsdo on gender
tidy(lm.bias.gender), #bias on gender
tidy(lm.empathy.gender), #empathy on gender
tidy(lm.grplik.gender), #group liking on gender
#Main DVs
tidy(lm.logsdo.empathy), #logsdo on empathy
tidy(lm.empathy.logsdo), #empathy on logsdo
tidy(lm.logsdo.bias), #logsdo on bias
tidy(lm.bias.logsdo), #bias on logsdo
tidy(lm.bias.empathy), #bias on empathy
tidy(lm.empathy.bias), #empathy on bias
tidy(lm.bias.logsdoE), #bias on logsdoE
#Experimental Effects
tidy(lm.logsdo.status), #logsdo on status condition
tidy(lm.bias.status), #bias on status condition
tidy(lm.empathy.status), #empathy on status condition
tidy(lm.logsdoD.status), #logsdo D on status condition
tidy(lm.logsdoE.status),#logsdo E on status condition

#Multivariate Regressions
tidy(lm.logsdo.status.gender), #logsdo on status and gender (interaction)
tidy(lm.logsdo.status.gender2), #logsdo on status and gender (main effects)
tidy(lm.logsdo.status.fem), #logsdo on status (girls only)
tidy(lm.logsdo.status.male), #logsdo on status (boys only)
tidy(lm.emp.status.gender), #empathy on status and gender
tidy(lm.emp.status.gender.age1),  #empathy on status, gender, and age (interaction)
tidy(lm.emp.status.gender.age2), #empathy on status, gender, and age (main effects)
tidy(lm.emp.status.parpolsocl), #empathy on status and parent social political views
tidy(lm.logsdo.status.parpolsocl), #logsdo on status and parent social political views
tidy(lm.logsdoE.status.parpolsocl), #logsdo D on status and parent social political views
tidy(lm.emp.status.ses), #empathy on status and family SES
tidy(lm.logsdo.status.ses), #logsdo on status and family SES
tidy(lm.logsdoE.status.ses)) #logsdo D on status and family SES

rownames.mod <- as.vector(c("lm.logsdo.age", #logsdo on age
                            "lm.bias.age", #bias on age
                            "lm.empathy.age", #empathy on age
                            "lm.grplik.age", #group liking on age
                            "lm.logsdoE.age", #logsdo-D on age
                            #Gender
                            "lm.logsdo.gender", #logsdo on gender
                            "lm.bias.gender", #bias on gender
                            "lm.empathy.gender", #empathy on gender
                            "lm.grplik.gender", #group liking on gender
                            #Main DVs
                            "lm.logsdo.empathy", #logsdo on empathy
                            "lm.empathy.logsdo", #empathy on logsdo
                            "lm.logsdo.bias", #logsdo on bias
                            "lm.bias.logsdo", #bias on logsdo
                            "lm.bias.empathy", #bias on empathy
                            "lm.empathy.bias", #empathy on bias
                            "lm.bias.logsdoE", #bias on logsdoE
                            #Experimental Effects
                            "lm.logsdo.status", #logsdo on status condition
                            "lm.bias.status", #bias on status condition
                            "lm.empathy.status", #empathy on status condition
                            "lm.logsdoD.status", #logsdo D on status condition
                            "lm.logsdoE.status",#logsdo E on status condition
                            
                            #Multivariate Regressions
                            "lm.logsdo.status.gender", #logsdo on status and gender (interaction"
                            "lm.logsdo.status.gender2", #logsdo on status and gender (main effects"
                            "lm.logsdo.status.fem", #logsdo on status (girls only"
                            "lm.logsdo.status.male", #logsdo on status (boys only"
                            "lm.emp.status.gender", #empathy on status and gender
                            "lm.emp.status.gender.age1",  #empathy on status, gender, and age (interaction"
                            "lm.emp.status.gender.age2", #empathy on status, gender, and age (main effects"
                            "lm.emp.status.parpolsocl", #empathy on status and parent social political views
                            "lm.logsdo.status.parpolsocl", #logsdo on status and parent social political views
                            "lm.logsdoE.status.parpolsocl", #logsdo D on status and parent social political views
                            "lm.emp.status.ses", #empathy on status and family SES
                            "lm.logsdo.status.ses", #logsdo on status and family SES
                            "lm.logsdoE.status.ses"))
#models <- mutate(models, names = rownames.mod)
models.glance <- as.matrix(rbind(
  glance(lm.logsdo.age), #logsdo on age
glance(lm.bias.age), #bias on age
glance(lm.empathy.age), #empathy on age
glance(lm.grplik.age), #group liking on age
glance(lm.logsdoE.age), #logsdo-D on age
#Gender
glance(lm.logsdo.gender), #logsdo on gender
glance(lm.bias.gender), #bias on gender
glance(lm.empathy.gender), #empathy on gender
glance(lm.grplik.gender), #group liking on gender
#Main DVs
glance(lm.logsdo.empathy), #logsdo on empathy
glance(lm.empathy.logsdo), #empathy on logsdo
glance(lm.logsdo.bias), #logsdo on bias
glance(lm.bias.logsdo), #bias on logsdo
glance(lm.bias.empathy), #bias on empathy
glance(lm.empathy.bias), #empathy on bias
glance(lm.bias.logsdoE), #bias on logsdoE
#Experimental Effects
glance(lm.logsdo.status), #logsdo on status condition
glance(lm.bias.status), #bias on status condition
glance(lm.empathy.status), #empathy on status condition
glance(lm.logsdoD.status), #logsdo D on status condition
glance(lm.logsdoE.status), #logsdo E on status condition

#Multivariate Regressions
glance(lm.logsdo.status.gender), #logsdo on status and gender (interaction)
glance(lm.logsdo.status.gender2), #logsdo on status and gender (main effects)
glance(lm.logsdo.status.fem), #logsdo on status (girls only)
glance(lm.logsdo.status.male), #logsdo on status (boys only)
glance(lm.emp.status.gender), #empathy on status and gender
glance(lm.emp.status.gender.age1),  #empathy on status, gender, and age (interaction)
glance(lm.emp.status.gender.age2), #empathy on status, gender, and age (main effects)
glance(lm.emp.status.parpolsocl), #empathy on status and parent social political views
glance(lm.logsdo.status.parpolsocl), #logsdo on status and parent social political views
glance(lm.logsdoE.status.parpolsocl), #logsdo D on status and parent social political views
glance(lm.emp.status.ses), #empathy on status and family SES
glance(lm.logsdo.status.ses), #logsdo on status and family SES
glance(lm.logsdoE.status.ses) #logsdo D on status and family SES
))
models.glance
#plot(density(resid(lm.logsdo.age)))
#plot(density(resid(lm.bias.age)))
#plot(density(resid(lm.empathy.age)))
#plot(density(resid(lm.grplik.age)))
#Gender
#plot(density(resid(lm.logsdo.gender)))
#plot(density(resid(lm.bias.gender)))
#plot(density(resid(lm.empathy.gender)))
#plot(density(resid(lm.grplik.gender)))
#Main DVs
#plot(density(resid(lm.logsdo.empathy)))
#plot(density(resid(lm.empathy.logsdo)))
#plot(density(resid(lm.logsdo.bias)))
#plot(density(resid(lm.bias.logsdo)))
#plot(density(resid(lm.bias.empathy)))
#plot(density(resid(lm.empathy.bias)))
#Experimental Effects
#plot(density(resid(lm.logsdo.status)))
#plot(density(resid(lm.bias.status)))
#plot(density(resid(lm.empathy.status)))
#Multivariate Regressions
#plot(density(resid(lm.logsdoD.status)))
#plot(density(resid(lm.logsdoE.status)))
#plot(density(resid(lm.logsdo.status.gender)))
#plot(density(resid(lm.logsdo.status.gender2)))
#plot(density(resid(lm.emp.status.gender)))

#Residuals plots

sink()
file.show("~//lin_models.txt")

##ad hocs#####
lm.sdoE.status.ses <- summary(lm(sdoE.mean ~ status*p.ses, data = sas.compl))  #sdo D on group status assignment and family SES
lm.sdo.status.ses.bias <- summary(lm(sdo.mean ~ status*p.ses+in.out.bias+par.polsocl, data = sas.compl))  #sdo D on group status assignment and family SES
lm.sdoE.status.ses3 <- summary(lm(sdoE.mean ~ status*p.age, data = sas.compl))  #sdo D on group status assignment and family SES

summary(lm.sdo.status.ses.bias)

#Visuals####
##Demographics####
sas.fac %>% group_by(p.gen) %>%
  select(p.age, p.gen) %>%
  ggplot(aes(x = p.age, group = p.gen, fill = p.gen)) +
  geom_histogram(binwidth = 1, position = "dodge",
                 color = "gray90", size = 1) +
  scale_x_continuous(breaks = seq(2, 12, 1)) +
  scale_color_manual(values = c("orange","purple"), aesthetics = "fill", label = c("female", "male"))


sas.fac %>% group_by(p.gen) %>%
  select(p.age, p.gen) %>%
  ggplot(aes(group = p.gen, fill = p.gen)) +
  geom_violin(aes(x = p.gen, y = p.age, color = p.gen)) +
  scale_color_manual(values = c("orange","purple"), aesthetics = c("fill", "color"), label = c("female", "male"))

sas.fac %>% group_by(p.gen) %>%
  select(p.age, p.gen) %>%
  ggplot() +
  geom_dotplot(aes(x = p.gen, y = p.age, group = p.gen, color = p.gen, fill = p.gen), binaxis = "y", binwidth = 0.25, stackdir = "center") +
  scale_color_manual(values = c("orange","purple"), aesthetics = c("fill", "color"), label = c("female", "male"))

png(filename = "demo.age.gender.box.png", width = 1920, height = 1080, units = "px", pointsize = 18, res = 160)
sas.fac %>% group_by(p.gen) %>%
  select(p.age, p.gen) %>%
  ggplot(aes(group = p.gen)) +
  geom_boxplot(aes(x = p.gen, y = p.age, color = p.gen), width = 2.5, size = 1.05) +
  scale_color_manual(values = c("orange","purple"), aesthetics = "color", label = c("female", "male"))+
  coord_fixed(.75)
dev.off()
file.show("~//demo.age.gender.box.png")

png(filename = "lm.sdoE.age.png", width = 1200, height = 1100, units = "px", pointsize = 25, res = 200)
g.sdoE.age <- ggplot(sas.fac.compl)+
  geom_point(aes(p.age, sdoE.mean))+
  geom_smooth(aes(p.age, sdoE.mean), method = "lm", se = F, color = "hotpink") +
  scale_x_continuous(limits = c(5, 11))+
  theme(text = element_text(size = 18))
g.sdoE.age
dev.off()
file.show("~//lm.sdoE.age.png")



##Histograms####
#DVs
png(filename = "h.main.png", width = 1200, height = 1100, units = "px", pointsize = 25, res = 200)
h1 <- sas %>% ggplot() +
          geom_density(aes(in.out.bias))
h2 <- sas %>% ggplot() +
  geom_density(aes(emp.mean))
h2.5 <- sas %>% ggplot() +
  geom_density(aes(log(emp.mean)))
h3 <- sas %>% ggplot() +
  geom_density(aes(sdo.mean))
h4 <- sas %>% ggplot() +
  geom_density(aes(log(sdo.mean)))
h5 <- sas %>% ggplot() +
  geom_density(aes(sdoD.mean))
h6 <- sas %>% ggplot() +
  geom_density(aes(log(sdoD.mean)))
h7 <- sas %>% ggplot() +
  geom_density(aes(sdoE.mean))
h8 <- sas %>% ggplot() +
  geom_density(aes(log(sdoE.mean)))
h.main <- grid.arrange(h1,h2,h2.5,h3,h4,h5,h6,h7,h8,nrow=5)
dev.off()
file.show("~//h.main.png")


#Demographics
png(filename = "h.demo.png", width = 1200, height = 1100, units = "px", pointsize = 25, res = 200)
h9 <- sas %>% ggplot() +
  geom_density(aes(p.age))
#h10 <- sas %>% ggplot(aes(p.gen)) +
#  stat_ecdf(geom = "step")
h10 <- sas %>% ggplot() +
  geom_density(aes(par.polsocl))
h11 <- sas %>% ggplot() +
  geom_density(aes(log(par.polsocl)))
h12 <- sas %>% ggplot() +
  geom_density(aes(par.polecon))
h13 <- sas %>% ggplot() +
  geom_density(aes(log(par.polecon)))
h14 <- sas %>% ggplot() +
  geom_density(aes(p.ses))
h15 <- sas %>% ggplot() +
  geom_density(aes(p.famchildnum))
h.demo <- grid.arrange(h9,h10,h11,h12,h13,h14,h15, nrow=4)
dev.off()
file.show("~//h.demo.png")



##Linear Models####
##Effects of Status####
png(filename = "lm.sdo.status.viol.png", width = 1920, height = 1080, units = "px", pointsize = 18, res = 160)
g.sdo.status.viol <- ggplot(sas.fac.compl) +
  geom_violin(aes(status, sdo.mean, fill = status, color = status))+
  scale_color_manual(values = c("palegreen3", "violetred1"), guide = "none")+
  scale_fill_manual(values = c("palegreen3", "violetred1"), label = c("low", "high"))+
  scale_y_continuous(breaks = seq(1, 3, 0.5)) +
  theme(text = element_text(size = 18))
g.sdo.status.viol
dev.off()
file.show("~//lm.sdo.status.viol.png")

png(filename = "lm.sdo.status.box.png", width = 1920, height = 1080, units = "px", pointsize = 25, res = 200)
g.sdo.status.box <- ggplot() +
  geom_boxplot(aes(status, sdo.mean, color = status), size =1.05, data = sas.fac.compl)+
  scale_color_manual(values = c("palegreen3", "violetred1"), label = c("low", "high"))+
  scale_y_continuous(breaks = seq(0, 3, 0.5))+
  coord_fixed(1.8)+
  theme(text = element_text(size = 18))
g.sdo.status.box
dev.off()
file.show("~//lm.sdo.status.box.png")

png(filename = "lm.sdo.status.jitter.png", width = 1920, height = 1080, units = "px", pointsize = 18, res = 160)
g.sdo.status.jitter <- ggplot(sas.fac.compl) +
  geom_jitter(aes(status, sdo.mean, color = in.out.bias), size = 3, width = 0.1)+
  scale_color_gradient(low = "midnightblue", high = "green")+
  scale_y_continuous(breaks = seq(1, 3, 0.5)) +
  theme(text = element_text(size = 18))
g.sdo.status.jitter
dev.off()
file.show("~//lm.sdo.status.jitter.png")

png(filename = "lm.sdoE.status.jitter.png", width = 1920, height = 1080, units = "px", pointsize = 18, res = 160)
g.sdoE.status.jitter <- ggplot(sas.fac.compl) +
  geom_jitter(aes(status, sdoE.mean, color = sdoE.mean), size = 3, width = 0.1)+
  scale_color_gradient(low = "midnightblue", high = "green")+
  scale_y_continuous(breaks = seq(1, 3, 0.5)) +
  theme(text = element_text(size = 18))
g.sdoE.status.jitter
dev.off()
file.show("~//lm.sdoE.status.jitter.png")

png(filename = "lm.sdo.status.line.png", width = 1200, height = 800, units = "px", pointsize = 25, res = 200)
g.sdo.status.line <- ggplot(aes(status, sdo.mean2, color = status), data = status.summary.compl) +
  geom_line(aes(group = 1), size = 0.5, linetype = "longdash", color = "turquoise")+
  geom_errorbar(aes(ymin = sdo.ci.lower, ymax = sdo.ci.upper), size = 1, width = 0.1)+
  geom_count(aes(status, sdo.mean2), size = 2)+
  scale_color_manual(values = c("palegreen3", "violetred1"), aesthetics = "color", label = c("Low", "High"))+
  scale_y_continuous(breaks = seq(1, 3, 0.5), limits = c(1.25,2.25))+
  #coord_fixed(1.8)+
  ylab("SDO")+
  xlab("Status")+
  labs(color = "Status")+
  theme(text = element_text(size = 16))
g.sdo.status.line
dev.off()
file.show("~//lm.sdo.status.line.png")

png(filename = "lm.emp.status.line.png", width = 1200, height = 800, units = "px", pointsize = 25, res = 200)
g.emp.status.line <- ggplot(aes(status, emp.mean2, color = status), data = status.summary) +
  geom_line(aes(group = 1), size = 0.5, linetype = "longdash", color = "turquoise")+
  geom_errorbar(aes(ymin = emp.ci.lower, ymax = emp.ci.upper), size = 1, width = 0.1)+
  geom_count(aes(status, emp.mean2), size = 2)+
  scale_color_manual(values = c("palegreen3", "violetred1"), aesthetics = "color", label = c("Low", "High"))+
  scale_y_continuous(breaks = seq(2, 3.6, 0.5), limits = c(2.25,3.75))+
  #coord_fixed(1.8)+
  xlab("Status")+
  ylab("Outgroup Empathy")+
  labs(color = "Status")+
  theme(text = element_text(size = 18))
g.emp.status.line
dev.off()
file.show("~//lm.emp.status.line.png")


g.empathy.status <- ggplot(sas) +
                      geom_point(aes(status, emp.mean))+
                      geom_smooth(aes(status, emp.mean), method = "lm") +
                    scale_x_continuous(breaks = seq(0,1,1))
g.empathy.status

g.empathy.status.viol <- ggplot(sas.fac) +
  geom_violin(aes(status, emp.mean, fill = status, color = status))+
  scale_color_manual(values = c("lightblue", "orange"))+
  scale_fill_manual(values = c("lightblue", "orange"))
g.empathy.status.viol

g.empathy.status.box <- ggplot(sas.fac) +
  geom_boxplot(aes(status, emp.mean,color = status), lwd = 1 )+
  scale_color_manual(values = c("lightblue", "orange"))
g.empathy.status.box


##Effects of Gender####
png(filename = "lm.sdo.gender.line.png", width = 1200, height = 1100, units = "px", pointsize = 25, res = 200)
g.sdo.gender.line <- ggplot(aes(p.gen, sdo.mean2, color = p.gen), data = gender.summary.compl) +
  geom_line(aes(group = 1), size = 0.5, linetype = "longdash", color = "turquoise")+
  geom_errorbar(aes(ymin = sdo.ci.lower, ymax = sdo.ci.upper), size = 1, width = 0.1)+
  geom_count(aes(p.gen, sdo.mean2), size = 2)+
  scale_color_manual(values = c("violet", "coral"), aesthetics = "color", label = c("female", "male"))+
  scale_y_continuous(breaks = seq(0, 3, 0.5), limits = c(1,3))+
  #coord_fixed(1.8)+
  ylab("SDO")+
  theme(text = element_text(size = 18))
g.sdo.gender.line
dev.off()
file.show("~//lm.sdo.gender.line.png")

#png(filename = "lm.sdo.malestat.line.png", width = 1200, height = 1100, units = "px", pointsize = 25, res = 200)
#g.sdo.malestat.line <- statgen.summary.compl %>% filter(p.gen == 0) %>%
#    ggplot(aes(status, sdo.mean2, color = status)) +
#    geom_line(aes(group = 1), size = 0.5, linetype = "longdash", color = "turquoise")+
#    geom_errorbar(aes(ymin = sdo.ci.lower, ymax = sdo.ci.upper), size = 1, width = 0.1)+
#    geom_count(aes(status, sdo.mean2), size = 2)+
#    scale_color_manual(values = c("palegreen3", "violetred1"), aesthetics = "color", label = c("low", "high"))+
#    scale_y_continuous(breaks = seq(0, 3, 0.5), limits = c(1,3))+
    #coord_fixed(1.8)+
#    xlab("Status (Male)")+
#    ylab("SDO")+
#    theme(text = element_text(size = 18))
#g.sdo.malestat.line
#dev.off()
#file.show("~//lm.sdo.malestat.line.png")

#png(filename = "lm.sdo.femstat.line.png", width = 1200, height = 1100, units = "px", pointsize = 25, res = 200)
#g.sdo.femstat.line <- statgen.summary.compl %>% filter(p.gen == 1) %>%
#  ggplot(aes(status, sdo.mean2, color = status)) +
#  geom_line(aes(group = 1), size = 0.5, linetype = "longdash", color = "turquoise")+
#  geom_errorbar(aes(ymin = sdo.ci.lower, ymax = sdo.ci.upper), size = 1, width = 0.1)+
#  geom_count(aes(status, sdo.mean2), size = 2)+
#  scale_color_manual(values = c("palegreen3", "violetred1"), aesthetics = "color", label = c("low", "high"))+
#  scale_y_continuous(breaks = seq(0, 3, 0.5), limits = c(1,3))+
#  #coord_fixed(1.8)+
#  xlab("Status (Female)")+
#  ylab("SDO")+
#  theme(text = element_text(size = 18))
#g.sdo.femstat.line
#dev.off()
#file.show("~//lm.sdo.femstat.line.png")

png(filename = "lm.emp.malestat.line.png", width = 1200, height = 1100, units = "px", pointsize = 25, res = 200)
g.emp.malestat.line <- statgen.summary %>% filter(p.gen == 0) %>%
  ggplot(aes(status, emp.mean2, color = status)) +
  geom_line(aes(group = 1), size = 0.5, linetype = "longdash", color = "turquoise")+
  geom_errorbar(aes(ymin = emp.ci.lower, ymax = emp.ci.upper), size = 1, width = 0.1)+
  geom_count(aes(status, emp.mean2), size = 2)+
  scale_color_manual(values = c("darkorchid1", "springgreen"), aesthetics = "color", label = c("low", "high"))+
  scale_y_continuous(breaks = seq(2, 4, 0.5), limits = c(2,4))+
  #coord_fixed(1.8)+
  xlab("Status (Male)")+
  ylab("Outgroup Empathy")+
  theme(text = element_text(size = 18))
g.emp.malestat.line
dev.off()
file.show("~//lm.emp.malestat.line.png")

png(filename = "lm.emp.femstat.line.png", width = 1200, height = 1100, units = "px", pointsize = 25, res = 200)
g.emp.femstat.line <- statgen.summary %>% filter(p.gen == 1) %>%
  ggplot(aes(status, emp.mean2, color = status)) +
  geom_line(aes(group = 1), size = 0.5, linetype = "longdash", color = "turquoise")+
  geom_errorbar(aes(ymin = emp.ci.lower, ymax = emp.ci.upper), size = 1, width = 0.1)+
  geom_count(aes(status, emp.mean2), size = 2)+
  scale_color_manual(values = c("darkorchid1", "springgreen"), aesthetics = "color", label = c("low", "high"))+
  scale_y_continuous(breaks = seq(2, 4, 0.5), limits = c(2,4))+
  #coord_fixed(1.8)+
  xlab("Status (Female)")+
  ylab("Outgroup Empathy")+
  theme(text = element_text(size = 18))
g.emp.femstat.line
dev.off()
file.show("~//lm.emp.femstat.line.png")

png(filename = "lm.emp.genstat.line.png", width = 1200, height = 1100, units = "px", pointsize = 25, res = 200)
g.emp.genstat.line <- statgen.summary %>% group_by(p.gen) %>%
  ggplot(aes(status, emp.mean2, group = p.gen, color = p.gen)) +
  geom_line(size = 0.5, linetype = "longdash", color = "turquoise", position = position_dodge(0.2))+
  geom_errorbar(aes(ymin = emp.ci.lower, ymax = emp.ci.upper), size = 1, width = 0.1, position = position_dodge(0.2))+
  geom_count(aes(status, emp.mean2), size = 2, position = position_dodge(0.2))+
  scale_color_manual(values = c("darkorchid1", "springgreen"), aesthetics = "color", label = c("Female", "Male"))+
  scale_y_continuous(breaks = seq(2, 4, 0.5), limits = c(2,4))+
  #coord_fixed(1.8)+
  xlab("Status")+
  ylab("Outgroup Empathy")+
  theme(text = element_text(size = 18))
g.emp.genstat.line
dev.off()
file.show("~//lm.emp.genstat.line.png")

png(filename = "lm.emp.genstat.line.png", width = 1200, height = 1100, units = "px", pointsize = 25, res = 200)
g.emp.genstat.line <- statgen.summary %>% group_by(p.gen) %>%
  ggplot(aes(status, emp.mean2, group = p.gen, color = p.gen)) +
  geom_line(size = 0.5, linetype = "longdash", color = "turquoise", position = position_dodge(0.2))+
  geom_errorbar(aes(ymin = emp.ci.lower, ymax = emp.ci.upper), size = 1, width = 0.1, position = position_dodge(0.2))+
  geom_count(aes(status, emp.mean2), size = 2, position = position_dodge(0.2))+
  geom_jitter(aes(status, emp.mean, alpha = p.age), data = sas.fac, position = position_jitter(0.01)) + 
  scale_color_manual(values = c("darkorchid1", "springgreen"), aesthetics = "color", label = c("Female", "Male"))+
  scale_y_continuous(breaks = seq(2, 4, 0.5), limits = c(2,4))+
  scale_alpha_continuous(range = c(0.01, 0.8))+
  #coord_fixed(1.8)+
  xlab("Status")+
  ylab("Outgroup Empathy")+
  theme(text = element_text(size = 18))
g.emp.genstat.line
dev.off()
file.show("~//lm.emp.genstat.line.png")


g.empathy.gender.viol <- ggplot(sas.fac) +
  geom_violin(aes(p.gen, emp.mean, fill = p.gen, color = p.gen))+
  scale_color_manual(values = c("violet", "coral"))+
  scale_fill_manual(values = c("violet", "coral"))
g.empathy.gender.viol

g.empathy.gender.box <- ggplot(sas.fac) +
  geom_boxplot(aes(p.gen, emp.mean,color = p.gen), lwd = 1 )+
  scale_color_manual(values = c("violet", "coral"))
g.empathy.gender.box

##SDO as Predictor####
png(filename = "lm.emp.sdo.png", width = 1200, height = 1100, units = "px", pointsize = 25, res = 200)
g.emp.sdo <- ggplot(sas.fac.compl)+
              geom_point(aes(sdo.mean, emp.mean))+
              geom_smooth(aes(sdo.mean, emp.mean), method = "lm", se = F, color = "red") +
              scale_x_continuous(limits = c(1, 4))+
              theme(text = element_text(size = 18))
g.emp.sdo
dev.off()
file.show("~//lm.emp.sdo.png")

png(filename = "lm.bias.sdo.png", width = 1200, height = 1100, units = "px", pointsize = 25, res = 200)
g.bias.sdo <- ggplot(sas.fac.compl)+
  geom_point(aes(sdo.mean, in.out.bias))+
  geom_smooth(aes(sdo.mean, in.out.bias), method = "lm", se = F, color = "blue") +
  #scale_x_continuous(limits = c(1, 4))+
  theme(text = element_text(size = 18))
g.bias.sdo
dev.off()
file.show("~//lm.bias.sdo.png")

png(filename = "lm.bias.sdoE.png", width = 1200, height = 1100, units = "px", pointsize = 25, res = 200)
g.bias.sdoE <- ggplot(sas.fac.compl)+
  geom_jitter(aes(sdoE.mean, in.out.bias), width = 0.03, height = 0.03, alpha = 0.5)+
  geom_smooth(aes(sdoE.mean, in.out.bias), method = "lm", se = F, color = "turquoise") +
  scale_x_continuous(limits = c(1, 4))+
  theme(text = element_text(size = 24))+
  xlab("SDO-E")+
  ylab("Ingroup Bias")
g.bias.sdoE
dev.off()
file.show("~//lm.bias.sdoE.png")

png(filename = "lm.emp.sdoE.png", width = 1200, height = 1100, units = "px", pointsize = 25, res = 200)
g.emp.sdoE <- ggplot(sas.fac.compl)+
  geom_jitter(aes(sdoE.mean, emp.mean), width = 0.03, height = 0.03, alpha = 0.5)+
  geom_smooth(aes(sdoE.mean, emp.mean), method = "lm", se = F, color = "orange") +
  scale_x_continuous(limits = c(1, 4))+
  theme(text = element_text(size = 24))+
  xlab("SDO-E")+
  ylab("Outgroup Empathy")
g.emp.sdoE
dev.off()
file.show("~//lm.emp.sdoE.png")

png(filename = "lm.sdo.ses.png", width = 1200, height = 1100, units = "px", pointsize = 25, res = 200)
g.sdo.ses <- ggplot(sas.compl)+
  geom_jitter(aes(p.ses, sdo.mean), width = 0.03, height = 0.03, alpha = 0.5)+
  geom_smooth(aes(p.ses, sdo.mean), method = "lm", se = F, color = "red") +
  #scale_x_continuous(limits = c(1, 4))+
  theme(text = element_text(size = 24))+
  ylab("SDO")+
  xlab("Family SES")
g.sdo.ses
dev.off()
file.show("~//lm.sdo.ses.png")

png(filename = "lm.sdoE.ses.png", width = 1200, height = 1100, units = "px", pointsize = 25, res = 200)
g.sdo.ses <- ggplot(sas.compl)+
  geom_jitter(aes(p.ses, sdoE.mean), alpha = 0.5, hjust = 0.02, vjust = 0.02)+
  geom_smooth(aes(p.ses, sdoE.mean), method = "lm", se = F, color = "turquoise") +
  #scale_x_continuous(limits = c(1, 4))+
  theme(text = element_text(size = 18))+
  ylab("SDO-E")+
  xlab("Family SES")
g.sdo.ses
dev.off()
file.show("~//lm.sdoE.ses.png")

