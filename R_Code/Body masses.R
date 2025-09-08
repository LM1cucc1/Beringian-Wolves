
library(here)
here()

m1width<-read.csv("m1_width_all.csv")
m1length<-read.csv("m1_length_all.csv")
P4width<-read.csv("P4_width_all.csv")
P4length<-read.csv("P4_Length_all.csv",header=T)


####

#m1 length histogram

library(ggplot2)

m1L_plot <- ggplot(m1length,aes(x=m1_length,fill=Group,color=Group)) +
  geom_histogram(alpha = 0.5, position = "identity",bins = 15) +
  scale_fill_manual(values=c("#FF8F33","tan3","wheat2","#00AACC", "#66F0FF","cyan4")) +
  scale_color_manual(values=c("black", "black", "black","black","black","black")) + 
  labs(x="m1 length (mm)",y="Frequency") + theme_classic() + stat_bin(bins=15) +
  geom_vline(xintercept=mean(m1length$m1_length), lwd=1, linetype=2, color="black") 

m1L_plot

pm1L <- ggplot(m1length, aes(x=Group, y=m1_length, fill=Group)) + 
  geom_boxplot(lwd = 0.5) + scale_fill_manual(values=c("#FF8F33","tan3","wheat2","#00AACC", "#66F0FF","cyan4")) + theme_classic()
pm1L

# deal with each metric one at a time

#####Body Mass########################################################################################################
#####################################################################################################################

# with Siberia

test1<-summary(aov(Body_mass~Group,data=m1length))
TukeyHSD(aov(Body_mass~Group,data=m1length))

test<-subsample_aov("Pleistocene-Siberia",4,100,m1length) # includes all groups but want to exclude the smallest sample size as well
# gotta figure out why this code is being annoying

# removing Siberia
m1length_nosib<-m1length[-c(which(m1length$Group=="Pleistocene-Siberia")),]

test1<-summary(aov(Body_mass~Group,data=m1length_nosib))
TukeyHSD(aov(Body_mass~Group,data=m1length_nosib))

test<-subsample_aov("Pleistocene-Yukon",3,100,m1length_nosib) # includes all groups but want to exclude the smallest sample size as well



#####m1length########################################################################################################
#####################################################################################################################

# with Siberia

test1<-summary(aov(m1_length~Group,data=m1length))
TukeyHSD(aov(m1_length~Group,data=m1length))

test<-subsample_aov("Pleistocene-Siberia",3,100,m1length) # includes all groups but want to exclude the smallest sample size as well
# gotta figure out why this code is being annoying

# removing Siberia
m1length_nosib<-m1length[-c(which(m1length$Group=="Pleistocene-Siberia")),]

test1<-summary(aov(m1_length~Group,data=m1length_nosib))
TukeyHSD(aov(m1_length~Group,data=m1length_nosib))

test<-subsample_aov("Pleistocene-Yukon",3,100,m1length_nosib) # includes all groups but want to exclude the smallest sample size as well

#### comparing Pleistocene and modern Yukon

Yukon_Pleist<-m1length[m1length$Group=="Pleistocene-Yukon",]
Yukon_recent<-m1length[m1length$Group=="Recent-Yukon",]
Yukon_all<-rbind(Yukon_Pleist,Yukon_recent)
kruskal.test(m1_length ~ Group, data = Yukon_all)# not different
wilcox.test(m1_length ~ Group,data = Yukon_all)#not different
ks.test(Yukon_Pleist$m1_length,Yukon_recent$m1_length)

#now subsample
subsample_metric("Pleistocene-Yukon","Recent-Yukon",3,100,m1length)# okay to ignore the warnings
#not different for WS or KS

#### comparing Pleistocene Yukon and Pleistocene Sibera

Yukon_Pleist<-m1length[m1length$Group=="Pleistocene-Yukon",]
Siberia_Pleist<-m1length[m1length$Group=="Pleistocene-Siberia",]
Siberia_Yukon<-rbind(Yukon_Pleist,Siberia_Pleist)
kruskal.test(m1_length ~ Group, data = Siberia_Yukon)
wilcox.test(m1_length ~ Group,data = Siberia_Yukon)
ks.test(Yukon_Pleist$m1_length,Siberia_Pleist$m1_length)

subsample_metric("Pleistocene-Siberia","Pleistocene-Yukon",3,100,m1length)# do not differ

#### comparing Pleistocene Yukon and Recent Canadian

Yukon_Pleist<-m1length[m1length$Group=="Pleistocene-Yukon",]
Recent_can<-m1length[m1length$Group=="Recent",]
Recent_Yukon<-rbind(Yukon_Pleist,Recent_can)
kruskal.test(m1_length ~ Group, data = Recent_Yukon)#differ
wilcox.test(m1_length ~ Group,data = Recent_Yukon)#differ
ks.test(Yukon_Pleist$m1_length,Recent_can$m1_length)

subsample_metric("Pleistocene-Yukon","Recent",3,100,m1length)# do not differ

#### comparing Pleistocene Yukon and Pleistocene Europe

Yukon_Pleist<-m1length[m1length$Group=="Pleistocene-Yukon",]
Europe_Pleist<-m1length[m1length$Group=="Pleistocene-Europe",]
Europe_Yukon<-rbind(Yukon_Pleist,Europe_Pleist)
kruskal.test(m1_length ~ Group, data = Europe_Yukon)#do not differ
wilcox.test(m1_length ~ Group,data = Europe_Yukon)#do not differ
ks.test(Yukon_Pleist$m1_length,Europe_Pleist$m1_length)

subsample_metric("Pleistocene-Yukon","Pleistocene-Europe",3,100,m1length)# do not differ

#### comparing Pleistocene Yukon and Pleistocene Alaska

Yukon_Pleist<-m1length[m1length$Group=="Pleistocene-Yukon",]
Alaska_Pleist<-m1length[m1length$Group=="Pleistocene-Alaska",]
Alaska_Yukon<-rbind(Yukon_Pleist,Alaska_Pleist)
kruskal.test(m1_length ~ Group, data = Alaska_Yukon)
wilcox.test(m1_length ~ Group,data = Alaska_Yukon)
ks.test(Yukon_Pleist$m1_length,Alaska_Pleist$m1_length)

subsample_metric("Pleistocene-Yukon","Pleistocene-Alaska",3,100,m1length)# one differs, one does not

#####m1width########################################################################################################
#####################################################################################################################

# with Siberia

test1<-summary(aov(m1_width~Group,data=m1width))
TukeyHSD(aov(m1_width~Group,data=m1width))

test<-subsample_aov("Pleistocene-Yukon",3,100,m1width) # includes all groups but want to exclude the smallest sample size as well
# gotta figure out why this code is being annoying

#### comparing Pleistocene and modern Yukon

Yukon_Pleist<-m1width[m1width$Group=="Pleistocene-Yukon",]
Yukon_recent<-m1width[m1width$Group=="Recent-Yukon",]
Yukon_all<-rbind(Yukon_Pleist,Yukon_recent)
kruskal.test(m1_width ~ Group, data = Yukon_all)# not different
wilcox.test(m1_width ~ Group,data = Yukon_all)#not different
ks.test(Yukon_Pleist$m1_width,Yukon_recent$m1_width)

#now subsample
subsample_metric("Pleistocene-Yukon","Recent-Yukon",3,100,m1width)# okay to ignore the warnings
#not different for WS or KS

#### comparing Pleistocene Yukon and Recent Canadian

Yukon_Pleist<-m1width[m1width$Group=="Pleistocene-Yukon",]
Recent_can<-m1width[m1width$Group=="Recent",]
Recent_Yukon<-rbind(Yukon_Pleist,Recent_can)
kruskal.test(m1_width ~ Group, data = Recent_Yukon)#differ
wilcox.test(m1_width ~ Group,data = Recent_Yukon)#differ
ks.test(Yukon_Pleist$m1_width,Recent_can$m1_width)

subsample_metric("Pleistocene-Yukon","Recent",3,100,m1width)# do not differ


#### comparing Pleistocene Yukon and Pleistocene Alaska

Yukon_Pleist<-m1width[m1width$Group=="Pleistocene-Yukon",]
Alaska_Pleist<-m1width[m1width$Group=="Pleistocene-Alaska",]
Alaska_Yukon<-rbind(Yukon_Pleist,Alaska_Pleist)
kruskal.test(m1_width ~ Group, data = Alaska_Yukon)
wilcox.test(m1_width ~ Group,data = Alaska_Yukon)
ks.test(Yukon_Pleist$m1_width,Alaska_Pleist$m1_width)

subsample_metric("Pleistocene-Yukon","Pleistocene-Alaska",3,100,m1width)# one differs, one does not

#####P4length########################################################################################################
#####################################################################################################################

# with Siberia

test1<-summary(aov(P4_Length~Group,data=P4length))
TukeyHSD(aov(P4_Length~Group,data=P4length))

test<-subsample_aov("Recent-Yukon",3,100,P4length) # includes all groups but want to exclude the smallest sample size as well
# gotta figure out why this code is being annoying

#### comparing Pleistocene and modern Yukon

Yukon_Pleist<-P4length[P4length$Group=="Pleistocene-Yukon",]
Yukon_recent<-P4length[P4length$Group=="Recent-Yukon",]
Yukon_all<-rbind(Yukon_Pleist,Yukon_recent)
kruskal.test(P4_Length ~ Group, data = Yukon_all)# not different
wilcox.test(P4_Length ~ Group,data = Yukon_all)#not different
ks.test(Yukon_Pleist$P4_Length,Yukon_recent$P4_Length)

#Nearly the same sample size, no resampling required

#### comparing Pleistocene Yukon and Recent Canadian

Yukon_Pleist<-P4length[P4length$Group=="Pleistocene-Yukon",]
Recent_can<-P4length[P4length$Group=="Recent",]
Recent_Yukon<-rbind(Yukon_Pleist,Recent_can)
kruskal.test(P4_Length ~ Group, data = Recent_Yukon)#differ
wilcox.test(P4_Length ~ Group,data = Recent_Yukon)#differ
ks.test(Yukon_Pleist$P4_Length,Recent_can$P4_Length)

subsample_metric("Pleistocene-Yukon","Recent",3,100,P4length)# do not differ


#####P4width########################################################################################################
#####################################################################################################################

test1<-summary(aov(P4_width~Group,data=P4width))
TukeyHSD(aov(P4_width~Group,data=P4width))

test<-subsample_aov("Pleistocene-Yukon",3,100,P4width) # includes all groups but want to exclude the smallest sample size as well
# gotta figure out why this code is being annoying

#### comparing Pleistocene and modern Yukon

Yukon_Pleist<-P4width[P4width$Group=="Pleistocene-Yukon",]
Yukon_recent<-P4width[P4width$Group=="Recent-Yukon",]
Yukon_all<-rbind(Yukon_Pleist,Yukon_recent)
kruskal.test(P4_width ~ Group, data = Yukon_all)# not different
wilcox.test(P4_width ~ Group,data = Yukon_all)#not different
ks.test(Yukon_Pleist$P4_width,Yukon_recent$P4_width)

#### comparing Pleistocene Yukon and Recent Canadian

Yukon_Pleist<-P4width[P4width$Group=="Pleistocene-Yukon",]
Recent_can<-P4width[P4width$Group=="Recent",]
Recent_Yukon<-rbind(Yukon_Pleist,Recent_can)
kruskal.test(P4_width ~ Group, data = Recent_Yukon)#differ
wilcox.test(P4_width ~ Group,data = Recent_Yukon)#differ
ks.test(Yukon_Pleist$P4_width,Recent_can$P4_width)

subsample_metric("Pleistocene-Yukon","Recent",3,100,P4width)# do not differ




