library(reshape2)
library(RColorBrewer)
library(scales)
library(plyr)
library(ggplot2)
library(readxl)

PB &lt;- data.frame(read_excel(&quot;Y:/LSmeans_YV_.xlsx&quot;, 1 ))
VL &lt;- data.frame(read_excel(&quot;Y:/LSmeans_YV_.xlsx&quot;,2 ))
PN &lt;- data.frame(read_excel(&quot;Y:/LSmeans_YV_.xlsx&quot;,3 ))
REMOD &lt;- data.frame(read_excel(&quot;Y:/LSmeans_YV_.xlsx&quot;,4))
PosREMOD &lt;- data.frame(read_excel(&quot;Y:/LSmeans_YV_.xlsx&quot;,5))
OrPosREMOD &lt;- data.frame(read_excel(&quot;Y:/LSmeans_YV_.xlsx&quot;,6))
Types &lt;- data.frame(read_excel(&quot;Y:/LSmeans_YV_.xlsx&quot;,7))
CategTypes &lt;- data.frame(read_excel(&quot;Y:/LSmeans_YV_.xlsx&quot;,8))
bytype &lt;- data.frame(read_excel(&quot;Y:/LSmeans_YV_.xlsx&quot;,9))
probtypes &lt;- data.frame(read_excel(&quot;Y:/LSmeans_YV_.xlsx&quot;,10))

#Plaque_Volume
PB$Gender&lt;- factor(PB$Gender)
PB$Covariates &lt;- factor(PB$Covariates)
PB$Covariates=factor(PB$Covariates,levels(PB$Covariates)[c(2,1)])

g1 &lt;- ggplot(PB, aes(x=Covariates, y=Estimate, fill=Gender))+
geom_bar(stat=&quot;identity&quot;, colour=&quot;black&quot;,position=&quot;dodge&quot;)+ theme_bw()+

geom_errorbar(aes(ymin=Lower,ymax=Upper,width=.25),position=position_dodge(0.9),size=1)+

xlab(&quot;Unadjusted: No covariates \nAdjusted: Covariates DM HTN &amp; Smoking&quot;) + ylab(&quot;Mean&quot;)+
ggtitle(&quot;Comparision between adjusted and unadjusted models \nfor the means of the Total Volume of
Plaque by gender&quot; )
g1

######Volume_Lap
VL$Gender&lt;- factor(VL$Gender)
VL$Covariates &lt;- factor(VL$Covariates)
VL$Covariates=factor(VL$Covariates,levels(VL$Covariates)[c(2,1)])

g2 &lt;- ggplot(VL, aes(x=Covariates, y=Estimate, fill=Gender)) +
geom_bar(stat=&quot;identity&quot;,colour=&quot;black&quot;,position=&quot;dodge&quot;)+ theme_bw() +
geom_errorbar(aes(ymin=Lower,ymax=Upper,width=.25),position=position_dodge(0.9),size=1)+

xlab(&quot;Unadjusted: No covariates \nAdjusted: Covariates DM HTN &amp; Smoking&quot;)+ ylab(&quot;Mean&quot;)+
ggtitle(&quot;Comparision between adjusted and unadjusted models \nfor the means of the Volume of Lap
by gender&quot; )
g2

######Plaque_Number
PN$Gender&lt;- factor(PN$Gender)
PN$Covariates &lt;- factor(PN$Covariates)

PN$Covariates=factor(PN$Covariates,levels(PN$Covariates)[c(2,1)])

g3 &lt;- ggplot(PN, aes(x=Covariates, y=Exponentiated, fill=Gender)) +
geom_bar(stat=&quot;identity&quot;, colour=&quot;black&quot;,position=&quot;dodge&quot;)+ theme_bw()+
geom_errorbar(aes(ymin=Lower, ymax= Upper, width=.25),position=position_dodge(0.9),size=1)

xlab(&quot;Unadjusted: No covariates \nAdjusted: Covariates DM HTN &amp; Smoking&quot;)+ ylab(&quot;Mean&quot;)+
ggtitle(&quot; Comparision between adjusted and unadjusted models \n for the means of the Total
Number of Plaques by gender&quot; )
g3

######Remodeled_Index
REMOD$Gender&lt;- factor(REMOD$Gender)
REMOD$Covariates &lt;- factor(REMOD$Covariates)
REMOD$Covariates &lt;- factor(REMOD$Covariates,levels(REMOD$Covariates)[c(2,1)])

g4 &lt;- ggplot(REMOD, aes(x=Covariates, y=ExpEstimate , fill=Gender)) +
geom_bar(stat=&quot;identity&quot;, colour=&quot;black&quot;,position=&quot;dodge&quot;)+ theme_bw() +
geom_errorbar(aes(ymin=Lower,ymax= Upper, width=.25),position=position_dodge(0.9),size=1)+

xlab(&quot;Unadjusted: No covariates \n Adjusted: Covariates DM HTN &amp; Smoking&quot;)+ ylab(&quot;Mean&quot;)+
ggtitle(&quot;Comparision between adjusted and unadjusted models \n for the means of the Remodeled
Index of the Plaques by gender&quot; )
g4

######Positive_Remodeled#

PosREMOD$Gender&lt;- factor(PosREMOD$Gender)
PosREMOD$Covariates &lt;- factor(PosREMOD$Covariates)
PosREMOD$Covariates &lt;- factor(PosREMOD$Covariates,levels(PosREMOD$Covariates)[c(2,1)])

g5 &lt;- ggplot(PosREMOD, aes(x=Covariates, y=Probability , fill=Gender)) +
geom_bar(stat=&quot;identity&quot;, colour=&quot;black&quot;,position=&quot;dodge&quot;)+ theme_bw()+
geom_errorbar(aes(ymin=Lower, ymax=Upper, width=.25),position=position_dodge(0.9),size=1)+

xlab(&quot;Unadjusted: No covariates (female = 75.5% &amp; male =74%) \n Adjusted: Covariates DM HTN &amp;
Smoking (female = 80.2% &amp; male =79.2%)&quot;)+ ylab(&quot;Probability Positive Remodeled Plaque&quot;)+
ggtitle(&quot;Comparison between adjusted and unadjusted models \n for the Probability of being Positive
Remodeled by gender&quot; )
g5

######OddsRatio of Types
Types$Gender &lt;- factor(Types$Gender)
Types$Covariates &lt;- factor(Types$Covariates)
Types$Covariates &lt;- factor(Types$Covariates, levels(Types$Covariates)[c(2,1)])

g7 &lt;- ggplot(Types, aes(x=Covariates, y=Estimate, fill = type )) +
geom_bar(stat=&quot;identity&quot;, colour=&quot;black&quot;,position=&quot;dodge&quot;)+ theme_bw() + geom_hline(yintercept =
1)+
geom_errorbar(aes(ymin=Lower, ymax=Upper, width=.25),position=position_dodge(0.9),size=1)+

xlab(&quot;Odds Ratio&quot;) +ylab(&quot;Odds Ratio&quot;)+
ggtitle(&quot;Comparision between adjusted and unadjusted models \n for the odds of the logits of every
type of plaque&quot; )

g7

######CategTypes
CategTypes$Covariates&lt;- factor(CategTypes$Covariates)
CategTypes$Comparisions&lt;- factor(CategTypes$Comparisions)
CategTypes$Covariates &lt;- factor(CategTypes$Covariates, levels(CategTypes$Covariates)[c(2,1)])

g8 &lt;- ggplot(CategTypes, aes(x=Covariates, y=Odds, fill = Comparisions))+
geom_bar(stat=&quot;identity&quot;, colour=&quot;black&quot;,position=&quot;dodge&quot;)+ theme_bw()+
geom_errorbar(aes(ymin=Lower, ymax=Upper, width=.25),
position=position_dodge(0.9),size=1)+geom_errorbar(aes(ymin=ALower, ymax=AUpper,
width=.25),color =&quot;red&quot;, position=position_dodge(0.9),size=1)xlab(&quot;Unadjusted: No covariates \n
Adjusted: Covariates DM HTN &amp; Smoking&quot;) +

xlab() + ylab(&quot;Mean&quot;)+
ggtitle(&quot;Comparision between adjusted and unadjusted models \n for the pairwise comparisions of four
different types of plaque\n CI in Red: Stepdown Bonferroni&quot; )
g8

####Probabilities of type_x bytype#
bytype$Covariates&lt;- factor(bytype$Covariates)
bytype$Type&lt;- factor(bytype$Type)
bytype$Gender&lt;- factor(bytype$Gender)
bytype$Covariates &lt;- factor(bytype$Covariates, levels(bytype$Covariates)[c(2,1)])

g9 &lt;- ggplot(bytype, aes(x=Covariates, y=Probability, fill = Gender ))+ facet_wrap(~Type)+
geom_bar(stat=&quot;identity&quot;, colour=&quot;black&quot;,position=&quot;dodge&quot;)+ theme_bw() +

geom_errorbar(aes(ymin=Lower, ymax=Upper, width=.25), position=position_dodge(0.9),size=1)+

xlab(&quot;Unadjusted: No covariates \n Adjusted: Covariates DM HTN &amp; Smoking&quot;) + ylab(&quot;Mean&quot;)+
ggtitle(&quot;Probability of having a type comparision between adjusted and unadjusted models by
gender\n CI in Red: Stepdown Bonferroni&quot; )
g9
