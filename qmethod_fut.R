
###
### Packs and environments ----
###

source("https://raw.githubusercontent.com/temporalista/r-essentials/master/packloader.R")


ipak(c("qmethod", 
       "readxl",
       "factoextra",
       "dplyr"))

###
### Input data: statements, sorts and participants ----
###
#update you working directory accordingly


source("my_qmb_plot.R")


qstatements <- read_xlsx("datos/redu_fut_metodoq_public.xlsx", sheet = "1 Qset")
qparticip <- read_xlsx("datos/redu_fut_metodoq_public.xlsx", sheet = "2 Qparticipantes")
qsort <- as.data.frame(read_xlsx("datos/redu_fut_metodoq_public.xlsx", sheet = "3 Qsort"))

#Name rows and remove first column
rownames(qsort) <- qsort[,1]
qsort <- qsort[,-1]

###
### Run Q method --------
###


##Screeplot to check the variances
qsort.pca <- prcomp(qsort)
screeplot(qsort.pca, 
          main = "Gráfico de sedimentación", 
          type = "lines")

get_eig(qsort.pca)

par(mar = c(15, 5, 5, 5))

fviz_eig(qsort.pca, addlabels=TRUE, hjust = -0.1,
         choice = c("variance"),
         main = "",
         ylab="Varianza Explicada (%)",
         xlab="Factor",
         barfill = "gray",
         barcolor = "gray"
)


qresults5 <- qmethod(qsort, 
                     nfactors = 5, 
                     rotation = 'varimax',
                     forced = T,
                     cor.method = "pearson")

qresults4 <- qmethod(qsort, 
                     nfactors = 4, 
                     rotation = 'varimax',
                     forced = T,
                     cor.method = "pearson")

qresults3 <- qmethod(qsort, 
                     nfactors = 3, 
                     rotation = 'varimax',
                     forced = T,
                     cor.method = "pearson")

qresults2 <- qmethod(qsort, 
                     nfactors = 2, 
                     rotation = 'varimax',
                     forced = T,
                     cor.method = "pearson")


q5.sum <- summary(qresults5)
q4.sum <- summary(qresults4)
q3.sum <- summary(qresults3)
q2.sum <- summary(qresults2)


##Characteristics of the factors: 
##av_rel_coef: Average reliability coefficient
##nload: number of loading Qsorts
##Eigenvalues
##expl_var: percentage of explained variance
##composite reliability
##Sandard error of factor scores

qresults4$f_char$characteristics
qresults3$f_char$characteristics
qresults2$f_char$characteristics


#decide number of factors
nf=3
qresults <- qresults3

###plotting a diagram of z-scores for each statement
par(lwd = 1.5, mar = c(4, 4, 1, 1) + 0.1)

plot(qresults, 
     xlab = 'z-scores', 
     ylab = 'Declaraciones', 
     pchlist = NULL, 
     colours = c("red","green","blue"), 
     fnames = NULL, 
     legend = TRUE,
     dist = TRUE, 
     pchlist.fill = NULL,
     leg.pos="bottomright", 
     xlim= NULL, 
     sort.items=T, 
     factors = NULL)

abline(v=0, col='grey')


#### Merge and explore factor results ---- 
my.qresults <- cbind(round(qresults$zsc, digits=2), qresults$zsc_n)
nfactors <- ncol(qresults$zsc)
# col.order <- as.vector(rbind(1:nfactors, (1:nfactors)+nfactors))
# my.qresults <- my.qresults[col.order]
my.qresults$sta_id<- rownames(my.qresults)

# merge original statements
my.qresults <- merge(qstatements[,1:2],my.qresults, by="sta_id")

#merge distinguish and concensus
qresults$qdc$sta_id<- rownames(qresults$qdc)
my.qresults <- merge(my.qresults,qresults$qdc, by="sta_id")
my.qresults <- my.qresults %>% rename(dist_cons=dist.and.cons)


#View ordered results for each factor
for (i in 1:nfactors) {
  View(my.qresults[order(my.qresults[5+i], decreasing = TRUE), ],
       title = paste0("Order for f", i))
}


#### Explore the table of distinguishing and consensus statements


# Consensus statements
my.qresults[which(my.qresults$dist_cons == "Consensus"), ]

# Statements distinguishing all factors
my.qresults[which(my.qresults$dist_cons == "Distinguishes all"), ]

# Statements distinguishing each factor (for results of > 2 factors)
my.qresults[which(my.qresults$dist_cons == "Distinguishes f1 only"), ]
my.qresults[which(my.qresults$dist_cons == "Distinguishes f2 only"), ]
my.qresults[which(my.qresults$dist_cons == "Distinguishes f3 only"), ]


### Loadings and flags ----
###Sorts are flagged (*) if they have a significant load (p<0.05) and the
###squared loading is larger than the sum of squared diffs

my.lof <-loa.and.flags(qresults)
my.lof$p_code <- rownames(my.lof)

my.lof <- merge(my.lof, qparticip, by="p_code")

View(my.lof)

###export all results if needed
# export.qm(qresults, file = paste("outputs/qreport_",nf,"f.txt", sep=""), style = "R")
# write.csv(my.qresults, file = paste("outputs/qresults_",nf,"f.csv", sep=""))
# write.csv(my.lof, file = paste("outputs/loadsandflags_",nf,"f.csv", sep=""), row.names=FALSE)


# --------------
# Bootstrapping ----
#
qb3 <- qmboots(qsort,
               nfactors = nf,
               nsteps=2000,
               load = "auto",
               rotation = "varimax",
               indet = "qindtest",
               fsi = TRUE,
               forced = T,
               distribution = NULL,
               cor.method="pearson")

save(qb3, file = paste("outputs/qbstrp_results_",nf,"f.RData", sep = ""))
load("outputs/qbstrp_results_ 3f.RData")
qstatements <- read_xlsx("inputs/redu_fut_metodoq_public.xlsx", sheet = "1 Qset")
nfactors=3
source("my_qmb_plot.R")

qmb.sum <- qmb.summary(qb3)
qmb.sum$statements$sta_id<- rownames(qmb.sum$statements)
qmb.sum$statements <- merge(qmb.sum$statements,qstatements[,1:2], by="sta_id")
qmb_stat <- qmb.sum$statements
qb3$orig.res$qdc$sta_id<- rownames(qb3$orig.res$qdc)
qmb_stat <-cbind(qmb_stat, dst_cons=qb3$orig.res$qdc[,(1)])

qmb_part <- qmb.sum$qsorts
qmb_part$p_code <- rownames(qmb_part)

# View(qmb_stat)
# View(qmb_part)


my.qmb.plot(qmbsum=qmb.sum,
            type = "zsc",
            nfactors=3,
            cex = 0.6,
            cex.leg = 1,
            errbar.col = rgb(0.4,0.4,0.4),
            errbar.lwd = 0.5,
            errbar.lty = "solid",
            leg.pos = "bottomleft",
            sort="difference",
            plot.std=F,
            plot.mark=T,
            ylabels=paste(qmb.sum$statements$sta_id, qmb.sum$statements$Declaracion),
            vertdist = 0.23,
            xlab= "Puntajes Z",
            ylab="",
            col=c("black","black","black"),
            pch=c(21,22,24,23),
            limits = c(-2.4,2.4),
)


my.qmb.plot(qmbsum=qmb.sum,
            type = "loa", 
            # main="Puntajes Z de las declaraciones",
            nfactors=3,
            cex = 0.6,
            cex.leg = 0.9,
            errbar.col = rgb(0.4,0.4,0.4), 
            errbar.lwd = 0.5, 
            errbar.lty = "solid",
            leg.pos = "bottomleft",
            sort="difference",
            limits = c(-0.5,1),
            plot.std=F,
            plot.mark=T,
            vertdist = 0.23,
            # limits = c(-2.4, 2.5),
            xlab= "Puntajes Z",
            ylab="Participantes",
            col=c("black","black","black"),
            # col.lab="black",
            pch=c(21,22,24,23)
            # pt.cex=c(0.8,0.8,0.8)
)


qmb_part$m1 <- (qmb_part$flag.freq1 >0.75)
qmb_part$m2 <- (qmb_part$flag.freq2 >0.75)
qmb_part$m3 <- (qmb_part$flag.freq3 >0.75)
qmb_part$mq <- qmb_part$m1 + qmb_part$m2 + qmb_part$m3

table(qmb_part$mq)

qb3$fsi
qb3$`zscore-stats`$`Bootstraped factor scores`
qb3f1 <- qb3$`zscore-stats`$factor1[,c("mean", "sd")] 
qb3f2 <- qb3$`zscore-stats`$factor2[,c("mean", "sd")]
qb3f3 <- qb3$`zscore-stats`$factor3[,c("mean", "sd")]

qb3_fscores <- merge(qb3$`zscore-stats`$factor1[,c("mean", "sd")],
                     qb3$`zscore-stats`$factor2[,c("mean", "sd")])

# 
# write.csv(qmb_stat, 
#           file = paste("outputs/qbfscores_",nf,"f.csv", sep=""))
# 
# write.csv(qmb_part, 
#           file = paste("outputs/qbfpartic_",nf,"f.csv", sep=""))
# 
# write.csv(qb3[["zscore-stats"]][["factor1"]], 
#           file = paste("output_20191017_bootstrap/qbfscores_f1",nf,"f.txt", sep=""))


