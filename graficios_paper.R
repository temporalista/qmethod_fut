
### Código reporducible para las figuras del artículo científico
###

source("https://raw.githubusercontent.com/temporalista/r-essentials/master/packloader.R")

ipak(c("qmethod",
       "readxl",
       "factoextra"))

source("my_qmb_plot.R")

load("outputs//qbstrp_results_ 3 f.RData")
qstatements <- read_xlsx("inputs/redu_fut_metodoq_public.xlsx", sheet = "1 Qset")
qmb.sum <- qmb.summary(qb3)

qmb.sum$statements$sta_id<- rownames(qmb.sum$statements)
qmb.sum$statements <- merge(qmb.sum$statements,qstatements[,1:2], by="sta_id")
qmb_stat <- qmb.sum$statements
qb3$orig.res$qdc$sta_id<- rownames(qb3$orig.res$qdc)
qmb_stat <-cbind(qmb_stat, dst_cons=qb3$orig.res$qdc[,(1)])

qmb_part <- qmb.sum$qsorts
qmb_part$p_code <- rownames(qmb_part)

windowsFonts(serif=windowsFont("Adobe Garamond Pro"))



#--- Figura 3 ----
qsort <- as.data.frame(read_xlsx("inputs/redu_fut_metodoq_public.xlsx", sheet = "3 Qsort"))

#Name rows and remove first column
rownames(qsort) <- qsort[,1]
qsort <- qsort[,-1]


qsort.pca <- prcomp(qsort)

plotf <- function(){
  fviz_eig(qsort.pca, addlabels=TRUE, hjust = -0.1,
         choice = c("variance"),
         main = "",
         ylab="Varianza Explicada (%)",
         xlab="Factor",
         barfill = "gray",
         barcolor = "gray",
         ticks = FALSE,
         
         ggtheme = theme_bw() +
           theme(axis.line = element_line(colour = "black"),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank())
         )
  }


png(filename="outputs/Figura 3 screeplot.png", 
    type="cairo",
    units="cm", 
    bg = "white",
    width=15, 
    height=12, 
    pointsize=9, 
    res=300,
    family="serif")

plotf()

dev.off()

cairo_pdf(filename="outputs/Figura 3 screeplot.pdf", 
    bg = "white",
    width=7.5, 
    height=6, 
    pointsize=9, 
    family="serif")

plotf()

dev.off()


# Figura 4 ----


plotq <- function(){my.qmb.plot(qmbsum=qmb.sum,
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
            limits = c(-2.2,2.2)
            )
}


png(filename="outputs/Figura 4 qmethod.png", 
    type="cairo",
    units="px", 
    bg = "white",
    width=1500, 
    height=1500, 
    pointsize=10, 
    res=300,
    family="serif")
plotq()
dev.off()

svg(filename="outputs/Figura 4 qmethod.svg", 
    bg = "white",
    width=6, 
    height=6, 
    pointsize=10, 
    family="serif")
plotq()

cairo_pdf(filename="outputs/Figura 4 qmethod.pdf", 
    bg = "white",
    width=6, 
    height=6, 
    pointsize=10, 
    family="serif")
plotq()


dev.off()


# --- Figura 5 participants ----

plotp <- function(){my.qmb.plot(qmbsum=qmb.sum,
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
                                limits = c(-0.4,1),
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
}



png(filename="outputs/Figura 5 participantes.png", 
    type="cairo",
    units="px", 
    bg = "white",
    width=1500, 
    height=1800, 
    pointsize=10, 
    res=300,
    family="serif")

plotp()
dev.off()


svg(filename="outputs/Figura 5 participantes.svg", 
    bg = "white",
    width=6, 
    height=7.2, 
    pointsize=10, 
    family="serif")

plotp()

dev.off()



cairo_pdf(filename="outputs/Figura 5 participantes.pdf", 
    bg = "white",
    width=6, 
    height=7.2, 
    pointsize=10, 
    family="serif")

plotp()

dev.off()
