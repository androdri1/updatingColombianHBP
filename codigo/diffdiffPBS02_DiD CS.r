# @Desc: Este es para el Calawy-Santata DiD con multiples periodos
# ver: https://cran.r-project.org/web/packages/did/vignettes/did-basics.html
library(did)
library(haven)
library(ggplot2)
#if(!require(devtools)) install.packages("devtools")
library("cowplot")

setwd("C:/Users/paul.rodriguez/Dropbox/Universidad del Rosario/Cross/IETS/Actualizacion PBS/codigo/datos")
#setwd("D:/Paul.Rodriguez/Dropbox (Personal)/Universidad del Rosario/Cross/IETS/Actualizacion PBS/codigo/datos")

actualizacionPBS <- read_dta("actualizacionPBS.dta")

nrow(actualizacionPBS)
str(actualizacionPBS)

actualizacionPBS$treat= actualizacionPBS$inclusionpbsyseguimiento

# Base de datos con solo las tecnologias que tienen al menos un usuario por año
actualizacionPBShabitual = subset(actualizacionPBS,nonzeros==8)

# Con al menos un usuario =========================================================

all.pun_attgt <- att_gt(yname = "nonzero", 
                        tname = "year",
                        idname = "idtecnologia",
                        gname = "G",
                        xformla = ~1,
                        data = actualizacionPBS #,group="notyettreated"
)
summary(all.pun_attgt) # Hummmm, tenemos lios con las tendencias paralelas


# Como un event study
all.pun.es <- aggte(all.pun_attgt, type = "dynamic")
summary(all.pun.es)

all.pun.ph=ggdid(all.pun.es)+geom_hline(yintercept=0)+ theme_bw()
all.pun.ph$labels$title=""
all.pun.ph$labels$x="Years since inclusion into HBP"
all.pun.ph$labels$y="Coefficient"

pdf("../../results/figures/CS_OneUser.pdf",width = 5, height= 2.5)
print(all.pun.ph)
ggsave("../../results/figures/CS_OneUser.png",plot=all.pun.ph,width = 5, height= 2.5)
dev.off()




# Con Personas Unicas =========================================================

all.pun_attgt <- att_gt(yname = "logpunicasmill", # "punicasmill",
                        tname = "year",
                        idname = "idtecnologia",
                        gname = "G",
                        xformla = ~1,
                        data = actualizacionPBShabitual #,group="notyettreated"
)
summary(all.pun_attgt) # Hummmm, tenemos lios con las tendencias paralelas


# Como un event study
all.pun.es <- aggte(all.pun_attgt, type = "dynamic")
summary(all.pun.es)

all.pun.ph=ggdid(all.pun.es)+geom_hline(yintercept=0)+ theme_bw()+ylim(-0.2, 4)
all.pun.ph$labels$title=""
all.pun.ph$labels$x="Years since inclusion into HBP"
all.pun.ph$labels$y="Coefficient"

pdf("../../results/figures/CS_usuarios.pdf",width = 5, height= 2.5)
all.pun.ph
dev.off()


# Con Frecuencias =============================================================

all.freq_attgt <- att_gt(yname = "logfrecuencia", # "frecuenciamill",
                        tname = "year",
                        idname = "idtecnologia",
                        gname = "G",
                        xformla = ~1,
                        data = actualizacionPBShabitual #,group="notyettreated"
)
summary(all.freq_attgt) # Hummmm, tenemos lios con las tendencias paralelas

# Como un event study
all.freq.es <- aggte(all.freq_attgt, type = "dynamic")
summary(all.freq.es)


all.freq.ph=ggdid(all.freq.es)+geom_hline(yintercept=0)+ theme_bw()+ylim(-0.2, .5)
all.freq.ph$labels$title=""
all.freq.ph$labels$x="Years since inclusion into HBP"
all.freq.ph$labels$y="Coefficient"

pdf("../../results/figures/CS_frecuencias.pdf",width = 5, height= 2.5)
all.freq.ph
dev.off()




# Con Gasto por tecnologia ====================================================

all.gpc_attgt <- att_gt(yname = "loggastoper", # "gastopermill",
                        tname = "year",
                        idname = "idtecnologia",
                        gname = "G",
                        xformla = ~1,
                        data = actualizacionPBShabitual #,group="notyettreated"
)
summary(all.gpc_attgt) # Hummmm, tenemos lios con las tendencias paralelas


# Como un event study
all.gpc.es <- aggte(all.gpc_attgt, type = "dynamic")
summary(all.gpc.es)

all.gpc.ph=ggdid(all.gpc.es)+geom_hline(yintercept=0)+ theme_bw()+ylim(-1.2, 1)
all.gpc.ph$labels$title=""
all.gpc.ph$labels$x="Years since inclusion into HBP"
all.gpc.ph$labels$y="Coefficient"

pdf("../../results/figures/CS_gasto.pdf",width = 5, height= 2.5)
all.gpc.ph
dev.off()



# Con uso alejado y especial ===================================================

all.ale_attgt <- att_gt(yname = "alejadayespecial", 
                        tname = "year",
                        idname = "idtecnologia",
                        gname = "G",
                        xformla = ~1,
                        data = actualizacionPBS #,group="notyettreated"
)
summary(all.ale_attgt) # Hummmm, tenemos lios con las tendencias paralelas


# Como un event study
all.ale.es <- aggte(all.ale_attgt, type = "dynamic")
summary(all.ale.es)

all.ale.ph=ggdid(all.ale.es)+geom_hline(yintercept=0)+ theme_bw()+ylim(-0.2, 1)
all.ale.ph$labels$title=""
all.ale.ph$labels$x="Years since inclusion into HBP"
all.ale.ph$labels$y="Coefficient"

pdf("../../results/figures/CS_alejada.pdf",width = 5, height= 2.5)
all.ale.ph
dev.off()


# H1. Procedimientos solamente ==================================================

procedinze_attgt <- att_gt(yname = "nonzero", 
                           tname = "year",
                           idname = "idtecnologia",
                           gname = "G",
                           xformla = ~1,
                           data = subset(actualizacionPBS, tipotecnologia==1 )  #,group="notyettreated"
)
procedipun_attgt <- att_gt(yname = "logpunicasmill", 
                                tname = "year",
                                idname = "idtecnologia",
                                gname = "G",
                                xformla = ~1,
                                data = subset(actualizacionPBShabitual, tipotecnologia==1 )  #,group="notyettreated"
)
procedigpc_attgt <- att_gt(yname = "loggastoper", # "gastopermill",
                                tname = "year",
                                idname = "idtecnologia",
                                gname = "G",
                                xformla = ~1,
                                data = subset(actualizacionPBShabitual, tipotecnologia==1 )  #,group="notyettreated"
)

# Como un event study
procedi.nze <- aggte(procedinze_attgt, type = "dynamic")
procedi.pun <- aggte(procedipun_attgt, type = "dynamic")
procedi.gpc <- aggte(procedigpc_attgt, type = "dynamic")
summary(procedi.nze)
summary(procedi.pun)
summary(procedi.gpc)


procedi.nze.ph=ggdid(procedi.nze)+geom_hline(yintercept=0)+ theme_bw()+
  ylim(-0.25, 1)+theme(legend.position = "none")
procedi.nze.ph$labels$title="At least one user"
procedi.nze.ph$labels$x="Years since inclusion into HBP"
procedi.nze.ph$labels$y="Coefficient"

procedi.pun.ph=ggdid(procedi.pun)+geom_hline(yintercept=0)+ theme_bw()+
  ylim(-3, 4)
procedi.pun.ph$labels$title="Unique users per million affiliates"
procedi.pun.ph$labels$x="Years since inclusion into HBP"
procedi.pun.ph$labels$y="Coefficient"

procedi.gpc.ph=ggdid(procedi.gpc)+geom_hline(yintercept=0)+ theme_bw()+
  ylim(-3, 4)+theme(legend.position = "none")
procedi.gpc.ph$labels$title="Expenditure per individual"
procedi.gpc.ph$labels$x="Years since inclusion into HBP"
procedi.gpc.ph$labels$y="Coefficient"

pdf("../../results/figures/CS_procedi.pdf",width = 7, height= 4)
plot_grid(procedi.nze.ph,procedi.pun.ph, procedi.gpc.ph, ncol = 2, nrow = 2, rel_widths=c(1,1.2))
dev.off()







# H3. Medicamentos =============================================================
# Basic vars..................

medicamentosnze_attgt <- att_gt(yname = "nonzero",
                                tname = "year",
                                idname = "idtecnologia",
                                gname = "G",
                                xformla = ~1,
                                data = subset(actualizacionPBS, tipotecnologia==0 )  #,group="notyettreated"
)
medicamentospun_attgt <- att_gt(yname = "logpunicasmill", 
                                tname = "year",
                                idname = "idtecnologia",
                                gname = "G",
                                xformla = ~1,
                                data = subset(actualizacionPBShabitual, tipotecnologia==0 )  #,group="notyettreated"
)
medicamentosgpc_attgt <- att_gt(yname = "loggastoper", 
                                tname = "year",
                                idname = "idtecnologia",
                                gname = "G",
                                xformla = ~1,
                                data = subset(actualizacionPBShabitual, tipotecnologia==0 )  #,group="notyettreated"
)

# Como un event study
medi.nze <- aggte(medicamentosnze_attgt, type = "dynamic")
medi.pun <- aggte(medicamentospun_attgt, type = "dynamic")
medi.gpc <- aggte(medicamentosgpc_attgt, type = "dynamic")
summary(medi.nze)
summary(medi.pun)
summary(medi.gpc)

medi.nze.ph=ggdid(medi.nze)+geom_hline(yintercept=0)+ theme_bw()+
  ylim(-0.25, 1)+theme(legend.position = "none")
medi.nze.ph$labels$title="At least one user"
medi.nze.ph$labels$x="Years since inclusion into HBP"
medi.nze.ph$labels$y="Coefficient"

medi.pun.ph=ggdid(medi.pun)+geom_hline(yintercept=0)+ theme_bw()+
  ylim(-3, 4)
medi.pun.ph$labels$title="Unique users per million affiliates"
medi.pun.ph$labels$x="Years since inclusion into HBP"
medi.pun.ph$labels$y="Coefficient"

medi.gpc.ph=ggdid(medi.gpc)+geom_hline(yintercept=0)+ theme_bw()+
  ylim(-3, 4)+theme(legend.position = "none")
medi.gpc.ph$labels$title="Expenditure per individual"
medi.gpc.ph$labels$x="Years since inclusion into HBP"
medi.gpc.ph$labels$y="Coefficient"

pdf("../../results/figures/CS_medi.pdf",width = 7, height= 4)
plot_grid(medi.nze.ph,medi.pun.ph, medi.gpc.ph, ncol = 2, nrow = 2, rel_widths=c(1,1.2))
dev.off()


# Un unico resultado
medi.simple.pun <- aggte(medicamentospun_attgt, type = "simple")
medi.simple.gpc <- aggte(medicamentosgpc_attgt, type = "simple")
summary(medi.simple.pun)
summary(medi.simple.gpc)


# Only medications spec vars ................
# IHH del ATC5
# Share institucional del ATC5 
#... humm, los missing son un l?o

all.sharei_attgt <- att_gt(yname = "share_insti_atc5", 
                            tname = "year",
                            idname = "idtecnologia",
                            gname = "G",
                            xformla = ~1,
                            data = subset(actualizacionPBShabitual, year>2012 ) #,group="notyettreated"
)
all.IHHatc5_attgt <- att_gt(yname = "both_IHH_atc5_bi", 
                            tname = "year",
                            idname = "idtecnologia",
                            gname = "G",
                            xformla = ~1,
                            data = subset(actualizacionPBShabitual, year>2012 ) #,group="notyettreated"
)

summary(all.IHHatc5_attgt) # Hummmm, tenemos lios con las tendencias paralelas
summary(all.sharei_attgt) # Hummmm, tenemos lios con las tendencias paralelas


# Como un event study
IHHatc5.es <- aggte(all.IHHatc5_attgt, type = "dynamic")
sharei.es <- aggte(all.sharei_attgt, type = "dynamic")
summary(IHHatc5.es)
summary(sharei.es)

# Como un event study

IHHatc5.ph=ggdid(IHHatc5.es)+geom_hline(yintercept=0)+
  theme_bw()+theme(legend.position = "none")
IHHatc5.ph$labels$title="IHH"
IHHatc5.ph$labels$x="Years since inclusion into HBP"
IHHatc5.ph$labels$y="Coefficient"


sharei.ph=ggdid(sharei.es)+geom_hline(yintercept=0)+ theme_bw()
sharei.ph$labels$title="Institutional Share"
sharei.ph$labels$x="Years since inclusion into HBP"
sharei.ph$labels$y="Coefficient"


pdf("../../results/figures/CS_medi2.pdf",width = 7, height= 2.5)
plot_grid(IHHatc5.ph, sharei.ph, ncol = 2, nrow = 1, rel_widths=c(1,1.2))
dev.off()

# H2. HTA (IETS) =======================================================================

ietsnze_attgt <- att_gt(yname = "nonzero", 
                           tname = "year",
                           idname = "idtecnologia",
                           gname = "G",
                           xformla = ~1,
                           data = subset(actualizacionPBS, tipotecnologia==1 )  #,group="notyettreated"
)
ietspun_attgt <- att_gt(yname = "logpunicasmill", # "punicasmill",
                        tname = "year",
                        idname = "idtecnologia",
                        gname = "Giets",
                        xformla = ~1,
                        print_details = TRUE,
                        data = subset(actualizacionPBShabitual, !(ietsAlguna==0 & pbsAlguna==1) & !is.na(Giets) )  #,group="notyettreated"
)
ietsgpc_attgt <- att_gt(yname = "loggastoper", # "gastopermill",
                        tname = "year",
                        idname = "idtecnologia",
                        gname = "Giets",
                        xformla = ~1,
                        data = subset(actualizacionPBShabitual, !(ietsAlguna==0 & pbsAlguna==1) & !is.na(Giets) )  #,group="notyettreated"
)


# Como un event study
iets.nze <- aggte(ietsnze_attgt, type = "dynamic")
iets.pun <- aggte(ietspun_attgt, type = "dynamic")
iets.gpc <- aggte(ietsgpc_attgt, type = "dynamic")
summary(iets.nze)
summary(iets.pun)
summary(iets.gpc)

iets.nze.ph=ggdid(iets.nze)+geom_hline(yintercept=0)+ theme_bw()+
  theme(legend.position = "none")
iets.nze.ph$labels$title="Unique users per million affiliates"
iets.nze.ph$labels$x="At least one user"
iets.nze.ph$labels$y="Coefficient"

iets.pun.ph=ggdid(iets.pun)+geom_hline(yintercept=0)+ theme_bw()
iets.pun.ph$labels$title="Unique users per million affiliates"
iets.pun.ph$labels$x="Years since inclusion into HBP"
iets.pun.ph$labels$y="Coefficient"

iets.gpc.ph=ggdid(iets.gpc)+geom_hline(yintercept=0)+ theme_bw()+
  theme(legend.position = "none")
iets.gpc.ph$labels$title="Expenditure per individual"
iets.gpc.ph$labels$x="Years since inclusion into HBP"
iets.gpc.ph$labels$y="Coefficient"

pdf("../../results/figures/CS_iets.pdf",width = 7, height= 4)
plot_grid(iets.nze.ph,iets.pun.ph, iets.gpc.ph, ncol = 2, nrow = 2, rel_widths=c(1,1.2))
dev.off()


