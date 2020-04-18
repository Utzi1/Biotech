pkgname <- "Biotech"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('Biotech')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("CellVit")
### * CellVit

flush(stderr()); flush(stdout())

### Name: CellVit
### Title: Compute the cell vitality
### Aliases: CellVit

### ** Examples

# we use a real dataset:
us <- c(55, 65, 49, 57)
st <- c(11, 12, 9, 6)

CellVit(unstained = us, stained = st)




cleanEx()
nameEx("Eadie_Hofstee")
### * Eadie_Hofstee

flush(stderr()); flush(stdout())

### Name: Eadie_Hofstee
### Title: Draw an Eadie-Hofstee graph (and compute the y-axis intercept)
### Aliases: Eadie_Hofstee

### ** Examples

# simulate values
sub <-seq(1,20,1)
vel <-((runif(1,14.7,15)*sub)/(runif(1,2.5,3)+sub))+rnorm(20,0,.3)
# plot them
Eadie_Hofstee(vel = vel, sub = sub)






cleanEx()
nameEx("MultiRate")
### * MultiRate

flush(stderr()); flush(stdout())

### Name: MultiRate
### Title: Compute the cell vitality after frosting
### Aliases: MultiRate

### ** Examples

# original data is going to be applied
ud  <- c(12, 22, 15, 17)
u24 <-  c(9, 3, 5, 4)

MultiRate(unstained_defro =ud, unstained_24h = u24)




cleanEx()
nameEx("VDi")
### * VDi

flush(stderr()); flush(stdout())

### Name: VDi
### Title: Compute the dispersion volume mind the units
### Aliases: VDi

### ** Examples

VDi(Ai = 0.45, HDi = 1.33)



cleanEx()
nameEx("c.any")
### * c.any

flush(stderr()); flush(stdout())

### Name: c.any
### Title: Compute the concentration of the source salts for specific
###   elements inside a fermentationbroth
### Aliases: c.any

### ** Examples


# c.any(ds = 50, ele = 'P', org = 'Fungus')
# c.any(ds = 50, ele = "N", org = 'Fungus')



cleanEx()
nameEx("dilfact")
### * dilfact

flush(stderr()); flush(stdout())

### Name: dilfact
### Title: Computing a dilution factor, a sample volume (Vorlagevolumen),
###   concentration after dilution Compute the dilution factor
### Aliases: dilfact

### ** Examples

dilfact(1, 100)



cleanEx()
nameEx("eG")
### * eG

flush(stderr()); flush(stdout())

### Name: eG
### Title: Compute gas content Keep an eye on the units
### Aliases: eG

### ** Examples

eG( VG = c(1, 2, 3), VL = c(6, 12, 24) )



cleanEx()
nameEx("velG.cal")
### * velG.cal

flush(stderr()); flush(stdout())

### Name: velG.cal
### Title: compute the actual gas flow (eliminate the error of the
###   rotameter) of either the big airlift reactor or the smaller one used
###   at the Biotechnikum at Forum Seestra<c3><9f>e
### Aliases: velG.cal

### ** Examples

velG.cal( velG.mes = 1, p.mes = 1.024, t.mes = 299.9, reactor = "small" )



cleanEx()
nameEx("vitality")
### * vitality

flush(stderr()); flush(stdout())

### Name: vitality
### Title: Vitality of a cell population
### Aliases: vitality

### ** Examples

vitality(stained = 3, unstained = 77)



cleanEx()
nameEx("vitrate")
### * vitrate

flush(stderr()); flush(stdout())

### Name: vitrate
### Title: Vitality rate of a cell population
### Aliases: vitrate

### ** Examples

vitrate(unstained_prefro = 33, unstained_defro = 12)



cleanEx()
nameEx("yield")
### * yield

flush(stderr()); flush(stdout())

### Name: yield
### Title: Compute yield count
### Aliases: yield

### ** Examples

# original data is going to be applied
uP  <- c(88, 73, 72, 97)
u24 <-  c(9, 3, 5, 4)
yield(unstained_prefro = uP, unstained_24h = u24)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
