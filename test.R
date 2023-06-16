library(VennDiagram)
library(grid)
library(glue)
source("./vennLabel.R")

RNA_list <- readRDS(file="data/RNA_list.rds")

asub.list <- list(
  c("podo", "nephron"),
  c("pt", "epi"),
  c("epi", "nephron", "pt"),
  c("epi", "nephron", "pt", "podo"),
  c("epi", "nephron", "pt", "podo", "pt_podo")
  )
pdf("data/plot.pdf")
for(a_sub in asub.list){
  v0 <- venn.label.diagram(RNA_list[a_sub], filename = NULL)
  grid.draw(v0)
  grid.newpage()
}
dev.off()


