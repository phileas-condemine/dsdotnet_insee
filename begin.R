library(data.table)
library(dplyr)
library(stringr)
ref=fread("referentiel.csv")


split=strsplit(ref$lib_brut2,split = "separator")

ref$lib_main=sapply(split,function(x){x[1]})

ref$lib_fac=sapply(split,function(x){x[2]})


split=strsplit(ref$lib_main,split = " ")

ref$namesize=sapply(split,function(x){length(x)})

cat=regexpr(pattern = "CATEGORIE",text = ref$lib_main)

ref$cat=unlist(sapply(1:nrow(ref),function(i){if(cat[i]>0){substr(x = ref$lib_main[i],start = cat[i]+10,stop = cat[i]+11)} else "" }))

ref$cat=gsub(pattern = " ",replacement = "",x = ref$cat)

ref$lib_main2=gsub(pattern= "CATEGORIE +[0-9A-Z]",replacement = "",x = ref$lib_main)

db=fread("professions_non_traitees.csv")

#Piste trop floue
# i=sample(1:22024,size = 1)
# for (i in 1:22024){
#   t=agrep(pattern = db$profs_x[i],x = ref$lib_main2)
# if (length(t)>0)
#   print(c(i,db$profs_x[i],ref$lib_main2[t]))
# }

refrare=subset(ref,ref$namesize==1)

refrare$lib_main2[str_length(refrare$lib_main2)==5]


perf=NULL
matched=NULL
for (i in 1:nrow(refrare)){
stem=gsub(pattern = "EURE",replacement = "",x = refrare$lib_main2[i])
stem=gsub(pattern = "EUR",replacement = "",x = stem)
stem=gsub(pattern = "IERE",replacement = "",x = stem)
stem=gsub(pattern = "IER",replacement = "",x = stem)
stem=gsub(pattern = "RICE",replacement = "",x = stem)
stem=gsub(pattern = "ANTE",replacement = "",x = stem)
stem=gsub(pattern = "ANT",replacement = "",x = stem)
stem=gsub(pattern = "AIRE",replacement = "",x = stem)
stem=gsub(pattern = "ISTE",replacement = "",x = stem)
stem=gsub(pattern = "GRAPHE",replacement = "",x = stem)
stem=gsub(pattern = "IENNE",replacement = "",x = stem)
stem=gsub(pattern = "IEN",replacement = "",x = stem)
stem=gsub(pattern = "EUX",replacement = "",x = stem)
stem=gsub(pattern = "EUSE",replacement = "",x = stem)

# easymatch=data.table(x=1:22024,dist=t(adist(x = stem,y = db$profs_x)))
# if((min(easymatch$dist.V1)<=2&str_length(stem)>6)|(min(easymatch$dist.V1)<=1&str_length(stem)>4)|(min(easymatch$dist.V1)==0&str_length(stem)<=3)){
#   print(c(i,stem,db$profs_x[which(easymatch$dist.V1==min(easymatch$dist.V1))]))
#   perf=c(perf,i)
#   matched=c(matched,which(easymatch$dist.V1==min(easymatch$dist.V1)))
# }
perf=c(perf,i)
print(c(i,stem,db$profs_x[grep(pattern = stem,x = db$profs_x,value=FALSE)]))
matched=c(matched,grep(pattern = stem,x = db$profs_x,value=FALSE))
}



Ca en trouve 138 avec doublons et à l'oeil on voit qu'il y a bcp de mal classés...

Il faudrait plutôt chercher quand le mot est trouvé exactement... grepl ?


recherche des mots les plus rares et donc les plus discriminants.

Mauvaise piste avec les mots uniques dans le ref parce qu'apprentissage sur un échantillon subjectif : un "EDF" appartient à une catégorie mais si on
entre dans les détails alors on peut avoir un agent EDF et comtable EDF avec libellés différents...

On peut quand même regarder le score associé...


# syn=fread("synonymes.csv")
