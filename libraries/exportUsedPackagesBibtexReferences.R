exportUsedPackagesBibtexReferences=function(fileName="outputs/rCitation.bib") {
####### References  #########
print (R.version)
loadedPackages = names(sessionInfo()$otherPkgs)
packageRef = data.frame(loadedPackages)
sink(fileName)
print(toBibtex(citation()))
for (i in 1:nrow(packageRef)){
#    print (packageRef$loadedPackages[i])
    print( toBibtex(citation(packageRef$loadedPackages[i])))
}
sink()

}