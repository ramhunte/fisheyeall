

# Display list of available date stamp folders for whiting
dir("R:/Confidential/FISHEyE/data/Whiting")
whitingdir <- "R:/Confidential/FISHEyE/data/Whiting/2024-06-14"
whitingfiles <- list.files(whitingdir)[grepl('RDS', list.files(whitingdir))]

try(if(length(whitingfiles) == 0) stop("There aren't any files to move in that folder"))

for(wfiles in whitingfiles) {

    file.copy(
        from = file.path(whitingdir, whitingfiles),
        to = "../Whiting",
        overwrite = T)

}
