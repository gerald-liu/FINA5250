assg_no <- 3
wd <- sprintf("D:/workspace/FINA5250/Assignment%s", assg_no)

knitr::purl(input = sprintf("%s/assg%s.rmd", wd, assg_no),
            output = sprintf("%s/assg%s.r", wd, assg_no),
            documentation = 0)

file.rename(from = sprintf("%s/assg%s.r", wd, assg_no),
            to = sprintf("%s/assg%s.txt", wd, assg_no))
