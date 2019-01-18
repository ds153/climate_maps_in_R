mask = function(img) {
        xy=merge(img$x,img$y)
        main_map = map("state",region=stateList,fill=TRUE,plot=FALSE,interior=FALSE,proj="")
        def = rep(F,length(img$x))
        for (k in seq(1,length(main_map$names))) {
           m_d = map("state",region=main_map$names[k],fill=TRUE,plot=FALSE,interior=FALSE,proj="")
           n_def=inout(data.frame(c(xy$x),c(xy$y)),data.frame(c(m_d$x),c(m_d$y)))
           def = def | n_def
        }
        for (j in seq(1,length(def))) { if(def[j]==F) img$z[j] = NA}
        return(img)
}