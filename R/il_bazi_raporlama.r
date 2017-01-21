##
#' @export
bolgeleri_birlestir<-function(bolge_verisi){

    bolge_verisi %>%
    mutate_each(funs(sum(.)),-(il:cevre_turu)) %>%
    slice(1)

}

#' @export
oranlari_hesapla<-function(istatistik_verisi){

    oy_oranlari<-
    istatistik_verisi %>%
    select(gecerli_oy:bagimsiz) %>%
    mutate_each(funs(./gecerli_oy),-gecerli_oy) %>%
    select(-gecerli_oy) %>%
    gather(parti,oy_orani) %>%
    mutate(topla=ifelse(parti %in% c("ak_parti","chp","mhp","hdp"),FALSE,TRUE))

    oy_oranlari<-
    oy_oranlari %>%
    filter(!topla) %>%
    select(-topla) %>%
    spread(parti,oy_orani) %>%
    cbind(oy_oranlari %>% summarise(diger=sum(oy_orani*topla))) %>%
    mutate_each(funs(round(.,4)*100))

    return(oy_oranlari)

}

# secim_data <- secim151101g
# il_ismi <- "İstanbul"
#' @export
il_bazi_giris<-function(secim_data,il_ismi,bolge_text=0){

    #Il verisini al
    il_data<-
    secim_data %>%
    filter(il==il_ismi)

    if(nrow(il_data)==0){
        stop("İl ismi yanlış.")
    }

    if(bolge_text>0){
        il_data <-
        il_data %>%
        filter(bolge==bolge_text)
        if(nrow(il_data) == 0){
            stop("Yanlış bölge veya bu ilin bölgesi yok.")
        }

        ilce_text <-
        il_data %>%
        filter(ilce != "İli") %>%
        distinct(ilce) %>%
        arrange(ilce) %>%
        unlist()

        ilce_text[length(ilce_text)] <- paste0("ve ",ilce_text[length(ilce_text)])
        ilce_text <- paste0(ilce_text,collapse=", ")
    }

    #Bolge sayisi
    if(bolge_text > 0){
        secim_bolge_sayisi <- 1
    }else{
        secim_bolge_sayisi <- max(il_data$bolge)
    }

    #Ilce sayisi
    ilce_sayisi <-
    il_data %>%
    filter(ilce != "İli") %>%
    distinct(ilce) %>%
    nrow(.)

    #Koy, mahalle, belde, cezaevi sayisi.
    cevre_sayilari<-
    il_data %>%
    distinct(ilce,cevre,cevre_turu) %>%
    group_by(cevre_turu) %>%
    summarise(count=n()) %>%
    tidyr::spread(cevre_turu,count)

    #İl istatistikleri
    il_istatistikleri <-
    il_data %>%
    filter(ilce == "İli" & cevre == "Genel" & cevre_turu == "Toplam")

    #İzmir, Ankara, İstanbul gibi birden çok bölge varsa birleştir
    if(secim_bolge_sayisi > 1){
        il_istatistikleri <- bolgeleri_birlestir(bolge_verisi=il_istatistikleri)
    }


    #Cumle haline getir.
    output_text<-
    paste0(
        ifelse(bolge_text > 0, paste0(il_ismi," ",bolge_text,". bölge ",ilce_text," ilçelerini kapsamaktadır."),""),
        ifelse(bolge_text > 0,"Bu bölgede ",paste0(il_ismi," ilinde bu seçimde ")),
           ifelse(secim_bolge_sayisi>1,paste0(secim_bolge_sayisi," seçim bölgesi "),""),
           ilce_sayisi," ilçe, ",
           ifelse(cevre_sayilari$Mahalle>0,paste0(cevre_sayilari$Mahalle," mahalle, "),""),
           ifelse(suppressWarnings(cevre_sayilari$Belde>0),paste0(cevre_sayilari$Belde," belde, "),""),
           ifelse(suppressWarnings(cevre_sayilari$Köy>0),paste0(cevre_sayilari$Köy," köy, "),""),
           ifelse(cevre_sayilari$Cezaevi>0,paste0(cevre_sayilari$Cezaevi," cezaevi, "),""),
           "ve toplamda ",format(il_istatistikleri$sandik,big.mark=".",decimal.mark=",")," seçim sandığı bulunmaktadır. ",
           ifelse(bolge_text > 0, "Seçim bölgesi","İl")," içerisinde toplam ", format(il_istatistikleri$kayitli_secmen,big.mark=".",decimal.mark=",")," kayıtlı seçmen bulunmaktadır. ",
           format(il_istatistikleri$oy_kullanan,big.mark=".",decimal.mark=",")," seçmen oy kullanmış, geçerli oy sayısı ise ",format(il_istatistikleri$gecerli_oy,big.mark=".",decimal.mark=",")," olmuştur. ",
           "Seçime katılım oranı %",format(round(il_istatistikleri$oy_kullanan/il_istatistikleri$kayitli_secmen,4)*100,big.mark=".",decimal.mark=",")," olarak gerçekleşmiştir."

    )

    #Partilerin oy oranlarını hesapla
    oy_oranlari<- oranlari_hesapla(il_istatistikleri)
    # il_istatistikleri %>%
    # select(gecerli_oy:bagimsiz) %>%
    # mutate_each(funs(./gecerli_oy),-gecerli_oy) %>%
    # select(-gecerli_oy) %>%
    # gather(parti,oy_orani) %>%
    # mutate(topla=ifelse(parti %in% c("ak_parti","chp","mhp","hdp"),FALSE,TRUE))
    #
    # oy_oranlari<-
    # oy_oranlari %>%
    # filter(!topla) %>%
    # select(-topla) %>%
    # spread(parti,oy_orani) %>%
    # cbind(oy_oranlari %>% summarise(diger=sum(oy_orani*topla))) %>%
    # mutate_each(funs(round(.,4)*100))

    #Cümleye ekle
    output_text <-
    paste0(output_text," ",
    "Toplam sonuçlarda ",
    "AK Parti %",format(oy_oranlari$ak_parti,big.mark=".",decimal.mark=","),", ",
    "CHP %",format(oy_oranlari$chp,big.mark=".",decimal.mark=","),", ",
    "MHP %",format(oy_oranlari$mhp,big.mark=".",decimal.mark=","),", ",
    "HDP %",format(oy_oranlari$hdp,big.mark=".",decimal.mark=","),", ",
    "ve diğer partiler ile bağımsızlar %",format(oy_oranlari$diger,big.mark=".",decimal.mark=",")," oy oranına sahip olmuşlardır."
    )

    output_text <- gsub(", ve"," ve",output_text)

    return(output_text)
}

#' @export
il_bazi_giris_tepe<-function(...){

    args<-list(...)
    # print(args)

    output_text<-il_bazi_giris(...,bolge_text=0)

    # print("Checkpoint 1")

    bolge_sayisi<-
    args$secim_data %>%
    filter(il==args$il_ismi) %>%
    summarise(max(bolge)) %>%
    unlist

    # print("Checkpoint 2")

    if(bolge_sayisi > 1){
        for(i in 1:bolge_sayisi){
            output_text<-paste0(output_text,"<p />",il_bazi_giris(...,bolge_text=i))
        }
    }

    return(output_text)
}


#' @export
il_bazi_oylar_daire_grafigi<-function(secim_list=list(`7 Haziran`=secim150607g,`1 Kasım`=secim151101g),il_ismi){

    secim_sayisi<-length(secim_list)
    secim_isimleri <- names(secim_list)

    oy_oranlari<-data_frame(parti=character(),oy_orani=numeric(),position=numeric(),donem=character())

    for(i in 1:secim_sayisi){

        il_istatistikleri <-
        secim_list[[i]] %>%
        filter(il == il_ismi & ilce =="İli" & cevre == "Genel" & cevre_turu == "Toplam")

        if(nrow(il_istatistikleri) > 1){
            il_istatistikleri <- bolgeleri_birlestir(il_istatistikleri)
        }

        #Partilerin oy oranlarını hesapla
        oy_oranlari <-
        oranlari_hesapla(il_istatistikleri) %>%
        gather(parti,oy_orani) %>%
        left_join(data.frame(parti=c("ak_parti","chp","mhp","hdp","diger")),.,by="parti") %>%
        mutate(position=100- (cumsum(oy_orani)-oy_orani/2),donem=secim_isimleri[i]) %>%
        rbind(oy_oranlari,.)


    }

    oy_oranlari <-
    oy_oranlari %>% mutate(donem=ordered(donem,levels=secim_isimleri))

    # oy_oranlari2 <-
    # il_istatistikleri %>%
    # mutate(ak_parti = 0.75*ak_parti,mhp=1.2*mhp,chp=0.9*chp) %>%
    # oranlari_hesapla(.) %>%
    # gather(parti,oy_orani) %>%
    # mutate(oy_orani = round(oy_orani / sum(oy_orani),4)*100) %>%
    # left_join(data.frame(parti=c("ak_parti","chp","mhp","hdp","diger")),.,by="parti") %>%
    # mutate(position=100 - (cumsum(oy_orani)-oy_orani/2),donem="1 Kasım") %>%
    # rbind(oy_oranlari,.)


    ggplot(data=oy_oranlari,aes(x="",y=oy_orani,fill=ordered(parti,levels=c("ak_parti","chp","mhp","hdp","diger")))) +
    geom_bar(stat="identity",width=1) +
    coord_polar("y",start=1/3,direction = -1) +
    scale_fill_manual(values=c("#E4670C","#D6001C","#003f91","#7330b4","grey"),labels=c("AK Parti","CHP","MHP","HDP","Diğer"),position="bottom") +
    scale_y_continuous(expand=c(0,0)) +
    scale_x_discrete(expand=c(0,0)) +
    facet_wrap(~donem) +
    theme_void() +
    theme(
    legend.position="top",
    panel.spacing.x = unit(0, "lines"),
    panel.spacing.y = unit(0, "lines"),
    # strip.switch.pad.grid = unit(0, "lines"),
    strip.text.x = element_text(size = 12, face = "bold", angle = 0,vjust=-1),
    axis.text.x=element_blank(),
    panel.border=element_rect(colour = "grey", fill=NA, size=0.2),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    strip.background = element_rect( fill="grey", size=0.2),
    panel.background = element_rect( color="grey",fill=NA, size=0)
    # plot.margin = unit(c(1,0.5,0,0.5), "lines")
    # panel.margin = unit(c(-0.5,0,-0.5,0), "lines")
    # axis.title.x = element_blank(),
    # axis.text.x=element_blank(),
    # axis.title.y = element_blank(),
    # panel.border = element_blank(),
    # panel.grid=element_blank(),
    # axis.ticks = element_blank(),
    # plot.title=element_text(size=14, face="bold")
     ) +
    geom_text(aes(label = paste0("%",oy_orani),y=position,x=1.7),size=3,angle=0) +
    # geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]),
    #         label = percent(value/100)), size=5)
    labs(fill="")

}

#' @export
il_bazi_ilce_katilim_oranlari<-function(secim_list=list(`20150607`=secim150607g,`20151101`=secim151101g),il_ismi){

    il_ilceler_verisi <-
    plyr::ldply(secim_list,
        .fun=(. %>%
            filter(il == il_ismi & ilce!="İli" & cevre=="Genel" & cevre_turu == "Toplam") %>%    select(ilce,kayitli_secmen,oy_kullanan,gecerli_oy) %>%
            # gather(deger_adi,deger,-ilce)
            tbl_df()
            ),.id="secim") %>%

            mutate(secim=ordered(secim,levels=names(secim_list))) %>%
            tbl_df()

    # ggplot(data=il_ilceler_verisi, aes(x=ilce)) +
    # geom_bar(aes(y=deger,fill=deger_adi),stat="identity",position="identity") +
    # # geom_bar(aes(y=kayitli_secmen),stat="identity",position="stack",fill="#77dd77",data="Kayıtlı Seçmen") +
    # # geom_bar(aes(y=oy_kullanan),stat="identity",position="stack",fill="red",data="Oy Kullanan") +
    # # geom_bar(aes(y=gecerli_oy),stat="identity",position="stack",fill="blue",data="Geçerli Oy") +
    # labs(x="",y="",
    #     title=paste0(il_ismi," Çevresi İlçelerin Seçime Katılım Oranları"),caption="") +
    # scale_x_discrete(expand = c(0, 0)) +
    # scale_y_continuous(labels = scales::percent,expand=c(0,0),limits=c(0,1)) +
    # theme_bw() +
    # theme(legend.position="top",legend.key.size=unit(1,"char"),legend.margin=unit(0.05,"cm"),
    #         plot.title=element_text(hjust=0.5),plot.caption=element_text(hjust=0.5),
    #         axis.text.x=element_text(angle=45,vjust=1,hjust=1))

# fill="#77dd77"
    grafik_bilgisi<-
    ggplot(data=il_ilceler_verisi, aes(x=ilce)) +
    # geom_bar(aes(y=deger,fill=deger_adi),stat="identity",position="identity") +
    geom_bar(aes(y=kayitli_secmen,fill=secim),stat="identity",position=position_dodge(width=1),alpha=0.6) +
    geom_bar(aes(y=oy_kullanan,group=secim,fill="Geçersiz Oylar"),stat="identity",position=position_dodge(width=1),alpha=1) +
    geom_bar(aes(y=gecerli_oy,group=secim,fill="Geçerli Oylar"),stat="identity",position=position_dodge(width=1)) +
    labs(x="",y="Seçmen Sayısı (x1000)",
        title=NULL) +
    scale_x_discrete(expand = c(0,0.75)) +
    scale_y_continuous(expand=c(0,0),labels = function(x) round(x/1000),limits=c(0,ceiling(max(il_ilceler_verisi$kayitli_secmen)/10000)*10000)) +
    # scale_y_continuous(expand=c(0,0),labels = function(x) round(x/1000),limits=c(0,ceiling(max(il_ilceler_verisi$deger)/10000)*10000)) +
    scale_fill_manual(name="",values=c("#00FF84","#00974E","#6CD6FF","#DB3800"),labels=c("7 Haziran Seçmen Sayısı","1 Kasım Seçmen Sayısı","Geçerli Oy","Geçersiz Oy")) +
    # scale_color_manual(name="",values=c("black","grey"),labels=c("Geçerli Oy","Geçersiz Oy")) +
    theme_bw() +
    theme(legend.position="top",
        legend.key.size=unit(1,"char"),
        # legend.margin=unit(0.05,"cm"),
            plot.title=element_text(hjust=0.5),plot.caption=element_text(hjust=0.5),
            axis.text.x=element_text(angle=45,vjust=1,hjust=1))

    return(grafik_bilgisi)
}

#' @export
il_bazi_ilce_parti_oy_oranlari<-function(secim_list=list(`20150607`=secim150607g,`20151101`=secim151101g),il_ismi,parti_ismi,parti_etiketi,renk_semasi){

    il_ilceler_verisi <-
    plyr::ldply(secim_list,
        .fun=(. %>%
            filter(il == il_ismi & ilce!="İli" & cevre=="Genel" & cevre_turu == "Toplam") %>% select_("ilce","gecerli_oy",parti_ismi) %>%
            # gather(deger_adi,deger,-ilce)
            tbl_df()
            ),.id="secim") %>%

            mutate(secim=ordered(secim,levels=names(secim_list))) %>%
            tbl_df()

    grafik_verisi<-
    il_ilceler_verisi %>%
    mutate_(oy_orani=paste(parti_ismi,"/gecerli_oy")) %>%
    transmute(secim=ordered(secim,levels=names(secim_list)),ilce,oy_orani=round(oy_orani,4)) %>%
    arrange(ilce) %>%
    tbl_df()

    # ggplot(data=il_ilceler_verisi, aes(x=ilce)) +
    # geom_bar(aes(y=deger,fill=deger_adi),stat="identity",position="identity") +
    # # geom_bar(aes(y=kayitli_secmen),stat="identity",position="stack",fill="#77dd77",data="Kayıtlı Seçmen") +
    # # geom_bar(aes(y=oy_kullanan),stat="identity",position="stack",fill="red",data="Oy Kullanan") +
    # # geom_bar(aes(y=gecerli_oy),stat="identity",position="stack",fill="blue",data="Geçerli Oy") +
    # labs(x="",y="",
    #     title=paste0(il_ismi," Çevresi İlçelerin Seçime Katılım Oranları"),caption="") +
    # scale_x_discrete(expand = c(0, 0)) +
    # scale_y_continuous(labels = scales::percent,expand=c(0,0),limits=c(0,1)) +
    # theme_bw() +
    # theme(legend.position="top",legend.key.size=unit(1,"char"),legend.margin=unit(0.05,"cm"),
    #         plot.title=element_text(hjust=0.5),plot.caption=element_text(hjust=0.5),
    #         axis.text.x=element_text(angle=45,vjust=1,hjust=1))

# fill="#77dd77"
    grafik_bilgisi<-
    ggplot(data=grafik_verisi, aes(x=ilce)) +
    # geom_bar(aes(y=deger,fill=deger_adi),stat="identity",position="identity") +
    geom_bar(aes(y=oy_orani,fill=secim),stat="identity",position=position_dodge(width=1),alpha=0.9) +
    labs(x="",y=paste0(parti_etiketi," Oy Oranı"),
        title=NULL) +
    scale_x_discrete(expand = c(0,0.75)) +
    scale_y_continuous(labels = scales::percent,expand=c(0,0),limits=c(0,1)) +
    # scale_y_continuous(expand=c(0,0),labels = function(x) round(x/1000),limits=c(0,ceiling(max(il_ilceler_verisi$kayitli_secmen)/10000)*10000)) +
    scale_fill_manual(name="",values=renk_semasi,labels=c("7 Haziran","1 Kasım")) +
    # scale_color_manual(name="",values=c("black","grey"),labels=c("Geçerli Oy","Geçersiz Oy")) +
    theme_bw() +
    theme(legend.position="top",
        legend.key.size=unit(1,"char"),
        # legend.margin=unit(0.05,"cm"),
            plot.title=element_text(hjust=0.5),plot.caption=element_text(hjust=0.5),
            axis.text.x=element_text(angle=45,vjust=1,hjust=1))

    return(grafik_bilgisi)
}



#' @export
oy_orani_histogram_il<-function(secim_verisi,il_ismi,baraj_ustu_partiler=c("ak_parti","chp","mhp","hdp")){

il_oy_oran<-
secim_verisi %>%
filter(grepl(il_ismi,il) & !(cevre_turu %in% c("Toplam"))) %>%
select_("ilce","cevre","cevre_turu","gecerli_oy",.dots=baraj_ustu_partiler) %>%
group_by(ilce,cevre,cevre_turu) %>%
summarise_each(funs="sum") %>%
mutate_each_(funs(round(./gecerli_oy,4)),baraj_ustu_partiler) %>%
gather(key=parti,value=oy_orani,-ilce,-cevre,-cevre_turu)  %>%
filter(parti != "gecerli_oy" & oy_orani > 0.05)

the_plot<-
ggplot(data=il_oy_oran) +
geom_histogram(aes(x=oy_orani,fill=ordered(parti,levels=baraj_ustu_partiler)),color="white",size=0.1,binwidth=0.01) +
# geom_density(aes(x=oy_orani,fill=parti),size=0.1,alpha=0.75,position="stack") +
labs(x="Parti Oy Oranı (%5 ve altı gösterilmemektedir.)",y="Çevre (ör. Mahalle, Köy) Sayısı",
    title=NULL) +
scale_fill_manual(name="",labels=c("AK Parti","CHP","MHP","HDP"),values=c("#E4670C","#D6001C","#003f91","#7330b4")) +
scale_x_continuous(labels = scales::percent,expand = c(0, 0),limits=c(0.05,1.05)) +
# scale_y_continuous(expand = c(0, 0)) +
theme_bw() +
theme(legend.position="top",legend.key.size=unit(1,"char"),legend.margin=unit(0.05,"cm"),plot.title=element_text(hjust=0.5),plot.caption=element_text(hjust=0.5))

bin_max<-max(ggplot_build(the_plot)$data[[1]][,"ymax"])

the_plot +
scale_y_continuous(expand=c(0,0),limits=c(0,ceiling(bin_max/5)*5))

}
