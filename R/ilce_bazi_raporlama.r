##

# secim_data <- secim151101g
# il_ismi <- "İstanbul"
#' @export
ilce_bazi_giris<-function(secim_data,il_ismi,ilce_ismi){

    #Ilce verisini al
    ilce_data<-
    secim_data %>%
    filter(il==il_ismi & ilce==ilce_ismi)

    if(nrow(ilce_data)==0){
        stop("İl veya ilçe ismi yanlış.")
    }

    #Koy, mahalle, belde, cezaevi sayisi.
    cevre_sayilari<-
    ilce_data %>%
    distinct(cevre,cevre_turu) %>%
    group_by(cevre_turu) %>%
    summarise(count=n()) %>%
    rbind(.,data.frame(cevre_turu=c("Mahalle","Köy","Belde","Cezaevi"),count=0)) %>%
    distinct(cevre_turu,.keep_all=TRUE) %>%
    tidyr::spread(cevre_turu,count)

    #İl istatistikleri
    ilce_istatistikleri <-
    ilce_data %>%
    filter(cevre == "Genel" & cevre_turu == "Toplam")

    #Cumle haline getir.
    output_text<-
    paste0(
            paste0(ilce_ismi," ilçesinde bu seçimde "),
           ifelse(cevre_sayilari$Mahalle>0,paste0(cevre_sayilari$Mahalle," mahalle, "),""),
           ifelse(suppressWarnings(cevre_sayilari$Belde>0),paste0(cevre_sayilari$Belde," belde, "),""),
           ifelse(suppressWarnings(cevre_sayilari$Köy>0),paste0(cevre_sayilari$Köy," köy, "),""),
           ifelse(cevre_sayilari$Cezaevi>0,paste0(cevre_sayilari$Cezaevi," cezaevi, "),""),
           "ve toplamda ",format(ilce_istatistikleri$sandik,big.mark=".",decimal.mark=",")," seçim sandığı bulunmaktadır. ",
           "İlçe içerisinde toplam ", format(ilce_istatistikleri$kayitli_secmen,big.mark=".",decimal.mark=",")," kayıtlı seçmen bulunmaktadır. ",
           format(ilce_istatistikleri$oy_kullanan,big.mark=".",decimal.mark=",")," seçmen oy kullanmış, geçerli oy sayısı ise ",format(ilce_istatistikleri$gecerli_oy,big.mark=".",decimal.mark=",")," olmuştur. ",
           "Seçime katılım oranı %",format(round(ilce_istatistikleri$oy_kullanan/ilce_istatistikleri$kayitli_secmen,4)*100,big.mark=".",decimal.mark=",")," olarak gerçekleşmiştir.<p />"

    )

    #Partilerin oy oranlarını hesapla
    oy_oranlari<- oranlari_hesapla(ilce_istatistikleri)
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
    "ve diğer partiler ile bağımsızlar %",format(oy_oranlari$diger,big.mark=".",decimal.mark=",")," oy oranına sahip olmuşlardır. "
    )

    output_text <- gsub(", ve"," ve",output_text)

    # oy_siralama <-
    # secim_data %>%
    # filter(ilce == "İli" & cevre == "Genel" & cevre_turu == "Toplam") %>%
    # group_by(il) %>%
    # summarise_each(funs(sum),kayitli_secmen:bagimsiz) %>%
    # ungroup() %>%
    # group_by(il) %>%
    # summarise_each(funs(./gecerli_oy),ak_parti:hdp) %>%
    # ungroup() %>%
    # mutate_each(funs(rank(-.)),-il) %>%
    # filter(il==il_ismi)
    #
    # if(bolge_text==0){
    #     output_text <-
    #     paste0(output_text," ",il_ismi,", 81 il arasından, partilerin kendi içindeki oy oranları sıralamalarına göre ",
    #     "AK Parti'nin ",oy_siralama$ak_parti,", ",
    #     "CHP'nin ",oy_siralama$chp,", ",
    #     "MHP'nin ",oy_siralama$mhp," ",
    #     "ve HDP'nin ",oy_siralama$hdp,". sıradaki ili olmuştur.",
    #     "^[Örneğin, AK Parti ",oy_siralama$ak_parti-1," ilde daha yüksek, ",81-oy_siralama$ak_parti," ilde ise daha düşük oy oranı görmüştür.] ")
    #
    # }

    return(output_text)
}


#' @export
ilce_bazi_oylar_daire_grafigi<-function(secim_list=list(`7 Haziran`=secim150607g,`1 Kasım`=secim151101g),il_ismi,ilce_ismi){

    secim_sayisi<-length(secim_list)
    secim_isimleri <- names(secim_list)

    oy_oranlari<-data_frame(parti=character(),oy_orani=numeric(),position=numeric(),donem=character())

    for(i in 1:secim_sayisi){

        ilce_istatistikleri <-
        secim_list[[i]] %>%
        filter(il == il_ismi & ilce == ilce_ismi & cevre == "Genel" & cevre_turu == "Toplam")

        #Partilerin oy oranlarını hesapla
        oy_oranlari <-
        oranlari_hesapla(ilce_istatistikleri) %>%
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
ilce_bazi_cevre_katilim_oranlari<-function(secim_list=list(`20150607`=secim150607g,`20151101`=secim151101g),il_ismi,ilce_ismi){

    ilce_cevreler_verisi <-
    plyr::ldply(secim_list,
        .fun=(. %>%
            filter(il == il_ismi & ilce== ilce_ismi & cevre_turu != "Toplam") %>%    select(cevre,cevre_turu,kayitli_secmen,oy_kullanan,gecerli_oy) %>%
            # gather(deger_adi,deger,-ilce)
            tbl_df()
            ),.id="secim") %>%

            mutate(secim=ordered(secim,levels=names(secim_list))) %>%
            group_by(secim,cevre,cevre_turu) %>%
            summarise_each(funs(sum)) %>%
            ungroup %>%
            tbl_df()

    if(nrow(ilce_cevreler_verisi) == 0){
        stop("İl veya ilçe ismi yanlış.")
    }

    eslenemeyenler <-
    ilce_cevreler_verisi %>%
    group_by(cevre,cevre_turu) %>%
    summarise(count=n()) %>%
    filter(count<2)

    if(nrow(eslenemeyenler)>0){
        ilce_cevreler_verisi<-
        ilce_cevreler_verisi %>%
        mutate(cevre=ifelse(paste0(cevre,cevre_turu) %in% paste0(eslenemeyenler$cevre,eslenemeyenler$cevre_turu),"Diger",cevre)) %>%
        group_by(secim,cevre,cevre_turu) %>%
        summarise_each(funs(sum)) %>%
        ungroup %>%
        tbl_df()
    }

    ilce_cevreler_degisim <-
    ilce_cevreler_verisi %>%
    mutate(katilim_orani = oy_kullanan / kayitli_secmen) %>%
    select(secim:kayitli_secmen,katilim_orani) %>%
    gather(sayi,deger,-(secim:cevre_turu)) %>%
    arrange(cevre,cevre_turu,sayi,secim) %>%
    group_by(cevre,cevre_turu,sayi) %>%
    mutate(degisim_oransal=(lead(deger,1)/deger - 1)*100,
            degisim_dogrusal= 100*(lead(deger,1) - deger)) %>%
    filter(!is.na(degisim_oransal)) %>%
    mutate(degisim=ifelse(sayi=="katilim_orani",degisim_dogrusal,degisim_oransal)) %>%
    select(cevre,cevre_turu,sayi,degisim) %>%
    ungroup() %>%
    spread(sayi,degisim)

    kayitli_secmen_sira <-
    ilce_cevreler_degisim %>% filter(cevre!="Diger") %>% arrange(desc(kayitli_secmen))

    katilim_orani_sira <-
    ilce_cevreler_degisim %>% filter(cevre!="Diger") %>% arrange(desc(katilim_orani))

    cevre_sayisi <- nrow(ilce_cevreler_degisim)

    yorum_bilgisi<-
    paste0("Kayıtlı seçmen sayısındaki değişim en yüksek oranda ",
    kayitli_secmen_sira$cevre[1]," (",ifelse(kayitli_secmen_sira$kayitli_secmen[1]>=0,"+","-"),"%",abs(round(kayitli_secmen_sira$kayitli_secmen[1],2)),")",
    " çevresinde (",kayitli_secmen_sira$cevre_turu[1],") ve en düşük oranda ",
    kayitli_secmen_sira$cevre[cevre_sayisi]," (",ifelse(kayitli_secmen_sira$kayitli_secmen[cevre_sayisi]>=0,"+","-"),"%",abs(round(kayitli_secmen_sira$kayitli_secmen[cevre_sayisi],2)),")",
    " çevresinde (",kayitli_secmen_sira$cevre_turu[cevre_sayisi],") görülmektedir. ",
    "Seçime katılım oranında değişim ise en yüksek oranda ",
    katilim_orani_sira$cevre[1]," (",ifelse(katilim_orani_sira$katilim_orani[1]>=0,"+","-"),"%",abs(round(katilim_orani_sira$katilim_orani[1],2)),")",
    " çevresinde (",katilim_orani_sira$cevre_turu[1],") ve en düşük oranda ",
    katilim_orani_sira$cevre[cevre_sayisi]," (",ifelse(katilim_orani_sira$katilim_orani[cevre_sayisi]>=0,"+","-"),"%",abs(round(katilim_orani_sira$katilim_orani[cevre_sayisi],2)),")",
    " çevresinde (",katilim_orani_sira$cevre_turu[cevre_sayisi],") görülmektedir."
    )

    cevre_listesi <-
    ilce_cevreler_verisi %>%
    mutate(cevre_turu_sira=0,
        cevre_turu_sira=ifelse(cevre_turu=="Köy",1,cevre_turu_sira),
        cevre_turu_sira=ifelse(cevre_turu=="Belde",2,cevre_turu_sira),
        cevre_turu_sira=ifelse(cevre_turu=="Cezaevi",3,cevre_turu_sira),
        cevre_turu_sira=cevre_turu_sira + ifelse(cevre == "Diger",0.9,0),
        cevre2=paste0(cevre," (",cevre_turu,")")) %>%
    arrange(cevre_turu_sira,cevre) %>%
    distinct(cevre2) %>%
    unlist()

    n_width<-30
    if(length(cevre_listesi) %% n_width >0 & n_width < length(cevre_listesi)){
        remaining_rows<-n_width - (length(cevre_listesi) %% n_width)
        ilce_cevreler_verisi <-
        rbind(ilce_cevreler_verisi,
            data.frame(secim=rep(names(secim_list),remaining_rows),
                        cevre=paste0("Doldol ",ceiling((1:(remaining_rows*2))/2)),
                        cevre_turu=rep("",remaining_rows*2),
                        kayitli_secmen=rep(0,remaining_rows*2),
                        oy_kullanan=rep(0,remaining_rows*2),
                        gecerli_oy=rep(0,remaining_rows*2)))
        cevre_listesi<-c(cevre_listesi,paste0("Doldol ",1:remaining_rows," ()"))
    }

    grafik_bilgisi<-list()

    for(i in 1:ceiling(length(cevre_listesi)/n_width))
        local({
            i<-i

        grafik_data<- ilce_cevreler_verisi %>% filter(paste0(cevre," (",cevre_turu,")") %in% cevre_listesi[((i-1)*n_width+1):min(i*n_width,length(cevre_listesi))])

        cevre_levels<-cevre_listesi[((i-1)*30+1):min(i*n_width,length(cevre_listesi))]
        p1<-
        ggplot(data=grafik_data, aes(x=ordered(paste0(cevre," (",cevre_turu,")"),levels=cevre_levels))) +
        # geom_bar(aes(y=deger,fill=deger_adi),stat="identity",position="identity") +
        geom_bar(aes(y=kayitli_secmen,fill=secim),stat="identity",position=position_dodge(width=1),alpha=0.6) +
        geom_bar(aes(y=oy_kullanan,group=secim,fill="Geçersiz Oylar"),stat="identity",position=position_dodge(width=1),alpha=1) +
        geom_bar(aes(y=gecerli_oy,group=secim,fill="Geçerli Oylar"),stat="identity",position=position_dodge(width=1)) +
        labs(x="",y="Seçmen Sayısı (x100)",
            title=NULL) +
        scale_x_discrete(expand = c(0,0.75),labels=ifelse(grepl("Doldol",cevre_levels),"",cevre_levels)) +
        scale_y_continuous(expand=c(0,0),labels = function(x) round(x/100),limits=c(0,ceiling(max(ilce_cevreler_verisi$kayitli_secmen)/1000)*1000)) +
        # scale_y_continuous(expand=c(0,0),labels = function(x) round(x/1000),limits=c(0,ceiling(max(il_ilceler_verisi$deger)/10000)*10000)) +
        scale_fill_manual(name="",values=c("#00FF84","#00974E","#6CD6FF","#DB3800"),labels=c("7 Haziran Seçmen Sayısı","1 Kasım Seçmen Sayısı","Geçerli Oy","Geçersiz Oy")) +
        # scale_color_manual(name="",values=c("black","grey"),labels=c("Geçerli Oy","Geçersiz Oy")) +
        theme_bw() +
        theme(legend.position="top",
            legend.key.size=unit(1,"char"),
            # legend.margin=unit(0.05,"cm"),
                plot.title=element_text(hjust=0.5),plot.caption=element_text(hjust=0.5),
                axis.text.x=element_text(angle=90,vjust=1,hjust=1))

        grafik_bilgisi[[i]]<<- p1
    })

    return(list(grafik=grafik_bilgisi,yorum=yorum_bilgisi))
}

#' @export
ilce_bazi_cevre_parti_oy_oranlari<-function(secim_list=list(`20150607`=secim150607g,`20151101`=secim151101g),il_ismi,ilce_ismi,parti_ismi,parti_etiketi,renk_semasi){

    ilce_cevreler_verisi <-
    plyr::ldply(secim_list,
        .fun=(. %>%
            filter(il == il_ismi & ilce== ilce_ismi & cevre_turu != "Toplam") %>%    select_("cevre","cevre_turu","gecerli_oy",parti_ismi) %>%
            # gather(deger_adi,deger,-ilce)
            tbl_df()
            ),.id="secim") %>%

            mutate(secim=ordered(secim,levels=names(secim_list))) %>%
            group_by(secim,cevre,cevre_turu) %>%
            summarise_each(funs(sum)) %>%
            ungroup %>%
            tbl_df()

    if(nrow(ilce_cevreler_verisi) == 0){
        stop("İl veya ilçe ismi yanlış.")
    }

    eslenemeyenler <-
    ilce_cevreler_verisi %>%
    group_by(cevre,cevre_turu) %>%
    summarise(count=n()) %>%
    filter(count<2)

    if(nrow(eslenemeyenler)>0){
        ilce_cevreler_verisi<-
        ilce_cevreler_verisi %>%
        mutate(cevre=ifelse(paste0(cevre,cevre_turu) %in% paste0(eslenemeyenler$cevre,eslenemeyenler$cevre_turu),"Diger",cevre)) %>%
        group_by(secim,cevre,cevre_turu) %>%
        summarise_each(funs(sum)) %>%
        ungroup %>%
        tbl_df()
    }

###
    grafik_verisi<-
    ilce_cevreler_verisi %>%
    mutate_(oy_orani=paste(parti_ismi,"/gecerli_oy")) %>%
    transmute(secim=ordered(secim,levels=names(secim_list)),cevre,cevre_turu,oy_orani=round(oy_orani,4)) %>%
    arrange(cevre,cevre_turu) %>%
    tbl_df()

    ilk_secim_oran <-
    grafik_verisi %>%
    filter(secim==names(secim_list)[1]) %>%
    arrange(desc(oy_orani)) %>%
    mutate(oy_orani=round(100*oy_orani,2)) %>%
    slice(c(1,nrow(.)))

    ikinci_secim_oran <-
    grafik_verisi %>%
    filter(secim==names(secim_list)[2]) %>%
    arrange(desc(oy_orani)) %>%
    mutate(oy_orani=round(100*oy_orani,2)) %>%
    slice(c(1,nrow(.)))

    degisim_verisi<-
    grafik_verisi %>%
    group_by(cevre,cevre_turu) %>%
    mutate(degisim= 100*(lead(oy_orani,1) - oy_orani)) %>%
    filter(!is.na(degisim)) %>%
    select(cevre,cevre_turu,degisim) %>%
    ungroup() %>%
    arrange(desc(degisim)) %>%
    slice(c(1,nrow(.)))

    yorum_bilgisi <-
    paste0(parti_etiketi,", 7 Haziran seçimlerinde ",il_ismi,"-",ilce_ismi," ilçesinin seçim çevreleri arasında en yüksek oy oranını ",
        ilk_secim_oran$cevre[1]," (",ilk_secim_oran$cevre_turu[1],")"," (%",ilk_secim_oran$oy_orani[1],"),",
        " en düşük oy oranını ",
        ilk_secim_oran$cevre[2]," (",ilk_secim_oran$cevre_turu[2],")"," (%",ilk_secim_oran$oy_orani[2],") ",
        " çevrelerinde görmüştür. ",
        "1 Kasım seçimlerinde ise en yüksek oy oranını ",
            ikinci_secim_oran$cevre[1]," (",ikinci_secim_oran$cevre_turu[1],")"," (%",ikinci_secim_oran$oy_orani[1],"),",
            " en düşük oy oranını ",
            ikinci_secim_oran$cevre[2]," (",ikinci_secim_oran$cevre_turu[2],")"," (%",ikinci_secim_oran$oy_orani[2],")",
            " çevrelerinde görmüştür. ",


        ifelse(degisim_verisi$degisim[1]<0,"Parti bu ilçedeki hiçbir çevrede oy oranını yükseltememiştir. ",""),
        ifelse(degisim_verisi$degisim[2]>0,"Parti bu ilçedeki bütün çevrelerde oy oranlarını yükseltmiştir. ",""),
        ifelse(degisim_verisi$degisim[1]>0,paste0("Puan bazında en büyük yükseliş ",degisim_verisi$cevre[1]," (",degisim_verisi$cevre_turu[1],")"," çevresinde (+%",abs(round(degisim_verisi$degisim[1],2)),") görülmektedir. "),""),
        ifelse(degisim_verisi$degisim[2]<0,paste0("Puan bazında en büyük düşüş ",ifelse(degisim_verisi$degisim[1]>0,"ise ",""),degisim_verisi$cevre[2]," (",degisim_verisi$cevre_turu[2],")"," çevresinde (-%",abs(round(degisim_verisi$degisim[2],2)),") görülmektedir."),"")

        #
        # "İki seçim arasında oy oranlarındaki değişimi sıralayacak olursak; en yüksek oranda değişim ",
        # degisim_verisi$ilce[1]," (",ifelse(degisim_verisi$degisim[1]>=0,"+","-"),"%",abs(round(degisim_verisi$degisim[1],2)),")",
        # " ilçesinde ve en düşük oranda ",
        # degisim_verisi$ilce[2]," (",ifelse(degisim_verisi$degisim[2]>=0,"+","-"),"%",abs(round(degisim_verisi$degisim[2],2)),")",
        # " ilçesinde görülmektedir."
    )

    cevre_listesi <-
    grafik_verisi %>%
    mutate(cevre_turu_sira=0,
        cevre_turu_sira=ifelse(cevre_turu=="Köy",1,cevre_turu_sira),
        cevre_turu_sira=ifelse(cevre_turu=="Belde",2,cevre_turu_sira),
        cevre_turu_sira=ifelse(cevre_turu=="Cezaevi",3,cevre_turu_sira),
        cevre_turu_sira=cevre_turu_sira + ifelse(cevre == "Diger",0.9,0),
        cevre2=paste0(cevre," (",cevre_turu,")")) %>%
    arrange(cevre_turu_sira,cevre) %>%
    distinct(cevre2) %>%
    unlist()

    n_width<-30
    if(length(cevre_listesi) %% n_width >0 & n_width < length(cevre_listesi)){
        remaining_rows<-n_width - (length(cevre_listesi) %% n_width)
        grafik_verisi <-
        rbind(grafik_verisi,
            data.frame(secim=rep(names(secim_list),remaining_rows),
                        cevre=paste0("Doldol ",ceiling((1:(remaining_rows*2))/2)),
                        cevre_turu=rep("",remaining_rows*2),
                        oy_orani=rep(0,remaining_rows*2)))
        cevre_listesi<-c(cevre_listesi,paste0("Doldol ",1:remaining_rows," ()"))
    }

    grafik_bilgisi<-list()

    for(i in 1:ceiling(length(cevre_listesi)/n_width))
        local({
            i<-i

        grafik_data<- grafik_verisi %>% filter(paste0(cevre," (",cevre_turu,")") %in% cevre_listesi[((i-1)*n_width+1):min(i*n_width,length(cevre_listesi))])

        cevre_levels<-cevre_listesi[((i-1)*30+1):min(i*n_width,length(cevre_listesi))]
        p1<-
        ggplot(data=grafik_data, aes(x=ordered(paste0(cevre," (",cevre_turu,")"),levels=cevre_levels))) +
        # geom_bar(aes(y=deger,fill=deger_adi),stat="identity",position="identity") +
        geom_bar(aes(y=oy_orani,fill=secim),stat="identity",position=position_dodge(width=1),alpha=0.9) +
        labs(x="",y=paste0(parti_etiketi," Oy Oranı"),
            title=NULL) +
        scale_x_discrete(expand = c(0,0.75),labels=ifelse(grepl("Doldol",cevre_levels),"",cevre_levels)) +
        scale_y_continuous(labels = scales::percent,expand=c(0,0),limits=c(0,1)) +
        scale_fill_manual(name="",values=renk_semasi,labels=c("7 Haziran","1 Kasım")) +
        # scale_color_manual(name="",values=c("black","grey"),labels=c("Geçerli Oy","Geçersiz Oy")) +
        theme_bw() +
        theme(legend.position="top",
            legend.key.size=unit(1,"char"),
            # legend.margin=unit(0.05,"cm"),
                plot.title=element_text(hjust=0.5),plot.caption=element_text(hjust=0.5),
                axis.text.x=element_text(angle=90,vjust=1,hjust=1))

        grafik_bilgisi[[i]]<<- p1
    })

    return(list(grafik=grafik_bilgisi,yorum=yorum_bilgisi))
}



#' @export
oy_orani_histogram_il_ilce<-function(secim_verisi,il_ismi,baraj_ustu_partiler=c("ak_parti","chp","mhp","hdp")){

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


#' @export
oy_orani_boxplot_ilce<-function(secim_verisi,il_ismi,ilce_ismi,baraj_ustu_partiler=c("ak_parti","chp","mhp","hdp")){

ilce_oy_oran<-
secim_verisi %>%
filter(il == il_ismi & ilce == ilce_ismi & !(cevre_turu %in% c("Toplam"))) %>%
select_("cevre","cevre_turu","gecerli_oy",.dots=baraj_ustu_partiler) %>%
# group_by(ilce,cevre,cevre_turu) %>%
# summarise_each(funs="sum") %>%
mutate_each_(funs(round(./gecerli_oy,4)),baraj_ustu_partiler) %>%
gather(key=parti,value=oy_orani,-cevre,-cevre_turu)  %>%
filter(parti != "gecerli_oy" & oy_orani > 0.05)

the_plot<-
ggplot(data=ilce_oy_oran) +
geom_boxplot(aes(x=ordered(parti,levels=baraj_ustu_partiler),y=oy_orani,fill=ordered(parti,levels=baraj_ustu_partiler)),color="#333333") +
# geom_density(aes(x=oy_orani,fill=parti),size=0.1,alpha=0.75,position="stack") +
labs(x="Partiler",y="Parti Oy Oranları (%5 altı dahil edilmemiştir.)",
    title=NULL) +
scale_fill_manual(name="",labels=c("AK Parti","CHP","MHP","HDP"),values=c("#E4670C","#D6001C","#003f91","#7330b4")) +
scale_x_discrete(labels=c("AK Parti","CHP","MHP","HDP")) +
scale_y_continuous(labels = scales::percent,expand = c(0, 0),limits=c(0.05,1.05)) +
# scale_y_continuous(expand = c(0, 0)) +
theme_bw() +
theme(legend.position="none",plot.title=element_text(hjust=0.5),plot.caption=element_text(hjust=0.5))

return(the_plot)
}

#' @export
oy_kaymalari_ilce<-function(il_ismi,ilce_ismi){

    data_1 <-
    secim150607g %>%
    filter(il==il_ismi & ilce==ilce_ismi & cevre_turu != "Toplam") %>%
    select(il,ilce,cevre,cevre_turu,gecerli_oy,ak_parti:hdp) %>%
    mutate(diger=gecerli_oy - ak_parti - chp - mhp - hdp) %>%
    group_by(il,ilce,cevre,cevre_turu) %>%
    summarise_each(funs(sum))

    colnames(data_1)[-(1:4)] <- paste0("haz_",colnames(data_1)[-(1:4)])

    # %>% mutate_each(funs(./gecerli_oy),ak_parti:hdp) %>% mutate(diger=1-ak_parti-chp-mhp-hdp)

    data_2 <-
    secim151101g %>%
    filter(il==il_ismi & ilce==ilce_ismi & cevre_turu != "Toplam") %>%
    select(il,ilce,cevre,cevre_turu,gecerli_oy,ak_parti:hdp) %>%
    mutate(diger=gecerli_oy - ak_parti - chp - mhp - hdp) %>%
    group_by(il,ilce,cevre,cevre_turu) %>%
    summarise_each(funs(sum))

    colnames(data_2)[-(1:4)] <- paste0("kas_",colnames(data_2)[-(1:4)])

    # %>% mutate_each(funs(./gecerli_oy),ak_parti:hdp) %>% mutate(diger=1-ak_parti-chp-mhp-hdp)

    butun_data <-
    full_join(data_1,data_2,by=c("il","ilce","cevre","cevre_turu")) %>%
    mutate_each(funs(ifelse(is.na(haz_gecerli_oy) | is.na(kas_gecerli_oy),"Diger",.)),il:cevre_turu) %>%
    mutate_each(funs(ifelse(is.na(.),0,.))) %>%
    mutate(id_num=ifelse(il!="Diger",row_number(),0)) %>%
    select(il:cevre_turu,id_num,everything()) %>%
    group_by(il,ilce,cevre,cevre_turu,id_num) %>%
    summarise_each(funs(sum(.)),-(il:id_num))  %>%
    ungroup() %>%
    mutate_each(funs(./haz_gecerli_oy),haz_ak_parti:haz_diger) %>% mutate_each(funs(./kas_gecerli_oy),kas_ak_parti:kas_diger) %>%
    transmute(il,ilce,cevre,cevre_turu,oy_agirlik=kas_gecerli_oy,
    # transmute(il,ilce,cevre,cevre_turu,oy_agirlik=(haz_gecerli_oy+kas_gecerli_oy)/2,
    ak_parti=kas_ak_parti - haz_ak_parti,
    chp=kas_chp - haz_chp,
    mhp=kas_mhp - haz_mhp,
    hdp=kas_hdp - haz_hdp,
    diger=kas_diger - haz_diger,
    toplam_degisim=(abs(ak_parti)+abs(chp)+abs(mhp)+abs(hdp)+abs(diger))/2) %>%
    tbl_df()

    parti_ikili <- expand.grid(parti=c("ak_parti","chp","mhp","hdp","diger"),parti2=c("ak_parti","chp","mhp","hdp","diger"),stringsAsFactors=FALSE)

    kayma_verisi<-
    plyr::ddply(.data=butun_data %>% mutate(sandik=row_number()),.variables=c("sandik"),.fun=kaymalari_hesapla,parti_grid = parti_ikili %>% filter(parti != parti2)) %>% tbl_df()

    # kayma_verisi %>% group_by(parti,parti2) %>% summarise(kayma=sum(kayma*oy_agirlik)) %>% ungroup() %>% mutate(kayma=round(kayma,4)*100) %>% spread(parti2,kayma,fill=0)

    kayma_form <-
    kayma_verisi %>%
    mutate(kayma_oy=kayma*oy_agirlik,kayma_oy=kayma_oy/sum(butun_data$oy_agirlik)) %>%
    group_by(parti,parti2) %>%
    # summarise(kayma=weighted.mean(kayma,oy_agirlik),oy_agirlik=sum(oy_agirlik)) %>% ungroup()
    summarise(kayma=sum(kayma_oy)) %>%
    ungroup() %>%
    rbind(.,cbind(parti_ikili,kayma=0)) %>%
    distinct(parti,parti2,.keep_all=TRUE)

    oy_kaymalari_tablo<-
    kayma_form %>%
    mutate(kayma=paste0("%",round(kayma,4)*100)) %>%
    mutate_each(funs(ordered(.,levels=c("ak_parti","chp","mhp","hdp","diger"))),parti,parti2) %>%
    mutate(parti=plyr::revalue(parti,c("ak_parti"="AK Parti","chp"="CHP","mhp"="MHP","hdp"="HDP","diger"="Diğer"))) %>%
    mutate(parti2=plyr::revalue(parti2,c("ak_parti"="AK Parti","chp"="CHP","mhp"="MHP","hdp"="HDP","diger"="Diğer"))) %>%
    spread(parti2,kayma,fill=0) %>% rename(` `=parti)

    net_kayma_veri<-
    left_join(kayma_form,kayma_form %>%
        transmute(parti,parti2,kayma2=-kayma),by=c("parti"="parti2","parti2"="parti")) %>%
        transmute(parti,parti2,net_kayma=100*round(kayma+kayma2,4)) %>%
        filter(net_kayma > 0) %>%
        # group_by(parti2) %>%
        # summarise(sum(net_kayma)) %>%
        tbl_df()

    net_oy_kaymalari_tablo<-
    parti_ikili %>%
    left_join(.,net_kayma_veri,by=c("parti","parti2")) %>%
    mutate(net_kayma=paste0("%",ifelse(is.na(net_kayma),0,net_kayma))) %>%
    mutate_each(funs(ordered(.,levels=c("ak_parti","chp","mhp","hdp","diger"))),parti,parti2) %>%
    mutate(parti=plyr::revalue(parti,c("ak_parti"="AK Parti","chp"="CHP","mhp"="MHP","hdp"="HDP","diger"="Diğer"))) %>%
    mutate(parti2=plyr::revalue(parti2,c("ak_parti"="AK Parti","chp"="CHP","mhp"="MHP","hdp"="HDP","diger"="Diğer"))) %>%
    spread(parti2,net_kayma) %>% rename(` `=parti)

    return(list(oy_kaymalari_tablo,net_oy_kaymalari_tablo))
}
