##
#' @export
rapor_getir<-function(il_adi,ilce_adi="il",rapor_turu=c("html_document","pdf_document","word_document"),cikti_klasoru=""){

    if(cikti_klasoru==""){
        cikti_klasoru <- getwd()
        print(paste0("Çıktı klasörü otomatik olarak belirlendi: ",cikti_klasoru))
    }

    if(!dir.exists(cikti_klasoru)){
        stop("Böyle bir klasör bulunmuyor. Lütfen geçerli bir klasör girin.")
    }

    extension_name <- switch(rapor_turu[1],html_document="html",pdf_document="pdf",word_document="docx")

    il_listesi <- secim151101g %>% filter(cevre_turu != "Toplam") %>% distinct(il)

    if(!(il_adi %in% il_listesi$il)){
        print("Yanlış il adı. İl listesini aşağıda bulabilirsiniz")
        print(il_listesi,n=Inf)
        stop("İl adı bulunamadı.")
    }

    if(ilce_adi == "il"){
        return(rmarkdown::render(system.file("rmd/il_bazi_rapor.Rmd",package="secimler",mustWork=TRUE),output_file=paste0(cikti_klasoru,"/",computer_friendlying(il_adi),"_il_raporu.",extension_name),output_format=rapor_turu[1]))
    }

    ilce_listesi <- secim151101g %>% filter(il == il_adi & cevre_turu != "Toplam") %>% distinct(ilce)

    if(!(ilce_adi %in% ilce_listesi$ilce)){
        print("Yanlış ilçe adı. İlçe listesini aşağıda bulabilirsiniz")
        print(ilce_listesi,n=Inf)
        stop("İlçe adı bulunamadı.")
    }

    return(rmarkdown::render(system.file("rmd/ilce_bazi_rapor.Rmd",package="secimler",mustWork=TRUE),output_file=paste0(cikti_klasoru,"/",computer_friendlying(il_adi),"_",computer_friendlying(ilce_adi),"_ilce_raporu.",extension_name),output_format=rapor_turu[1]))

}

#' @export
klasor_getir<-function(il_adi,ilce_adi="il",rapor_turu=c("html_document","pdf_document"),cikti_klasoru=eval(substitute(getwd(),globalenv()))){
    extension_name <- switch(rapor_turu[1],html_document="html",pdf_document="pdf")
    print(paste0(cikti_klasoru,"/",computer_friendlying(il_adi),"_il_raporu.",extension_name))
}


#' @export
computer_friendlying<-function(mytext){
	mytext<-gsub(" ","_",mytext)
	mytext<-gsub("ç","c",mytext)
	mytext<-gsub("ş","s",mytext)
	mytext<-gsub("ğ","g",mytext)
	mytext<-gsub("ü","u",mytext)
	mytext<-gsub("ö","o",mytext)
	mytext<-gsub("ı","i",mytext)
	mytext<-gsub("Ç","C",mytext)
	mytext<-gsub("Ş","S",mytext)
	mytext<-gsub("Ğ","G",mytext)
	mytext<-gsub("Ü","U",mytext)
	mytext<-gsub("Ö","O",mytext)
	mytext<-gsub("İ","I",mytext)
	mytext
}
