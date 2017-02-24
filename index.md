## Seçimler R paketi

Bu paket ülkemizdeki 7 Haziran 2015 ve 1 Kasım 2015 tarihli genel seçimlerin sandık bazındaki verilerini ve bu verilerden yararlanarak oluşturlabilecek raporların altyapı fonksiyonlarını içermektedir. Eğitim amaçlı bir veri ve analiz paketidir.

Kullanım adımlarını bu dökümanın aşağısında bulabilirsiniz.

### Otomatik Raporlama Neden Önemli?

Sürekli tekrarlanan ve kalıpları az çok belirli işler için (ör. piyasa raporları, hava durumu raporları, maç yorumları) daha az iş gücü ve zaman ayırmak mümkün.

Bir kez iyi tasarlanmış bir rapor taslağı oluşturulduktan sonra her bir raporun oluşturulması, o raporun bir insan tarafından baştan yazılmasından çok daha kısa sürmekte ve çok daha tutarlı hale gelmektedir. Otomatik oluşturulmuş veri analiz raporları endüstrinin yeni standardı haline gelmektedir.

Farklı örnekler için Automated Insights ve Narrative Science gibi şirketleri inceleyebilirsiniz.

### Örnek Raporlar

Paket vasıtasıyla iller ve ilçeler için seçim sonuç ve karşılaştırma raporları çıkarabilirsiniz. Birkaç örnek görmek için aşağıdaki raporları inceleyebilirsiniz.

 + İstanbul İl Geneli ([html](https://berkorbay.github.io/secimler/ornek_raporlar/Istanbul_il_raporu.html) \| [pdf](https://berkorbay.github.io/secimler/ornek_raporlar/Istanbul_il_raporu.pdf))
 + İzmir İl Geneli ([html](https://berkorbay.github.io/secimler/ornek_raporlar/Izmir_il_raporu.html) \| [pdf](https://berkorbay.github.io/secimler/ornek_raporlar/Izmir_il_raporu.pdf))
 + Ankara İl Geneli ([html](https://berkorbay.github.io/secimler/ornek_raporlar/Ankara_il_raporu.html) \| [pdf](https://berkorbay.github.io/secimler/ornek_raporlar/Ankara_il_raporu.pdf))
 + İstanbul Beşiktaş ([html](https://berkorbay.github.io/secimler/ornek_raporlar/Istanbul_Besiktas_ilce_raporu.html) \| [pdf](https://berkorbay.github.io/secimler/ornek_raporlar/Istanbul_Besiktas_ilce_raporu.pdf))
 + İzmir Konak ([html](https://berkorbay.github.io/secimler/ornek_raporlar/Izmir_Konak_ilce_raporu.html) \| [pdf](https://berkorbay.github.io/secimler/ornek_raporlar/Izmir_Konak_ilce_raporu.pdf))
 + Ankara Çankaya ([html](https://berkorbay.github.io/secimler/ornek_raporlar/Ankara_Cankaya_ilce_raporu.html) \| [pdf](https://berkorbay.github.io/secimler/ornek_raporlar/Ankara_Cankaya_ilce_raporu.pdf))

Aşağıdaki adımları uygulayarak istediğiniz il veya ilçe raporunu çıkarabilirsiniz. Raporlar hakkında iletişime geçmek için LinkedIn'den bana ulaşabilirsiniz ([tıklayın](https://www.linkedin.com/in/berkorbay)).

## Paketi nasıl kullanacağım?

Paketi kullanmak son derece basit.

1. İlk adımda bilgisayarınızda R 3.3.1 veya daha yüksek bir sürümünün olduğundan emin olun. R programını indirmek için [tıklayın](https://cloud.r-project.org/).

2. R'ı çalıştırdıktan sonra aşağıdaki kodları kopyala yapıştır ile ekleyin.

  ```r
  #Paketleri bu adresten yükle
  options(repos="http://cran.rstudio.com/")
  #devtools R'ın GitHub üzerinden paket yüklememize izin vermesini sağlayacak.
  install.packages("devtools")
  #Paketimizi indirelim.
  devtools::install_github("berkorbay/secimler")

  #Şimdi indirdiğimiz paketi yükleyelim
  library(secimler)
  ```

3. Paketi yükledikten sonra iki seçimin de sandık bazındaki verilerine `secim150607g` ve `secim151101g` yazarak ulaşabilirsiniz.

Paket ve R üzerinde detaylı analizler yapmak istiyorsanız [buraya tıklayarak](https://r338.github.io/ab-2017/) ilgili eğitim dökümanlarına ulaşabilirsiniz. (Gün 2 secimler paketi ile işlemleri içeriyor).

## Raporları nasıl oluşturacağım?

Rapor oluşturmak için ayrıca Pandoc denilen bir programı bilgisayarınıza indirip yüklemeniz gerekiyor. Yükleme talimatları için [tıklayın](http://pandoc.org/installing.html). Raporları üç çıktı formatında alabilirsiniz: HTML, PDF ve Word. PDF için ayrıca LaTeX yüklemeniz gerekiyor. Pandoc sayfasında ilgili talimatları bulacaksınız. Pandoc'u ve yükledikten sonra eğer üstteki kodları da çalıştırdıysanız R'a aşağıdaki komutları yazmanız yeterli.

  ```r
  #Eğer R'ı tekrar başlattıysanız paketi tekrar çağırmanız gerekiyor.
  library(secimler)
  #Örneğin Ankara'nın html formatında seçim raporunu istiyoruz.
  rapor_getir(il_adi="Ankara",rapor_turu="html_document")
  #Diyelim ki Ankara'nın Bala ilçesinin seçim raporunu word formatında istiyoruz
  rapor_getir(il_adi="Ankara",ilce_adi="Bala",rapor_turu="word_document")
  ```

## Sorular ve Sorunlar

İlgili sorularınızı ve sorunlarınızı bana buradan Issues sekmesinden belirtebilir veya bana LinkedIn üzerinden erişebilirsiniz ([tıklayın](https://www.linkedin.com/in/berkorbay)).

## Teşekkürler

Bu paketteki verilerin ham halleri TÜİK ve YSK tarafından sağlanmıştır. Ayrıca gerek paketin oluşmasında yardımcı olan, gerek test aşamasında destek olan ve yeri geldiğinde fikirleriyle gelişmesini sağlayan başta Osman Coşkunoğlu, Mustafa Baydoğan, Atilla Orbay, Yeşim Kamile Aktuğlu ve Nezih Bilgin olmak üzere herkese çok teşekkür ederim.
