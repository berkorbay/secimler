#'@title 7 Haziran 2015 Genel Seçimleri Sandık Bazında Seçmen ve Oy Sayıları
#'
#'@description
#'Bu veri setinde 7 Haziran 2015 Milletvekili Genel Seçimi'nin sandık bazında seçmen bilgileri ve oy sayıları bulunmaktadır. Veri setinin bazı satırlarında toplam bilgileri verilmiştir. Her sütunun açıklamasında toplam bilgilerinin nasıl ifade edildiği belirtilmektedir.
#'
#'@param il İlgili sandığın seçim ili veya bölgesini ifade eder. Sadece Ankara (2), İstanbul (3) ve İzmir (2) bölgelere ayrılmışlardır.
#'@param ilce İlgili sandığın ilçesini ifade eder. Satırlarda illerin toplam istatistiklerini belirten değer İli ile ifade edilmiştir, diğer bütün değerler ilçe isimleridir.
#'@param cevre İlgili sandığın seçim çevresi isimlerini içerir. Bu sütundaki özel değerler Genel, İl-İlçe Genel ve Köy-Belde Genel ifadelerinden oluşmaktadır. Özel değerler sırasıyla ilin toplam sandık ve oy sayılarını, ilçelerinin ve merkezinin toplam sandık ve oy sayılarını ve köylerinin ve beldelerinin toplam sandık ve oy sayılarını belirtir.
#'@param cevre_turu İlgili sandığın bulunduğu seçim çevresinin türünü içerir. Seçim çevreleri Köy, Belde, Mahalle veya Cezaevi olabilirler. Bu sütundaki Toplam değeri ilgili satırın bir sandık verisi içermediğini, birden çok sandığın toplamının verilerini içerir. Toplamın neleri kapsadığını aynı satırın kendisinden önceki sütunlarında verilen bilgilerde bulabilirsiniz.
#'@param sandik Sandık numarasını veya toplam değerdeki sandık sayısını belirtir. Eğer aynı satırda `cevre_turu` değeri Toplam ise toplam sandık sayısını, başka bir değer ise sandık numarasını verir.
#'@param kayitli_secmen İlgili sandıkta (eğer `cevre_turu` Toplam ise ilgili bölgede) kayıtlı toplam seçmen sayısını verir.
#'@param oy_kullanan İlgili sandıkta (eğer `cevre_turu` Toplam ise ilgili bölgede) oy kullanan toplam kişi sayısını verir. Bazı sandıklarda oy kullanan kişi sayısı kayıtlı seçmen sayısından fazla olabilir. Bunun sebepleri arasında görevlilerin (ör. sandık görevlisi, polis memuru) tutanakla kendi kayıtlı sandıklarının yerine başka sandıklarda oy atabilmeleri ve kayıt hataları bulunmaktadır.
#'@param gecerli_oy İlgili sandıktaki (eğer `cevre_turu` Toplam ise ilgili bölgede) geçerli oy sayısını verir. Oylar çeşitli sebeplerle geçerli sayılmayabilmektedir. (Ör. Boş oy, çift mühür, oyun nereye basıldığının anlaşılamaması.)
"secim150607g"

#'@title 1 Kasım 2015 Genel Seçimleri Sandık Bazında Seçmen ve Oy Sayıları
#'
#'@description
#'Bu veri setinde 1 Kasım 2015 Milletvekili Genel Seçimi'nin sandık bazında seçmen bilgileri ve oy sayıları bulunmaktadır. Veri setinin bazı satırlarında toplam bilgileri verilmiştir. Her sütunun açıklamasında toplam bilgilerinin nasıl ifade edildiği belirtilmektedir.
#'
#'@param il İlgili sandığın seçim ili veya bölgesini ifade eder. Sadece Ankara (2), İstanbul (3) ve İzmir (2) bölgelere ayrılmışlardır.
#'@param ilce İlgili sandığın ilçesini ifade eder. Satırlarda illerin toplam istatistiklerini belirten değer İli ile ifade edilmiştir, diğer bütün değerler ilçe isimleridir.
#'@param cevre İlgili sandığın seçim çevresi isimlerini içerir. Bu sütundaki özel değerler Genel, İl-İlçe Genel ve Köy-Belde Genel ifadelerinden oluşmaktadır. Özel değerler sırasıyla ilin toplam sandık ve oy sayılarını, ilçelerinin ve merkezinin toplam sandık ve oy sayılarını ve köylerinin ve beldelerinin toplam sandık ve oy sayılarını belirtir.
#'@param cevre_turu İlgili sandığın bulunduğu seçim çevresinin türünü içerir. Seçim çevreleri Köy, Belde, Mahalle veya Cezaevi olabilirler. Bu sütundaki Toplam değeri ilgili satırın bir sandık verisi içermediğini, birden çok sandığın toplamının verilerini içerir. Toplamın neleri kapsadığını aynı satırın kendisinden önceki sütunlarında verilen bilgilerde bulabilirsiniz.
#'@param sandik Sandık numarasını veya toplam değerdeki sandık sayısını belirtir. Eğer aynı satırda `cevre_turu` değeri Toplam ise toplam sandık sayısını, başka bir değer ise sandık numarasını verir.
#'@param kayitli_secmen İlgili sandıkta (eğer `cevre_turu` Toplam ise ilgili bölgede) kayıtlı toplam seçmen sayısını verir.
#'@param oy_kullanan İlgili sandıkta (eğer `cevre_turu` Toplam ise ilgili bölgede) oy kullanan toplam kişi sayısını verir. Bazı sandıklarda oy kullanan kişi sayısı kayıtlı seçmen sayısından fazla olabilir. Bunun sebepleri arasında görevlilerin (ör. sandık görevlisi, polis memuru) tutanakla kendi kayıtlı sandıklarının yerine başka sandıklarda oy atabilmeleri ve kayıt hataları bulunmaktadır.
#'@param gecerli_oy İlgili sandıktaki (eğer `cevre_turu` Toplam ise ilgili bölgede) geçerli oy sayısını verir. Oylar çeşitli sebeplerle geçerli sayılmayabilmektedir. (Ör. Boş oy, çift mühür, oyun nereye basıldığının anlaşılamaması.)
"secim151101g"
