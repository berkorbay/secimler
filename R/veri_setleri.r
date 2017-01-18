#'@title 7 Haziran 2015 Genel Seçimleri Sandık Bazında Seçmen ve Oy Sayıları
#'
#'@description
#'Bu veri setinde 7 Haziran 2015 Milletvekili Genel Seçimi'nin sandık bazında seçmen bilgileri ve oy sayıları bulunmaktadır. Veri setinin bazı satırlarında toplam bilgileri verilmiştir. Her sütunun açıklamasında toplam bilgilerinin nasıl ifade edildiği belirtilmektedir.
#'
#'@param il İlgili sandığın seçim ilini ifade eder.
#'@param bolge Sadece Ankara (2), İstanbul (3) ve İzmir (2) seçim bölgelerine ayrılmışlardır. Diğer bütün iller için bölge numarası 1'dir.
#'@param ilce İlgili sandığın ilçesini ifade eder. Satırlarda illerin toplam istatistiklerini belirten değer İli ile ifade edilmiştir, diğer bütün değerler ilçe isimleridir.
#'@param cevre İlgili sandığın seçim çevresi isimlerini içerir. Bu sütundaki özel değerler Genel, İl-İlçe Genel ve Köy-Belde Genel ifadelerinden oluşmaktadır. Özel değerler sırasıyla ilin toplam sandık ve oy sayılarını, ilçelerinin ve merkezinin toplam sandık ve oy sayılarını ve köylerinin ve beldelerinin toplam sandık ve oy sayılarını belirtir.
#'@param cevre_turu İlgili sandığın bulunduğu seçim çevresinin türünü içerir. Seçim çevreleri Köy, Belde, Mahalle veya Cezaevi olabilirler. Bu sütundaki Toplam değeri ilgili satırın bir sandık verisi içermediğini, birden çok sandığın toplamının verilerini içerir. Toplamın neleri kapsadığını aynı satırın kendisinden önceki sütunlarında verilen bilgilerde bulabilirsiniz.
#'@param sandik Sandık numarasını veya toplam değerdeki sandık sayısını belirtir. Eğer aynı satırda `cevre_turu` değeri Toplam ise toplam sandık sayısını, başka bir değer ise sandık numarasını verir.
#'@param kayitli_secmen İlgili sandıkta (eğer `cevre_turu` Toplam ise ilgili bölgede) kayıtlı toplam seçmen sayısını verir.
#'@param oy_kullanan İlgili sandıkta (eğer `cevre_turu` Toplam ise ilgili bölgede) oy kullanan toplam kişi sayısını verir. Bazı sandıklarda oy kullanan kişi sayısı kayıtlı seçmen sayısından fazla olabilir. Bunun sebepleri arasında görevlilerin (ör. sandık görevlisi, polis memuru) tutanakla kendi kayıtlı sandıklarının yerine başka sandıklarda oy atabilmeleri ve kayıt hataları bulunmaktadır.
#'@param gecerli_oy İlgili sandıktaki (eğer `cevre_turu` Toplam ise ilgili bölgede) geçerli oy sayısını verir. Oylar çeşitli sebeplerle geçerli sayılmayabilmektedir. (Ör. Boş oy, çift mühür, oyun nereye basıldığının anlaşılamaması.)
#'@param ak_parti Adalet ve Kalkınma Partisi (AK PARTİ) 14 Ağustos 2001 tarihinde kurulmuştur. Merkez sağ partisidir. İdeolojileri sosyal muhafazakarlık, muhafazakar demokrasi, ekonomik liberalizm ve yeni Osmanlıcılıktır. 2002 genel seçimlerinden Kasım 2015 genel seçimlerine kadar en yüksek oyu alıp iktidar partisi olmuşlardır. (Kaynak: Vikipedi)
#'@param chp Cumhuriyet Halk Partisi (CHP) 9 Eylül 1923'te Mustafa Kemal Atatürk tarafından kurulmuştur. Merkez sol partisidir. İdeolojileri Atatürkçülük, sosyal demokrasi ve demokratik soldur. 2002 genel seçimlerinden Kasım 2015 genel seçimlerine kadar en yüksek ikinci oyu alıp ana muhalefet partisi olmuşlardır. (Kaynak: Vikipedi)
#'@param mhp Milliyetçi Hareket Partisi (MHP) 9 Şubat 1969 tarihinde kurulmuştur. Sağ partidir. İdeolojileri Türk milliyetçiliği, ülkücülük ve dokuz ışıktır. Kasım 2015 genel seçimlerinde en yüksek üçüncü oyu alıp muhalefet olmuşlardır. (Kaynak: Vikipedi)
#'@param hdp Halkların Demokratik Partisi (HDP) 27 Ekim 2013 tarihinde kurulmuştur. Sol partidir. İdeolojileri siyasi çoğulculuk, sosyalizm, radikal demokrasi, demokratik sosyalizm, feminizm ve doğrudan demokrasidir. Kasım 2015 genel seçimlerinde en yüksek dördüncü oyu alıp muhalefet olmuşlardır. (Kaynak: Vikipedi)
#'@param dyp Doğru Yol Partisi (DYP)
#'@param anadolu_partisi Anadolu Partisi
#'@param hak_par Hak ve Özgürlükler Partisi (HAK-PAR)
#'@param kp Komünist Parti (KP)
#'@param millet_partisi Millet Partisi
#'@param hap Hak ve Adalet Partisi (HAP)
#'@param mep Merkez Parti (MEP)
#'@param turk_parti Toplumsal Uzlaşma Reform ve Kalkınma Partisi (TURK Parti)
#'@param hkp Halkın Kurtuluş Partisi (HKP)
#'@param ldp Liberal Demokrat Parti (LDP)
#'@param saadet_partisi Saadet Partisi
#'@param dsp Demokratik Sol Parti (DSP)
#'@param yurt_parti Yurt Partisi
#'@param dp Demokrat Parti (DP)
#'@param vatan_partisi Vatan Partisi
#'@param btp Bağımsız Türkiye Partisi (BTP)
#'@param bagimsiz Bağımsız adaylar.
"secim150607g"

#'@title 1 Kasım 2015 Genel Seçimleri Sandık Bazında Seçmen ve Oy Sayıları
#'
#'@description
#'Bu veri setinde 1 Kasım 2015 Milletvekili Genel Seçimi'nin sandık bazında seçmen bilgileri ve oy sayıları bulunmaktadır. Veri setinin bazı satırlarında toplam bilgileri verilmiştir. Her sütunun açıklamasında toplam bilgilerinin nasıl ifade edildiği belirtilmektedir.
#'
#'@param il İlgili sandığın seçim ilini ifade eder.
#'@param bolge Sadece Ankara (2), İstanbul (3) ve İzmir (2) seçim bölgelerine ayrılmışlardır. Diğer bütün iller için bölge numarası 1'dir.
#'@param ilce İlgili sandığın ilçesini ifade eder. Satırlarda illerin toplam istatistiklerini belirten değer İli ile ifade edilmiştir, diğer bütün değerler ilçe isimleridir.
#'@param cevre İlgili sandığın seçim çevresi isimlerini içerir. Bu sütundaki özel değerler Genel, İl-İlçe Genel ve Köy-Belde Genel ifadelerinden oluşmaktadır. Özel değerler sırasıyla ilin toplam sandık ve oy sayılarını, ilçelerinin ve merkezinin toplam sandık ve oy sayılarını ve köylerinin ve beldelerinin toplam sandık ve oy sayılarını belirtir.
#'@param cevre_turu İlgili sandığın bulunduğu seçim çevresinin türünü içerir. Seçim çevreleri Köy, Belde, Mahalle veya Cezaevi olabilirler. Bu sütundaki Toplam değeri ilgili satırın bir sandık verisi içermediğini, birden çok sandığın toplamının verilerini içerir. Toplamın neleri kapsadığını aynı satırın kendisinden önceki sütunlarında verilen bilgilerde bulabilirsiniz.
#'@param sandik Sandık numarasını veya toplam değerdeki sandık sayısını belirtir. Eğer aynı satırda `cevre_turu` değeri Toplam ise toplam sandık sayısını, başka bir değer ise sandık numarasını verir.
#'@param kayitli_secmen İlgili sandıkta (eğer `cevre_turu` Toplam ise ilgili bölgede) kayıtlı toplam seçmen sayısını verir.
#'@param oy_kullanan İlgili sandıkta (eğer `cevre_turu` Toplam ise ilgili bölgede) oy kullanan toplam kişi sayısını verir. Bazı sandıklarda oy kullanan kişi sayısı kayıtlı seçmen sayısından fazla olabilir. Bunun sebepleri arasında görevlilerin (ör. sandık görevlisi, polis memuru) tutanakla kendi kayıtlı sandıklarının yerine başka sandıklarda oy atabilmeleri ve kayıt hataları bulunmaktadır.
#'@param gecerli_oy İlgili sandıktaki (eğer `cevre_turu` Toplam ise ilgili bölgede) geçerli oy sayısını verir. Oylar çeşitli sebeplerle geçerli sayılmayabilmektedir. (Ör. Boş oy, çift mühür, oyun nereye basıldığının anlaşılamaması.)
#'@param ak_parti Adalet ve Kalkınma Partisi (AK PARTİ) 14 Ağustos 2001 tarihinde kurulmuştur. Merkez sağ partisidir. İdeolojileri sosyal muhafazakarlık, muhafazakar demokrasi, ekonomik liberalizm ve yeni Osmanlıcılıktır. 2002 genel seçimlerinden Kasım 2015 genel seçimlerine kadar en yüksek oyu alıp iktidar partisi olmuşlardır. (Kaynak: Vikipedi)
#'@param chp Cumhuriyet Halk Partisi (CHP) 9 Eylül 1923'te Mustafa Kemal Atatürk tarafından kurulmuştur. Merkez sol partisidir. İdeolojileri Atatürkçülük, sosyal demokrasi ve demokratik soldur. 2002 genel seçimlerinden Kasım 2015 genel seçimlerine kadar en yüksek ikinci oyu alıp ana muhalefet partisi olmuşlardır. (Kaynak: Vikipedi)
#'@param mhp Milliyetçi Hareket Partisi (MHP) 9 Şubat 1969 tarihinde kurulmuştur. Sağ partidir. İdeolojileri Türk milliyetçiliği, ülkücülük ve dokuz ışıktır. Kasım 2015 genel seçimlerinde en yüksek üçüncü oyu alıp muhalefet olmuşlardır. (Kaynak: Vikipedi)
#'@param hdp Halkların Demokratik Partisi (HDP) 27 Ekim 2013 tarihinde kurulmuştur. Sol partidir. İdeolojileri siyasi çoğulculuk, sosyalizm, radikal demokrasi, demokratik sosyalizm, feminizm ve doğrudan demokrasidir. Kasım 2015 genel seçimlerinde en yüksek dördüncü oyu alıp muhalefet olmuşlardır. (Kaynak: Vikipedi)
#'@param bbp Büyük Birlik Partisi (BBP)
#'@param btp Bağımsız Türkiye Partisi (BTP)
#'@param dp Demokrat Parti (DP)
#'@param dsp Demokratik Sol Parti (DSP)
#'@param dyp Doğru Yol Partisi (DYP)
#'@param hak_par Hak ve Özgürlükler Partisi (HAK-PAR)
#'@param hkp Halkın Kurtuluş Partisi (HKP)
#'@param ldp Liberal Demokrat Parti (LDP)
#'@param millet_partisi Millet Partisi
#'@param saadet_partisi Saadet Partisi
#'@param vatan_partisi Vatan Partisi
#'@param bagimsiz Bağımsız adaylar.
"secim151101g"
