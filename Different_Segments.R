library(zoo)

# Set variables as timeseries
real = ts(newdata$Freq)
baseline = ts(newdata$yhat)
add = ts(newdata$ad)
date_time = newdata$date_time

#make one dataset with the baseline and forecasts as timeseries
data = zoo(cbind(real, baseline))
head(data)

#intitialize params
addlength = 5
actualsum = 0
forecastedsum = 0
index = 0
impact = ts(replicate(sum(newdata$ad != 0), 0))
addinfo = newdata[FALSE,]

# loop through data for adds
for(i in 1:nrow(data)){
  if(newdata$ad[i] > 0){
    for(j in 1:addlength){
      actualsum = real[i+j] + actualsum
      forecastedsum = baseline[i+j] + forecastedsum
    }
    impact[index] = actualsum - forecastedsum
    addinfo[index,] = newdata[i,]
    index = index + 1
    actualsum = 0
    forecastedsum = 0
  }
  
}
#Make one big dataframe with all add info and impact per add
impact = data.frame(impact)
impact = impact[1:4557,]
addinfo = cbind(addinfo, impact)
#delete last row because no impact can be calculated at 30-06-2019 23:58
addinfo = addinfo[1:4556,]

sqldf("SELECT distinct(program_category_before)
      FROM addinfo")


#segment the impact - Goodad
GA1 = addinfo[addinfo$goodad=="1",]
GA0 = addinfo[addinfo$goodad=="0",]
impact_GA1 = mean(GA1$impact)
impact_GA0 = mean(GA0$impact)

#segment the impact - spotlength
SL1 = addinfo[addinfo$spotlength=="1",]
SL2 = addinfo[addinfo$spotlength=="2",]
SL3 = addinfo[addinfo$spotlength=="3",]
impact_SL1 = mean(SL1$impact)
impact_SL2 = mean(SL2$impact)
impact_SL3 = mean(SL3$impact)

#segment the impact - position in break
PB1 = addinfo[addinfo$position_in_break > '0' | addinfo$position_in_break < '11', ]
PB2 = addinfo[addinfo$position_in_break > '10' | addinfo$position_in_break < '91', ]
PB3 = addinfo[addinfo$position_in_break > '90' | addinfo$position_in_break < '100', ]
impact_PB1 = mean(PB1$impact)
impact_PB2 = mean(PB2$impact)
impact_PB3 = mean(PB3$impact)

#segment the impact - Operator
OR1 = addinfo[addinfo$operator == 'Ad Alliance', ]
OR2 = addinfo[addinfo$operator == 'Ster', ]
OR3 = addinfo[addinfo$operator == 'Talpa TV', ]
impact_OR1 = mean(OR1$impact)
impact_OR2 = mean(OR2$impact)
impact_OR3 = mean(OR3$impact)

#segment the impact - GRP
GRP0 = addinfo[addinfo$gross_rating_point == '0', ]
GRP1 = addinfo[addinfo$gross_rating_point > '0' | addinfo$gross_rating_point < '0.5', ]
GRP2 = addinfo[addinfo$gross_rating_point >= '0.5' | addinfo$gross_rating_point < '1', ]
GRP3 = addinfo[addinfo$gross_rating_point >= '1', ]
impact_GRP0 = mean(GRP0$impact)
impact_GRP1 = mean(GRP1$impact)
impact_GRP2 = mean(GRP2$impact)
impact_GRP3 = mean(GRP3$impact)

#segment the impact - Time of day
# TOD0 = addinfo[addinfo$time > '00:00:00'| addinfo$time <= '05:00:00', ]
# TOD1 = addinfo[addinfo$time > '05:00:00' | addinfo$time <= '12:00:00', ]
# TOD2 = addinfo[addinfo$time > '12:00:00' | addinfo$time <= '18:00:00', ]
# TOD3 = addinfo[addinfo$time > '18:00:00' | addinfo$time <= '23:59:59', ]
# impact_TOD0 = mean(TOD0$impact)
# impact_TOD1 = mean(TOD1$impact)
# impact_TOD2 = mean(TOD2$impact)
# impact_TOD3 = mean(TOD3$impact)

#segment the impact - day of week


#segment the impact - product type
Wasmachines = addinfo[addinfo$product_category == 'wasmachines',]
Televisie = addinfo[addinfo$product_category == 'televisies',]
Laptops = addinfo[addinfo$product_category == 'laptops',]
impact_wasmachines = mean(Wasmachines$impact)
impact_televisie = mean(Televisie$impact)
impact_laptops = mean(Laptops$impact)

#segment the impact - type of program before/after
bpopulaire_wetenschap = addinfo[addinfo$program_category_before == '(populaire) wetenschap',]
bactualiteiten = addinfo[addinfo$program_category_before == 'actualiteiten',]
bactuele_sportinformatie = addinfo[addinfo$program_category_before == 'actuele sportinformatie',]
balgemene_consumenten_informatie = addinfo[addinfo$program_category_before == 'algemene consumenten informatie',]
banimation_film = addinfo[addinfo$program_category_before == 'animation film',]
banimation_serie_cartoon = addinfo[addinfo$program_category_before == 'animation serie/cartoon',]
bauto_motor_boot = addinfo[addinfo$program_category_before == 'auto/motor/boot/fiets/verkeer',]
bBTL_series_spanning = addinfo[addinfo$program_category_before == 'btl series: spanning',]
bBTL_series_drama = addinfo[addinfo$program_category_before == 'btl series: drama',]
bBTL_series_sitcomedy = addinfo[addinfo$program_category_before == 'btl series: (sit)comedy',]
bBTL_series_overig = addinfo[addinfo$program_category_before == 'btl series: overig',]
bBTL_series_soap = addinfo[addinfo$program_category_before == 'btl series: soap',]
bBLT_films_comedy = addinfo[addinfo$program_category_before == 'blt films: comedy',]
bBLT_films_drama = addinfo[addinfo$program_category_before == 'blt films: drama',]
bBLT_films_overig = addinfo[addinfo$program_category_before == 'blt films: overig',]
bBLT_films_spanning = addinfo[addinfo$program_category_before == 'blt films: spanning',]
bcabaret_kleinkunst = addinfo[addinfo$program_category_before == 'cabaret/kleinkunst',]
bclips = addinfo[addinfo$program_category_before == 'clip(s)',]
bcomment_of_event = addinfo[addinfo$program_category_before == 'comment of event',]
bdebate_talkshow = addinfo[addinfo$program_category_before =='debate/talk show',]
bdocumentary_film = addinfo[addinfo$program_category_before == 'documentary film',]
bdocumentary_serie = addinfo[addinfo$program_category_before == 'documentary serie',]
bdocusoap = addinfo[addinfo$program_category_before == 'docusoap/reality serie',]
beten_drinken_koken = addinfo[addinfo$program_category_before == 'eten/drinken/koken',]
bfilm = addinfo[addinfo$program_category_before == 'film',]
bgame_quiz = addinfo[addinfo$program_category_before == 'game/quiz',]
bgezondheid_lifestyle_opvoeding_coaching = addinfo[addinfo$program_category_before == 'gezondheid/lifestyle/opvoeding/coaching',]
bgodsdienst_verkondiging = addinfo[addinfo$program_category_before == 'godsdienst/verkondiging',]
bjustitie_recht = addinfo[addinfo$program_category_before == 'justitie/recht',]
bkinderen_non_fictie = addinfo[addinfo$program_category_before == 'kinderen: non fictie',]
bkinderfilms_tekenen_animatie_poppen = addinfo[addinfo$program_category_before == 'kinderfilms: tekenfilm/animatie/poppen',]
bkunst = addinfo[addinfo$program_category_before == 'kunst',]
bmagazine = addinfo[addinfo$program_category_before == 'magazine',]
bnatuur_milieu = addinfo[addinfo$program_category_before == 'natuur & milieu',]
bnews_flash = addinfo[addinfo$program_category_before == 'news/flash',]
bniet_opgegeven = addinfo[addinfo$program_category_before == 'niet opgegeven',]
bnieuws =  addinfo[addinfo$program_category_before == 'nieuws',]
bnld_films_comedy = addinfo[addinfo$program_category_before == 'nld films: comedy',]
bnld_films_drama = addinfo[addinfo$program_category_before == 'nld films: drama',]
bnld_films_spanning = addinfo[addinfo$program_category_before == 'nld films: spanning',]
bnld_series_sitcomedy = addinfo[addinfo$program_category_before == 'nld series: (sit)comedy',,]
bnld_series_drama = addinfo[addinfo$program_category_before == 'nld series: drama',]
bnld_series_overig = addinfo[addinfo$program_category_before == 'nld series: overig',]
bnld_series_soap = addinfo[addinfo$program_category_before == 'nld series: soap',]
bnld_series_spanning = addinfo[addinfo$program_category_before == 'nld series: spanning',]
bon_stage = addinfo[addinfo$program_category_before == 'on stage',]
bother_advertising = addinfo[addinfo$program_category_before == 'other advertising',]
bother_studio_structured = addinfo[addinfo$program_category_before == 'other studio/structured/show',]
bother_varied = addinfo[addinfo$program_category_before == 'other varied',]
boverig_amusement = addinfo[addinfo$program_category_before == 'overig amusement',]
boverige_non_fictie = addinfo[addinfo$program_category_before == 'overige non fictie',]
boverige_sportinformatie = addinfo[addinfo$program_category_before == 'overige sportinformatie',]
boverige_sportreportage = addinfo[addinfo$program_category_before == 'overige sportreportage',]
bpopulaire_muziek = addinfo[addinfo$program_category_before == 'populaire muziek: videoclips',]
bprogramme_trailer = addinfo[addinfo$program_category_before == 'programme trailer',]
breality_show = addinfo[addinfo$program_category_before == 'reality show',]
breality_structured = addinfo[addinfo$program_category_before == 'reality structured',]
breizen_vakantie_toerisme = addinfo[addinfo$program_category_before == 'reizen/vakantie/toerisme',]
bsatirisch_programma = addinfo[addinfo$program_category_before == 'satirisch programma',]
bserie = addinfo[addinfo$program_category_before == 'serie',]
bshort_structured_sequence = addinfo[addinfo$program_category_before == 'short structured/sequence',]
bshow = addinfo[addinfo$program_category_before == 'show',]
bspel_quiz = addinfo[addinfo$program_category_before == 'spel & quiz',]
btalentenjacht_of_auditieprogramma = addinfo[addinfo$program_category_before == 'talentenjacht of auditieprogramma',]
btekstuele_informatie = addinfo[addinfo$program_category_before == 'tekstuele informatie',]
bvoetbalreportage = addinfo[addinfo$program_category_before == 'voetbalreportage',]
bweerbericht = addinfo[addinfo$program_category_before == 'weerbericht',]
bwonen_interieurs_tuin = addinfo[addinfo$program_category_before == 'wonen/interieurs/tuin/doe het zelf',]

impact_bpopulaire_wetenschap = mean(bpopulaire_wetenschap$impact)
impact_bactualiteiten = mean(bactualiteiten$impact)
impact_bactuele_sportinformatie = mean(bactuele_sportinformatie$impact)
impact_balgemene_consumenten_informatie = mean(balgemene_consumenten_informatie$impact)
impact_banimation_film = mean(banimation_film$impact)
impact_banimation_serie_cartoon = mean(banimation_serie_cartoon$impact)
impact_bauto_motor_boot = mean(bauto_motor_boot$impact)
impact_bBTL_series_spanning = mean(bBTL_series_spanning$impact)
impact_bBTL_series_drama = mean(bBTL_series_drama$impact)
impact_bBTL_series_sitcomedy = mean(bBTL_series_sitcomedy$impact)
impact_bBTL_series_overig = mean(bBTL_series_overig$impact)
impact_bBTL_series_soap = mean(bBTL_series_soap$impact)
impact_bBLT_films_comedy = mean(bBLT_films_comedy$impact)
impact_bBLT_films_drama = mean(bBLT_films_drama$impact)
impact_bBLT_films_overig = mean(bBLT_films_overig$impact)
impact_bBLT_films_spanning = mean(bBLT_films_spanning$impact)
impact_bcabaret_kleinkunst = mean(bcabaret_kleinkunst$impact)
impact_bclips = mean(bclips$impact)
impact_bcomment_of_event = mean(bcomment_of_event$impact)
impact_bdebate_talkshow = mean(bdebate_talkshow$impact)
impact_bdocumentary_film = mean(bdocumentary_film$impact)
impact_bdocumentary_serie = mean(bdocumentary_serie$impact)
impact_bdocusoap = mean(bdocusoap$impact)
impact_beten_drinken_koken = mean(beten_drinken_koken$impact)
impact_bfilm = mean(bfilm$impact)
impact_bgame_quiz = mean(bgame_quiz$impact)
impact_bgezondheid_lifestyle_opvoeding_coaching = mean(bgezondheid_lifestyle_opvoeding_coaching$impact)
impact_bgodsdienst_verkondiging = mean(bgodsdienst_verkondiging$impact)
impact_bjustitie_recht = mean(bjustitie_recht$impact)
impact_bkinderen_non_fictie = mean(bkinderen_non_fictie$impact)
impact_bkinderfilms_tekenen_animatie_poppen = mean(bkinderfilms_tekenen_animatie_poppen$impact)
impact_bkunst = mean(bkunst$impact)
impact_bmagazine = mean(bmagazine$impact)
impact_bnatuur_milieu = mean(bnatuur_milieu$impact)
impact_bnews_flash = mean(bnews_flash$impact)
impact_bniet_opgegeven = mean(bniet_opgegeven$impact)
impact_bnieuws =  mean(bnieuws$impact)
impact_bnld_films_comedy = mean(bnld_films_comedy$impact)
impact_bnld_films_drama = mean(bnld_series_drama$impact)
impact_bnld_films_spanning = mean(bnld_films_spanning$impact)
impact_bnld_series_sitcomedy = mean(bnld_series_sitcomedy$impact)
impact_bnld_series_drama = mean(bnld_series_drama$impact)
impact_bnld_series_overig = mean(bnld_series_overig$impact)
impact_bnld_series_soap = mean(bnld_series_soap$impact)
impact_bnld_series_spanning = mean(bnld_series_spanning$impact)
impact_bon_stage = mean(bon_stage$impact)
impact_bother_advertising = mean(bother_advertising$impact)
impact_bother_studio_structured = mean(bother_studio_structured$impact)
impact_bother_varied = mean(bother_varied$impact)
impact_boverig_amusement = mean(boverig_amusement$impact)
impact_boverige_non_fictie = mean(boverige_non_fictie$impact)
impact_boverige_sportinformatie = mean(boverige_sportinformatie$impact)
impact_boverige_sportreportage = mean(boverige_sportreportage$impact)
impact_bpopulaire_muziek = mean(bpopulaire_muziek$impact)
impact_bprogramme_trailer = mean(bprogramme_trailer$impact)
impact_breality_show = mean(breality_show$impact)
impact_breality_structured = mean(breality_structured$impact)
impact_breizen_vakantie_toerisme = mean(breizen_vakantie_toerisme$impact)
impact_bsatirisch_programma = mean(bsatirisch_programma$impact)
impact_bserie = mean(bserie$impact)
impact_bshort_structured_sequence = mean(bshort_structured_sequence$impact)
impact_bshow = mean(bshow$impact)
impact_bspel_quiz = mean(bspel_quiz$impact)
impact_btalentenjacht_of_auditieprogramma = mean(btalentenjacht_of_auditieprogramma$impact)
impact_btekstuele_informatie = mean(btekstuele_informatie$impact)
impact_bvoetbalreportage = mean(bvoetbalreportage$impact)
impact_bweerbericht = mean(bweerbericht$impact)
impact_bwonen_interieurs_tuin = mean(bwonen_interieurs_tuin$impact)


apopulaire_wetenschap = addinfo[addinfo$program_category_before == '(populaire) wetenschap',]
aactualiteiten = addinfo[addinfo$program_category_before == 'actualiteiten',]
aactuele_sportinformatie = addinfo[addinfo$program_category_before == 'actuele sportinformatie',]
aalgemene_consumenten_informatie = addinfo[addinfo$program_category_before == 'algemene consumenten informatie',]
aanimation_film = addinfo[addinfo$program_category_before == 'animation film',]
aanimation_serie_cartoon = addinfo[addinfo$program_category_before == 'animation serie/cartoon',]
aauto_motor_boot = addinfo[addinfo$program_category_before == 'auto/motor/boot/fiets/verkeer',]
aBTL_series_spanning = addinfo[addinfo$program_category_before == 'btl series: spanning',]
aBTL_series_drama = addinfo[addinfo$program_category_before == 'btl series: drama',]
aBTL_series_sitcomedy = addinfo[addinfo$program_category_before == 'btl series: (sit)comedy',]
aBTL_series_overig = addinfo[addinfo$program_category_before == 'btl series: overig',]
aBTL_series_soap = addinfo[addinfo$program_category_before == 'btl series: soap',]
aBLT_films_comedy = addinfo[addinfo$program_category_before == 'blt films: comedy',]
aBLT_films_drama = addinfo[addinfo$program_category_before == 'blt films: drama',]
aBLT_films_overig = addinfo[addinfo$program_category_before == 'blt films: overig',]
aBLT_films_spanning = addinfo[addinfo$program_category_before == 'blt films: spanning',]
acabaret_kleinkunst = addinfo[addinfo$program_category_before == 'cabaret/kleinkunst',]
aclips = addinfo[addinfo$program_category_before == 'clip(s)',]
acomment_of_event = addinfo[addinfo$program_category_before == 'comment of event',]
adebate_talkshow = addinfo[addinfo$program_category_before =='debate/talk show',]
adocumentary_film = addinfo[addinfo$program_category_before == 'documentary film',]
adocumentary_serie = addinfo[addinfo$program_category_before == 'documentary serie',]
adocusoap = addinfo[addinfo$program_category_before == 'docusoap/reality serie',]
aeten_drinken_koken = addinfo[addinfo$program_category_before == 'eten/drinken/koken',]
afilm = addinfo[addinfo$program_category_before == 'film',]
agame_quiz = addinfo[addinfo$program_category_before == 'game/quiz',]
agezondheid_lifestyle_opvoeding_coaching = addinfo[addinfo$program_category_before == 'gezondheid/lifestyle/opvoeding/coaching',]
agodsdienst_verkondiging = addinfo[addinfo$program_category_before == 'godsdienst/verkondiging',]
ajustitie_recht = addinfo[addinfo$program_category_before == 'justitie/recht',]
akinderen_non_fictie = addinfo[addinfo$program_category_before == 'kinderen: non fictie',]
akinderfilms_tekenen_animatie_poppen = addinfo[addinfo$program_category_before == 'kinderfilms: tekenfilm/animatie/poppen',]
akunst = addinfo[addinfo$program_category_before == 'kunst',]
amagazine = addinfo[addinfo$program_category_before == 'magazine',]
anatuur_milieu = addinfo[addinfo$program_category_before == 'natuur & milieu',]
anews_flash = addinfo[addinfo$program_category_before == 'news/flash',]
aniet_opgegeven = addinfo[addinfo$program_category_before == 'niet opgegeven',]
anieuws =  addinfo[addinfo$program_category_before == 'nieuws',]
anld_films_comedy = addinfo[addinfo$program_category_before == 'nld films: comedy',]
anld_films_drama = addinfo[addinfo$program_category_before == 'nld films: drama',]
anld_films_spanning = addinfo[addinfo$program_category_before == 'nld films: spanning',]
anld_series_sitcomedy = addinfo[addinfo$program_category_before == 'nld series: (sit)comedy',,]
anld_series_drama = addinfo[addinfo$program_category_before == 'nld series: drama',]
anld_series_overig = addinfo[addinfo$program_category_before == 'nld series: overig',]
anld_series_soap = addinfo[addinfo$program_category_before == 'nld series: soap',]
anld_series_spanning = addinfo[addinfo$program_category_before == 'nld series: spanning',]
aon_stage = addinfo[addinfo$program_category_before == 'on stage',]
aother_advertising = addinfo[addinfo$program_category_before == 'other advertising',]
aother_studio_structured = addinfo[addinfo$program_category_before == 'other studio/structured/show',]
aother_varied = addinfo[addinfo$program_category_before == 'other varied',]
aoverig_amusement = addinfo[addinfo$program_category_before == 'overig amusement',]
aoverige_non_fictie = addinfo[addinfo$program_category_before == 'overige non fictie',]
aoverige_sportinformatie = addinfo[addinfo$program_category_before == 'overige sportinformatie',]
aoverige_sportreportage = addinfo[addinfo$program_category_before == 'overige sportreportage',]
apopulaire_muziek = addinfo[addinfo$program_category_before == 'populaire muziek: videoclips',]
aprogramme_trailer = addinfo[addinfo$program_category_before == 'programme trailer',]
areality_show = addinfo[addinfo$program_category_before == 'reality show',]
areality_structured = addinfo[addinfo$program_category_before == 'reality structured',]
areizen_vakantie_toerisme = addinfo[addinfo$program_category_before == 'reizen/vakantie/toerisme',]
asatirisch_programma = addinfo[addinfo$program_category_before == 'satirisch programma',]
aserie = addinfo[addinfo$program_category_before == 'serie',]
ashort_structured_sequence = addinfo[addinfo$program_category_before == 'short structured/sequence',]
ashow = addinfo[addinfo$program_category_before == 'show',]
aspel_quiz = addinfo[addinfo$program_category_before == 'spel & quiz',]
atalentenjacht_of_auditieprogramma = addinfo[addinfo$program_category_before == 'talentenjacht of auditieprogramma',]
atekstuele_informatie = addinfo[addinfo$program_category_before == 'tekstuele informatie',]
avoetbalreportage = addinfo[addinfo$program_category_before == 'voetbalreportage',]
aweerbericht = addinfo[addinfo$program_category_before == 'weerbericht',]
awonen_interieurs_tuin = addinfo[addinfo$program_category_before == 'wonen/interieurs/tuin/doe het zelf',]

impact_apopulaire_wetenschap = mean(apopulaire_wetenschap$impact)
impact_aactualiteiten = mean(aactualiteiten$impact)
impact_aactuele_sportinformatie = mean(aactuele_sportinformatie$impact)
impact_aalgemene_consumenten_informatie = mean(aalgemene_consumenten_informatie$impact)
impact_aanimation_film = mean(aanimation_film$impact)
impact_aanimation_serie_cartoon = mean(aanimation_serie_cartoon$impact)
impact_aauto_motor_boot = mean(aauto_motor_boot$impact)
impact_aBTL_series_spanning = mean(aBTL_series_spanning$impact)
impact_aBTL_series_drama = mean(aBTL_series_drama$impact)
impact_aBTL_series_sitcomedy = mean(aBTL_series_sitcomedy$impact)
impact_aBTL_series_overig = mean(aBTL_series_overig$impact)
impact_aBTL_series_soap = mean(aBTL_series_soap$impact)
impact_aBLT_films_comedy = mean(aBLT_films_comedy$impact)
impact_aBLT_films_drama = mean(aBLT_films_drama$impact)
impact_aBLT_films_overig = mean(aBLT_films_overig$impact)
impact_aBLT_films_spanning = mean(aBLT_films_spanning$impact)
impact_acabaret_kleinkunst = mean(acabaret_kleinkunst$impact)
impact_aclips = mean(aclips$impact)
impact_acomment_of_event = mean(acomment_of_event$impact)
impact_adebate_talkshow = mean(adebate_talkshow$impact)
impact_adocumentary_film = mean(adocumentary_film$impact)
impact_adocumentary_serie = mean(adocumentary_serie$impact)
impact_adocusoap = mean(adocusoap$impact)
impact_aeten_drinken_koken = mean(aeten_drinken_koken$impact)
impact_afilm = mean(afilm$impact)
impact_agame_quiz = mean(agame_quiz$impact)
impact_agezondheid_lifestyle_opvoeding_coaching = mean(agezondheid_lifestyle_opvoeding_coaching$impact)
impact_agodsdienst_verkondiging = mean(agodsdienst_verkondiging$impact)
impact_ajustitie_recht = mean(ajustitie_recht$impact)
impact_akinderen_non_fictie = mean(akinderen_non_fictie$impact)
impact_akinderfilms_tekenen_animatie_poppen = mean(akinderfilms_tekenen_animatie_poppen$impact)
impact_akunst = mean(akunst$impact)
impact_amagazine = mean(amagazine$impact)
impact_anatuur_milieu = mean(anatuur_milieu$impact)
impact_anews_flash = mean(anews_flash$impact)
impact_aniet_opgegeven = mean(aniet_opgegeven$impact)
impact_anieuws =  mean(anieuws$impact)
impact_anld_films_comedy = mean(anld_films_comedy$impact)
impact_anld_films_drama = mean(anld_series_drama$impact)
impact_anld_films_spanning = mean(anld_films_spanning$impact)
impact_anld_series_sitcomedy = mean(anld_series_sitcomedy$impact)
impact_anld_series_drama = mean(anld_series_drama$impact)
impact_anld_series_overig = mean(anld_series_overig$impact)
impact_anld_series_soap = mean(anld_series_soap$impact)
impact_anld_series_spanning = mean(anld_series_spanning$impact)
impact_aon_stage = mean(aon_stage$impact)
impact_aother_advertising = mean(aother_advertising$impact)
impact_aother_studio_structured = mean(aother_studio_structured$impact)
impact_aother_varied = mean(aother_varied$impact)
impact_aoverig_amusement = mean(aoverig_amusement$impact)
impact_aoverige_non_fictie = mean(aoverige_non_fictie$impact)
impact_aoverige_sportinformatie = mean(aoverige_sportinformatie$impact)
impact_aoverige_sportreportage = mean(aoverige_sportreportage$impact)
impact_apopulaire_muziek = mean(apopulaire_muziek$impact)
impact_aprogramme_trailer = mean(aprogramme_trailer$impact)
impact_areality_show = mean(areality_show$impact)
impact_areality_structured = mean(areality_structured$impact)
impact_areizen_vakantie_toerisme = mean(areizen_vakantie_toerisme$impact)
impact_asatirisch_programma = mean(asatirisch_programma$impact)
impact_aserie = mean(aserie$impact)
impact_ashort_structured_sequence = mean(ashort_structured_sequence$impact)
impact_ashow = mean(ashow$impact)
impact_aspel_quiz = mean(aspel_quiz$impact)
impact_atalentenjacht_of_auditieprogramma = mean(atalentenjacht_of_auditieprogramma$impact)
impact_atekstuele_informatie = mean(atekstuele_informatie$impact)
impact_avoetbalreportage = mean(avoetbalreportage$impact)
impact_aweerbericht = mean(aweerbericht$impact)
impact_awonen_interieurs_tuin = mean(awonen_interieurs_tuin$impact)


#segment the impact - RAIN OR NO RAIN(??)


#segment the impact - Channel
MTV = addinfo[addinfo$channel == 'MTV', ]
Spike = addinfo[addinfo$channel == 'Spike', ]
BBC_First_Holland = addinfo[addinfo$channel == 'BBC First Holland', ]
TLC = addinfo[addinfo$channel == 'TLC', ]
Fox_Sports_3 = addinfo[addinfo$channel == 'Fox Sports 3', ]
TF_Kitchen = addinfo[addinfo$channel == '24Kitchen', ]
Fox_Sports_1 = addinfo[addinfo$channel == 'Fox Sports 1', ]
National_Geographic_Channel = addinfo[addinfo$channel == 'National Geographic Channel', ]
Discovery_Channel = addinfo[addinfo$channel == 'Discovery Channel', ]
Fox = addinfo[addinfo$channel == 'Fox', ]
Comedy_Central = addinfo[addinfo$channel == 'Comedy Central', ]
RTL_4 = addinfo[addinfo$channel == 'RTL 4', ]
RTL_4 = addinfo[addinfo$channel == 'RTL 5', ]
ID = addinfo[addinfo$channel == 'ID', ]
NPO1 = addinfo[addinfo$channel == 'NPO1', ]
NPO3 = addinfo[addinfo$channel == 'NPO3', ]
Viceland = addinfo[addinfo$channel == 'Viceland', ]
RTL_Z = addinfo[addinfo$channel == 'RTL Z', ]
NPO2 = addinfo[addinfo$channel == 'NPO2', ]
Net5 = addinfo[addinfo$channel == 'Net5', ]
RTL_8 = addinfo[addinfo$channel == 'RTL 8', ]
Eurosport = addinfo[addinfo$channel == 'Eurosport', ]
Veronica = addinfo[addinfo$channel == 'Veronica', ]
SBS_6 = addinfo[addinfo$channel == 'SBS 6', ]
Fox_Sports_2 = addinfo[addinfo$channel == 'Fox Sports 2', ]
SBS_9 = addinfo[addinfo$channel == 'SBS 9', ]
TV538 = addinfo[addinfo$channel == 'TV538', ]
SLAM = addinfo[addinfo$channel == 'Slam!TV', ]
RTL_Z = addinfo[addinfo$channel == 'RTL Z', ]
RTL_Crime = addinfo[addinfo$channel == 'RTL Crime', ]

impact_MTV = mean(MTV$impact)
impact_Spike = mean(Spike$impact)
impact_BBC_First_Holland = mean(BBC_First_Holland$impact)
impact_TLC = mean(TLC$impact)
impact_Fox_Sports_3 = mean(Fox_Sports_3$impact)
impact_TF_Kitchen = mean(TF_Kitchen$impact)
impact_Fox_Sports_1 = mean(Fox_Sports_1$impact)
impact_National_Geographic_Channel = mean(National_Geographic_Channel$impact)
impact_Discovery_Channel = mean(Discovery_Channel$impact)
impact_Fox = mean(Fox$impact)
impact_Comedy_Central = mean(Comedy_Central$impact)
impact_RTL_4 = mean(RTL_4$impact)
impact_RTL_4 = mean(RTL_4$impact)
impact_ID = mean(ID$impact)
impact_NPO1 = mean(NPO1$impact)
impact_NPO3 = mean(NPO3$impact)
impact_Viceland = mean(Viceland$impact)
impact_RTL_Z = mean(RTL_Z$impact)
impact_NPO2 = mean(NPO2$impact)
impact_Net5 = mean(Net5$impact)
impact_RTL_8 = mean(RTL_8$impact)
impact_Eurosport = mean(Eurosport$impact)
impact_Veronica = mean(Veronica$impact)
impact_SBS_6 = mean(SBS_6$impact)
impact_Fox_Sports_2 = mean(Fox_Sports_2$impact)
impact_SBS_9 = mean(SBS_9$impact)
impact_TV538 = mean(TV538$impact)
impact_SLAM = mean(SLAM$impact)
impact_RTL_Z = mean(RTL_Z$impact)
impact_RTL_Crime = mean(RTL_Crime$impact)