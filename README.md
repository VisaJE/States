# Strategy game

## Suunnitelman päivitys

*  Suurin työntekirjöiden määrä kirjataan työ-luokkaan, ei laitoksiin.
*  Töissä ei erikseen ole tuottavuuden indeksiä. Sen korvaa määrittelyssä asetettu tuotteiden määrä.
*  Tietokanta ei määrää työryhmään enemmän kuin mahdollisen määrän työntekijöitä.
*  Sekä töille että tuotteille on vertailumetodit, joita kassa käyttää niiden yhdistelyyn ja lajitteluun.
*  Tuotteista immutable.
*  Tuotteiden tyytyväisyysfunktio palauttaa kansan kokoon suhteutetun arvon, joka on negatiivinen, jos tuotteen määrä ei riitä, ja muuten positiivinen. Tarpeen ja tyytyväisyyden funktiot siis toteutuvat samassa metodissa.
*  Käyttöliittymän ja pelin väliin lisätty pelaaja-luokka, johon myös tekoäly kuuluu. Peli antaa ihmispelaajan toimia pelaaja-luokan kautta ja tallentaa kunkin vuorot suoritettavaksi.
*  Vuorojen tallentamiseksi on olemassa vuoro-tyyppi. Tähän voisi myös lisätä ainakin vuoron ohittamisen vaihtoehdon, mutta myös mahdolliset hyökkäykset jne. kehitysjutut.