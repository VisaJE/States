# Strategy game

## Suunnitelman päivitys
*  Suurin työntekirjöiden määrä kirjataan työ-luokkaan, ei laitoksiin.
*  Töissä ei erikseen ole tuottavuuden indeksiä. Sen korvaa määrittelyssä asetettu tuotteiden määrä.
*  Tietokanta ei määrää työryhmään enemmän kuin mahdollisen määrän työntekijöitä.
*  Sekä töille että tuotteille on vertailumetodit, joita kassa käyttää niiden yhdistelyyn ja lajitteluun.
*  Tuotteista immutable.
*  Tuotteiden tyytyväisyysfunktio palauttaa kansan kokoon suhteuttamattoman arvon, joka on negatiivinen, jos tuotteen määrä ei riitä, ja muuten positiivinen. Tarpeen ja tyytyväisyyden funktiot siis toteutuvat samassa metodissa.