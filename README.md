# Strategy game

## Suunnitelman p�ivitys
*  Suurin ty�ntekirj�iden m��r� kirjataan ty�-luokkaan, ei laitoksiin.
*  T�iss� ei erikseen ole tuottavuuden indeksi�. Sen korvaa m��rittelyss� asetettu tuotteiden m��r�.
*  Tietokanta ei m��r�� ty�ryhm��n enemm�n kuin mahdollisen m��r�n ty�ntekij�it�.
*  Sek� t�ille ett� tuotteille on vertailumetodit, joita kassa k�ytt�� niiden yhdistelyyn ja lajitteluun.
*  Tuotteista immutable.
*  Tuotteiden tyytyv�isyysfunktio palauttaa kansan kokoon suhteuttamattoman arvon, joka on negatiivinen, jos tuotteen m��r� ei riit�, ja muuten positiivinen. Tarpeen ja tyytyv�isyyden funktiot siis toteutuvat samassa metodissa.