# Strategy game

## Suunnitelman p�ivitys

*  Suurin ty�ntekirj�iden m��r� kirjataan ty�-luokkaan, ei laitoksiin.
*  T�iss� ei erikseen ole tuottavuuden indeksi�. Sen korvaa m��rittelyss� asetettu tuotteiden m��r�.
*  Tietokanta ei m��r�� ty�ryhm��n enemm�n kuin mahdollisen m��r�n ty�ntekij�it�.
*  Sek� t�ille ett� tuotteille on vertailumetodit, joita kassa k�ytt�� niiden yhdistelyyn ja lajitteluun.
*  Tuotteista immutable.
*  Tuotteiden tyytyv�isyysfunktio palauttaa kansan kokoon suhteutetun arvon, joka on negatiivinen, jos tuotteen m��r� ei riit�, ja muuten positiivinen. Tarpeen ja tyytyv�isyyden funktiot siis toteutuvat samassa metodissa.
*  K�ytt�liittym�n ja pelin v�liin lis�tty pelaaja-luokka, johon my�s teko�ly kuuluu. Peli antaa ihmispelaajan toimia pelaaja-luokan kautta ja tallentaa kunkin vuorot suoritettavaksi.
*  Vuorojen tallentamiseksi on olemassa vuoro-tyyppi. T�h�n voisi my�s lis�t� ainakin vuoron ohittamisen vaihtoehdon, mutta my�s mahdolliset hy�kk�ykset jne. kehitysjutut.