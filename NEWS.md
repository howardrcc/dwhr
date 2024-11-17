
dwhr 1.6.2
==========

* Documentatie uitgebreid en verbeterd.
* foreignkeyCheck wordt nu altijd uitgevoerd per dimensie. Als deze optie op FALSE staat resulteert een foreign 
  key constraint fout alleen tot een warning. Anders wordt dashb afgesloten.
* In getMembers mogelijkheid tot recursieve aanroep sterk ingeperk. Bij data met foreignkey problemen kan dit anders leiden tot
  stack overflows.
* addMeasure, addMeasureDerrived etc, kunnen nu voor meerdere dimensies in 1 keer worden uitgevoerd. Dit voorkomt het meerdere keren herhalen
  van measure lists etc als dit voor alle dimensies toch hetzelfde is.

dwhr 1.6.1
==========

Een aantal kleine aanpassingen count measure functie,footer, cog, wrench, popover en level1 of parentfilter.

dwhr 1.6.0
==========

Diverse aanpassingen voor upgrade naar de meest recente versies van highcharter, DT en shinyjqui.  Alleen shinyjs moet nog met een oudere versie 1.0 geinstalleerd zijn.


dwhr 1.5.0
==========

* clone.star met meerdere opties
* setSelection: extra checks en in pipe te gebruiken
* setOrdering: in pipe te gebruiken
* addDimView: nieuwe optie ignoreParent. De levels van de dimensie zijn nu niet hierarchisch geordend maar zijn onafhankelijk. Parentlevel is altijd het topknooppunt en beinvloedt dan ook niet meer de drill-actie
* custom aggregates krijgen nu 3 parameters mee: env, dim en sd. De sd parameter is de subset-data van de group by.
* eerste aanpassing voor caching
* errorhandling vereenvoudigd
* busy indicator verbeterd


dwhr 1.4.0
==========

changes

* clones list in env
* filterRowGroup optie voor datatable
* expandList mogelijkheid voor highcharts

dwhr 1.3.0
==========

changes

* ondersteuning sparklines in datatable
* geen dtPrev gebruiken in datatable


dwhr 1.2.0
==========

changes

* ondersteuning Highstock functionaliteit
* topx functionaliteit toegevoegd
* sorteerbaarheid van kolommen datatable aangepast. Als er een footer is: kolom is sorteerbaar.

breaking changes

* functies in highcharts-options moeten een env parameter hebben met daarin de environment van het sterschema.


dwhr 1.1.0
==========

changes

* implementatie van accordion in UI

* batchStateFinished variabele toegevoegd aan client

* latexEscape uitgebreid met vervangen van \n naar \\newline

* omgaan met verschillen tussen opensource shiny-server en commercial shiny-server


dwhr 1.0.0
==========

ipn 26 feb 2018
