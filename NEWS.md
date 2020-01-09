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