    function startIntro(){
        var intro = introJs();
        
        function checkExist(e) {
            
            if (typeof e.element !== 'undefined') {
                var top = 0;
                
                if (e.element !== null) {
                    top = e.element.offsetTop;
                }
                
                return (e.element !== null & top != -9999);    
            } 
            
            return true;

        }
        
        var steps = [
                { 
                    intro: "Dit is een korte introductie in het gebruik van dit dashboard. De verschillende items op het scherm krijgen ieder een toelichting. Deze intro kan ieder moment beëindigd worden via de esc-toets of door op de stop-button te klikken."
                },
                
                {
                    element: document.querySelector('#kpiDimensie'),
                    intro: "Dit is de lijst met indicatoren. Er is er altijd maar 1 geselecteerd. Getoonde cijfers zijn afhankelijk van wat er verder in het dashboard geselecteerd wordt als rapportagemaand (standaard is dat vorige maand) en bedrijfsonderdeel (Radboudumc of (optioneel) een gekozen afdeling).",
                    position: 'right'
                },
                {
                    element: document.querySelector('#kpiBreadcrumb'),
                    intro: "Navigeer met behulp van deze mappenstructuur naar de relevante groep indicatoren.",
                    position: 'right'
                },
                {
                    element: document.querySelector('#kpiDimensie').querySelector(".dataTables_length"),
                    intro: "Kies hier het aantal indicatoren dat op één pagina getoond wordt.",
                    position: 'right'
                },
                {
                    element: document.querySelector('#kpiDimensie').querySelector(".dataTables_filter"),
                    intro: "Type hier de naam of een deel van de naam van een indicator om deze te zoeken.",
                    position: 'right'
                },
                {
                    element: document.querySelector('#kpiDimensie').querySelector(".dataTables_paginate"),
                    intro: "Spring snel naar een volgende pagina via deze paginanummers.",
                    position: 'right'
                },
                {
                    element: document.querySelector('#kpiDimensie').querySelector(".kpiInfoAnchor"),
                    intro: "Bekijk de definitie van de indicator door op het i-bolletje te klikken.",
                    position: 'right'
                },
                {
                    element: document.querySelector('#kplDimensie'),
                    intro: "Dit zijn de afdelingen die bijdragen aan de geselecteerde indicator. Deze lijst komt overeen met de tweede grafiek rechts. Er kan desgewenst één afdeling gekozen worden. Als er een afdeling is gekozen dan past de lijst met indicatoren zich hierop aan. Tevens verschijnt er een derde grafiek die de tijdlijn van de geselecteerde indicator toont voor de gekozen afdeling. Een keuze kan weer ongedaan worden gemaakt door op  <span style=\"color: red;\">Verwijder Filter</span> te klikken. Ook in dit deel van het dashboard kan er op (een deel van de) Afdelingsnaam gezocht worden en kan een ander paginanummer worden gekozen.",
                    position: 'right'
                },
                {
                    element: document.querySelector('#mndDimensie'),
                    intro: "Kies hier of cijfers per maand (‘in de maand’) worden weergegeven of dat ze cumulatief (‘t/m de maand’) binnen het betreffende jaar getoond worden.",
                    position: "bottom"
                },
                {
                    element: document.querySelector('#perInstDimensie'),
                    intro: "Dit is de tijdlijn van het Radboudumc. Kies in deze grafiek welke peilmaand gewenst is. Er is altijd 1 peilmaand geselecteerd. Standaard is dit de laatste afgesloten periode. De elementen waaruit de grafiek is opgebouwd (realisatie, prognose enz.) kunnen individueel aan- en uitgezet worden door op de legenda te klikken.",
                    position: 'bottom'
                },
                {
                    element: document.querySelector('#kpl2Dimensie'),
                    intro: "Dit zijn de afdelingen die bijdragen aan de geselecteerde indicator. Deze grafiek komt overeen met de tabel linksonder. Ook hier kunnen de elementen waaruit de grafiek is opgebouwd individueel aan- en uitgezet worden door op de legenda te klikken. In deze grafiek kan een afdeling worden geselecteerd. Er verschijnt een derde grafiek die de tijdlijn van de geselecteerde indicator toont voor de gekozen afdeling. Een keuze kan weer ongedaan worden gemaakt door aan de linkerkant (bij Afdelingen) op <span style=\"color: red;\">Verwijder Filter</span> te klikken.",
                    position: 'left'
                },
                {
                    element: document.querySelector('#perAfdDimensie'),
                    intro: "Dit is de tijdlijn van de geselecteerde afdeling. Ook hier kunnen de elementen waaruit de grafiek is opgebouwd individueel aan- en uitgezet worden door op de legenda te klikken. Ook hier kan een peilmaand gekozen worden, de bovenste grafiek zal deze keuze automatisch overnemen.",
                    position: 'top'
                }
                ];
        
        intro.setOptions({
            nextLabel: "Volgende",
            prevLabel: "Vorige",
            skipLabel: "Stop",
            doneLabel: "Stop",
            exitOnOverlayClick: false,
            disableInteraction: true,
            steps: steps.filter(checkExist)
            
        });
        // debugger;
        intro.start();
    }