# 4. Estratègia de go-to-market

## On encaixa el meu perfil

*Supòsit: perfil de consultor o analista de Power BI. Ajustar quan tinguem més context (sector, experiència, xarxa de contactes).* 

- **Avantatges:** modelatge de dades, DAX (és una capa semàntica), parlar amb negoci, traduir objectius en KPIs.
- **Buits a tancar (prioritat):** SQL avançat → BigQuery o Snowflake → dbt (models i semantic layer) → Claude Dashboards i connectors.
- **Posicionament:** «T'ajudo a tenir una sola versió de la veritat perquè la teva empresa pugui preguntar a la IA i fiar-se de la resposta.» Vendre el resultat, no l'eina.

## Segments (ICP) per prioritat

| Segment | Encaix | Per què |
|---|---|---|
| Scale-ups B2B amb Snowflake/BigQuery (50-300 p.) | 90 | Warehouse ja fet, poc equip de dades, molta demanda ad hoc |
| Empreses que ja paguen Claude Team/Enterprise | 85 | L'eina ja hi és; falta la capa de dades fiable |
| E-commerce i DTC (20-200 p.) | 75 | Dades a Shopify, GA4, ads: cal centralitzar, retorn ràpid |
| Agències i consultores (partnership) | 60 | Canal de revenda o marca blanca |
| Pimes industrials amb ERP i Excel | 45 | Gran dolor però primer cal el warehouse |
| Corporacions amb Power BI governat | 30 | Complement, no substitució |
| Startups sense dades | 20 | Millor contingut que servei |

## Escala de serveis (preus orientatius, per validar)

| Servei | Preu | Durada |
|---|---|---|
| Diagnòstic Data & IA (porta d'entrada) | 2.500-5.000 € | 2 setmanes |
| Pilot Claude Dashboards (1 departament) | 6.000-12.000 € | 4 setmanes |
| Implementació completa | 25.000-60.000 € | 10-14 setmanes |
| Data partner (retainer) | 1.500-4.000 €/mes | Recurrent |
| Workshop per a equips | 1.500-3.000 €/sessió | Mig dia |

Embut: contingut → diagnòstic → pilot → implementació → retainer.

## Pla de 90 dies

| Setmanes | Acció | Indicador |
|---|---|---|
| 0-2 | Laboratori propi: warehouse demo + dbt + Claude Dashboards, documentat aquí | Demo funcional |
| 1-13 | Contingut a LinkedIn: Power BI vs Claude Dashboards amb casos reals | 2 posts/setmana + vídeo demo |
| 2-4 | Llista de 100 comptes del segment prioritari | 100 empreses i decisors |
| 4-10 | Prospecció amb oferta de diagnòstic | 10 reunions |
| 6-11 | 2 pilots a preu reduït a canvi de cas d'èxit | 2 pilots signats |
| 10-13 | Productitzar: playbook, plantilles, preus | 1 retainer |

## Missatge comercial

- **Dolor:** «Cada departament té els seus números, ningú es fia dels dashboards i l'equip de dades no dona l'abast.»
- **Promesa:** «En quatre setmanes, un departament fent preguntes a Claude sobre mètriques certificades, amb el SQL visible darrere de cada número.»
- **Prova:** demo amb dades del sector del client; després, el primer cas d'èxit.

## Riscos per al negoci

- Producte en beta: pot canviar o limitar-se. → Vendre la capa de dades, agnòstica d'eina.
- Microsoft (Copilot a Power BI) i OpenAI (agent de dades) competeixen pel mateix espai. → Mateixa resposta: el valor és sota l'eina.
- Grans consultores entraran quan el mercat maduri. → Avantatge de primer moviment i especialització en pimes i scale-ups.
