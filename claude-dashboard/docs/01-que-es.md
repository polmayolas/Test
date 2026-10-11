# 1. Què és Claude Design i què és Claude Dashboards

*Recerca de l'11 d'octubre de 2026.*

Sovint es barregen dos productes diferents d'Anthropic. Per al BI, el rellevant és **Claude Dashboards**.

## Claude Design (abril de 2026, Anthropic Labs)

- Crea prototips, presentacions, one-pagers i visuals de màrqueting a partir d'una descripció.
- En l'onboarding llegeix el codi i els fitxers de disseny de l'equip per construir un sistema de disseny (colors, tipografia, components) i l'aplica als projectes posteriors.
- Exporta a PDF, PPTX, HTML, URL interna i Canva; pot passar la feina a Claude Code.
- **Per al BI:** útil per fer maquetes de dashboards (wireframes), no per connectar-se a dades. Des d'octubre de 2026 ja no és beta i és a tots els plans.

## Claude Dashboards (beta des del 8 d'octubre de 2026)

- Dashboards **vius** creats a partir de preguntes en llenguatge natural.
- Es connecta a **Amazon Redshift, BigQuery, ClickHouse, Databricks i Snowflake**, i a altres connectors (p. ex. Salesforce).
- Claude escriu el SQL; **cada número mostra la consulta que el genera** i quan s'ha refrescat.
- Les dades es refresquen soles; cada lector consulta amb **els seus propis permisos** sobre la font.
- Exporta o passa la feina a Amplitude, Grafana, Hex, Mixpanel, Omni, Perplexity, PostHog i Sigma. Looker, monday.com i Tableau "aviat". **Power BI no hi és.**
- Beta als plans de pagament (Pro, Max, Team, Enterprise). A Enterprise està desactivat per defecte i l'activa un administrador (Organization settings > Artifacts).
- Anthropic el posiciona **al costat** de les eines de BI, per a preguntes ràpides i exploratòries.

## El que no és (encara)

- No té una **capa semàntica pròpia consolidada**: el SQL s'escriu per gràfic. S'ha anunciat suport a models semàntics exposats via connector.
- No té **seguretat per files** pròpia: depèn dels permisos del warehouse.
- No fa informes pixel-perfect, regulats ni enviaments programats com un BI clàssic.

## Conclusió

Claude Dashboards és una **interfície de consum** excel·lent sobre dades ben preparades. La feina de preparar-les (i de governar-les) és on hi ha el servei.
