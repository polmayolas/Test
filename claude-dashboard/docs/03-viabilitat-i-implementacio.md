# 3. Pot tot el BI d'una empresa passar a Claude Dashboards?

## Resposta curta

**Avui, no tot. Bona part, sí**, si a sota hi ha un warehouse amb mètriques certificades i permisos ben definits.

| Pot passar a Claude avui | Millor mantenir al BI clàssic |
|---|---|
| Preguntes de negoci del dia a dia | Informes regulats, a consell o amb format fix |
| Dashboards operatius per equip | Tancaments comptables i conciliacions |
| Exploració abans de crear un informe oficial | Empreses sense warehouse al núvol |
| Dashboard de direcció lligat a objectius (amb mètriques centralitzades) | Organitzacions molt integrades a Microsoft 365 |

## Comparativa (valoració pròpia, 0-5, octubre 2026)

| Capacitat | Claude | Power BI |
|---|---|---|
| Preguntes en llenguatge natural | 5 | 3 |
| Velocitat fins al primer dashboard | 5 | 2 |
| Traçabilitat de cada número | 5 | 3 |
| Connexió a warehouses al núvol | 4 | 5 |
| Capa semàntica governada | 2 | 5 |
| Seguretat per rols i files | 3 | 5 |
| Informes regulats i enviaments programats | 1 | 4 |
| Maduresa i suport enterprise | 2 | 5 |
| Integració Microsoft 365 / Excel | 2 | 5 |

## Arquitectura objectiu

```
Fonts (ERP, CRM, e-commerce, ads, Excel)
  → Ingesta ELT (Fivetran, Airbyte, scripts)
    → Warehouse (Snowflake, BigQuery, Databricks)
      → Capa semàntica i governança (dbt, mètriques certificades, owners, rols, polítiques de files)
        → Consum: Claude Dashboards i preguntes · Power BI per al reporting oficial
```

Clau: cada lector d'un Claude Dashboard consulta el warehouse **amb els seus propis permisos**, de manera que l'accés limitat per rols es defineix **al warehouse** (rols, row access policies, esquemes per departament), no al dashboard.

## Procés d'implementació (empresa de 50-300 persones, estimació)

| # | Fase | Setmanes | Entregables |
|---|------|----------|-------------|
| 1 | Diagnòstic i inventari de fonts | 0-2 | Mapa de fonts, objectius d'empresa, KPIs prioritaris |
| 2 | Plataforma de dades: warehouse i ingesta | 2-6 | Warehouse, ingesta ELT, models de staging |
| 3 | Capa semàntica i diccionari de mètriques | 4-8 | Models dbt, mètriques certificades amb owner, tests |
| 4 | Governança i accés per rols | 5-8 | Rols, polítiques de files, vistes per departament |
| 5 | Dashboards per departament | 7-11 | Dashboard nord d'empresa + un per departament |
| 6 | Preguntes de negoci i adopció | 10-13 | Playbook de preguntes, formació, champions |
| 7 | Operació contínua | 13+ | Qualitat de dades, noves mètriques, revisió mensual |

## Centralització de mètriques

Regla d'or: **cada mètrica es defineix i es calcula un sol cop**, a la capa semàntica, amb un owner. Els dashboards i les preguntes llegeixen la mètrica certificada; mai recalculen sobre taules crues. El diccionari inicial per departament és a [`data/metriques.json`](../data/metriques.json) (Direcció, Vendes, Màrqueting, Finances, Operacions, Clients, Persones).

## Reptes i mitigació

| Repte | Severitat | Mitigació |
|---|---|---|
| Fonts disperses sense warehouse | 5 | Inventari i ingesta a un sol warehouse abans de cap dashboard |
| Mètriques diferents a cada departament | 5 | Diccionari amb owner i càlcul centralitzat |
| Respostes plausibles però incorrectes | 4 | Preguntes sobre vistes certificades; revisar el SQL en decisions crítiques |
| Permisos i dades sensibles | 4 | Rols i polítiques de files al warehouse |
| Producte en beta | 4 | Arquitectura agnòstica d'eina |
| RGPD i ubicació de les dades | 4 | Revisió legal, DPA, minimització |
| Cost de consultes | 3 | Taules agregades; cada lector re-executa en obrir |
| Gestió del canvi | 3 | Champions, playbook, formació curta |

## Early adopter ideal

Empresa de 50-300 persones, **ja amb Snowflake o BigQuery**, equip de dades petit (0-2 persones) desbordat de peticions, direcció que ja usa Claude (Team o Enterprise), i sense un Power BI profundament governat que defensar.
