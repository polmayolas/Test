# 2. Mercat i futur del BI: val la pena apostar-hi ja?

## El mercat avui

| Font | Base | Previsió | Creixement anual |
|------|------|----------|------------------|
| Ken Research | 34,8 bn $ (2025) | 56,8 bn $ (2031) | 8,4% |
| Fortune Business Insights | 29,4 bn $ (2023) | 54,3 bn $ (2030) | 9,1% |
| Data Bridge | 27,1 bn $ (2022) | 58,8 bn $ (2030) | 10,15% |
| Meticulous Research | n/d | 56,9 bn $ (2032) | 7,2% |

El BI és un mercat **madur que creix** (7-11% anual), dominat per Microsoft (Power BI dins de Fabric), Google (Looker), Salesforce (Tableau), Qlik i ThoughtSpot.

## Senyals de canvi

- **Gartner (Predicts 2026):** el 2028, el 60% dels usuaris de self-service analytics faran servir LLMs generalistes per a anàlisi ad hoc, però el **reporting de producció es quedarà a les plataformes de BI**.
- **Gartner:** el 60% dels projectes d'agentic analytics que depenen només de MCP **fallaran per manca d'una capa semàntica consistent**.
- **dbt Labs (benchmark 2026, via resums):** el text-to-SQL directe sobre esquema cru arriba al 64,5% d'encert; amb una capa semàntica modelada, fins al 98,2% (Claude Sonnet 4.6: del 90,0% al 98,2%).
- **Competència:** OpenAI va llançar un agent de dades un mes abans que construeix dashboards dins de Power BI, Tableau i ThoughtSpot. Microsoft empeny Copilot dins de Power BI.

## Escenaris

1. **2026-2027, coexistència.** Pilots de Claude, Copilot i agents per a preguntes ad hoc; el reporting oficial es queda a Power BI/Tableau. Demanda: pilots i formació.
2. **2027-2028, la capa de preguntes s'imposa.** Els dashboards poc usats se substitueixen per preguntes i dashboards generats. Sense mètriques certificades, apareixen números contradictoris. Demanda: capa semàntica i governança.
3. **Des del 2028, BI = dades governades + agents.** L'eina de visualització es torna intercanviable. Demanda: arquitectes de dades i partners recurrents.

*(Escenaris propis, no previsions quantificades.)*

## Resposta: deixo Power BI?

**No el deixis; canvia què vens.** El que val en els tres escenaris és el mateix: modelar dades, definir mètriques, governar-les i traduir objectius de negoci en KPIs. Això ja ho fas a Power BI (DAX és una capa semàntica). L'aposta és:

- **Seguir facturant Power BI** mentre construeixes la nova oferta.
- **Afegir** SQL avançat, un warehouse al núvol (BigQuery o Snowflake), dbt i Claude.
- **Posicionar-te** com a especialista en la capa de dades per a l'era de la IA, agnòstic d'eina. Si Claude guanya, guanyes; si guanya Copilot, també.
