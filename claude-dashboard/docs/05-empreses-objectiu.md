# 5. Empreses objectiu a Espanya (segment: scale-ups B2B amb warehouse al núvol)

*Recerca de l'11 d'octubre de 2026. Mida segons la base de dades de WaLead i les pàgines de les empreses; stack segons ofertes de feina públiques. Les ofertes poden ser antigues: cal validar abans de contactar.*

## A. Stack compatible confirmat per ofertes de feina

| # | Empresa | Seu | Mida | Què fan | Evidència de stack | Angle |
|---|---|---|---|---|---|---|
| 1 | Lodgify | Barcelona | ~350 | SaaS de lloguer vacacional | BigQuery, dbt, Tableau, Amplitude (ofertes de data analyst) | Ja tenen warehouse i dbt: pilot de preguntes ad hoc per a Growth i Producte |
| 2 | Holded (grup Visma) | Barcelona | 51-200 | ERP al núvol per a pimes | BigQuery i dbt; analista júnior que dona servei a Producte, Vendes, Suport i Màrqueting | Equip de dades petit i molta demanda: el cas de manual |
| 3 | Typeform | Barcelona (distribuïda) | 200-500 | Formularis i enquestes | Snowflake, dbt, Looker, Census; busquen manager d'analytics engineering amb capa semàntica | Capa semàntica madura: Claude Dashboards com a capa de self-service per a GTM |
| 4 | Clarity AI | Madrid | ~300 | Dades de sostenibilitat (SaaS) | Snowflake, dbt, Airflow, QuickSight (oferta del 2025) | Empresa de dades; interès en self-service intern. Verificar stack actual |
| 5 | Cobee (Pluxee) | Madrid | 51-200 | Retribució flexible | dbt i ClickHouse (perfil d'un ex-empleat, 2024-25); ClickHouse és compatible amb Claude Dashboards | Decisió possiblement a nivell de grup Pluxee |
| 6 | Seedtag | Madrid | 600+ | Publicitat contextual amb IA | BigQuery, dbt, Airflow, Kafka | Fora del rang de mida: compte "stretch", entrar per un departament (Vendes o Finances) |

## B. Bon encaix de perfil, stack per verificar

| # | Empresa | Seu | Mida | Senyal |
|---|---|---|---|---|
| 7 | Red Points | Barcelona | ~270 | Equip de dades (data engineer, data analyst de Producte i Operacions); SaaS de protecció de marca |
| 8 | Playtomic | Madrid | 201-500 | Equip de BI; analista que defineix KPIs i dashboards per a negoci, finances i operacions; SaaS per a clubs |
| 9 | Embat | Madrid | Series B | Analista júnior que consolida dades de CRM, màrqueting i customer success en dashboards: probablement sense warehouse central (diagnòstic + warehouse) |
| 10 | Abacum | Barcelona / Nova York | 51-200 | SaaS d'FP&A per a mid-market; client i alhora possible partner (els seus clients necessiten mètriques certificades) |

## Descartada com a exemple

- **Kantox** (Barcelona, BNP Paribas): stack AWS amb Iceberg, Trino i Dagster. Trino no és avui una font de Claude Dashboards. Bona empresa, mal encaix tècnic ara mateix.

## Següent pas

1. Validar el stack actual de cada empresa a LinkedIn (perfils de l'equip de dades) i a les seves pàgines d'ofertes.
2. Identificar el decisor: Head of Data, VP Finance/CFO o COO.
3. Obrir amb l'oferta de diagnòstic i una demo amb dades del seu sector.

## Fonts

- Lodgify: [Senior Product Data Analyst](https://jobs.lever.co/lodgify/a2194b77-3b91-4769-a177-02bde62cca75), [Data Analyst (jobfluent)](https://www.jobfluent.com/jobs/data-analyst-barcelona-db838f)
- Holded: [Junior Data Analyst](https://www.wearedevelopers.com/en/jobs/ext/5704076/junior-data-analyst)
- Typeform: [Manager of Analytics Engineering](https://job-boards.greenhouse.io/typeform/jobs/7076539), [Built In](https://builtin.com/job/senior-manager-analytics-engineering/7949991)
- Clarity AI: [Data Operations Engineer Specialist](https://jobfluent.com/jobs/data-operations-engineer-specialist-madrid-76d1b1)
- Cobee: [Data Analyst](https://talent.octopusventures.com/companies/cobee/jobs/31253782-data-analyst), [perfil Malt](https://en.malt.es/profile/juanmanuelcornejo)
- Seedtag: [Data Engineer](https://jobs.seedtag.com/jobs/7338784-data-engineer), [Personio](https://seedtag-1.jobs.personio.com/job/2374501)
- Red Points: [Data Analyst](https://jobs.eightroads.com/companies/red-points/jobs/43703663-data-analyst)
- Playtomic: [Data Analyst](https://playtomic.factorial.es/job_posting/data-analyst-275975)
- Embat: [Data Analyst](https://embat.factorialhr.com/job_posting/data-analyst-315623)
- Kantox: [Senior Data Engineer](https://apply.workable.com/kantox/jobs/view/6AD979D7E8.md)
