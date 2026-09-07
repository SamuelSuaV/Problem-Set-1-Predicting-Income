# Problem Set 1: Predicting Income

## Descripción del proyecto

Paquete de replicación del Problem Set 1 de *Big Data & Machine Learning para Economía
Aplicada* (MECA 4107, Universidad de los Andes). Construimos y evaluamos modelos del
ingreso laboral individual para Bogotá a partir de la muestra 2018 de la GEIH, en torno
a tres análisis:

1. **Perfil edad–ingreso** — `log(w) = β1 + β2·Age + β3·Age² + u` (incondicional y
   condicional), con un intervalo de confianza *bootstrap* para la edad pico implícita.
2. **Brecha de ingreso por género** — `log(w) = β1 + β2·Female + u` (incondicional y
   condicional), recuperando el coeficiente de género vía Frisch–Waugh–Lovell con
   errores estándar analíticos y *bootstrap*.
3. **Predicción del ingreso** — enfoque de *validation set* (entrenamiento en los
   *chunks* 1–7, validación en 8–10), comparando especificaciones por RMSE de
   validación, LOOCV y un análisis de importancia de variables.

**Autores:** Maria Jose Perez, Juan Manuel Lozano, Samuel Suárez \
Universidad de los Andes — 2026

---

## Estructura del repositorio

```
Problem Set 1: Predicting Income/
│
├── README.md
│
├── script/                           # Todo el código, numerado y ejecutado en orden
│   ├── 00_Master_File.r
│   ├── 01_Web_Scrapping.r
│   ├── 02_Data_Cleaning.r
│   ├── 03_Data_Description.r
│   ├── 04_age_labor_income.r
│   ├── 05_gender_gap.r
│   ├── 06_income_prediction_train.r
│   ├── 07_income_prediction_validation.r
│   └── 08_income_imputation_comparison.r
│
├── data/                             # Datos generados por el código (no editar a mano)
│   ├── geih_scrap.rds                # Salida de 01: muestra cruda combinada
│   └── geih_clean.rds                # Salida de 02: muestra de análisis limpia
│
├── output/                           # Solo resultados generados
│   ├── figures/                      # Figuras .png
│   ├── tables/                       # Tablas de estimación en .tex
│   └── models/                       # Modelos entrenados serializados (.rds)
│
├── LaTex/                            # Documento escrito (main.tex) y slides parciales
└── presentation/                    # Fuentes de los slide decks (age / gap / pred)
```

---

## Instrucciones de replicación

Desde la raíz del repositorio (esta carpeta, `Problem Set 1: Predicting Income/`),
correr el master file (`script/00_Master_File.r`), que ejecuta los demás scripts en
orden numérico:

```bash
Rscript script/00_Master_File.r            # corre todo el pipeline
Rscript script/00_Master_File.r 04 05      # corre solo esos pasos (por su prefijo)
FORCE_SCRAPE=1 Rscript script/00_Master_File.r   # fuerza el raspado (paso 01)
```

- Por defecto el paso 01 (raspado) se **salta si `data/geih_scrap.rds` ya existe**,
  para no volver a golpear las páginas de la GEIH en cada corrida.
- `FORCE_SCRAPE=1` (valores válidos: `1`, `true`, `yes`, `y`) delante del comando
  fuerza el raspado aunque el `.rds` crudo exista; borrar `data/geih_scrap.rds` tiene
  el mismo efecto. En Windows `cmd`, primero `set FORCE_SCRAPE=1` y luego el `Rscript`.
- Pasar prefijos de paso (`01`, `4`, …) como argumentos corre solo ese subconjunto,
  útil para iterar sobre una sección sin rehacer todo.

Cada script lee y escribe rutas relativas (`"data/..."`, `"output/..."`) respecto al
directorio de trabajo, por lo que debe ejecutarse desde la raíz. A alto nivel:

- `00_Master_File.r` — script maestro; centraliza la orquestación del pipeline y evita
  volver a raspar la página si `data/geih_scrap.rds` ya existe.
- `01`–`03` construyen los datos: raspado, limpieza y estadística descriptiva.
- `04`–`05` son los análisis de las Secciones 1 y 2 (perfil edad–ingreso y brecha de
  género).
- `06`–`08` son la Sección 3 (predicción del ingreso): entrenamiento, validación e
  impacto de la imputación de ingresos faltantes.

Todas las figuras y tablas se regeneran automáticamente en `output/`; el documento en
`LaTex/main.tex` las incorpora vía `\input` / `\includegraphics`, de modo que se
mantiene sincronizado al recorrer el pipeline.

---

## Estructura del código

| Script | Responsabilidad |
| --- | --- |
| `00_Master_File.r` | Script maestro; salta el raspado si el `.rds` crudo ya existe. |
| `01_Web_Scrapping.r` | Raspa los 10 *chunks* HTML de `https://ignaciomsarmiento.github.io/GEIH2018_sample/`, etiqueta cada fila con `subset` (1–10, la llave del *split* train/validación de la Sección 3) y guarda `data/geih_scrap.rds`. |
| `02_Data_Cleaning.r` | Filtra a la muestra de análisis (`age >= 18 & ocu == 1`), selecciona variables, construye la `balance_table` de datos faltantes en `y_total_m` y exporta `data/geih_clean.rds` con los `NA` sin imputar. |
| `03_Data_Description.r` | Estadística descriptiva y gráficos exploratorios, todos ponderados por el factor de expansión `fex_c` vía `srvyr`. Exporta las tablas `output/tables/*.tex` y figuras `output/figures/*.png` del documento. |
| `04_age_labor_income.r` | Estima los perfiles edad–ingreso incondicional y condicional, la edad pico implícita y su IC *bootstrap*. |
| `05_gender_gap.r` | Estima la brecha de género incondicional y condicional, recupera el coeficiente vía Frisch–Waugh–Lovell con SE analíticos y *bootstrap*, y compara los perfiles edad–ingreso predichos por sexo. |
| `06_income_prediction_train.r` | Reestima los cinco modelos de las Secciones 1–2 sobre el *split* de entrenamiento (`subset` 1–7), los compara contra la versión de muestra completa y guarda los modelos ajustados en `output/models/`. |
| `07_income_prediction_validation.r` | Añade cinco especificaciones adicionales, compara las diez por RMSE de validación, AIC, BIC y LOOCV exacto (atajo de *leverage*), y grafica la importancia de variables del mejor modelo. |
| `08_income_imputation_comparison.r` | Compara las diez especificaciones con entrenamiento de casos completos vs. entrenamiento con `y_total_m` imputado por *predictive mean matching* (`mice`), para medir el efecto del problema de selección en el ingreso faltante. |


---

## Salidas

Todos los resultados se generan automáticamente en `output/` con nombres
autoexplicativos en `snake_case`.

- **Figuras** (`output/figures/`): `age_income_profile.png`, `income_dist_sex.png`,
  `income_dist_age.png`, `income_dist_formal.png`, `scatter_income_age_educ.png`,
  `section3_variable_importance.png`, `section3_imputation_density.png`, …
- **Tablas** (`output/tables/`, formato `.tex` con `booktabs`, forzadas a `[H]`):
  `balance_table.tex`, `general_continuous.tex`, `by_sex_continuous.tex`,
  `age_income_regression.tex`, `section3_model_comparison.tex`,
  `section3_train_vs_full.tex`, `section3_imputation_comparison.tex`, …
- **Modelos** (`output/models/`): `section3_train_models.rds` — modelos de la
  Sección 1/2 ajustados sobre el *split* de entrenamiento.


---

## Software / entorno

- **R** 4.4.1 (2024-06-14).
- **Paquetes:** `tidyverse`, `rvest`, `httr`, `jsonlite`, `srvyr`, `broom`,
  `kableExtra`, `scales`, `boot`, `mice`.
- Las dependencias se gestionan en línea con `pacman::p_load(...)` al inicio de cada
  script: `p_load()` instala automáticamente los paquetes que falten y luego los
  carga, así que no hay un paso de instalación separado.
- **Documento escrito:** compilar desde `LaTex/` con
  `pdflatex -interaction=nonstopmode main.tex` (dos pasadas, para TOC y referencias
  cruzadas), después de recorrer `02`/`03` para que los `.tex`/figuras estén al día.

---

## Datos

Usamos datos de la gran encuesta integrada de hogares en Bogotá, 2018. `data/geih_scrap.rds` y
`data/geih_clean.rds` son salidas de `01` y `02` respectivamente. La fuente original
es la muestra pública de la GEIH 2018 en
`https://ignaciomsarmiento.github.io/GEIH2018_sample/`.

Variables clave:

- `y_total_m` — ingreso laboral mensual total (asalariado + independiente); *outcome*.
- `age >= 18 & ocu == 1` — filtro de la muestra de análisis.
- `fex_c` — factor de expansión a nivel de persona; peso de toda la descriptiva.
- `subset` — 1–10, el *chunk* de origen de cada fila; *split* de la Sección 3
  (train 1–7 / validación 8–10).
- `relab`, `maxEducLevel`, `sex` — relación laboral, nivel educativo máximo y sexo
  (ver `CLAUDE.md` para la codificación completa).
