# Dashboard Territorial Industrial — Entre Ríos

**Aplicación web interactiva para el análisis territorial del empleo privado y los parques industriales de la provincia de Entre Ríos, Argentina.**

[![Shiny](https://img.shields.io/badge/Shiny-app-blue?logo=r)](https://mj8qpg-nicolas-gottig.shinyapps.io/dashboard_parquesV2/)

---

## Demo

Accedé a la aplicación en vivo:  
**[https://mj8qpg-nicolas-gottig.shinyapps.io/dashboard_parquesV2/](https://mj8qpg-nicolas-gottig.shinyapps.io/dashboard_parquesV2/)**

---

## Sobre la aplicación

Dashboard desarrollado en R/Shiny para el **Ministerio de Producción de Entre Ríos**, orientado a:

- Analizar la distribución y especialización del empleo privado registrado por departamento
- Visualizar la cobertura geográfica e infraestructura de los parques industriales provinciales
- Identificar la presencia sectorial de industrias en cada departamento

### Funcionalidades principales

#### 1. Mapa de empleo privado
Visualización del empleo privado registrado por departamento, con dos modos de análisis:
- **Cantidad de puestos:** coropletas por total de puestos según año, mes y sector(es)
- **Especialización relativa:** cociente de localización (LQ) que indica si un departamento concentra más o menos empleo en un sector respecto al promedio provincial

#### 2. Mapa de parques industriales
Mapa de los 38 parques industriales de Entre Ríos con:
- Filtros por departamento, promoción impositiva y servicios de infraestructura disponibles
- Popups con detalle de empresas instaladas, empleados, superficie y equipamiento
- Indicador visual del nivel de infraestructura relativo al promedio provincial

#### 3. Heatmap sectorial
Mapa de calor estático que muestra la frecuencia de presencia de cada sector industrial por departamento, útil para comparar la diversificación productiva territorial.

---

## Fuentes de datos

| Dataset | Fuente | Descripción |
|---------|--------|-------------|
| Empleo privado | [Ministerio de Producción — datos.produccion.gob.ar](https://datos.produccion.gob.ar/dataset/puestos-de-trabajo-por-departamento-partido-y-sector-de-actividad) | Puestos de trabajo privados por departamento, sector y período |
| Parques industriales | [Dirección General de Industria y Parques Industriales — parquesindustriales.entrerios.gov.ar](https://parquesindustriales.entrerios.gov.ar/nosotros) | Infraestructura, empresas, sectores y promociones de 38 parques |
| Cartografía | [INDEC](https://www.indec.gob.ar/) | Shapefiles de departamentos y red vial de Entre Ríos |

---

## Estructura del proyecto

```
monitor-productivo-er/
├── app.R                          # Aplicación Shiny principal (~1.100 líneas)
├── dashboard_parquesV2.Rproj      # Proyecto RStudio
│
├── data/
│   ├── empleo_reducido.txt        # Empleo privado por año/mes/depto/sector (TSV)
│   ├── parques_para_shiny.txt     # 38 parques industriales + infraestructura (TSV)
│   ├── puertos_er.csv             # Puertos fluviales de Entre Ríos
│   └── geo/                       # Shapefiles INDEC
│       ├── pxdptodatosok.shp      # Polígonos de departamentos
│       ├── provinciaPolygon.shp   # Límite provincial
│       ├── vial_nacionalLine.shp  # Red vial nacional
│       └── vial_provincialLine.shp
│
└── www/
    ├── styles.css                 # Estilos personalizados (~600 líneas)
    └── ISE logo.png               # Logo institucional
```

---

## Instalación y uso local

### Requisitos
- R 4.0 o superior
- RStudio (recomendado)

### Pasos

```r
# 1. Clonar el repositorio
# git clone https://github.com/NicoGottig/monitor-productivo-er.git

# 2. Instalar dependencias
install.packages(c(
  "shiny", "bslib", "leaflet", "sf",
  "dplyr", "tidyr", "stringr", "readr",
  "plotly", "scales"
))

# 3. Ejecutar la aplicación (desde la carpeta raíz del repo)
shiny::runApp()
```

También podés abrir el archivo `dashboard_parquesV2.Rproj` en RStudio y presionar **Run App** en el editor de `app.R`.

---

## Dependencias R

| Paquete | Versión mínima | Uso |
|---------|---------------|-----|
| shiny | ≥ 1.7 | Framework web |
| bslib | ≥ 0.5 | Temas Bootstrap |
| leaflet | ≥ 2.1 | Mapas interactivos |
| sf | ≥ 1.0 | Datos espaciales (shapefiles) |
| dplyr | ≥ 1.0 | Transformación de datos |
| tidyr | ≥ 1.3 | Pivoting y unnest |
| stringr | ≥ 1.5 | Manejo de strings |
| readr | ≥ 2.1 | Lectura TSV/CSV |
| plotly | ≥ 4.10 | Heatmap interactivo |
| scales | ≥ 1.2 | Paletas de color y formatos |

---

## Créditos

Desarrollado por el **Instituto de Seguimiento Económico (ISE)** para el Ministerio de Producción de Entre Ríos.
