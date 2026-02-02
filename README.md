# Trabajo_R_Tesis

Repositorio de trabajo para la tesis (R + Git + RStudio).  
Estructura normalizada (snake_case, sin tildes/espacios).

## Estructura de carpetas
- `data/` → bases de datos crudas (eod, ipc, etc)
- `scripts/` → scripts en R que generan resultados
- `auditoria_variables/`
    - `tablas/` → archivos finales exportados (.csv)
    - `graficos/` → figuras finales (.png, .pdf)
    - `respaldos/` → versiones antiguas de resultados que ya no son finales

## Flujo de ejecución
1. **EOD Audit** (`scripts/01_eod_audit.R`)
2. **IPC Trimestral** (`scripts/02_ipc_trimestral.R`)
3. **Salarios 2008–2022** (`scripts/03_salarios_ipc_2008_2022.R`)
4. **Análisis metodológico** (`scripts/04_analisis_metodologico.R`)
5. Ejecutar todo con `scripts/make_all.R`.

## Salidas
Los resultados (tablas y gráficos) se guardan en:
`auditoria_variables/tablas` y `auditoria_variables/graficos`.

---
Proyecto configurado con `here::i_am()` para rutas portables.

## Flujo de trabajo (2 computadores)

1. **Sincroniza** antes de trabajar:
   ```bash
   git pull
   
2. Trabaja en R/RStudio. Exporta SIEMPRE con rutas relativas (usa here() y helpers).

3.Guarda y versiona cambios:
git add -A
git commit -m "feat|fix|doc|chore: descripción breve"
git push

4. En el otro equipo:
git pull

