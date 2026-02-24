# Prompt Template

Analizar el programa {{program_name}} con la siguiente información disponible:

- AST consolidado: {{ast_json}}
- Grafo semántico: {{graph_json}}
- Reglas de negocio extraídas: {{rules_json}}

Objetivos del análisis:
- Identificar variables críticas y su flujo en el programa.
- Revisar las reglas de negocio aplicadas y su trazabilidad.
- Detectar posibles inconsistencias o mejoras en el control de flujo.

Formato de respuesta esperado:
- Resumen claro y estructurado.
- Referencias a los archivos de origen cuando corresponda.
- Conclusiones técnicas y de negocio diferenciadas.
