Challenge Técnico – Extracción de Reglas de Negocio desde COBOL (Análisis Estático)
===================================================================================

Se muestra de manera práctica y clara, cómo a partir de componentes COBOL
se puede realizar un análisis estático para identificar reglas de negocio
y representarlas en forma de grafo semántico. El proceso incluye además
visualizaciones que ayudan a comprender el flujo SCLM (Edit → Build → Promote).

La carpeta contiene todo lo necesario para ejecutar el pipeline y reproducir
los resultados obtenidos. La idea es que quien lo reciba pueda correrlo de principio
a fin y ver las mismas salidas, sin necesidad de configuraciones adicionales.

Cómo correr el pipeline
-----------------------

1. Abrir una consola de Windows (CMD).
2. Ir a la carpeta raíz del proyecto:
   cd C:\Users\Mateo\challenge-deliverables
3. Ejecutar:
   run.bat

Durante la ejecución se mostrarán mensajes [OK] que confirman cada etapa.
Al final, se habrán generado los archivos y gráficos que ilustran el flujo.

Ejemplo de salida completa
--------------------------

Ejecutando pipeline completo
[OK] AST exportado a asts\CICBP02.json
[OK] AST exportado a asts\COBAP01.json
[OK] AST exportado a asts\UTLVAL01.json
[OK] AST exportado a asts\ACCREC-STUB.json
[OK] AST exportado a asts\PARAMS-STUB.json
[OK] AST exportado a asts\JOB1.json
[OK] Consolidado exportado a asts/consolidado.json
[INFO] Condición JCL detectada en CICBSTEP: (0,NE) (El paso se ejecuta solo si el return code es distinto de 0.)
[OK] Grafo semántico generado en semantic-graph/grafo_semantico.json
[OK] Control flow exportado en semantic-graph/control_flow.json
[OK] Grafo válido con 6 nodos y 5 aristas.
[OK] Grafo guardado en doc\grafo_semantico.png
[OK] Clasificación exportada a rules/clasificacion.json
[OK] Reglas de negocio exportadas a rules/reglas.json
Pipeline completo ejecutado.
Resultados de invariantes:
- Level-88 encontrados: 23 (mínimo 7) -> OK
- Campos COMP-3 encontrados: 10 (mínimo 3) -> OK
- CALL dinámica: OK
- ON SIZE ERROR: OK
- COND=(0,NE) en JCL: OK (efecto: el paso se ejecuta solo si el return code es distinto de 0)
[OK] Todos los invariantes cumplidos.
[INFO] Validacion de invariantes finalizada.
Presione una tecla para continuar . . .

Qué hace cada etapa
-------------------

- parse_cobol.py y parse_jcl.py: generan representaciones (ASTs) de programas y jobs.
- consolidar_json.py: une esos ASTs en un archivo consolidado.
- build_graph.py: construye el grafo semántico con las relaciones detectadas.
- validate_graph.py: revisa que el grafo sea consistente.
- visualize_graph.py: crea imágenes del flujo (call graph, control flow, data lineage).
- classify_graph.py: clasifica nodos en técnico vs. negocio.
- extract_rules.py: extrae reglas de negocio a partir del grafo.

Qué se genera
-------------

- asts/: ASTs individuales y consolidado.json
- semantic-graph/: grafo_semantico.json
- doc/: imágenes de los subgrafos
- rules/: clasificación y reglas de negocio

Ejemplo de reglas
-----------------

- El job JOB1 ejecuta el programa COBAP01.
- El job JOB1 ejecuta el programa CICBP02.
- El programa COBAP01 utiliza el copybook ACCREC-STUB.
- El programa COBAP01 utiliza el copybook PARAMS-STUB.
- El programa UTLVAL01 está definido pero no se invoca en el flujo actual.

Notas finales
-------------

Este README es una guía rápida para ejecutar y entender lo que se genera.
Se adjunta el documento técnico que explica en detalle, con un lenguaje más ejecutivo,
el ciclo SCLM y los errores comunes en cada etapa.

Historial de cambios
--------------------

Consulta el archivo CHANGELOG.md para ver las actualizaciones realizadas en el Documento Técnico y otros componentes del proyecto.
