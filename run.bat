@echo off
echo Ejecutando pipeline completo

REM 0. Parsear fuentes COBOL y JCL
python pipeline\parse_cobol.py
python pipeline\parse_jcl.py

REM 1. Generar AST consolidado
python pipeline\consolidar_json.py

REM 2. Construir grafo semántico
python pipeline\build_graph.py

REM 2.5 Validar grafo semántico
python pipeline\validate_graph.py

REM 3. Visualizar subgrafos
python pipeline\visualize_graph.py

REM 4. Clasificar técnico vs negocio
python pipeline\classify_graph.py

REM 5. Extraer reglas de negocio
python pipeline\extract_rules.py

echo Pipeline completo ejecutado.

REM Paso 8: Validar invariantes (tests automáticos)
python tests\test_invariants.py

pause
