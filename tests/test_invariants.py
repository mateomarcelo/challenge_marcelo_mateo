import json

def test_invariants(consolidado_file="asts/consolidado.json"):
    with open(consolidado_file, "r", encoding="utf-8") as f:
        consolidado = json.load(f)

    level88_count = 0
    comp3_count = 0
    call_detected = False
    on_size_error_detected = False
    cond_detected = False

    # Revisar programas COBOL
    for prog in consolidado.get("programs", []):
        # Data Division
        for var in prog.get("dataDivision", []):
            if var.get("usage") == "LEVEL-88":
                level88_count += 1
            if var.get("usage") == "COMP-3":
                comp3_count += 1

        # Procedure Division
        for stmt in prog.get("procedureDivision", []):
            if stmt.get("statementType") == "CALL":
                call_detected = True
            if stmt.get("statementType") == "ON SIZE ERROR":
                on_size_error_detected = True

    # Revisar condiciones JCL en JOB1
    if "JOB1" in consolidado:
        for step in consolidado["JOB1"].get("steps", []):
            if step.get("condition") == "(0,NE)":
                cond_detected = True

    print("Resultados de invariantes:")
    print(f"- Level-88 encontrados: {level88_count} (mínimo 7) -> {'OK' if level88_count >= 7 else 'NO DETECTADO'}")
    print(f"- Campos COMP-3 encontrados: {comp3_count} (mínimo 3) -> {'OK' if comp3_count >= 3 else 'NO DETECTADO'}")
    print(f"- CALL dinámica: {'OK' if call_detected else 'NO DETECTADO'}")
    print(f"- ON SIZE ERROR: {'OK' if on_size_error_detected else 'NO DETECTADO'}")
    print(f"- COND=(0,NE) en JCL: {'OK (efecto: el paso se ejecuta solo si el return code es distinto de 0)' if cond_detected else 'NO DETECTADO'}")

    if level88_count >= 7 and comp3_count >= 3 and call_detected and on_size_error_detected and cond_detected:
        print("[OK] Todos los invariantes cumplidos.")
    else:
        print("[WARN] Algunos invariantes no se cumplen.")
    print("[INFO] Validacion de invariantes finalizada.")

if __name__ == "__main__":
    test_invariants()
