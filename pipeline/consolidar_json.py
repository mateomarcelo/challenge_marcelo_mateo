import json
import os

def consolidar_json(asts_dir="asts", output_file="asts/consolidado.json"):
    consolidado = {"programs": []}

    for fname in os.listdir(asts_dir):
        if fname.endswith(".json"):
            with open(os.path.join(asts_dir, fname), "r", encoding="utf-8") as f:
                try:
                    data = json.load(f)
                    # Si es un JOB, guardarlo aparte
                    if fname.upper().startswith("JOB"):
                        consolidado[fname.replace(".json", "")] = data
                    else:
                        consolidado["programs"].append(data)
                except Exception as e:
                    print(f"[WARN] No se pudo cargar {fname}: {e}")

    with open(output_file, "w", encoding="utf-8") as out:
        json.dump(consolidado, out, indent=4)

    print(f"[OK] Consolidado exportado a {output_file}")

if __name__ == "__main__":
    consolidar_json()
