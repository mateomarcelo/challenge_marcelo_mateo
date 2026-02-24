import json
import os
import re

def parse_cobol(file_path):
    program = {"programName": None, "dataDivision": [], "procedureDivision": [], "sections": []}

    with open(file_path, "r") as f:
        for line in f:
            line_up = line.strip().upper()

            # Nombre del programa
            if line_up.startswith("PROGRAM-ID"):
                program["programName"] = line_up.split()[1]

            # Variables en DATA DIVISION
            elif re.match(r"^\d+\s+[A-Z0-9-]+", line_up):
                parts = line_up.split()
                level = parts[0]
                name = parts[1]
                usage = None
                if "COMP-3" in line_up:
                    usage = "COMP-3"
                if level == "88":
                    usage = "LEVEL-88"
                program["dataDivision"].append({
                    "level": level,
                    "name": name,
                    "usage": usage,
                    "text": line.strip()
                })

            # Sentencias en PROCEDURE DIVISION
            else:
                if re.search(r"\bCALL\s+[A-Z0-9'\-]+", line_up):
                    program["procedureDivision"].append({"statementType": "CALL", "text": line.strip()})
                elif re.search(r"\bON\s+SIZE\s+ERROR\b", line_up):
                    program["procedureDivision"].append({"statementType": "ON SIZE ERROR", "text": line.strip()})
                elif re.search(r"\bPERFORM\b", line_up):
                    program["procedureDivision"].append({"statementType": "PERFORM", "text": line.strip()})
                elif re.search(r"\bIF\b", line_up) or re.search(r"\bELSE\b", line_up) or re.search(r"\bEND-IF\b", line_up):
                    program["procedureDivision"].append({"statementType": "IF/ELSE", "text": line.strip()})
                elif re.search(r"\bEVALUATE\b", line_up):
                    program["procedureDivision"].append({"statementType": "EVALUATE", "text": line.strip()})
                elif re.match(r"^[A-Z0-9-]+\s+SECTION", line_up):
                    program["sections"].append({"sectionName": line_up.split()[0], "text": line.strip()})
                elif re.match(r"^[A-Z0-9-]+\.", line_up):
                    program["sections"].append({"paragraphName": line_up.split()[0].replace(".", ""), "text": line.strip()})

    return program

if __name__ == "__main__":
    src_dirs = ["src", "asts"]  # ahora recorre ambas carpetas
    for src_dir in src_dirs:
        for fname in os.listdir(src_dir):
            if fname.endswith(".cbl"):
                result = parse_cobol(os.path.join(src_dir, fname))
                out_file = os.path.join("asts", fname.replace(".cbl", ".json"))
                with open(out_file, "w", encoding="utf-8") as out:
                    json.dump(result, out, indent=4)
                print(f"[OK] AST exportado a {out_file}")
