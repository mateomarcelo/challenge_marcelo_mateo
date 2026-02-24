import json
import os
import re

def parse_jcl(file_path):
    job = {"jobName": None, "steps": []}

    with open(file_path, "r") as f:
        for line in f:
            line_up = line.strip().upper()

            if re.search(r"//\S+\s+JOB", line_up):
                job["jobName"] = line_up.split()[0].replace("//", "")

            elif re.search(r"//\S+\s+EXEC", line_up):
                step_name = line_up.split()[0].replace("//", "")
                program = None
                exec_type = None
                condition = None
                explanation = None

                match_pgm = re.search(r"PGM=([A-Z0-9]+)", line_up)
                if match_pgm:
                    program = match_pgm.group(1)
                    exec_type = "PGM"

                if "PROC" in line_up:
                    exec_type = "PROC"

                match_cond = re.search(r"\([^)]*\)", line_up)
                if match_cond:
                    condition = match_cond.group(0).strip().upper()
                    if condition == "(0,NE)":
                        explanation = "El paso se ejecuta solo si el return code es distinto de 0."

                job["steps"].append({
                    "name": step_name,
                    "program": program,
                    "condition": condition,
                    "conditionExplanation": explanation,
                    "execType": exec_type,
                    "dd": []
                })

            elif re.search(r"//\S+\s+DD", line_up):
                parts = line_up.split()
                dd_name = parts[0].replace("//", "")
                dd_value = " ".join(parts[2:])
                if job["steps"]:
                    job["steps"][-1]["dd"].append({"name": dd_name, "value": dd_value})

            elif line_up.startswith("//*"):
                if "comments" not in job:
                    job["comments"] = []
                job["comments"].append(line.replace("//*", "").strip())

    return job

if __name__ == "__main__":
    src_dirs = ["src", "asts"]  # también recorre ambas carpetas
    for src_dir in src_dirs:
        for fname in os.listdir(src_dir):
            if fname.endswith(".jcl"):
                result = parse_jcl(os.path.join(src_dir, fname))
                out_file = os.path.join("asts", fname.replace(".jcl", ".json"))
                with open(out_file, "w", encoding="utf-8") as out:
                    json.dump(result, out, indent=4)
                print(f"[OK] AST exportado a {out_file}")
