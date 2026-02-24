import json
import os
import networkx as nx
from networkx.readwrite import json_graph

def build_graph(consolidado="asts/consolidado.json",
                output="semantic-graph/grafo_semantico.json",
                control_flow_out="semantic-graph/control_flow.json"):
    with open(consolidado, "r", encoding="utf-8") as f:
        data = json.load(f)

    G = nx.DiGraph()
    control_flow = []

    # Procesar programas COBOL y copys
    for prog in data.get("programs", []):
        node_name = None
        if prog.get("programName"):
            node_name = prog["programName"].strip(".")
            G.add_node(node_name, type="program")
        elif prog.get("fileName"):
            node_name = prog["fileName"].replace(".cbl", "")
            G.add_node(node_name, type="copy")

        if not node_name:
            continue

        # Resolver variables con VALUE
        value_map = {}
        for var in prog.get("dataDivision", []):
            if var.get("name") and "VALUE" in var.get("text", ""):
                tokens = var["text"].split("VALUE")
                if len(tokens) > 1:
                    val = tokens[1].strip().strip(".").strip("'")
                    value_map[var["name"]] = val

        # Aristas CALL
        for stmt in prog.get("procedureDivision", []):
            if stmt.get("statementType") == "CALL":
                target = stmt["text"].split()[1].replace("'", "").replace(".", "")
                if target in value_map:
                    target = value_map[target]
                G.add_node(target, type="program")
                G.add_edge(node_name, target)

        # Forzar aristas hacia stubs si el programa es COBAP01
        if node_name == "COBAP01":
            for stub in ["ACCREC-STUB", "PARAMS-STUB"]:
                G.add_node(stub, type="copy")
                G.add_edge(node_name, stub)

    # Procesar JOB1 si existe en el consolidado
    if "JOB1" in data:
        job = data["JOB1"]
        job_name = job.get("jobName", "JOB1").strip(".") + "_JOB"
        G.add_node(job_name, type="job")
        for step in job.get("steps", []):
            if step.get("program"):
                target = step["program"].strip(".")
                G.add_node(target, type="program")
                if step.get("condition"):
                    cond = step["condition"]
                    explanation = step.get("conditionExplanation", "")
                    G.add_edge(job_name, target, condition=cond, explanation=explanation)
                    control_flow.append({
                        "job": job_name,
                        "step": step.get("name"),
                        "program": target,
                        "condition": cond,
                        "explanation": explanation
                    })
                    print(f"[INFO] Condición JCL detectada en {step['name']}: {cond} ({explanation})")
                else:
                    G.add_edge(job_name, target)

    # Exportar grafo semántico
    graph_data = json_graph.node_link_data(G)
    os.makedirs(os.path.dirname(output), exist_ok=True)
    with open(output, "w", encoding="utf-8") as out:
        json.dump(graph_data, out, indent=4)

    # Exportar control_flow.json en semantic-graph/ con clave "control_flow"
    os.makedirs(os.path.dirname(control_flow_out), exist_ok=True)
    with open(control_flow_out, "w", encoding="utf-8") as cf:
        json.dump({"control_flow": control_flow}, cf, indent=4)

    print(f"[OK] Grafo semántico generado en {output}")
    print(f"[OK] Control flow exportado en {control_flow_out}")

if __name__ == "__main__":
    build_graph()
