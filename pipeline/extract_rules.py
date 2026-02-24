import json
import os
import networkx as nx

def extract_rules(graph_file="semantic-graph/grafo_semantico.json",
                  classification_file="rules/clasificacion.json",
                  output_file="rules/reglas.json"):

    # Cargar grafo
    with open(graph_file, "r") as f:
        data = json.load(f)
    G = nx.readwrite.json_graph.node_link_graph(data)

    # Cargar clasificación
    with open(classification_file, "r") as f:
        classification = json.load(f)

    rules = []

    # Regla 1: cada Job invoca a sus programas
    for edge in G.edges:
        src, tgt = edge
        if classification.get(src, {}).get("category") == "Job" and classification.get(tgt, {}).get("category") == "Program":
            rules.append(f"El job {src} ejecuta el programa {tgt}.")

    # Regla 2: programas que dependen de STUBs
    for edge in G.edges:
        src, tgt = edge
        if classification.get(tgt, {}).get("category") == "Copybook Stub":
            rules.append(f"El programa {src} utiliza el copybook {tgt}.")

    # Regla 3: programas aislados
    for node in G.nodes:
        if G.degree(node) == 0 and classification.get(node, {}).get("category") == "Program":
            rules.append(f"El programa {node} está definido pero no se invoca en el flujo actual.")

    # Exportar reglas
    os.makedirs(os.path.dirname(output_file), exist_ok=True)
    with open(output_file, "w") as out:
        json.dump(rules, out, indent=4, ensure_ascii=False)

    print(f"[OK] Reglas de negocio exportadas a {output_file}")

if __name__ == "__main__":
    extract_rules()
