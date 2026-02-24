import json
import os
import networkx as nx

def classify_node(node_name):
    if node_name.startswith("JOB"):
        return "Job"
    elif node_name.endswith("-STUB"):
        return "Copybook Stub"
    elif node_name.startswith("COB") or node_name.startswith("CIC") or node_name.startswith("UTL"):
        return "Program"
    else:
        return "Unknown"

def classify_graph(graph_file="semantic-graph/grafo_semantico.json", output_file="rules/clasificacion.json"):
    with open(graph_file, "r") as f:
        data = json.load(f)
    G = nx.readwrite.json_graph.node_link_graph(data)

    classified = {}
    for node in G.nodes:
        category = classify_node(node)
        classified[node] = {"category": category}

    os.makedirs(os.path.dirname(output_file), exist_ok=True)
    with open(output_file, "w") as out:
        json.dump(classified, out, indent=4)
    print(f"[OK] Clasificación exportada a {output_file}")

if __name__ == "__main__":
    classify_graph()
