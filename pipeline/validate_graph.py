import json
import networkx as nx
import os

def validate_graph(graph_file="semantic-graph/grafo_semantico.json"):
    # Normalizar la ruta para evitar duplicados
    graph_file = os.path.normpath(graph_file)

    if not os.path.exists(graph_file):
        print(f"[WARN] {graph_file} no existe.")
        return

    with open(graph_file, "r", encoding="utf-8") as f:
        data = json.load(f)

    # Validar que tenga estructura de grafo con "nodes" y "edges"
    if not isinstance(data, dict) or "nodes" not in data or "edges" not in data:
        print(f"[WARN] {graph_file} no es un grafo válido, se omite.")
        return

    # Construir grafo desde node_link_data (usa "edges")
    G = nx.node_link_graph(data)

    # Validaciones básicas
    if not G.nodes:
        print("[WARN] El grafo no contiene nodos.")
    if not G.edges:
        print("[WARN] El grafo no contiene aristas.")

    print(f"[OK] Grafo válido con {len(G.nodes)} nodos y {len(G.edges)} aristas.")

if __name__ == "__main__":
    validate_graph()
