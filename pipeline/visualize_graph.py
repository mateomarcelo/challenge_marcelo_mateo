import os
import json
import networkx as nx
import matplotlib.pyplot as plt

def load_graph_from_json(path):
    path = os.path.normpath(path)  # normalizar ruta
    with open(path, "r", encoding="utf-8") as f:
        data = json.load(f)
    # Solo procesar si es un grafo válido con "nodes" y "edges"
    if isinstance(data, dict) and "nodes" in data and "edges" in data:
        return nx.node_link_graph(data)
    else:
        print(f"[WARN] {path} no es un grafo válido, se omite.")
        return None

def visualize_graph(G, title, output_file):
    plt.figure(figsize=(8, 6))
    pos = nx.spring_layout(G)
    nx.draw(
        G, pos,
        with_labels=True,
        node_color="lightblue",
        edge_color="gray",
        node_size=2000,
        font_size=10
    )
    plt.title(title)
    plt.savefig(output_file)
    plt.close()
    print(f"[OK] Grafo guardado en {output_file}")

def visualize_all():
    folder = "semantic-graph"
    for fname in os.listdir(folder):
        # Solo grafo_semantico.json y otros grafos válidos
        if fname not in ["grafo_semantico.json", "call_graph.json", "data_lineage.json"]:
            continue
        path = os.path.join(folder, fname)
        G = load_graph_from_json(path)
        if G is None:
            continue
        title = fname.replace(".json", "")
        output_file = os.path.join("doc", f"{title}.png")
        visualize_graph(G, title, output_file)

if __name__ == "__main__":
    visualize_all()
