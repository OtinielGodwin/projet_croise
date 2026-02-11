import requests
import json
import csv
import os

BASE_URL = "http://172.22.215.130:8080/v2/archived"

DATASETS = ["sequences", "cancer", "biomedical", "social"]
COUNTRY = "FR"        
TRIMESTER = "2023Q1"

OUTPUT_DIR = "api_data"
os.makedirs(OUTPUT_DIR, exist_ok=True)


def fetch_data(dataset, country, trimester):
    url = f"{BASE_URL}/{dataset}/{country}/{trimester}"
    print(f"Téléchargement : {url}")
    
    r = requests.get(url, timeout=60)
    r.raise_for_status()
    
    return r.json()


def save_json(data, dataset):
    path = os.path.join(OUTPUT_DIR, f"{dataset}.json")
    with open(path, "w", encoding="utf-8") as f:
        json.dump(data, f, indent=2, ensure_ascii=False)
    print(f"JSON sauvegardé → {path}")


def flatten_dict(d, parent_key='', sep='_'):
    """Aplatit un JSON imbriqué pour le CSV"""
    items = []
    for k, v in d.items():
        new_key = f"{parent_key}{sep}{k}" if parent_key else k
        if isinstance(v, dict):
            items.extend(flatten_dict(v, new_key, sep=sep).items())
        else:
            items.append((new_key, v))
    return dict(items)


def save_csv(data, dataset):
    if not isinstance(data, list):
        data = [data]

    flat_data = [flatten_dict(item) for item in data if isinstance(item, dict)]
    if not flat_data:
        print(f"Aucune donnée exploitable pour CSV ({dataset})")
        return

    keys = sorted({k for row in flat_data for k in row.keys()})
    path = os.path.join(OUTPUT_DIR, f"{dataset}.csv")

    with open(path, "w", newline='', encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=keys)
        writer.writeheader()
        writer.writerows(flat_data)

    print(f"CSV sauvegardé → {path}")


def detect_sequence_field(record):
    """Trouve automatiquement un champ contenant une séquence biologique"""
    for key, value in record.items():
        if not isinstance(value, str):
            continue
        v = value.upper()
        if set(v) <= set("ACGTUN") and len(v) > 20:
            return key
        if set(v) <= set("ACDEFGHIKLMNPQRSTVWY") and len(v) > 20:
            return key
    return None


def save_fasta(data, dataset):
    if not isinstance(data, list):
        data = [data]

    fasta_lines = []
    for i, record in enumerate(data):
        if not isinstance(record, dict):
            continue

        seq_field = detect_sequence_field(record)
        if not seq_field:
            continue

        seq = record[seq_field].replace("\n", "").replace(" ", "")
        header = record.get("id") or record.get("name") or f"{dataset}_{i}"
        fasta_lines.append(f">{header}")
        fasta_lines.append(seq)

    if not fasta_lines:
        print(f"Aucune séquence détectée pour FASTA ({dataset})")
        return

    path = os.path.join(OUTPUT_DIR, f"{dataset}.fasta")
    with open(path, "w") as f:
        f.write("\n".join(fasta_lines))

    print(f"FASTA sauvegardé → {path}")


def main():
    for dataset in DATASETS:
        try:
            data = fetch_data(dataset, COUNTRY, TRIMESTER)
            save_json(data, dataset)
            save_csv(data, dataset)
            save_fasta(data, dataset)
            print("----")
        except Exception as e:
            print(f"Erreur avec {dataset}: {e}")


if __name__ == "__main__":
    main()
