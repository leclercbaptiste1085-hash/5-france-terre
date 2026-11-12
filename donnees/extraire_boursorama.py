# Extrait les comptes annuels des pages « chiffres clés » de Boursorama (brut/bour_*.html) vers entreprises.csv
# Lancer depuis donnees/ : python extraire_boursorama.py
import csv, html, re

PAGES = {"BON": "Bonduelle", "EXE": "Exel Industries", "LOUP": "LDC", "SAVE": "Savencia"}
POSTES = {"Chiffre d'affaires", "Résultat opérationnel", "Résultat net", "Effectif"}

def cellules(fichier):
    s = open(fichier, encoding="utf-8", errors="replace").read()
    s = re.sub(r"<script.*?</script>", "", s, flags=re.S)
    s = html.unescape(re.sub(r"<[^>]+>", "|", s)).replace("\xa0", " ").replace(" ", " ")
    return [c.strip() for c in s.split("|") if c.strip()]

lignes = []
for code, nom in PAGES.items():
    c = cellules(f"brut/bour_{code}.html")
    vus, annees = set(), None
    for k, x in enumerate(c):
        if re.fullmatch(r"\d\d\.\d\d", x) and all(re.fullmatch(r"\d\d\.\d\d", y) for y in c[k:k + 4]):
            annees = [2000 + int(y[3:]) for y in c[k:k + 4]]
            clot = c[k][:2]
        if x in POSTES and annees and x not in vus:
            vus.add(x)
            for a, v in zip(annees, c[k + 1:k + 5]):
                v = v.replace(" ", "")
                if re.fullmatch(r"-?\d+", v):
                    lignes.append([nom, a, clot, x, int(v)])
    assert {"Chiffre d'affaires", "Résultat net"} <= vus, f"{nom} : postes manquants {vus}"

with open("entreprises.csv", "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f)
    w.writerow(["entreprise", "exercice", "mois_cloture", "poste", "valeur_keur"])
    w.writerows(lignes)
print(len(lignes), "lignes")
