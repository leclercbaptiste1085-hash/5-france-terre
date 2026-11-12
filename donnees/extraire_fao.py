# Extrait des fichiers FAOSTAT (bulk Europe) les rendements du blé et les échanges agricoles de la France.
# Fichiers à télécharger dans brut/fao/ :
#   https://bulks-faostat.fao.org/production/Production_Crops_Livestock_E_Europe.zip
#   https://bulks-faostat.fao.org/production/Trade_CropsLivestock_E_Europe.zip
#   https://bulks-faostat.fao.org/production/Inputs_FertilizersNutrient_E_Europe.zip (enregistré sous engrais.zip)
# Lancer depuis donnees/ : python extraire_fao.py
import csv, collections, zipfile

def lignes(zip_, nom):
    with zipfile.ZipFile(f"brut/fao/{zip_}") as z, z.open(nom) as f:
        r = csv.reader(line.decode("latin-1") for line in f)
        h = next(r)
        ans = [(i, int(c[1:])) for i, c in enumerate(h) if c.startswith("Y")]
        for row in r:
            yield row, ans

# 1. Rendement du blé (kg/ha -> t/ha)
PAYS = {"France": "France", "Germany": "Allemagne",
        "United Kingdom of Great Britain and Northern Ireland": "Royaume-Uni"}
with open("brut/fao_rendement_ble.csv", "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f); w.writerow(["pays", "annee", "t_ha"])
    for row, ans in lignes("fao.zip", "Production_Crops_Livestock_E_Europe_NOFLAG.csv"):
        if row[5] == "Wheat" and row[7] == "Yield" and row[2] in PAYS:
            assert row[8] == "kg/ha"
            w.writerows([PAYS[row[2]], a, round(float(row[i]) / 1000, 3)] for i, a in ans if row[i])

# 2. Échanges de la France (milliers USD) : total = somme des produits détaillés (code CPC numérique),
#    agrégats FAO pour les boissons alcoolisées (F1907) et les fruits et légumes (F1889)
d = collections.defaultdict(float)
for row, ans in lignes("trade.zip", "Trade_CropsLivestock_E_Europe_NOFLAG.csv"):
    if row[2] != "France" or row[7] not in ("Export value", "Import value"):
        continue
    cpc = row[4].lstrip("'")
    k = "agri" if cpc[:1].isdigit() else {"F1907": "alcool", "F1889": "fl"}.get(cpc)
    if k:
        for i, a in ans:
            if row[i]:
                d[(k, row[7][:2], a)] += float(row[i])
with open("brut/fao_commerce_france.csv", "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f); w.writerow(["annee", "agri_x", "agri_m", "alcool_x", "alcool_m", "fl_x", "fl_m"])
    for a in range(1961, 2030):
        v = [d.get((k, e, a)) for k in ("agri", "alcool", "fl") for e in ("Ex", "Im")]
        if None not in v:
            w.writerow([a] + [round(x) for x in v])
# 3. Engrais de la France (tonnes d'éléments nutritifs N, P2O5, K2O) : production, importations, usage agricole
ELEM = {"Nutrient nitrogen N (total)": "Azote", "Nutrient phosphate P2O5 (total)": "Phosphate",
        "Nutrient potash K2O (total)": "Potasse"}
POSTE = {"Production": "production", "Import quantity": "importations", "Agricultural Use": "usage"}
with open("brut/fao_engrais_france.csv", "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f); w.writerow(["element", "poste", "annee", "tonnes"])
    for row, ans in lignes("engrais.zip", "Inputs_FertilizersNutrient_E_Europe_NOFLAG.csv"):
        # ce fichier n'a pas de colonne CPC : Item en 4, Element en 6
        if row[2] == "France" and row[4] in ELEM and row[6] in POSTE:
            w.writerows([ELEM[row[4]], POSTE[row[6]], a, float(row[i])] for i, a in ans if row[i])
print("ok")
