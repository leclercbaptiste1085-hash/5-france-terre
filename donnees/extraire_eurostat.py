# Extrait des fichiers JSON Eurostat (brut/) les boues d'épuration et le compostage des déchets municipaux.
# Téléchargement :
#   https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/env_ww_spd?unit=THS_T&geo=FR&geo=DE&...
#   https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/env_wasmun?wst_oper=RCY_C_D&unit=KG_HAB&geo=FR&...
# Lancer depuis donnees/ : python extraire_eurostat.py
import csv, json

def lire(fichier):
    d = json.load(open(f"brut/{fichier}", encoding="utf-8"))
    dims, tailles = d["id"], d["size"]
    cat = {k: list(d["dimension"][k]["category"]["index"]) for k in dims}
    def val(**kw):
        i = 0
        for k, s in zip(dims, tailles):
            i = i * s + cat[k].index(kw[k])
        return d["value"].get(str(i))
    return cat, val

# Boues : dernière année où le total et l'usage agricole sont renseignés
cat, val = lire("eurostat_boues.json")
with open("brut/eurostat_boues_pays.csv", "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f); w.writerow(["pays", "annee", "total_kt", "agriculture", "compost", "incineration", "decharge", "autre"])
    for g in cat["geo"]:
        for a in reversed(cat["time"]):
            k = dict(freq="A", ww_tp="URB_TP", unit="THS_T", geo=g, time=a)
            tot = val(ww_tpar="SLD", **k)
            if tot and val(ww_tpar="SLD_AGR", **k) is not None:
                w.writerow([g, a, tot] + [val(ww_tpar=p, **k) or 0 for p in
                                          ("SLD_AGR", "SLD_COMP", "SLD_INCN", "SLD_LAND", "SLD_OTH")])
                break

# Compostage et méthanisation des déchets municipaux (kg/habitant)
cat, val = lire("eurostat_dechets_municipaux.json")
with open("brut/eurostat_compostage.csv", "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f); w.writerow(["pays", "annee", "kg_hab"])
    for g in cat["geo"]:
        for a in cat["time"]:
            v = val(freq="A", wst_oper="RCY_C_D", unit="KG_HAB", geo=g, time=a)
            if v is not None:
                w.writerow([g, a, v])
print("ok")
