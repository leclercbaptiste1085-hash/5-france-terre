# Sources des données

- `brut/fao_rendement_ble.csv` (t/ha) et `brut/fao_commerce_france.csv` (milliers USD) : FAOSTAT, fichiers en masse
  Production_Crops_Livestock_E_Europe et Trade_CropsLivestock_E_Europe (versions de décembre 2025 et juillet 2026),
  extraits par extraire_fao.py. Total agricole = somme des produits détaillés (code CPC numérique) ; boissons alcoolisées
  = agrégat F1907 ; fruits et légumes = agrégat F1889.
- `brut/eurostat_rendement_ble.json`, `brut/eurostat_exploitations.json` : Eurostat apro_cpsh1 et ef_m_farmleg,
  conservés pour contrôle (séries 2010-2026, cohérentes avec FAOSTAT et l'INSEE).
- `brut/insee_exploitations.csv`, `brut/insee_specialisation.csv` : INSEE, Tableaux de l'économie française, fiche
  « Nombre d'exploitations » (https://www.insee.fr/fr/statistiques/7728861, figures 1 et 2 ; page et classeur
  AGRI24-F12.xlsx copiés dans brut/), d'après Agreste, recensements agricoles. France métropolitaine.
- `brut/cgaaer_haies.csv` : CGAAER, rapport n° 22114, avril 2023, p. 10-11 (brut/cgaaer_haies_2023.pdf).
- `brut/ler_dupraz.csv` : 1,34 mesuré (peupliers, Vézénobres) et 1,60 simulé (noyers, Restinclières) : diaporama
  Dupraz et al. repris par la Mission bocage (brut/pnr_mission_bocage_agroforesterie.pdf) ; 1,3 mesuré et 1,6 simulé :
  texte de C. Dupraz, concours agro-véto 2023 (brut/dupraz_concours_2023.pdf) ; 1,4 simulé sur 40 ans, interception de
  la lumière 1,39, eau 1,55 : INRAE (brut/inrae_agroforesterie_agrivoltaisme.pdf).
- `entreprises.csv` (k€) : pages « chiffres clés » de Boursorama (brut/bour_*.html, consultées le 12/11/2026), extraites
  par extraire_boursorama.py. ABC Bourse, utilisé pour le projet mer, ne publie pas les comptes d'Exel ni de Savencia et
  décale d'un an les exercices de Bonduelle.
- Balance agroalimentaire (Douanes, Agreste) : Académie d'agriculture, fiche 10.02.Q06 révisée en août 2026
  (brut/academie_agriculture_commerce_ext.pdf).
- Joualles : brut/wikipedia_joualle.txt et brut/vergers_comminges_joualles.html.
