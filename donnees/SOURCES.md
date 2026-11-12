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
- `brut/fao_engrais_france.csv` (tonnes d'éléments nutritifs) : FAOSTAT Inputs_FertilizersNutrient_E_Europe
  (version de juillet 2026), extrait par extraire_fao.py.
- Hugo, « La terre appauvrie par la mer » : brut/hugo_miserables_t5_l2_ch1.html (Wikisource, texte validé, éd. 1890).
- Champs d'épandage de Paris : É. Dufour, Métropolitiques (brut/dufour_metropolitiques_epandage.pdf).
- `brut/eurostat_boues_pays.csv`, `brut/eurostat_compostage.csv` : Eurostat env_ww_spd et env_wasmun (JSON dans brut/),
  extraits par extraire_eurostat.py.
- `brut/starck_azote_excreta.csv` : Starck, Fardet, Esculier, arXiv 2310.06880, 2023, figure 1 et texte p. 9
  (160 ktN air, 124 ktN eau et sols, 31 ktN cultures ; moyenne 2015-2020) (brut/starck_2023_azote_excreta.pdf).
- Dehesa : Real Decreto 4/2014, art. 6 et annexe (BOE-A-2014-318) ; Agronews Castilla y León, 5 juillet 2017.
- `brut/ordres_grandeur_azote.csv` : synthèse (sources par ligne dans le fichier). Moules : Eurostat fish_aq2a 2024
  (MUS 45 787 t + MSM 2 988 t, brut/eurostat_aquaculture_fr.json) x 13,7 kg N/t (Taylor et al., 2019) ; laminaires :
  65 000 t x 4,5 kg N/t frais (23 g N et 5,1 kg de biomasse par mètre de filière, Limfjorden) ; fleuves : SDES
  (brut/sdes_flux_azote_mer.pdf, 607 000 t, moyenne sur dix ans).
