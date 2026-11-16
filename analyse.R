# La France et la terre : le plateau, la spécialisation, et ce que l'arbre rapporte
# Sources : FAOSTAT (rendements, échanges), INSEE/Agreste (recensements agricoles), CGAAER (haies),
#           Dupraz et al., INRAE (agroforesterie), Boursorama (comptes des industriels)
# Lancer depuis le dossier du projet : Rscript analyse.R
# (extraction préalable : donnees/extraire_fao.py et donnees/extraire_boursorama.py)

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(scales)
})

brut <- "donnees/brut/"
dir.create("sorties", showWarnings = FALSE)
res <- file("sorties/resultats.txt", "w", encoding = "UTF-8")
note <- function(...) { cat(sprintf(...), "\n", file = res); cat(sprintf(...), "\n") }

# ---------------------------------------------------------------------------
# Style : fond blanc, bleu et vert, police Calibri (identique au projet mer)
# ---------------------------------------------------------------------------
papier <- "white"; encre <- "#1B2A3A"; vert <- "#2E8B57"; bleu <- "#1F5A8C"; brume <- "#B8C7D6"

theme_set(theme_minimal(base_size = 12, base_family = "Calibri") +
  theme(plot.background  = element_rect(fill = papier, colour = NA),
        panel.background = element_rect(fill = papier, colour = NA),
        plot.title.position = "plot", plot.caption.position = "plot",
        plot.title    = element_text(face = "bold", colour = encre, size = 16, margin = margin(b = 4)),
        plot.subtitle = element_text(colour = "#4A5568", margin = margin(b = 12)),
        plot.caption  = element_text(colour = "#6B7280", size = 8, hjust = 0, margin = margin(t = 10)),
        axis.text     = element_text(colour = "#4A5568"),
        panel.grid.major.y = element_line(colour = "#E3E8EE"),
        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        strip.text    = element_text(face = "bold", colour = encre, hjust = 0),
        legend.position = "none", plot.margin = margin(16, 20, 12, 16)))

enregistrer <- function(g, fichier, l = 8, h = 5) ggsave(file.path("sorties", fichier), g, width = l, height = h, dpi = 300)
lire <- function(f) read.csv(paste0(brut, f), encoding = "UTF-8")

# ---------------------------------------------------------------------------
# 1. Le plateau : rendement du blé (t/ha), moyenne mobile sur 5 ans
# ---------------------------------------------------------------------------
ble <- lire("fao_rendement_ble.csv") |>
  arrange(pays, annee) |> group_by(pays) |>
  mutate(lisse = as.numeric(stats::filter(t_ha, rep(1 / 5, 5), sides = 2))) |> ungroup()

fr <- filter(ble, pays == "France")
moy <- function(a, b) mean(fr$t_ha[fr$annee >= a & fr$annee <= b])
note("Blé France : %.2f t/ha (1961-65) -> %.2f (1992-96) -> %.2f (2020-24)", moy(1961, 1965), moy(1992, 1996), moy(2020, 2024))
note("Gain annuel moyen : %.3f t/ha/an de 1963 à 1994, %.3f de 1994 à 2022",
     (moy(1992, 1996) - moy(1961, 1965)) / 31, (moy(2020, 2024) - moy(1992, 1996)) / 28)

der <- max(ble$annee)
g1 <- ggplot(ble, aes(annee, t_ha, group = pays)) +
  annotate("rect", xmin = 1996, xmax = der, ymin = -Inf, ymax = Inf, fill = brume, alpha = 0.25) +
  annotate("text", x = 1997, y = 1.2, label = "Depuis 1996 : plus aucun progrès en France", hjust = 0,
           family = "Calibri", colour = "#4A5568", size = 3.6) +
  geom_line(aes(colour = pays == "France"), alpha = 0.25, linewidth = 0.4) +
  geom_line(aes(y = lisse, colour = pays == "France", linewidth = pays == "France"), na.rm = TRUE) +
  geom_text(data = filter(ble, annee == der - 2), aes(y = lisse, label = pays, colour = pays == "France"),
            hjust = -0.1, family = "Calibri", size = 3.8) +
  scale_colour_manual(values = c(`TRUE` = vert, `FALSE` = bleu)) +
  scale_linewidth_manual(values = c(`TRUE` = 1.6, `FALSE` = 0.6)) +
  scale_x_continuous(breaks = seq(1960, 2020, 10), expand = expansion(mult = c(0.01, 0.14))) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Le rendement du blé français a triplé en trente-cinq ans,\npuis s'est arrêté",
       subtitle = "Rendement du blé, tonnes par hectare (trait épais : moyenne mobile sur 5 ans)",
       x = NULL, y = NULL, caption = "Source : FAOSTAT, Production_Crops_Livestock (Wheat, Yield).")
enregistrer(g1, "1_rendement_ble.png")

# ---------------------------------------------------------------------------
# 2. Les exploitations : nombre (recensements agricoles, France métropolitaine)
# ---------------------------------------------------------------------------
expl <- lire("insee_exploitations.csv")
note("Exploitations : %.0f milliers en 1970, %.0f en 2020 (%.0f %%)", expl$milliers[1], tail(expl$milliers, 1),
     100 * (tail(expl$milliers, 1) / expl$milliers[1] - 1))

g2 <- ggplot(expl, aes(factor(annee), milliers)) +
  geom_col(aes(fill = annee == 2020), width = 0.65) +
  geom_text(aes(label = label_number(big.mark = " ", accuracy = 1)(milliers)),
            vjust = -0.5, family = "Calibri", colour = encre, size = 3.8) +
  scale_fill_manual(values = c(`TRUE` = vert, `FALSE` = bleu)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Quatre exploitations sur cinq ont disparu depuis 1970",
       subtitle = "Nombre d'exploitations agricoles, en milliers",
       x = NULL, y = NULL,
       caption = "Source : Agreste, recensements agricoles, via INSEE (édition 2024). Champ : France métropolitaine.") +
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
enregistrer(g2, "2_exploitations.png")

# ---------------------------------------------------------------------------
# 3. La spécialisation : variation du nombre d'exploitations par orientation, 2010-2020
# ---------------------------------------------------------------------------
spe <- lire("insee_specialisation.csv") |>
  mutate(var = n2020 / n2010 - 1, poly = grepl("^Polyculture", specialisation),
         specialisation = reorder(specialisation, var))
with(spe, note("Polyculture-polyélevage : %.0f %% ; grandes cultures : %.0f %%",
               100 * var[poly], 100 * var[specialisation == "Grandes cultures"]))

g3 <- ggplot(spe, aes(var, specialisation)) +
  geom_col(aes(fill = poly), width = 0.65) +
  geom_vline(xintercept = 0, colour = encre, linewidth = 0.4) +
  geom_text(aes(label = percent(var, accuracy = 1, suffix = " %"), hjust = ifelse(var < 0, 1.15, -0.15)),
            family = "Calibri", colour = encre, size = 3.6) +
  scale_fill_manual(values = c(`TRUE` = vert, `FALSE` = bleu)) +
  scale_x_continuous(labels = label_percent(suffix = " %"), expand = expansion(mult = c(0.15, 0.12))) +
  labs(title = "Les grandes cultures tiennent ; les fermes qui mêlaient\ncultures et élevage perdent près d'un tiers des leurs",
       subtitle = "Variation du nombre d'exploitations entre 2010 et 2020, selon la spécialisation",
       x = NULL, y = NULL,
       caption = "Source : Agreste, recensements agricoles 2010 et 2020, via INSEE. Champ : France métropolitaine.") +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "#E3E8EE"))
enregistrer(g3, "3_specialisation.png")

# ---------------------------------------------------------------------------
# 4. La balance : taux de couverture (exportations / importations)
#    Le vin et les spiritueux sont retirés : ils masquent l'état du reste de l'agriculture.
# ---------------------------------------------------------------------------
s1 <- "Agriculture et alimentation, hors boissons alcoolisées"
s2 <- "Fruits et légumes"
com <- lire("fao_commerce_france.csv") |>
  transmute(annee, !!s1 := (agri_x - alcool_x) / (agri_m - alcool_m), !!s2 := fl_x / fl_m) |>
  pivot_longer(-annee, names_to = "serie", values_to = "taux")
dc <- max(com$annee)
cv <- function(s, a) com$taux[com$serie == s & com$annee == a]
pic <- filter(com, serie == s1) |> slice_max(taux)
note("Couverture hors alcools : %.0f %% en 1990, pic %.0f %% en %d, %.0f %% en %d",
     100 * cv(s1, 1990), 100 * pic$taux, pic$annee, 100 * cv(s1, dc), dc)
note("Couverture fruits et légumes : %.0f %% en %d", 100 * cv(s2, dc), dc)

g4 <- ggplot(com, aes(annee, taux, colour = serie)) +
  geom_hline(yintercept = 1, colour = encre, linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = 2023, y = 0.93, label = "Équilibre : on exporte autant qu'on importe", hjust = 1,
           family = "Calibri", colour = "#4A5568", size = 3.3) +
  geom_line(linewidth = 1.2) +
  geom_text(data = filter(com, annee == dc), aes(label = percent(taux, accuracy = 1, suffix = " %")),
            hjust = -0.2, vjust = 1.1, family = "Calibri", size = 3.8) +
  annotate("text", x = 1990, y = 1.5, label = "Agriculture et alimentation\n(hors vins et spiritueux)",
           colour = bleu, family = "Calibri", size = 3.6) +
  annotate("text", x = 1992, y = 0.45, label = "Fruits et légumes", colour = vert, family = "Calibri", size = 3.6) +
  scale_colour_manual(values = setNames(c(bleu, vert), c(s1, s2))) +
  scale_x_continuous(breaks = seq(1960, 2020, 10), expand = expansion(mult = c(0.01, 0.07))) +
  scale_y_continuous(labels = label_percent(suffix = " %"), limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Sans le vin, la France n'exporte plus\nque ce qu'elle importe",
       subtitle = "Taux de couverture : exportations rapportées aux importations, en valeur",
       x = NULL, y = NULL,
       caption = paste("Source : FAOSTAT, Trade_CropsLivestock (somme des produits agricoles détaillés ; agrégats",
                       "« Alcoholic Beverages » et « Fruit and Vegetables »).\nHors produits de la mer : les niveaux diffèrent des chiffres des Douanes (Agreste)."))
enregistrer(g4, "4_couverture.png")

# ---------------------------------------------------------------------------
# 5. Les haies : ce qui disparaît chaque année, ce qui est planté
# ---------------------------------------------------------------------------
haies <- lire("cgaaer_haies.csv") |>
  mutate(lib = factor(paste(type, periode, sep = "\n"), levels = paste(type, periode, sep = "\n")))
note("Haies : perte nette %d km/an (2006-2014) -> %d km/an (2017-2021), plantation ~%d km/an",
     haies$km_an[1], haies$km_an[2], haies$km_an[3])

g5 <- ggplot(haies, aes(lib, km_an)) +
  geom_col(aes(fill = type), width = 0.6) +
  geom_text(aes(label = label_number(big.mark = " ")(km_an)), vjust = -0.5, family = "Calibri", colour = encre, size = 4) +
  scale_fill_manual(values = c(`Perte nette` = bleu, Plantation = vert)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Les haies disparaissent deux fois plus vite qu'il y a dix ans",
       subtitle = "Kilomètres de haies par an, France métropolitaine",
       x = NULL, y = NULL,
       caption = "Source : CGAAER, La haie, levier de la planification écologique, rapport n° 22114, avril 2023 (estimation AFAC-Agroforesteries et Solagro).") +
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
enregistrer(g5, "5_haies.png")

# ---------------------------------------------------------------------------
# 6. Ce que l'arbre rapporte : surface équivalente (LER, land equivalent ratio)
#    LER = rendement relatif des cultures + rendement relatif des arbres, par rapport aux peuplements purs.
#    100 ha en agroforesterie produisent autant que 100 x LER ha où arbres et cultures sont séparés.
# ---------------------------------------------------------------------------
ler <- lire("ler_dupraz.csv") |>
  mutate(ha = 100 * ler, lib = paste0(systeme, "\n", statut), lib = factor(lib, levels = rev(lib)))

g6 <- ggplot(ler, aes(ha, lib)) +
  geom_col(aes(fill = statut), width = 0.6) +
  geom_vline(xintercept = 100, colour = encre, linewidth = 0.4, linetype = "dashed") +
  geom_text(aes(label = paste0(ha, " ha")), hjust = -0.15, family = "Calibri", colour = encre, size = 4) +
  scale_fill_manual(values = c(Mesuré = vert, Simulé = brume)) +
  scale_x_continuous(limits = c(0, 185), breaks = c(0, 50, 100, 150), expand = expansion(mult = c(0, 0))) +
  labs(title = "100 hectares d'arbres et de cultures mêlés\nproduisent autant que 130 à 160 hectares séparés",
       subtitle = "Surface nécessaire, arbres et cultures séparés, pour produire autant que 100 ha en agroforesterie",
       x = NULL, y = NULL,
       caption = paste("Sources : Dupraz et al., INRA-INRAE, sites de Vézénobres (Gard) et Restinclières (Hérault) ;",
                       "C. Dupraz, texte du concours agro-véto 2023.\nVert : valeur mesurée sur des arbres récoltés. Gris : valeurs simulées sur une rotation complète.")) +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "#E3E8EE"))
enregistrer(g6, "6_agroforesterie.png")

# ---------------------------------------------------------------------------
# 7. Les industriels : marge nette
#    Exercices décalés : Bonduelle clôt fin juin, Exel fin septembre, LDC fin février.
# ---------------------------------------------------------------------------
ent <- read.csv("donnees/entreprises.csv", encoding = "UTF-8") |>
  filter(poste %in% c("Chiffre d'affaires", "Résultat net")) |>
  select(-mois_cloture) |>
  pivot_wider(names_from = poste, values_from = valeur_keur) |>
  rename(ca = `Chiffre d'affaires`, rn = `Résultat net`) |>
  mutate(ca_m = ca / 1000, marge = rn / ca,
         entreprise = factor(entreprise, levels = c("Exel Industries", "Bonduelle", "LDC", "Savencia"))) |>
  arrange(entreprise, exercice)
for (e in levels(ent$entreprise)) with(filter(ent, entreprise == e),
  note("%s : CA %.0f -> %.0f M€ (%d-%d), marge nette %.1f %% -> %.1f %%",
       e, first(ca_m), last(ca_m), first(exercice), last(exercice), 100 * first(marge), 100 * last(marge)))

g7 <- ggplot(ent, aes(exercice, marge)) +
  geom_hline(yintercept = 0, colour = encre, linewidth = 0.4) +
  geom_col(aes(fill = marge >= 0), width = 0.6) +
  geom_text(aes(label = percent(marge, accuracy = 0.1, decimal.mark = ",", suffix = " %"), vjust = ifelse(marge >= 0, -0.5, 1.4)),
            family = "Calibri", colour = encre, size = 3.3) +
  facet_wrap(~entreprise, nrow = 1, scales = "free_x") +
  scale_fill_manual(values = c(`TRUE` = vert, `FALSE` = bleu)) +
  scale_y_continuous(labels = label_percent(suffix = " %"), expand = expansion(mult = c(0.15, 0.15))) +
  labs(title = "Transformer la production agricole rapporte peu",
       subtitle = "Marge nette (résultat net / chiffre d'affaires), par exercice",
       x = NULL, y = NULL,
       caption = paste("Source : comptes publiés, via Boursorama. Exercices clos fin juin (Bonduelle), fin septembre (Exel),",
                       "fin février (LDC : 2026 = mars 2025 - février 2026), fin décembre (Savencia)."))
enregistrer(g7, "7_entreprises_marge.png", l = 10, h = 4.8)

# ---------------------------------------------------------------------------
# 8. Les engrais : production nationale rapportée à l'usage agricole, début et fin de série
#    Au-dessus de 100 %, la France produit plus d'engrais qu'elle n'en épand.
#    L'usage agricole publié par la FAO est irrégulier entre 2003 et 2014 (creux sans rapport avec
#    les surfaces) : on compare donc deux périodes stables, 1961-1965 et 2020-2024.
# ---------------------------------------------------------------------------
eng <- lire("fao_engrais_france.csv") |>
  mutate(periode = case_when(annee <= 1965 ~ "1961-1965", annee >= 2020 ~ "2020-2024")) |>
  filter(!is.na(periode)) |>
  group_by(element, periode, poste) |> summarise(t = mean(tonnes), .groups = "drop") |>
  pivot_wider(names_from = poste, values_from = t) |>
  mutate(taux = production / usage, element = factor(element, levels = c("Potasse", "Azote", "Phosphate")))
for (i in seq_len(nrow(eng))) with(eng[i, ],
  note("Engrais, %s, %s : production %.0f kt, importations %.0f kt, usage %.0f kt, production / usage %.0f %%",
       element, periode, production / 1000, importations / 1000, usage / 1000, 100 * taux))

g8 <- ggplot(eng, aes(element, taux, fill = periode)) +
  geom_hline(yintercept = 1, colour = encre, linewidth = 0.4, linetype = "dashed") +
  geom_col(position = position_dodge(width = 0.7), width = 0.62) +
  geom_text(aes(label = percent(taux, accuracy = 1, suffix = " %")), position = position_dodge(width = 0.7),
            vjust = -0.5, family = "Calibri", colour = encre, size = 3.8) +
  annotate("text", x = 1.5, y = 1.06, label = "Production = besoins", hjust = 0.5,
           family = "Calibri", colour = "#4A5568", size = 3.3) +
  scale_fill_manual(values = c(`1961-1965` = bleu, `2020-2024` = vert)) +
  scale_y_continuous(labels = label_percent(suffix = " %"), expand = expansion(mult = c(0, 0.1))) +
  labs(title = "En 1961, la France produisait ses engrais ;\nelle importe aujourd'hui presque tout",
       subtitle = "Production nationale d'engrais rapportée à l'usage agricole (éléments nutritifs, moyennes sur 5 ans)",
       x = NULL, y = NULL, fill = NULL,
       caption = paste("Source : FAOSTAT, Inputs_FertilizersNutrient (N, P2O5, K2O). Bleu : 1961-1965 ; vert : 2020-2024.",
                       "\nLes années intermédiaires ne sont pas tracées : l'usage agricole publié y est irrégulier.")) +
  theme(legend.position = "top", legend.justification = "left", legend.text = element_text(colour = "#4A5568"))
enregistrer(g8, "8_engrais.png")

# ---------------------------------------------------------------------------
# 9. Les boues d'épuration : part rendue au sol (épandage direct + compost), par pays
#    Seuls les pays dont la donnée date de 2019 ou après sont retenus.
# ---------------------------------------------------------------------------
noms <- c(FR = "France", DE = "Allemagne", NL = "Pays-Bas", BE = "Belgique", AT = "Autriche", ES = "Espagne",
          IE = "Irlande", PL = "Pologne", SE = "Suède", DK = "Danemark", CH = "Suisse")
boues <- lire("eurostat_boues_pays.csv") |>
  filter(annee >= 2019, pays %in% names(noms)) |>
  mutate(sol = (agriculture + compost) / total_kt, nom = paste0(noms[pays], " (", annee, ")"),
         nom = reorder(nom, sol))
with(filter(boues, pays %in% c("FR", "DE", "NL")),
     note("Boues rendues au sol : %s", paste(sprintf("%s %.0f %%", pays, 100 * sol), collapse = ", ")))

g9 <- ggplot(boues, aes(sol, nom)) +
  geom_col(aes(fill = pays == "FR"), width = 0.65) +
  geom_text(aes(label = percent(sol, accuracy = 1, suffix = " %")), hjust = -0.15, family = "Calibri", colour = encre, size = 3.6) +
  scale_fill_manual(values = c(`TRUE` = vert, `FALSE` = bleu)) +
  scale_x_continuous(labels = label_percent(suffix = " %"), limits = c(0, 1.12), breaks = seq(0, 1, 0.25),
                     expand = expansion(mult = c(0, 0))) +
  labs(title = "La France rend ses boues d'épuration à la terre\nbien plus que ses voisins",
       subtitle = "Part des boues d'épuration urbaines épandues sur les champs ou compostées",
       x = NULL, y = NULL,
       caption = "Source : Eurostat, env_ww_spd (stations urbaines ; dernière année disponible, indiquée entre parenthèses). Le reste est surtout incinéré.") +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "#E3E8EE"))
enregistrer(g9, "9_boues.png")

# ---------------------------------------------------------------------------
# 10. Mais l'azote ne suit pas : devenir de l'azote des excréments en France (moyenne 2015-2020)
# ---------------------------------------------------------------------------
az <- lire("starck_azote_excreta.csv") |> mutate(destination = factor(destination, levels = rev(destination)))
note("Azote des excréments : %s", paste(sprintf("%s %.0f %% (%d kt)", az$destination, 100 * az$part, az$kt_n), collapse = " ; "))

g10 <- ggplot(az, aes(part, destination)) +
  geom_col(aes(fill = grepl("^Rendu", destination)), width = 0.6) +
  geom_text(aes(label = sprintf("%s  (%d kt)", percent(part, accuracy = 1, suffix = " %"), kt_n)),
            hjust = -0.1, family = "Calibri", colour = encre, size = 3.9) +
  scale_fill_manual(values = c(`TRUE` = vert, `FALSE` = bleu)) +
  scale_x_continuous(labels = label_percent(suffix = " %"), limits = c(0, 0.72), expand = expansion(mult = c(0, 0))) +
  labs(title = "Pourtant, 90 % de l'azote de nos excréments est perdu",
       subtitle = "Devenir de l'azote excrété en France, moyenne 2015-2020 (milliers de tonnes d'azote par an)",
       x = NULL, y = NULL,
       caption = paste("Source : Starck, Fardet et Esculier (LEESU), Fate of nitrogen in French human excreta, 2023 (prépublication arXiv 2310.06880).",
                       "\nL'azote est surtout dans l'urine (85 %) ; les stations d'épuration le renvoient dans l'air par dénitrification avant qu'il n'atteigne les boues.")) +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "#E3E8EE"))
enregistrer(g10, "10_azote_excreta.png", h = 3.8)

# ---------------------------------------------------------------------------
# 11. Ordres de grandeur : l'azote en jeu chaque année, et ce que chaque levier peut capter
#     Échelle logarithmique : les flux vont de 0,3 à 1 900 milliers de tonnes.
# ---------------------------------------------------------------------------
og <- lire("ordres_grandeur_azote.csv") |> mutate(flux = factor(flux, levels = rev(flux)))
mer <- sum(og$kt_n[og$groupe == "mer"]); fleuves <- og$kt_n[og$groupe == "perte"]
note("Mer cultivée : %.2f kt N retirées, soit %.2f %% des %d kt perdues par les fleuves ; x%.0f pour en capter 10 %%",
     mer, 100 * mer / fleuves, fleuves, 0.1 * fleuves / mer)

g11 <- ggplot(og, aes(kt_n * 1000, flux)) +
  geom_segment(aes(x = 100, xend = kt_n * 1000, yend = flux, colour = groupe), linewidth = 9) +
  geom_text(aes(label = paste(label_number(big.mark = " ", accuracy = 10)(kt_n * 1000), "t")),
            hjust = -0.12, family = "Calibri", colour = encre, size = 3.8) +
  scale_colour_manual(values = c(terre = "#7A8CA3", perte = bleu, levier = vert, mer = "#5FA8A0")) +
  scale_x_log10(labels = label_number(big.mark = " "), breaks = 10^(2:6), limits = c(100, 1.5e7),
                expand = expansion(mult = c(0, 0))) +
  labs(title = "La mer cultivée ne peut pas absorber ce que la terre perd :
le levier est à terre",
       subtitle = "Azote en jeu chaque année en France, en tonnes (échelle logarithmique : chaque graduation vaut 10 fois la précédente)",
       x = NULL, y = NULL,
       caption = paste("Sources : FAOSTAT ; SDES (flux des fleuves) ; Starck, Fardet et Esculier, 2023 ; Eurostat fish_aq2a (moules 2024)",
                       "et Taylor et al., 2019 (13,7 kg N par tonne) ;
laminaires : 65 000 t récoltées (Institut Agro) et 4,5 kg N par tonne fraîche (essais danois de laminaire sucrée). Ordres de grandeur, non additifs.")) +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "#E3E8EE"))
enregistrer(g11, "11_ordres_grandeur_azote.png", l = 9, h = 4.6)

# ---------------------------------------------------------------------------
# 12. Le bilan azoté des sols agricoles (France métropolitaine, 2020) : ce qui entre, ce que les cultures prennent,
#     ce qui se perd. Les apports sont empilés par origine.
# ---------------------------------------------------------------------------
bil <- lire("insee_bilan_azote_2020.csv") |>
  mutate(poste = factor(poste, levels = c("Apports", "Prélevé par les cultures", "Perdu dans l'air", "Surplus net laissé dans les sols")),
         origine = factor(origine, levels = rev(origine)))
apports <- sum(bil$mt_n[bil$poste == "Apports"])
note("Bilan azoté 2020 : apports %.1f Mt, prélevés %.1f Mt (%.0f %% des apports)", apports,
     bil$mt_n[bil$poste == "Prélevé par les cultures"], 100 * bil$mt_n[bil$poste == "Prélevé par les cultures"] / apports)

couleurs <- c("Engrais minéraux, boues et composts" = "#7A8CA3", "Déjections animales" = bleu, "Légumineuses" = vert,
              "Dépôts atmosphériques" = brume, "Prélevé par les cultures" = vert, "Volatilisation" = "#5FA8A0", "Surplus net" = bleu)
g12 <- ggplot(bil, aes(poste, mt_n, fill = origine)) +
  geom_col(width = 0.62, colour = "white", linewidth = 0.4) +
  geom_text(aes(label = ifelse(mt_n >= 0.25, paste0(sub(", ", ",\n", origine), "\n", format(mt_n, decimal.mark = ","), " Mt"), "")),
            position = position_stack(vjust = 0.5), family = "Calibri", colour = "white", size = 3.1, lineheight = 0.9) +
  geom_text(data = bil |> group_by(poste) |> summarise(t = sum(mt_n)), aes(poste, t, label = paste(format(t, decimal.mark = ","), "Mt")),
            inherit.aes = FALSE, vjust = -0.5, family = "Calibri", colour = encre, size = 4) +
  scale_fill_manual(values = couleurs) +
  scale_x_discrete(labels = function(x) gsub(" (laissé|dans|par)", "\n\\1", x)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Sur 4,2 millions de tonnes d'azote apportées aux sols,\nles cultures n'en prélèvent que 2,7",
       subtitle = "Bilan azoté des terres agricoles, France métropolitaine, 2020 (millions de tonnes d'azote)",
       x = NULL, y = NULL,
       caption = "Source : INSEE, Transformations de l'agriculture et des consommations alimentaires, édition 2024, fiche 4.7 (d'après le SDES). Les apports sont ventilés selon les parts publiées.") +
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
enregistrer(g12, "12_bilan_azote.png", l = 9, h = 5.4)

# ---------------------------------------------------------------------------
# 13. L'arbre dans les champs : surfaces comparées (échelle logarithmique, segments partant de 1 000 ha)
# ---------------------------------------------------------------------------
surf <- lire("surfaces_agroforesterie.csv") |> mutate(surface = factor(surface, levels = rev(surface)))
gc <- surf$ha[surf$groupe == "mono"]
note("Agroforesterie moderne dans les parcelles : %.2f %% des grandes cultures ; traditionnelle : %.1f %%",
     100 * 12500 / gc, 100 * 170000 / gc)

g13 <- ggplot(surf, aes(ha, surface)) +
  geom_segment(aes(x = 1000, xend = ha, yend = surface, colour = groupe), linewidth = 9) +
  geom_text(aes(label = paste(label_number(big.mark = " ")(ha), "ha")), hjust = -0.12, family = "Calibri", colour = encre, size = 3.8) +
  scale_colour_manual(values = c(total = "#7A8CA3", mono = bleu, passe = brume, mixte = vert, objectif = "#5FA8A0")) +
  scale_x_log10(labels = label_number(big.mark = " "), breaks = 10^(3:7), limits = c(1000, 3e8), expand = expansion(mult = c(0, 0))) +
  labs(title = "L'arbre dans les champs : 0,1 % des grandes cultures",
       subtitle = "Surfaces en France métropolitaine, en hectares (échelle logarithmique : chaque graduation vaut 10 fois la précédente)",
       x = NULL, y = NULL,
       caption = paste("Sources : Agreste, Mémento 2024 ; Eurostat apro_cpsh1 ; GRAB, guide du pré-verger, 2024 ; J.-J. Dubois, Pollution atmosphérique, 2016 ;",
                       "\nPlan national de développement de l'agroforesterie, 2015 (étude AGROOF). Pas de recensement officiel récent de l'agroforesterie.")) +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "#E3E8EE"))
enregistrer(g13, "13_surfaces_agroforesterie.png", l = 9.5, h = 4.6)

# ---------------------------------------------------------------------------
# 14-15. Est-ce finançable ? Un hectare de blé seul contre un hectare de blé et de noyers, sur 45 ans
# Flux annuels par hectare, en euros constants ; année 0 = plantation, année 45 = coupe des noyers.
# ---------------------------------------------------------------------------
h <- list(
  duree      = 45,     # coupe des noyers à croissance lente « vers cinquante ans » (Réussir, 27/10/2022) ; 45 retenu
  taux       = 0.04,   # taux d'actualisation central ; sensibilité à 2 % et 6 %
  charges    = 1600,   # charges du blé tendre, €/ha, 2025 (Arvalis, cité par Réussir)
  part_intr  = 0.31,   # part des intrants dans le coût de production du blé (SEMAE, 2026) : seuls ces frais disparaissent sous l'arbre
  produit    = c(normal = 1500, crise = 1200), # produit du blé, €/ha : 2025 = 1 200, « plus de 20 % sous la moyenne 2019-2023 » (Arvalis)
  aide_pac   = 280,    # aides PAC ≈ 40 €/t (Arvalis) × 7 t/ha ; versées en entier sous 100 arbres/ha (guide PAC 2024)
  plant      = 1750,   # plantation, € HT/ha : « 1 500 à 2 000 » (Réussir, 2022)
  aide_plant = 0.70,   # subvention de la plantation : « 50 à 80 % » (Réussir, 2022)
  entretien  = 25,     # entretien annuel, €/ha (évaluation ministérielle pour les haies, reprise faute de mieux)
  emprise    = 0.05,   # surface sous les arbres non cultivée : « 4 à 5 % » (Réussir, 2022)
  perte_fin  = 0.20,   # baisse du rendement intercalaire en fin de vie des arbres : « jusqu'à 20 % » (F. Liagre, Agroof)
  debut_perte= 15,     # « déclin progressif après quinze ans » (témoignage Réussir, 2022)
  bois_m3    = 40,     # bois d'œuvre : 50 arbres/ha, 20 % de pertes, 1 m3 par arbre (Réussir, 2022)
  plaq_m3    = 80,     # plaquettes à la coupe : « 80 m3 » (idem)
  plaq_an    = 22.5,   # plaquettes d'élagage dès la 20e année : « 20 à 25 m3/ha/an » (idem)
  plaq_net   = 7.5,    # 27,5 €/m3 vendus (« 25 à 30 ») moins 20 €/m3 de prestation de broyage (idem)
  prix_bois  = c(prudent = 300, central = 600, optimiste = 1000)) # « 1 000 €/m3 » = estimation optimiste (Terre-net, 2012)

flux <- function(produit, aides, prix_bois) {
  t <- 0:h$duree
  rdt <- ifelse(t <= h$debut_perte, 1, 1 - h$perte_fin * (t - h$debut_perte) / (h$duree - h$debut_perte))
  part <- (1 - h$emprise) * rdt                         # production de blé relative au blé seul
  aide <- if (aides) h$aide_pac else 0
  ble_seul <- ifelse(t == 0, 0, produit + aide - h$charges)
  agro <- ifelse(t == 0, 0, produit * part + aide - h$charges * (1 - h$part_intr * h$emprise)) -
    ifelse(t == 0, h$plant * (1 - if (aides) h$aide_plant else 0), h$entretien) +
    ifelse(t >= 20 & t < h$duree, h$plaq_an * h$plaq_net, 0) +
    ifelse(t == h$duree, h$bois_m3 * prix_bois + h$plaq_m3 * h$plaq_net, 0)
  data.frame(t, ble_seul, agro, ecart = agro - ble_seul)
}
van <- function(x, r = h$taux) sum(x / (1 + r)^(seq_along(x) - 1))
tri <- function(x) tryCatch(uniroot(function(r) van(x, r), c(-0.5, 0.5))$root, error = function(e) NA)
nom <- function(ble, aides) paste0("Blé ", ifelse(ble == "normal", "année normale", "année de crise"), ", ", ifelse(aides, "avec aides", "sans aides"))
couleurs <- c("Blé année normale, avec aides" = bleu, "Blé année normale, sans aides" = brume,
              "Blé année de crise, avec aides" = vert, "Blé année de crise, sans aides" = "#9FD3B5")

sc <- expand.grid(ble = names(h$produit), aides = c(TRUE, FALSE), bois = names(h$prix_bois), stringsAsFactors = FALSE) |>
  rowwise() |> mutate(
    f = list(flux(h$produit[[ble]], aides, h$prix_bois[[bois]])),
    van_ble = van(f$ble_seul), van_agro = van(f$agro), gain = van(f$ecart), tri = tri(f$ecart),
    besoin = -min(cumsum(f$ecart))) |> ungroup()
for (i in seq_len(nrow(sc))) with(sc[i, ], note(
  "Finançable ? blé %s, %s aides, bois %s : VAN blé seul %.0f, agroforesterie %.0f, gain %.0f €/ha ; TRI de l'écart %.1f %% ; creux de trésorerie %.0f €/ha",
  ble, ifelse(aides, "avec", "sans"), bois, van_ble, van_agro, gain, 100 * tri, besoin))
seuil <- function(produit, aides) uniroot(function(p) van(flux(produit, aides, p)$ecart), c(0, 5000))$root
for (b in names(h$produit)) for (a in c(TRUE, FALSE))
  note("Prix du bois d'équilibre (gain nul, 4 %%) : blé %s, %s aides : %.0f €/m3", b, ifelse(a, "avec", "sans"), seuil(h$produit[[b]], a))
for (r in c(0.02, 0.06)) note("Sensibilité au taux %.0f %% (blé normal, avec aides, bois 600) : gain %.0f €/ha",
                              100 * r, van(flux(1500, TRUE, 600)$ecart, r))

# 14. Trésorerie cumulée de l'écart (agroforesterie moins blé seul), bois au prix central
cum <- sc |> filter(bois == "central") |> mutate(scenario = nom(ble, aides)) |>
  select(scenario, f) |> unnest(f) |> group_by(scenario) |> mutate(cumul = cumsum(ecart)) |> ungroup()
g14 <- ggplot(cum, aes(t, cumul, colour = scenario)) +
  geom_hline(yintercept = 0, colour = "#9AA5B1") +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(values = couleurs) +
  scale_y_continuous(labels = label_number(big.mark = " ", suffix = " €")) +
  coord_cartesian(ylim = c(-5500, 2500)) +
  annotate("text", x = 44.3, y = 2200, hjust = 1, family = "Calibri", colour = encre, size = 3.6,
           label = sprintf("Coupe du bois, 45e année : la courbe remonte à +%s à +%s €",
                           label_number(big.mark = " ")(round(min(cum$cumul[cum$t == 45]), -2)),
                           label_number(big.mark = " ")(round(max(cum$cumul[cum$t == 45]), -2)))) +
  labs(title = "L'arbre coûte pendant 44 ans et paie la 45e",
       subtitle = "Trésorerie cumulée d'un hectare de blé et de noyers, comparée à un hectare de blé seul (€ constants, non actualisés, bois à 600 €/m3)",
       x = "Années après la plantation", y = NULL,
       caption = paste("Hypothèses : plantation 1 750 €/ha (aidée à 70 % dans les scénarios avec aides), 50 noyers/ha, 40 m3 de bois d'œuvre à 45 ans,",
                       "\nemprise 5 %, rendement du blé −20 % en fin de rotation, plaquettes dès 20 ans. Sources : Réussir, Arvalis, SEMAE, Terre-net (détail dans le README).")) +
  theme(legend.position = "top", legend.title = element_blank(), legend.justification = "left")
enregistrer(g14, "14_tresorerie_agroforesterie.png", l = 9.5, h = 5.4)

# 15. Gain actualisé selon le prix du bois
grille <- expand.grid(prix = seq(0, 1200, 25), ble = names(h$produit), aides = c(TRUE, FALSE), stringsAsFactors = FALSE) |>
  rowwise() |> mutate(gain = van(flux(h$produit[[ble]], aides, prix)$ecart)) |> ungroup() |>
  mutate(scenario = nom(ble, aides))
g15 <- ggplot(grille, aes(prix, gain, colour = scenario)) +
  geom_hline(yintercept = 0, colour = "#9AA5B1") +
  geom_vline(xintercept = unname(h$prix_bois), colour = "#C9D2DC", linetype = "dashed") +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(values = couleurs) +
  scale_x_continuous(labels = label_number(suffix = " €/m3", big.mark = " ")) +
  scale_y_continuous(labels = label_number(big.mark = " ", suffix = " €")) +
  labs(title = "Tout se joue sur le prix du bois dans 45 ans",
       subtitle = "Gain actualisé à 4 % d'un hectare agroforestier sur un hectare de blé seul, selon le prix du bois de noyer (€/ha)",
       x = "Prix du bois d'œuvre de noyer", y = NULL,
       caption = "Pointillés : prix prudent (300), central (600) et optimiste (1 000 €/m3, estimation citée par Terre-net, 2012). Mêmes hypothèses que le graphique précédent.") +
  theme(legend.position = "top", legend.title = element_blank(), legend.justification = "left")
enregistrer(g15, "15_gain_prix_bois.png", l = 9.5, h = 5.4)

close(res)
