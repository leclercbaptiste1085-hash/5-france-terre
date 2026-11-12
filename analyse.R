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
  labs(title = "En 1961, la France produisait ses engrais ;
elle importe aujourd'hui presque tout",
       subtitle = "Production nationale d'engrais rapportée à l'usage agricole (éléments nutritifs, moyennes sur 5 ans)",
       x = NULL, y = NULL, fill = NULL,
       caption = paste("Source : FAOSTAT, Inputs_FertilizersNutrient (N, P2O5, K2O). Bleu : 1961-1965 ; vert : 2020-2024.",
                       "
Les années intermédiaires ne sont pas tracées : l'usage agricole publié y est irrégulier.")) +
  theme(legend.position = "top", legend.justification = "left", legend.text = element_text(colour = "#4A5568"))
enregistrer(g8, "8_engrais.png")

close(res)
