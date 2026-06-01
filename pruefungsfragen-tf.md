# Mündliche Programmier-Prüfung – Bezug zum `tf`-Paket

> **Hinweis für die Prüferin / den Prüfer:**
> Diese Fragensammlung dient als improvisierte mündliche Prüfung, um einzuschätzen,
> ob die/der Studierende über ausreichende Programmier- und Software-Engineering-Kenntnisse
> verfügt, um die Bachelorarbeit am [`tf`-Paket](https://github.com/tidyfun/tf) zu beginnen.
> Die Fragen sind nach Schwierigkeit und Themengebiet gegliedert. Zu jeder Frage gibt es
> stichpunktartige *Erwartete Antworten* als Orientierung – diese müssen nicht wörtlich
> getroffen werden. Ziel ist, das **Verständnis** und die **Denkweise** zu prüfen, nicht
> auswendig gelerntes Detailwissen.
>
> Empfehlung: Mit den Grundlagen (Teil 1–2) beginnen, dann anhand der Code-Snippets
> (Teil 5) ins Gespräch kommen. 30–45 Minuten sind realistisch.

---

## Teil 1 – Grundlagen R & Programmierung

1. **Was ist der Unterschied zwischen einem `vector`, einer `list` und einem `data.frame` in R?**
   *Erwartet:* Vektor = homogen (ein Typ), Liste = heterogen (beliebige Elemente, auch verschachtelt),
   data.frame = Liste gleich langer Spalten/Vektoren. Bezug zu `tf`: ein `tf`-Objekt ist „im Kern“
   eine Liste, verhält sich aber wie ein Vektor von Funktionen.

2. **Was bedeutet „vektorisiertes" Rechnen in R, und warum ist es schneller als eine `for`-Schleife?**
   *Erwartet:* Operationen auf ganzen Vektoren statt elementweise in R-Schleife; Schleife läuft
   intern in C; weniger Overhead. Wichtig im Paket bei den Rechen-/Statistik-Operationen.

3. **Was ist der Unterschied zwischen `<-` und `=`? Und was macht der Pipe-Operator `|>`?**
   *Erwartet:* `<-` Zuweisung (idiomatisch), `=` u. a. für Argumente; `|>` reicht das Ergebnis
   links als erstes Argument der Funktion rechts weiter (siehe Snippet C).

4. **Was sind `NA`, `NULL`, `NaN` und `Inf`? Wo liegt jeweils der Unterschied?**
   *Erwartet:* `NA` = fehlender Wert (typisiert), `NULL` = „nichts"/leeres Objekt der Länge 0,
   `NaN` = ungültiges Rechenergebnis, `Inf` = Unendlich. Im Paket werden leere Funktionen z. B.
   als `NA`-Einträge behandelt.

5. **Was bedeutet der `%||%`-Operator (`a %||% b`)?**
   *Erwartet:* „null-coalescing": gib `a` zurück, falls nicht `NULL`, sonst `b`. Wird im Paket
   für Default-Werte verwendet, z. B. `domain <- domain %||% range(data$arg)`.

---

## Teil 2 – Objektorientierung in R (S3, vctrs)

6. **R kennt mehrere OOP-Systeme (S3, S4, R5/RC, R6). Was ist das Besondere an S3?**
   *Erwartet:* S3 = leichtgewichtig, basiert auf einem `class`-Attribut und Methoden-Dispatch
   über Namenskonvention `generic.class`. Keine formale Klassendefinition nötig.

7. **Wie funktioniert Methoden-Dispatch bei S3? Was passiert bei `print(x)`, wenn `x` die Klasse
   `c("tfd_reg", "tfd", "tf")` hat?**
   *Erwartet:* R sucht der Reihe nach `print.tfd_reg`, `print.tfd`, `print.tf`, dann `print.default`.
   Klassen-Vektor = Vererbungshierarchie von speziell nach allgemein.

8. **Im Paket steht in der `CLAUDE.md`: „Use `ClassName.method` pattern for S3 methods" und
   „Follow vctrs framework". Was leistet das `vctrs`-Paket gegenüber „nacktem" S3?**
   *Erwartet:* vctrs definiert klare Regeln für Größe (`vec_size`), Prototypen (`ptype`),
   Typ-Kombination (`vec_ptype2`) und Casting (`vec_cast`); sorgt für konsistentes Verhalten
   bei `c()`, Subsetting, in data.frames etc. → daher die Dateien `vctrs-cast.R`, `vctrs-ptype2.R`.

9. **Warum kann man ein `tf`-Objekt als Spalte in einen `data.frame` packen, eine gewöhnliche
   Liste von Funktionen aber nicht so einfach?**
   *Erwartet:* Weil `tf` über vctrs als „echter" Vektor mit definierter Länge/Subsetting auftritt;
   das ist gerade der Kernnutzen des Pakets (Funktionen neben Skalaren/Faktoren in einem data.frame).

---

## Teil 3 – Fachliche / paket-spezifische Konzepte

10. **Das Paket hat zwei Hauptklassen: `tfd` und `tfb`. Was ist der konzeptionelle Unterschied?**
    *Erwartet:* `tfd` = *tidy functional data*, Rohdaten auf einem Gitter (arg/value-Paare);
    `tfb` = *tidy functional basis*, Darstellung in einer Basis (z. B. Splines oder FPC),
    also über Koeffizienten + Basis. Trade-off: Speicher/Glättung vs. Rohtreue.

11. **Was bedeuten die Begriffe `arg`, `value`, `domain` und `evaluator` bei einem `tfd`-Objekt?**
    *Erwartet:* `arg` = Argument-/Stützstellen (x-Werte, „Zeitpunkte"), `value` = Funktionswerte,
    `domain` = Definitionsbereich (Intervallgrenzen), `evaluator` = Funktion, die zwischen den
    Stützstellen aus-/interpoliert.

12. **Was ist der Unterschied zwischen einem „regular" und einem „irregular" `tfd`
    (Klassen `tfd_reg` vs. `tfd_irreg`)?**
    *Erwartet:* regular = alle Funktionen auf demselben gemeinsamen `arg`-Gitter; irregular =
    jede Funktion kann eigene/unterschiedliche Stützstellen haben (z. B. wegen `NA`s).

13. **Warum braucht man bei funktionalen Daten überhaupt einen „evaluator"/Interpolation –
    anders als bei einem normalen Zahlenvektor?**
    *Erwartet:* Eine Funktion ist konzeptionell auf dem ganzen `domain` definiert, gespeichert
    sind aber nur endlich viele Stützstellen → Werte dazwischen müssen interpoliert werden.

---

## Teil 4 – Funktionale Programmierung (`purrr`)

14. **Das Paket nutzt `purrr::map()`, `map2()`, `map_lgl()` etc. Was macht `map()` und was ist der
    Vorteil gegenüber einer `for`-Schleife?**
    *Erwartet:* `map` wendet eine Funktion auf jedes Listenelement an und gibt eine Liste zurück;
    deklarativer, keine manuelle Index-/Akkumulator-Verwaltung, typsichere Varianten (`map_lgl`
    → logischer Vektor, `map_dbl` → double …).

15. **Was bedeutet die anonyme Funktion `\(x) x[y]` bzw. `\(.x) max(.x$value)`?**
    *Erwartet:* `\(...)` ist Kurzschreibweise für `function(...)` (ab R 4.1). Lambda/anonyme Funktion.

16. **In `tf_fwise` steht `f_map <- as_mapper(.f, ...)`. Was leistet `as_mapper` und warum erlaubt
    das Paket Formeln wie `~ max(.x$value)`?**
    *Erwartet:* `as_mapper` wandelt Funktionen *oder* Formeln in eine aufrufbare Funktion um;
    ergibt bequeme, kurze Syntax für die Nutzer:innen.

---

## Teil 5 – Code-Snippets erklären (Kern der Prüfung)

> Hier jeweils das Snippet zeigen und um Erklärung / „lautes Mitdenken" bitten.

### Snippet A – Konstruktor & Default-Werte (`R/tfb-spline.R`)

```r
new_tfb_spline <- function(data, domain = NULL, arg = NULL,
                           penalized = TRUE, global = FALSE,
                           verbose = FALSE, ...) {
  if (vec_size(data) == 0) {
    ret <- new_vctr(
      data,
      domain = numeric(2),
      arg = numeric(),
      family = character(),
      class = c("tfb_spline", "tfb", "tf")
    )
    return(ret)
  }

  domain <- domain %||% range(data$arg)
  ...
}
```

**Fragen:**
- Was passiert in der `if (vec_size(data) == 0)`-Verzweigung und warum prüft man diesen Fall
  ganz am Anfang? *(Erwartet: Sonderfall „leere Eingabe" → leeres Prototyp-Objekt; „early return"
  / Guard-Clause, vermeidet Fehler im Rest.)*
- Erklären Sie `domain <- domain %||% range(data$arg)`. *(Default = Wertebereich der Argumente.)*
- Was bewirkt der `...`-Parameter in der Argumentliste? *(Weiterreichen zusätzlicher Argumente,
  hier später z. B. an `s()`/`gam()`.)*
- Wofür steht der Klassenvektor `c("tfb_spline", "tfb", "tf")`? *(S3-Vererbung, siehe Frage 7.)*

### Snippet B – Subsetting-Operator (`R/brackets.R`)

```r
`[.tf` <- function(x, i, j, interpolate = TRUE, matrix = TRUE) {
  if (!interpolate && is_tfb(x)) {
    interpolate <- TRUE
    cli::cli_inform(
      "{.arg interpolate} ignored for data in basis representation."
    )
  }
  # decompose matrix i into separate i (row indices) and j (arg values)
  matrix_i <- FALSE
  if (!missing(i) && is.matrix(i)) {
    if (ncol(i) != 2) {
      cli::cli_abort("Matrix {.arg i} must have exactly 2 columns.")
    }
    ...
  }
  ...
}
```

**Fragen:**
- Was bedeutet `` `[.tf` `` als Funktionsname? Welche generische Funktion wird hier überschrieben?
  *(Eigene Methode für den `[`-Operator für Klasse `tf`; Backticks, weil `[` ein Sonderzeichen ist.)*
- Was prüft `!missing(i)`? Warum kann man hier nicht einfach `is.null(i)` schreiben?
  *(`missing()` testet, ob das Argument beim Aufruf überhaupt angegeben wurde.)*
- Warum wird für `tfb` `interpolate` zwangsweise auf `TRUE` gesetzt? *(Basisdarstellung ist
  ohnehin „kontinuierlich"/immer auswertbar; siehe Frage 10/13.)*
- Wozu dienen `cli::cli_inform` / `cli::cli_abort` statt `message`/`stop`?
  *(Einheitliche, formatierte Meldungen; Projektkonvention laut `CLAUDE.md`.)*

### Snippet C – Pipe + funktionale Verarbeitung (`R/fwise.R`)

```r
tf_fwise <- function(x, .f, arg = tf_arg(x), ...) {
  assert_tf(x)
  assert_arg(arg = arg, x = x)
  x_ <- x[, arg, matrix = FALSE]
  f_map <- as_mapper(.f, ...)
  ret <- map(x_, f_map)
  setNames(ret, names(x))
}

tf_fmax <- function(x, arg = tf_arg(x), na.rm = FALSE) {
  x |>
    tf_fwise(\(.x) max(.x$value, na.rm = na.rm), arg = arg) |>
    unlist(use.names = FALSE) |>
    setNames(names(x))
}
```

**Fragen:**
- Lesen Sie die `tf_fmax`-Pipe vor: Was passiert Schritt für Schritt?
  *(Funktion → fwise-Max je Kurve → Liste zu Vektor flach machen → Namen setzen.)*
- Wozu dienen `assert_tf(x)` und `assert_arg(...)` ganz am Anfang? *(Defensive Programmierung /
  Input-Validierung; frühe, klare Fehlermeldungen.)*
- Wie hängen `tf_fmax` und `tf_fwise` zusammen? *(`tf_fmax` ist Spezialfall, der `tf_fwise`
  mit einer konkreten Lambda nutzt → Code-Wiederverwendung.)*
- Was bewirkt `unlist(use.names = FALSE)` und warum danach trotzdem `setNames(names(x))`?

### Snippet D – aussagekräftige Fehler/Warnungen (`R/tfd-class.R`)

```r
u_args <- unlist(arg, use.names = FALSE)
if (domain[1] > min(u_args) || max(u_args) > domain[2]) {
  cli::cli_abort("Evaluations must be inside the domain.")
}
```

**Fragen:**
- Was wird hier inhaltlich geprüft, und warum ist diese Prüfung sinnvoll?
  *(Alle Stützstellen müssen innerhalb des Definitionsbereichs liegen.)*
- Erklären Sie die Bedingung mit `||`. Was ist der Unterschied zwischen `||` und `|` in R?
  *(`||` = skalar, kurzschließend; `|` = elementweise auf Vektoren.)*

### Snippet E – Test mit `testthat` (`tests/testthat/test-tfd-class.R`)

```r
test_that("tfd.numeric works", {
  set.seed(1234)
  x <- runif(100)
  f <- tfd(x)
  expect_s3_class(f, "tfd_reg")
  expect_length(f, 1)
  expect_identical(attr(f, "arg"), list(1:100))
  expect_identical(attr(f, "domain"), c(1L, 100L))
})
```

**Fragen:**
- Was ist der Zweck eines Unit-Tests? Was prüft dieser konkret?
- Warum steht hier `set.seed(1234)`? *(Reproduzierbarkeit bei Zufallszahlen.)*
- Was bedeuten `expect_s3_class`, `expect_length`, `expect_identical`? Worin unterscheiden sich
  `expect_identical` und `expect_equal`? *(identical = strikt inkl. Typ; equal = mit Toleranz.)*
- Wenn Sie ein neues Feature einbauen würden – würden Sie zuerst den Test oder den Code schreiben?
  Begründen Sie. *(Offene Frage → TDD-Verständnis.)*

---

## Teil 6 – Paket-Infrastruktur & Software Engineering

17. **Wozu dienen die Dateien `DESCRIPTION` und `NAMESPACE` in einem R-Paket?**
    *Erwartet:* `DESCRIPTION` = Metadaten, Version, Autoren, Abhängigkeiten (`Imports`/`Suggests`/`Depends`);
    `NAMESPACE` = welche Funktionen exportiert (`export`) bzw. importiert werden – steuert Sichtbarkeit.

18. **Was ist der Unterschied zwischen `Imports`, `Suggests` und `Depends` im `DESCRIPTION`?**
    *Erwartet:* `Imports` = nötig, wird installiert aber nicht angehängt; `Depends` = wird mit
    angehängt (sparsam nutzen); `Suggests` = optional (z. B. für Tests/Vignetten wie `testthat`, `fda`).

19. **Im Paket stehen über Funktionen Kommentare mit `#'` und `@param`, `@return`, `@export`.
    Was ist das und was passiert damit?**
    *Erwartet:* roxygen2-Dokumentation; `devtools::document()` erzeugt daraus die `.Rd`-Hilfeseiten
    in `man/` und aktualisiert `NAMESPACE`.

20. **Welche Schritte würden Sie ausführen, um eine Änderung am Paket lokal zu testen?**
    *Erwartet (laut `CLAUDE.md`):* `devtools::load_all()` (neu laden), `devtools::test()` (Tests),
    `devtools::document()` (Doku), evtl. `devtools::check()` (voller Check).

21. **Was bedeutet die Namenskonvention im Paket: exportierte Funktionen heißen `tf_…` und
    sind in `snake_case`? Warum sind solche Konventionen wichtig?**
    *Erwartet:* Einheitlichkeit, Lesbarkeit, Auffindbarkeit (alle `tf_`-Funktionen via Autocomplete),
    Vermeidung von Namenskollisionen.

---

## Teil 7 – Git & Versionskontrolle

22. **Wozu dient Git, und was ist der Unterschied zwischen Git und GitHub?**
    *Erwartet:* Git = verteiltes Versionskontrollsystem (lokal); GitHub = Hosting-Plattform/Remote
    + Zusammenarbeit (Issues, PRs).

23. **Erklären Sie den typischen Ablauf: `git add` → `git commit` → `git push`. Was passiert in
    jedem Schritt?**
    *Erwartet:* `add` = Änderungen in die Staging-Area; `commit` = Snapshot lokal mit Nachricht;
    `push` = lokale Commits zum Remote (z. B. GitHub) hochladen.

24. **Was ist ein Branch und warum arbeitet man nicht direkt auf `main`?**
    *Erwartet:* unabhängige Entwicklungslinie; schützt stabilen `main`-Stand; ermöglicht parallele
    Arbeit und Review vor dem Mergen. (Laut `CLAUDE.md`: PRs gegen `main`.)

25. **Was ist ein Pull Request (PR) und wozu dient ein Code-Review?**
    *Erwartet:* Antrag, einen Branch in einen anderen zu mergen; ermöglicht Review/Diskussion/CI
    vor der Integration; Qualitätssicherung.

26. **Was bedeutet ein „Merge-Konflikt" und wie geht man damit um?**
    *Erwartet:* zwei Änderungen an derselben Stelle; Git kann nicht automatisch zusammenführen →
    manuell auflösen (Konfliktmarker `<<<<`/`====`/`>>>>` editieren), dann committen.

27. **Was würden Sie in einer guten Commit-Nachricht schreiben? (Vgl. echte Historie unten.)**
    *Erwartet:* kurze, präzise Zusammenfassung im Imperativ; ggf. Bezug zum Issue; *warum*, nicht
    nur *was*.

    Beispiele aus der echten Git-Historie des Pakets:
    ```
    perf(depth): vectorize fsd() with Gram-matrix pairwise norms
    Fix checkmate imports for rlang export conflicts
    Bump version to 0.4.1
    ```
    *Frage:* Was bedeutet vermutlich das Präfix `perf(depth):`? *(Conventional-Commits-Stil:
    Art der Änderung + betroffener Bereich.)*

28. **Was ist „Continuous Integration" (CI)? Im Repo gibt es einen `R-CMD-check`-Workflow.**
    *Erwartet:* automatisches Bauen/Testen bei jedem Push/PR; fängt Fehler früh; Badge im README
    zeigt Status.

---

## Teil 8 – Offene / weiterführende Fragen (Motivation & Denkweise)

29. **Sie sollen in `tf` eine neue Funktion `tf_fmedian()` (Median je Funktion) ergänzen.
    Wie gingen Sie vor – an welchem bestehenden Code würden Sie sich orientieren?**
    *Erwartet:* an `tf_fmax`/`tf_fmin` orientieren (`tf_fwise` + Lambda), Doku via roxygen,
    Test schreiben, `load_all()`/`test()`, Branch + PR. → prüft Transfer.

30. **Wo würden Sie nachschauen, wenn Sie nicht wissen, was eine Funktion tut –
    in der Hilfe, im Quellcode, in den Tests?** *(Offen; gut ist „alle drei", v. a. Tests als
    ausführbare Beispiele.)*

31. **Welche Programmiersprachen/Werkzeuge kennen Sie bereits, und wie schätzen Sie Ihren
    eigenen Kenntnisstand in R ein?** *(Selbsteinschätzung, Gesprächseinstieg/-abschluss.)*

---

### Kurz-Bewertungsraster (optional)

| Bereich | schwach | solide | stark |
|---|---|---|---|
| R-Grundlagen (Teil 1) | | | |
| S3 / vctrs (Teil 2) | | | |
| Fachkonzepte tf (Teil 3) | | | |
| Funktionale Prog. (Teil 4) | | | |
| Code lesen/erklären (Teil 5) | | | |
| Paket-Infrastruktur (Teil 6) | | | |
| Git (Teil 7) | | | |
| Transfer/Denkweise (Teil 8) | | | |
