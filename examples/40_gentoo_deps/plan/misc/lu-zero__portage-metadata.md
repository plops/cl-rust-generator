lu-zero/portage-metadata

was sind die drei wichtigsten / kompliziertesten algorithmen in diesem
software? wie machen sie die software zu dem was sie ist? gib auch
eine kurze beschreibung der software.
 

## Kurzantwort

`portage-atom` ist eine Rust-Bibliothek zum Parsen und Vergleichen von Gentoo/Portage-Package-Atoms gemäß der Package Manager Specification (PMS 9), implementiert mit `winnow`-Parserkombinatoren. [1](#0-0)  Die drei komplexesten Algorithmen sind: **(1)** der PMS-Versionsvergleich (Algorithm 3.1) in `Ord for Version`, **(2)** der rekursive Dependency-String-Parser mit Lookahead-Disambiguierung in `parse_dep_entry`/`is_use_conditional`, und **(3)** die USE-Conditional-Auswertung `evaluate_use`, die den Abhängigkeitsbaum zur Laufzeit reduziert. [2](#0-1) [3](#0-2) [4](#0-3) 

---

## Was die Software ist

Die Crate stellt Typen (`Cpn`, `Cpv`, `Dep`, `DepEntry`, `Version`, `SlotDep`, `UseDep`) und Parser für Atom-Strings wie `>=dev-lang/rust-1.75.0:0[ssl]::gentoo` bereit. [5](#0-4) [6](#0-5) 

## Die drei Kernalgorithmen

### 1. PMS-Versionsvergleich — `Ord for Version` (Algorithm 3.1)

Der semantisch heikelste Teil: Versionen wie `1.2.3a_alpha4_rc7_p8-r9` müssen exakt nach PMS geordnet werden — numerische Komponenten links nach rechts (fehlende = 0), dann Buchstabe, dann Suffixe (wobei `_p` über der Basisversion sortiert, `_alpha`/`_beta`/`_pre`/`_rc` darunter), dann Revision. [7](#0-6)  Die Suffix-Ordnung selbst ist ein eigener Sub-Algorithmus über `SuffixKind::order()`. [8](#0-7)  Dazu kommt die nichttriviale `Eq`/`Hash`-Konsistenz: `1 == 1.0 == 1.0.0`, weil Trailing-Zeros normalisiert werden müssen, damit `Hash` zu `Ord` passt. [9](#0-8) [10](#0-9)  Dieser Algorithmus macht die Bibliothek überhaupt erst nützlich: Ohne korrekte Versionsordnung wären Operatoren wie `>=` oder `~` bedeutungslos.

### 2. Dependency-String-Parser — `parse_dep_entry` + `is_use_conditional`

Der Parser muss einen ebuild-`*DEPEND`-String in einen `DepEntry`-Baum zerlegen: Atome, `||`-/`^^`-/`??`-Gruppen, `flag? ( ... )`-Conditionals und `( ... )`-Gruppen, beliebig verschachtelt. [11](#0-10)  Die Komplexität steckt in der Disambiguierung: `foo? (` ist ein USE-Conditional, aber `cat/foo` ein Atom — `is_use_conditional` löst das per Lookahead auf das `? (`-Diskriminant mit Early-Exit bei `/`, `:`, `[`. [12](#0-11)  `parse_dep_entry` dispatcht dann auf dem ersten Zeichen, und `cut_err` erzwingt harte Fehler statt Backtracking — laut Changelog ~27–35 % schneller durch Backtracking-Eliminierung. [3](#0-2) [13](#0-12)  Darunter orchestriert `parse_dep` die Teilparser (Blocker, Operator, CPV/CPN, Slot, USE-Deps, Repo) und validiert PMS-Regeln wie „Glob `*` nur mit `=`". [14](#0-13) 

### 3. USE-Conditional-Auswertung — `DepEntry::evaluate_use`

Nach dem Parsen ist der Baum noch unaufgelöst; `evaluate_use` reduziert ihn rekursiv gegen eine Menge aktiver USE-Flags: aktive `flag?`-Knoten werden durch ihre Kinder ersetzt, inaktive (bzw. `!flag?` bei aktivem Flag) fallen weg, leere Gruppen werden entfernt. [15](#0-14)  Das ist der Schritt, der aus statischer Syntax eine konkrete, installierbare Abhängigkeitsliste macht — also die Brücke zwischen Parsing und echter Paketverwaltungs-Semantik.

## Notes

- `parse_slot_dep` (Slot/Subslot/`:=`/`:*`) und die `Version`-Parser sind ebenfalls nichttrivial, aber klar den drei oben genannten untergeordnet. [16](#0-15) [17](#0-16) 
- Das README warnt explizit, dass der Code größtenteils KI-generiert und noch nicht vollständig auditiert ist. [18](#0-17) 

Wiki pages you might want to explore:
- [Dependency Atoms (Dep) (lu-zero/portage-atom)](/wiki/lu-zero/portage-atom#2.3)
- [Glossary (lu-zero/portage-atom)](/wiki/lu-zero/portage-atom#6)



 
lu-zero/portage-metadata

## Kurzantwort

`portage-metadata` ist eine Rust-Bibliothek zum Lesen und Schreiben von Gentoo-`md5-cache`-Metadatendateien (`KEY=VALUE`-Format) gemäß PMS 9 [1](#0-0) . Die drei wichtigsten/komplexesten Algorithmen sind:

1. **Die rekursiven Winnow-Ausdrucksparser** für `REQUIRED_USE`, `LICENSE`, `RESTRICT`/`PROPERTIES` und `SRC_URI` — sie wandeln verschachtelte Gentoo-Ausdrucksgrammatiken in typisierte ASTs um.
2. **Die zweistufige Cache-Parsing-Pipeline** (`RawCacheEntry` → `ParseState` → `CacheEntry`) — sie macht aus rohem Text validierte, stark typisierte Metadaten.
3. **Die rekursive Deduplizierung** (`EbuildMetadata::dedup` u. a.) — sie normalisiert doppelte Einträge, die durch Eclass-Vererbung entstehen.

---

## 1. Ausdrucksparser mit Winnow-Kombinatoren

Das Herzstück: `parse_required_use_entry` dispatcht per `peek` auf das nächste Zeichen zu den Gruppen-Parsern `||`, `^^`, `??`, Klammergruppen oder USE-Konditionalen/Flags [2](#0-1) . `parse_required_use_entries` wiederholt dies rekursiv und flacht die Ergebnisse in einen `Vec<RequiredUseExpr>` ab [3](#0-2) .

Dasselbe Muster treibt `LicenseExpr` [4](#0-3) , `RestrictExpr` [5](#0-4)  und `SrcUriEntry` an — letzterer mit Extras wie `fetch+`/`mirror+`-Präfixen und `->`-Umbenennungen [6](#0-5) . `cut_err` sorgt dafür, dass einmal begonnene Gruppen bei Fehlern hart abbrechen statt still zurückzufallen [7](#0-6) . Diese Parser machen die Bibliothek zu mehr als einem Zeilenleser: Sie verstehen die PMS-Grammatik (PMS 8.2) semantisch.

## 2. Cache-Parsing-Pipeline

`RawCacheEntry` ist eine zero-copy-Sicht, die nur `KEY=VALUE`-Zeilen liefert [8](#0-7) . `ParseState::feed` sammelt die Felder als `&str`-Slices [9](#0-8) , und `finish` ruft dann alle Feldparser auf — `RestrictExpr::parse`, `parse_dep_field`, `Phase::parse_line`, `parse_slot` usw. — und baut `EbuildMetadata` [10](#0-9) . `CacheEntry::parse_impl` verbindet beide Stufen [11](#0-10) . Das ist der Einstiegspunkt, der die Software zu dem macht, was sie ist: ein PMS-konformer md5-cache-Deserializer.

## 3. Rekursive Deduplizierung

`EbuildMetadata::dedup` entfernt Duplikate in allen `*DEPEND`-Bäumen sowie in `required_use` und `license` [12](#0-11) . `dedup_dep` arbeitet rekursiv durch `UseConditional`/`AllOf`/`AnyOf`-Kinder [13](#0-12) ; analog normalisieren `RequiredUseExpr::dedup` [14](#0-13)  und `LicenseExpr::dedup` [15](#0-14)  ihre Bäume. Hintergrund: Eclasses hängen Werte doppelt an — ohne Dedup wären die geparsten Metadaten nicht mit pkgcraft/Portage-Ausgaben vergleichbar.

## Notes

- Die eigentliche Dependency-Grammatik (`DEPEND` etc.) liegt im externen Crate `portage-atom` (`DepEntry`), nicht in diesem Repo [16](#0-15) .
- Warnung im README: Der Code ist größtenteils KI-generiert und nicht vollständig auditiert [17](#0-16) .

Wiki pages you might want to explore:
- [Overview (lu-zero/portage-metadata)](/wiki/lu-zero/portage-metadata#1)
- [Dependency-style Expression Parsers (lu-zero/portage-metadata)](/wiki/lu-zero/portage-metadata#3.1)
- [Glossary (lu-zero/portage-metadata)](/wiki/lu-zero/portage-metadata#5)

