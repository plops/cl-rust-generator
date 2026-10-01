https://deepwiki.com/he-yufeng/RepoWiki

RepoWiki is an open-source tool designed to generate comprehensive, structured wiki documentation for any codebase using Large Language Models (LLMs). It serves as a local-first alternative to SaaS documentation platforms, offering a CLI-centric workflow that transforms raw source code into navigable documentation featuring module breakdowns, architecture diagrams, and importance-ranked reading guides


was sind die drei wichtigsten (oder komplexesten) algorithmen in diesem repo? erklaere wie sie die software zu dem machen was sie ist



Die drei Kernalgorithmen sind: **PageRank über dem Import-Graphen** (`DependencyGraph` in `graph.py`), der die "wichtigsten" Dateien bestimmt und die Lesereihenfolge sowie `repowiki map` speist; **TF-IDF-Retrieval mit Modulkarten-Boost** (`SimpleRAG`/`ModuleIndex` in `rag.py`), das `repowiki chat` ohne Embedding-Service ermöglicht; und die **Multi-Pass-LLM-Analysepipeline** (`Analyzer` in `analyzer.py`), die aus dem Code strukturierte Wiki-Seiten erzeugt und per Content-Hash-Cache inkrementell arbeitet. [1](#0-0) [2](#0-1) [3](#0-2) 

---

## 1. PageRank auf dem Abhängigkeitsgraphen

`DependencyGraph.build_from_project()` parst Importe über sprachspezifische Regex-Muster (`_IMPORT_PATTERNS` deckt Python, JS/TS, Go, Rust, Java ab) und baut einen `networkx.DiGraph`, in dem Kanten `datei -> importiert datei` bedeuten. [4](#0-3) [5](#0-4) 

Der eigentliche Algorithmus ist `_pagerank_power_iteration`: eine handimplementierte, deterministische Power-Iteration mit Damping-Faktor `alpha=0.85` und eigener Behandlung von "dangling nodes" (Knoten ohne Ausgangskanten verteilen ihren Anteil gleichmäßig). Bewusst nicht `networkx.pagerank`, weil das seit networkx 3.6 scipy benötigt — ein Trade-off für eine schlanke CLI-Installation. [6](#0-5) 

**Warum das die Software prägt:** Das Ranking ist das Rückgrat mehrerer Features — `rank_files()`/`get_core_files()` liefern die Top-20-Dateien, die in `Analyzer._generate_reading_guide()` als sortierte Eingabe für den Reading-Guide-Prompt dienen (Scan-Reihenfolge füllt nur den Rest auf). [7](#0-6) [8](#0-7)  Derselbe Graph liefert Einstiegspunkte (`in_degree <= 1`, `out_degree > 0`), isolierte Dateien (Dead Code) und Zyklen über strongly connected components — alles null LLM-Aufrufe. [9](#0-8)  Der `repowiki map`-Befehl ist im Kern genau dieser Algorithmus als Standalone-Ausgabe. [10](#0-9) 

## 2. TF-IDF-Retrieval mit Modulkarten-Boost

`SimpleRAG` zerlegt Dateien in Chunks, tokenisiert sie und speichert pro Chunk einen `Counter`-TF-Vektor; einmalig wird der IDF-Vektor über dem Korpus berechnet. `retrieve()` rankt per Kosinus-Ähnlichkeit — komplett ohne externe Abhängigkeiten oder Embedding-API. [11](#0-10) [12](#0-11) 

Der interessante Teil ist der **Zwei-Kanal-Retrieval**: `ModuleIndex` führt eine zweite TF-IDF-Suche über den LLM-generierten Modulbeschreibungen aus und liefert `file_scores()` als Boost-Werte. [13](#0-12)  In `retrieve()` wird `alpha * boost` zum Score addiert, aber das Sortier-Tupel `(1 if direct > 0, total, i)` garantiert, dass lexikalische Treffer immer über boost-only-Treffern stehen — die Karten füllen Lücken, verdrängen aber nie direkte Treffer. Das löst das klassische Problem, dass eine paraphrasierte (oder chinesische) Frage keinerlei Wortüberlappung mit dem Code hat, aber mit der natürlichsprachlichen Modulbeschreibung. [14](#0-13) [15](#0-14) 

Dazu kommt `index_incremental`: pro Datei werden Inhalts-Hashes verglichen, unveränderte Chunks samt TF-Vektoren werden wiederverwendet und nur IDF neu berechnet — macht Warm-Starts und kleine Edits billig. [16](#0-15) 

## 3. Multi-Pass-LLM-Pipeline mit Content-Hash-Caching

`Analyzer.analyze()` ist weniger ein einzelner Algorithmus als ein orchestrierter Ablauf: 4 strukturierte Pässe (Overview → Module → Architektur → Reading Guide), wobei spätere Pässe die Ergebnisse früherer nutzen (z.B. `overview.one_liner` in den Modul-Prompts, Modul-Zusammenfassungen im Guide-Prompt). Nebenläufigkeit über `asyncio.Semaphore`. [17](#0-16) 

Jeder Pass wird über einen Cache-Key adressiert, der **Modell, Sprache und Content-Hash der tatsächlichen Prompt-Eingaben** enthält — z.B. `:guide:{tree_hash}:{content_hash(rankings + module_summaries)}`, sodass eine reine Import-Änderung, die das PageRank-Ranking umsortiert, den gecachten Guide invalidiert. [18](#0-17) [19](#0-18)  Das ist das Fundament für "inkrementelle Re-Runs": ein Re-Scan nach kleinem Edit kostet null API-Calls für unveränderte Module. [20](#0-19) 

**Warum das die Software prägt:** Der ganze Wert von RepoWiki — strukturierte Wikis statt roher LLM-Dumps — kommt daher, dass jedes Ergebnis in ein Pydantic-Modell (`WikiData`, `ModuleDoc`, `ArchitectureDiagram`) geparst wird, mit defensiver Filterung auf `model_fields` und Fallback auf ein leeres Modell bei JSON-Parse-Fehlern. [21](#0-20) 

## Notes

- Ein vierter Kandidat wäre `python_skeleton()` in `src/repowiki/core/skeleton.py`: AST-basierte Extraktion von Signaturen/Docstrings für oversized Dateien statt stumpfer 4096-Zeichen-Truncation. Die Datei selbst war im Kontext nicht enthalten; beschrieben wird sie im README. [22](#0-21) 
- Ebenfalls relevant: `index_fingerprint` (SHA-256 über Root + Pfade + Inhaltshashes) für den on-disk RAG-Index-Cache. [23](#0-22) 
- `_resolve_import` (Auflösung relativer Importe, z.B. Python-Paket- und JS-Relative-Module) lag außerhalb des gezeigten Zeilenbereichs — wird aber zentral in `build_from_project` aufgerufen. [24](#0-23) 
