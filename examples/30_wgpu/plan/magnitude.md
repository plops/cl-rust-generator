

https://github.com/magnitudedev/magnitude
Magnitude is an AI coding agent platform engineered from the ground up to optimize the performance of open models. It aims to match the capabilities of leading closed-model agents while offering significant cost advantages through pass-through pricing and specialized multi-agent orchestration.


was sind die drei wichtigsten (oder komplexesten) algorithmen in
diesem repo? erklaere wie sie die software zu dem machen was sie ist

## Kurzantwort

Die drei komplexesten "Algorithmen" in diesem Repo sind weniger klassische Datenstruktur-Algorithmen als vielmehr drei Kernmechanismen, die Magnitude zu einem Multi-Agenten-Coding-System machen:

1. **Das Event-Sourcing-Modell** (Projections + Workers über einem `EventBusCore`) — der fundamentale Zustandsalgorithmus des ganzen Systems. [1](#0-0) 
2. **Der Fork-/Delegations-Algorithmus** im `ExecutionManager` — verwaltet den Lebenszyklus isolierter Sub-Agenten (`init`, `dispose`, `fork`, Layer-Caching). [2](#0-1) 
3. **Der Compaction-Algorithmus** — eine Zustandsmaschine, die Kontextfenster-Token schätzt und entscheidet, wann die Konversation komprimiert werden muss. [3](#0-2) 

---

## Details

### 1. Event-Sourcing: State durch Events statt Mutation

Strikt getrennt: **Projections** sind reaktive, read-only Zustandscontainer, die aus dem Event-Log hydriert werden; **Workers** führen Seiteneffekte aus (LLM-Calls, Shell-Befehle) und dürfen Zustand nur über neue Events ändern. Der `ExecutionManager` importiert `Projection`, `Fork` und `WorkerBusTag` aus `@magnitudedev/event-core` und verkabelt u. a. `ConversationProjection`, `AgentStatusProjection`, `TurnProjection`, `TaskGraphProjection` und `SessionContextProjection`. [4](#0-3) 

Das macht die Software *replayfähig, testbar und mehragentenfähig* — jeder Agent ist nur eine Sicht auf denselben Event-Strom.

### 2. Fork-Lifecycle: Multi-Agenten als Laufzeitkonstrukt

Der `ExecutionManager` besitzt den pro-Fork-Lebenszyklus. Forks sind isolierte Ausführungskontexte mit eigenem Memory und eigener `forkId`, über die Leader-Rollen Engineer-, Scout- oder Architect-Subagenten spawnen. [2](#0-1) 

Der Leader-Prompt kodifiziert die dazugehörige Strategie: Kontextsammlung an Scouts delegieren, Token-Effizienz durch Batching und Delegation. [5](#0-4) 

### 3. Compaction: Token-basierte Kontextfenster-Verwaltung

`recomputePolicy` berechnet aus einer Token-Schätzung und `RoleConfig`-Limits die Flags `shouldCompact` und `contextLimitBlocked`; während einer laufenden Compaction wird `contextLimitBlocked` konserviert, damit `compaction_failed` die Retry-Intention erkennt — ein differenzierter Zustandsautomat (`idle` vs. aktive Compaction). [3](#0-2) 

Ohne diesen Algorithmus würden Agenten bei langen Sessions hart am Kontextlimit scheitern; er macht langlebige autonome Arbeit erst möglich.

## Notes

- **Codec/Driver-System**: Es gibt eine Spec für einen Encode→Driver→Decode-Turn-Loop (`TurnEngine`, `Codec`, `Driver`, `ModelAdapter`), der Wire-Format-Unterschiede (native/completions/xml-act) hinter einem `Stream<TurnPartEvent>` abstrahiert — als Draft allerdings noch Spezifikation, nicht Implementierung. [6](#0-5) 
- **`util/edit.ts`**: Der einzige klassische String-Algorithmus — exaktes Find-Replace mit `countOccurrences` zur Mehrfachtreffer-Erkennung; funktional wichtig für Datei-Edits, aber deutlich einfacher als die drei oben. [7](#0-6) 
- Einordnung basiert auf den gezeigten Snippets und dem Glossar; die vollständigen Implementierungen (z. B. `EventBusCore`, `Harness`, `Cortex`) waren im Kontext nur referenziert, nicht eingebettet.
