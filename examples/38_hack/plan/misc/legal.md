Hier ist eine strukturierte deutsche Zusammenfassung der Dokumente sowie eine Übersicht der Punkte, die für deinen Hackathon **besonders heikel oder knifflig** sind.

---

### ⚠️ Was ist für deinen Hackathon besonders knifflig? (Die Fallstricke)

Auch wenn du deinen Code ohnehin Open Source auf GitHub veröffentlichst und kein Problem mit Modell-Training hast, gibt es einige wichtige Hürden und Risiken:

1. **Die Kostenfalle bei Guthaben-Ende (Auto-Billing):**
   * Um die API nutzen zu können, musst du zwingend ein **Zahlungsmittel (Kreditkarte) hinterlegen** (Abschnitt 1.2 der Credit Terms).
   * Sobald deine 150 $ Hackathon-Guthaben aufgebraucht sind, schaltet Meta **nicht** einfach ab, sondern bucht jede weitere Nutzung automatisch zu Standardpreisen von deiner Kreditkarte ab (Abschnitt 3.6). 
   * *Tipp:* Setze dir im Account unbedingt ein striktes Ausgabenlimit oder prüfe dein Token-Volumen regelmäßig.

2. **GitHub-Veröffentlichung & Open-Source-Lizenz (Vorsicht bei Meta-Materialien):**
   * Eigener Code ist kein Problem, aber: Du darfst **keine von Meta bereitgestellten „Downloadable Materials“** (z. B. Coding-Harness-Code, SDK-Binaries, interne Bibliotheken) in dein öffentliches GitHub-Repository einbinden oder weiterverteilen (Abschnitte 1, 10.1(xiv)).
   * Es ist untersagt, die Dienste so zu verwenden oder weiterzugeben, dass Meta-Materialien einer Open-Source-Lizenz unterworfen werden (Abschnitt 10.1(xv)). Achte darauf, im Repository nur deine Aufrufe / Wrapper zu veröffentlichen und die Meta-API-Bibliotheken als externe Abhängigkeit (Dependency) zu belassen.
   * **API-Keys niemals comitten:** Klingt banal, passiert bei Hackathons aber schnell. API-Keys sind strikt geheimzuhalten (Abschnitt 3.1).

3. **Kein Haftungsschutz (Indemnification) mit Promo-Credits:**
   * Meta bietet zwar einen IP-Freistellungsanspruch (Indemnity) bei Drittanbieter-Klagen, dieser ist jedoch bei der Nutzung von **Promotional Credits ausdrücklich ausgeschlossen** (Abschnitt 17.2). Wenn du nur Hackathon-Guthaben nutzt, bist du rechtlich komplett auf dich allein gestellt.

4. **Strenge Regeln für autonome KI-Agenten:**
   * Falls euer Projekt ein agentenbasiertes System ist (Tools aufrufen, Code ausführen, APIs steuern): Die AUP verbietet es, Agenten ohne Kontrollmechanismen laufen zu lassen. Insbesondere dürfen keine unumkehrbaren/destruktiven Aktionen (Dateien löschen, Verträge eingehen etc.) ohne ausdrückliche menschliche Freigabe (*Human-in-the-Loop*) autonom ausgeführt werden.

5. **Strikte Altersgrenze (18+):**
   * Weder du noch potenzielle Nutzer deines Projekts dürfen unter 18 Jahre alt sein. Euer Hackathon-Projekt darf sich **nicht** an Minderjährige richten (Abschnitt 10.1).

6. **Kennzeichnung & Provenienz:**
   * Metadaten, Wasserzeichen oder Kennzeichnungen von Meta-Outputs dürfen nicht entfernt werden. Du musst Endnutzern gegenüber transparent machen, dass es sich um KI-Inhalte handelt (AUP & Abschnitt 1). Du darfst KI-Outputs nicht als rein von Menschen erstellt ausgeben.

---

### 📋 Zusammenfassung der Dokumente

#### 1. Meta Model API – Terms of Service (API-Nutzungsbedingungen)
* **Zwei Servicestufen:**
  * *Contributor Services:* Meta nutzt deine Inputs & Outputs zur Weiterentwicklung und zum Training von Modellen (im Coding Harness standardmäßig aktiv).
  * *Standard Services:* Meta verarbeitet Daten nur als Auftragsverarbeiter („Processor“) und trainiert nicht damit.
* **Rechte an Inhalten:** Du behältst die Eigentumsrechte an deinen Inputs und Outputs. Du bist verantwortlich für deren Korrektheit und rechtliche Zulässigkeit.
* **Verwendungsverbote:**
  * Kein Trainieren, Fine-Tunen oder Distillieren von Modellen, die zu Meta-Modellen in Konkurrenz stehen.
  * Kein Reselling der API auf Standalone-Basis (z. B. kein eigener API-Gateway-/Proxy-Dienst für Dritte).
  * Kein Reverse-Engineering oder Extrahieren von Modellgewichten.
* **Haftungsbeschränkung:** Bereitstellung erfolgt „as-is“. Haftung von Meta ist (außer bei Freistellung/Vertraulichkeit) auf maximal 1.000 $ oder die Zahlungen der letzten 12 Monate beschränkt.
* **Kündigung & Monitoring:** Meta darf Nutzungsdaten überwachen und Accounts bei Richtlinienverstößen fristlos sperren.

#### 2. Acceptable Use Policy (AUP – Zulässige Nutzung)
* **Allgemeine Verbote:** Keine rechtswidrigen Inhalte, Hassrede, Belästigung, Erstellung von Malware/Cyber-Angriffswerkzeugen, keine Deepfakes oder Stimmenklone ohne Zustimmung.
* **Biometrie & Überwachung:** Verbot von Gesichtserkennung, Verhaltens-Tracking über Kameras, Emotionserkennung am Arbeitsplatz/Schulen oder Social Scoring.
* **Gefährliche Bereiche:** Keine Nutzung im Zusammenhang mit Militär, Waffen, Nuklearindustrie, kritischer Infrastruktur, Betäubungsmitteln oder medizinischer/rechtlicher Beratung ohne Lizenz.
* **Transparenz:** Verbot von Täuschung (Outputs dürfen nicht als menschengemacht dargestellt werden; Risiken müssen offengelegt werden).

#### 3. Hackathon Credit Terms (Guthaben-Bedingungen)
* **Umfang:** Einmalig **150 USD** an Promo-Guthaben pro Teilnehmer (nicht übertragbar, keine Barablöse).
* **Frist:** Der Claim muss bis spätestens **4. November 2026 um 21:00 Uhr Eastern Time** eingereicht werden.
* **Gültigkeit:** Das Guthaben verfällt nach Bewilligung nicht zeitlich, erlischt jedoch bei Kontolöschung oder Verstoß.
* **Kreditkarte zwingend:** Zur Account-Erstellung und API-Nutzung muss ein valides Zahlungsmittel hinterlegt sein.
* **Teambedingungen:** Jedes Teammitglied muss seinen eigenen Account und Claim nutzen; Zusammenlegen auf ein einzelnes Konto ist unzulässig.


Ganz einfach erklärt: Ein **IP-Freistellungsanspruch** (im Englischen *IP Indemnification*) ist eine Art **rechtlicher Schutzschirm oder eine Versicherung**, die dir ein Software-Anbieter gibt.

Hier ist die Aufschlüsselung:

---

### 1. Worum geht es bei den Begriffen?
* **IP** steht für *Intellectual Property* = **Geistiges Eigentum** (also Urheberrecht, Copyright, Patente, Marken).
* **Freistellung** (*Indemnification*) bedeutet: *„Wenn jemand von dir Geld fordert, springe ich für dich ein, zahle deine Anwälte und übernehme den Schaden.“*

---

### 2. Das Problem bei KI-Modellen (Das Szenario)
Große KI-Modelle wurden mit Milliarden Texten, Bildern und Code-Zeilen aus dem Internet trainiert. 

Angenommen, du lässt dir von der Meta-KI eine Funktion oder einen Algorithmus für dein Projekt generieren. Jetzt passiert Folgendes:
1. Die KI hat diesen Code fast 1:1 aus einem fremden, urheberrechtlich geschützten Projekt kopiert.
2. Der ursprüngliche Entwickler oder eine Firma sieht dein Projekt und sagt: *„Du hast meinen Code gestohlen und verletzt mein Urheberrecht! Ich verklage dich auf Schadensersatz.“*

---

### 3. Was der Anspruch normalerweise tut (für zahlende Kunden)
Viele große KI-Anbieter (wie Meta, Microsoft oder Google) sagen ihren regulär **zahlenden Kunden**:
> *„Keine Sorge: Wenn du wegen des KI-Outputs von einem Dritten verklagt wirst, weil wir das Modell trainiert haben, schicken wir unsere Anwälte, verteidigen dich vor Gericht und zahlen am Ende den Schaden oder Vergleich.“*

Das nennt man **IP-Freistellung**.

---

### 4. Warum das im Hackathon eine Rolle spielt (Der Haken)
In Metas AGB steht in Abschnitt 17.2 eine kleine Klammer: 
> *„...if you are using the Standard Services **(not through promotional credits)**...“*

Das bedeutet:
* Wenn du regulär echtes Geld an Meta bezahlst, greift dieser Schutzschirm.
* **Sobald du aber das kostenlose Hackathon-Guthaben (die 150 $ Promo-Credits) nutzt, gilt der Schutzschirm NICHT.**
* Sollte also jemals jemand behaupten, die KI hätte urheberrechtlich geschütztes Material ausgespuckt und du hättest es genutzt, ist Meta rechtlich fein raus. Meta stellt dir keine Anwälte und übernimmt keine Kosten – du müsstest dich selbst verteidigen.

### Fazit für die Praxis
Für ein kleines Hackathon-Projekt ist das Prozessrisiko im echten Leben zwar sehr gering, aber juristisch bist du bei der Nutzung von Promo-Guthaben **ohne Airbag** unterwegs.
