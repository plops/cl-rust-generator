//! `02_lang` — Sprachtabelle: Schrift-Bereiche, Modell, Richtung, Pangramme.
//!
//! `in_script` entscheidet, welche Zeichen zu einer Sprache gehören; der
//! endgültige Zeichensatz schneidet das zusätzlich mit Modell-Wörterbuch und
//! Schrift (`03_corpus::Charset`).

/// Universelles Erkennungsmodell (PP-OCRv6 small, Latein + CJK + Griechisch).
pub const UNIVERSAL: &str = "PP-OCRv6_small_rec_onnx";

const LATIN: &[(char, char)] = &[
    ('A', 'Z'),
    ('a', 'z'),
    ('\u{C0}', '\u{D6}'),
    ('\u{D8}', '\u{F6}'),
    ('\u{F8}', '\u{17F}'),
    ('\u{1E9E}', '\u{1E9E}'),
];
const CYRILLIC: &[(char, char)] = &[('\u{400}', '\u{4FF}')];
const GREEK: &[(char, char)] = &[('\u{370}', '\u{3FF}')];
const JAPANESE: &[(char, char)] = &[
    ('\u{3041}', '\u{309F}'),
    ('\u{30A0}', '\u{30FF}'),
    ('\u{4E00}', '\u{9FFF}'),
];
const HAN: &[(char, char)] = &[('\u{4E00}', '\u{9FFF}')];
const HANGUL: &[(char, char)] = &[('\u{AC00}', '\u{D7A3}')];
const THAI: &[(char, char)] = &[('\u{E01}', '\u{E5B}')];
const ARABIC: &[(char, char)] = &[('\u{621}', '\u{64A}'), ('\u{660}', '\u{669}')];
const DEVANAGARI: &[(char, char)] = &[('\u{900}', '\u{97F}')];
const TAMIL: &[(char, char)] = &[('\u{B80}', '\u{BFF}')];

/// Zeichen, die jede Sprache benutzen darf.
pub(crate) const COMMON: &str = "0123456789 .,;:!?-()'\"%/";

/// Eine unterstützte Sprache.
#[derive(Debug)]
pub struct Lang {
    /// ISO-639-1-Code (auch Korpus-Dateiname und Wikipedia-Subdomain).
    pub code: &'static str,
    /// Anzeigename.
    pub name: &'static str,
    /// Codepoint-Bereiche der Schrift.
    pub ranges: &'static [(char, char)],
    /// Sprachtypische Satz-/Sonderzeichen zusätzlich zu `COMMON`.
    pub extra: &'static str,
    /// Sprachspezifisches Erkennungsmodell (Ordnername unter `models/`).
    pub model: &'static str,
    /// Rechts-nach-links (visuell gespiegelt gerendert, ohne Shaping).
    pub rtl: bool,
    /// Kuratierte Beispielsätze (Sonderzeichen-lastig).
    pub pangrams: &'static [&'static str],
}

impl Lang {
    /// Gehört `c` zur Schrift dieser Sprache (vor Wörterbuch-/Font-Filter)?
    #[must_use]
    pub fn in_script(&self, c: char) -> bool {
        COMMON.contains(c)
            || self.extra.contains(c)
            || self.ranges.iter().any(|&(lo, hi)| (lo..=hi).contains(&c))
    }
}

/// Alle Sprachen in Anzeige-Reihenfolge.
pub const LANGS: &[Lang] = &[
    Lang {
        code: "de",
        name: "Deutsch",
        ranges: LATIN,
        extra: "„“‚‘–…€§",
        model: UNIVERSAL,
        rtl: false,
        pangrams: &[
            "Falsches Üben von Xylophonmusik quält jeden größeren Zwerg.",
            "Zwölf Boxkämpfer jagen Viktor quer über den großen Sylter Deich.",
            "Größe, Maß, Fuß und Straße: ÄÖÜ äöü ß ẞ",
            "„Grüße aus Köln“ – 17 € für 3 Brötchen …",
        ],
    },
    Lang {
        code: "fr",
        name: "Français",
        ranges: LATIN,
        extra: "«»’–…€",
        model: UNIVERSAL,
        rtl: false,
        pangrams: &[
            "Portez ce vieux whisky au juge blond qui fume.",
            "Voix ambiguë d’un cœur qui au zéphyr préfère les jattes de kiwis.",
            "« Là-bas, l’été, où ça brûle » : çà et là, déjà Noël.",
        ],
    },
    Lang {
        code: "en",
        name: "English",
        ranges: LATIN,
        extra: "–…",
        model: UNIVERSAL,
        rtl: false,
        pangrams: &[
            "The quick brown fox jumps over the lazy dog.",
            "Pack my box with five dozen liquor jugs.",
            "Sphinx of black quartz, judge my vow: 42 (yes)!",
        ],
    },
    Lang {
        code: "es",
        name: "Español",
        ranges: LATIN,
        extra: "¿¡«»–…€",
        model: UNIVERSAL,
        rtl: false,
        pangrams: &[
            "El veloz murciélago hindú comía feliz cardillo y kiwi.",
            "¿Qué pingüino añejo exige ñandúes? ¡Olé!",
            "La cigüeña tocaba el saxofón detrás del palenque de paja.",
        ],
    },
    Lang {
        code: "pl",
        name: "Polski",
        ranges: LATIN,
        extra: "„”–…",
        model: "latin_PP-OCRv5_mobile_rec_onnx",
        rtl: false,
        pangrams: &[
            "Pchnąć w tę łódź jeża lub ośm skrzyń fig.",
            "Zażółć gęślą jaźń.",
            "Mężny bądź, chroń pułk twój i sześć flag.",
        ],
    },
    Lang {
        code: "ru",
        name: "Русский",
        ranges: CYRILLIC,
        extra: "«»–…",
        model: "eslav_PP-OCRv5_mobile_rec_onnx",
        rtl: false,
        pangrams: &[
            "Съешь же ещё этих мягких французских булок, да выпей чаю.",
            "В чащах юга жил бы цитрус? Да, но фальшивый экземпляр!",
            "Широкая электрификация южных губерний даст мощный толчок подъёму сельского хозяйства.",
        ],
    },
    Lang {
        code: "uk",
        name: "Українська",
        ranges: CYRILLIC,
        extra: "«»’–…",
        model: "eslav_PP-OCRv5_mobile_rec_onnx",
        rtl: false,
        pangrams: &[
            "Чуєш їх, доцю, га? Кумедна ж ти, прощайся без ґольфів!",
            "Щастям б'єш жук їх глицю в фон й ґедзь пріч.",
        ],
    },
    Lang {
        code: "el",
        name: "Ελληνικά",
        ranges: GREEK,
        extra: "«»–…",
        model: "el_PP-OCRv5_mobile_rec_onnx",
        rtl: false,
        pangrams: &[
            "Ξεσκεπάζω την ψυχοφθόρα βδελυγμία.",
            "Γαζίες και μυρτιές δεν θα βρω πια στο χρυσαφί ξέφωτο.",
        ],
    },
    Lang {
        code: "ja",
        name: "日本語",
        ranges: JAPANESE,
        extra: "、。「」・ー々",
        model: UNIVERSAL,
        rtl: false,
        pangrams: &[
            "いろはにほへと ちりぬるを わかよたれそ つねならむ",
            "色は匂へど散りぬるを我が世誰ぞ常ならむ",
            "東京都の天気は晴れ、気温は二十五度です。",
            "カタカナとひらがなと漢字を混ぜた文章。",
        ],
    },
    Lang {
        code: "zh",
        name: "中文",
        ranges: HAN,
        extra: "，。、「」《》；：！？（）",
        model: UNIVERSAL,
        rtl: false,
        pangrams: &[
            "天地玄黄，宇宙洪荒。日月盈昃，辰宿列张。",
            "我能吞下玻璃而不伤身体。",
            "北京是中华人民共和国的首都。",
        ],
    },
    Lang {
        code: "ko",
        name: "한국어",
        ranges: HANGUL,
        extra: "",
        model: "korean_PP-OCRv5_mobile_rec_onnx",
        rtl: false,
        pangrams: &[
            "키스의 고유조건은 입술끼리 만나야 하고 특별한 기술은 필요치 않다.",
            "다람쥐 헌 쳇바퀴에 타고파",
            "동해 물과 백두산이 마르고 닳도록",
        ],
    },
    Lang {
        code: "th",
        name: "ไทย",
        ranges: THAI,
        extra: "",
        model: "th_PP-OCRv5_mobile_rec_onnx",
        rtl: false,
        pangrams: &[
            "เป็นมนุษย์สุดประเสริฐเลิศคุณค่า กว่าบรรดาฝูงสัตว์เดรัจฉาน",
            "จงฝ่าฟันพัฒนาวิชาการ อย่าล้างผลาญฤๅเข่นฆ่าบีฑาใคร",
            "สวัสดีครับ ยินดีต้อนรับ",
        ],
    },
    Lang {
        code: "ar",
        name: "العربية",
        ranges: ARABIC,
        extra: "،؛؟",
        model: "arabic_PP-OCRv5_mobile_rec_onnx",
        rtl: true,
        pangrams: &[
            "نص حكيم له سر قاطع وذو شأن عظيم مكتوب على ثوب أخضر",
            "مرحبا بالعالم",
            "اللغة العربية جميلة",
        ],
    },
    Lang {
        code: "hi",
        name: "हिन्दी",
        ranges: DEVANAGARI,
        extra: "",
        model: "devanagari_PP-OCRv5_mobile_rec_onnx",
        rtl: false,
        pangrams: &[
            "ऋषियों को सताने वाले दुष्ट राक्षसों के राजा रावण का सर्वनाश करने वाले",
            "नमस्ते दुनिया",
            "हिन्दी भाषा",
        ],
    },
    Lang {
        code: "ta",
        name: "தமிழ்",
        ranges: TAMIL,
        extra: "",
        model: "ta_PP-OCRv5_mobile_rec_onnx",
        rtl: false,
        pangrams: &[
            "யாமறிந்த மொழிகளிலே தமிழ்மொழி போல் இனிதாவது எங்கும் காணோம்",
            "வணக்கம் உலகம்",
        ],
    },
];

/// Index einer Sprache per Code.
#[must_use]
pub fn by_code(code: &str) -> Option<usize> {
    LANGS.iter().position(|l| l.code == code)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn codes_are_unique_and_found() {
        for (i, l) in LANGS.iter().enumerate() {
            assert_eq!(by_code(l.code), Some(i));
        }
        assert_eq!(by_code("xx"), None);
    }

    #[test]
    fn pangrams_are_in_script() {
        for l in LANGS {
            assert!(!l.pangrams.is_empty(), "{}", l.code);
            for p in l.pangrams {
                for c in p.chars() {
                    assert!(l.in_script(c), "{}: {c:?} in {p}", l.code);
                }
            }
        }
    }

    #[test]
    fn german_special_chars_belong_to_de() {
        let de = &LANGS[by_code("de").unwrap()];
        for c in "äöüßÄÖÜẞ„“€".chars() {
            assert!(de.in_script(c), "{c}");
        }
        assert!(!de.in_script('ж'));
    }
}
