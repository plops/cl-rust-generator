for i in Cargo.toml src/main.rs ../../../{README.org,SUPPORTED*,transpiler-tests.lisp,rust.md,examples/01_gcd/gen00.lisp,examples/21_mandelbrot/gen00.lisp} 
do
    echo "// start of "$i
    cat $i
done

echo "examples for converting Rust into cl-rust-generator's Lisp input language are:
File	Role
README.org	Quick-start, syntax rules, form catalog table, worked examples
SUPPORTED_FORMS.md	Auto-generated, verified catalog of every supported form with Lisp→Rust pairs
transpiler-tests.lisp	Machine-checked source of truth — every form's Lisp input and exact Rust output
rust.md	Notes on Rust operator precedence used by the generator
examples/01_gcd/gen00.lisp, examples/03_glium/gen00.lisp, examples/21_mandelbrot/gen00.lisp (and other gen00.lisps)	Full real-world programs showing the generator used end-to-end
Why these files

README.org is the top-level usage guide: it explains the two golden rules (-→: for paths, strings are verbatim escape hatches) , gives a form-group table covering literals, arithmetic, control flow, bindings, items, etc. README.org:76-91 , and shows two complete worked examples translating a gcd function and a Point struct/impl into Rust README.org:188-244 . This is the single best \"instructions\" doc for an AI learning the input language.

SUPPORTED_FORMS.md is generated straight from the test suite README.org:18-23 , so every example in it (e.g. range, coerce, deftrait, struct, literals) is guaranteed correct SUPPORTED_FORMS.md:1276-1290 SUPPORTED_FORMS.md:2403-2413 SUPPORTED_FORMS.md:1550-1564 . This is the most exhaustive reference of \"Lisp form → Rust output\" pairs, ideal for an AI to learn the mapping.

transpiler-tests.lisp is the actual ground truth *test-cases* list that SUPPORTED_FORMS.md is generated from transpiler-tests.lisp:38-44 , containing hundreds of :lisp/:rust pairs across categories like literals, symbols, arithmetic, bindings, and blocks transpiler-tests.lisp:962-1019 . Because it's directly used by the test harness (run-tests.sh) and doc generator, it is the most reliable and largest set of examples.

Full example programs (gen00.lisp files in examples/) show how these forms compose into whole Rust programs and are exactly what past work used to teach the transpiler's usage to an AI — the planning prompt in this repo explicitly instructs: \"schau dir auch examples/21_mandelbrot/gen00.lisp und das daraus erzeugte file examples/21_mandelbrot/mandelbrot/src/main.rs an. dies zeigt wie ich den lisp zu rust transpiler nutzen moechte\" prompt.txt:6-8 . This is direct evidence that pairing a gen00.lisp with its generated main.rs is the established pattern for demonstrating transpiler usage to an AI.
"
