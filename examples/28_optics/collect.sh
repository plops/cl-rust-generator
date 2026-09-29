for i in `find . -type f |grep -v /target/|grep -v ~$`
do
    echo "// start of "$i
    cat $i
done

echo "das walkthrough.md dokument ist fuer mich unverstaendlich. schreibe eine bessere version davon, die den leser abholt und mitnimmt. erklaere die abkuerzungen, und scope des dokuments. schreibe eine diskussion.

mache auch einen ueberblick ueber die architektur des programs. nutze mermaid diagramme.

mache auch einen review des quellcodes, der feature (fehlen welche) und der test abdeckung.

"
