for i in gentoo-dep-tui/{*.md,Cargo.toml,{src,src/*,tests}/*.rs} plan/20261113_03_review2/walkthrough.md plan/20261114_01_review3/manual_screencaps.md
do
    echo "// start of "$i
    #ls $i
    cat $i
done
