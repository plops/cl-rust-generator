for i in gentoo-dep-tui/{Cargo.toml,{src,tests}/*.rs} plan/20261113_02_review/walkthrough.md
do
    echo "// start of "$i
    cat $i
done
