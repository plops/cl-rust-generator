for i in gentoo-dep-tui/{*.md,Cargo.toml,{src,src/*,tests}/*.rs} plan/202611*review5/*.md
#for i in gentoo-dep-tui/{*.md,Cargo.toml} plan/2026111*_review4/manual_screencaps.md
do
    echo "// start of "$i
    #ls $i
    cat $i
done
