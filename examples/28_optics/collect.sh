for i in `find . -type f |grep -v /target/|grep -v ~$`
do
    echo "// start of "$i
    cat $i
done
