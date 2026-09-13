for i in `find . -type f|grep -v .png|grep -v Cargo.lock|grep -v target/|grep -v '.*~'`; do echo "start of "$i;cat $i;done > /dev/shm/f
