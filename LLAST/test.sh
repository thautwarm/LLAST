for a in ../ir-snippets/*.ll
do
    llc $a -o $a.s;
    gcc -no-pie -C $a.s -o $a.out
    ./$a.out
    e=$?
    a=$(echo $a| cut -f3 -d/| cut -f1 -d.)
    printf "$e \treturned by $a\n"
done
