for a in ../ir-snippets/*
do
    lli $a;
    e=$?
    a=$(echo $a| cut -f3 -d/| cut -f1 -d.)
    printf "$e \treturned by $a\n"
done
