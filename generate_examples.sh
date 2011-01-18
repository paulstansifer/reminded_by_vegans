for i in `seq 7`; do

    echo "2011-01-2$i" | mail rbv -s "test-message $i" 

done

    