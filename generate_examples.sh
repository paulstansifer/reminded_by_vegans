for i in `seq 9`; do

    echo "2011-03-0$i" | mail rbv -s "test-message $i" 
    echo "2011-03-1$i" | mail rbv -s "test-message $i" 

done

    