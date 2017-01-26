#!/bin/bash
echo "#generated file" > temp.sed
echo 's/SUBROUTINE AD_COST/SUBROUTINE AD_COST()/' >>temp.sed
for i in `grep '^ *SUBROUTINE ' $1 | awk '{print $2}'`
do 
  # extract the name
  srName=${i%%\(*}
  echo "/^ *SUBROUTINE $srName(/i\\" >> temp.sed
  case "$srName" in 
    "AD_COST"|"ad_cost" ) 
      echo "!\$openad XXX Template ad_template.revolve.f" >> temp.sed
      ;;
    *) 
      echo "!\$openad XXX Template ad_template.split.f" >> temp.sed
      ;;
  esac
done
cat $1 | sed -f temp.sed > $2
rm -f temp.sed  
