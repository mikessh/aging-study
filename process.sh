for i in {2..6}
do
   java -Xmx40G -jar migec.jar Checkout -cute age${i}_barcodes.txt age${i}_R1.fastq.gz age${i}_R2.fastq.gz checkout/
   cp checkout/checkout.filelist.txt checkout_logs/age${i}_checkout.filelist.txt
   cp checkout/checkout.log.txt checkout_logs/age${i}_checkout.log.txt
done

cat checkout_logs/age2_checkout.filelist.txt | head -n 1 > checkout/checkout.filelist.txt
cat checkout_logs/age2_checkout.log.txt | head -n 1 > checkout/checkout.log.txt

for i in {2..6}
do
   cat checkout_logs/age${i}_checkout.filelist.txt | awk 'FNR>1' >> checkout/checkout.filelist.txt
   cat checkout_logs/age${i}_checkout.log.txt | awk 'FNR>1' >> checkout/checkout.log.txt
done

java -Xmx40G -jar migec.jar Histogram checkout/ histogram/

java -Xmx40G -jar migec.jar AssembleBatch --force-collision-filter --force-overseq 2 --default-mask 0:1 checkout/ histogram/ assemble/

cd assemble
for f in *.fastq
do
   s=${f%_R2.t2.cf.fastq};
   mitcr -pset ../age-pset -report ../cdrblast/mitcr.log.txt $f ../cls/$s.cls;
done