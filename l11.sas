options yearcutoff=2000 nodate pageno=1 linesize=80 pagesize=60;

filename in1 'C:\Users\ariel\Documents\BU_RCode\BS820-RCode\data\stan.txt';

data stan;
 infile in1;
 input dob mmddyy9. doa mmddyy9. dot mmddyy9. dls mmddyy9.
 id age dead dur surg trans wtime m1 m2 m3 reject;
 surv1=dls-doa;
 surv2=dls-dot;
 *ageaccpt=(doa-dob)/365.25;
 ageaccpt=yrdif( dob, doa);
 agetrans=(dot-dob)/365.25;
 wait=dot-doa;
 if dot=. then trans=0; else trans=1;
 *format dob /*mmddyy9.*/ doa /*mmddyy9.*/ dot mmddyy9. dls mmddyy9.;
run;
ods rtf file=STAnal bodytitle style=journal;
proc print data=stan (obs=10);
 var dob doa dot dls dead trans surg m1 m2 m3;
 format dob doa dot dls mmddyy9.;
 title1 'Stanford Heart Transplant Data (first 10)';
run;
proc means data=stan n sum;
 class trans;
 var surv1 dead;
 title1 'Follow-Up Time and Events';
run;

data stan2;
 set stan;
if dot= . then do;
 time0 = 0;
 time1 = max(0, dls - doa);
 plant = 0;
 dead1 = dead;
 FUdurat = time1 - time0;
 output;
end;
else do;
 time0 = 0;
 time1 = max(0, (dot -1) - doa);
 plant = 0;
 dead1 = 0;
 FUdurat = time1 - time0;
 output;
 time0 = max(0, (dot - 1) - doa);
 time1 = max(0, dls - (doa - 1));
 plant = 1;
 dead1 = dead;
 FUdurat = time1 - time0;
 output;
end;
run;

proc means data = stan2;
	var ageaccpt;
run;

proc phreg data=stan2;
 model (time0, time1)*dead1(0)=plant surg ageaccpt / ties=exact;
 title1 'Cox Regression with Split Time';
run;
