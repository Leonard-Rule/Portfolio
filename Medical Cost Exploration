/*Summary stats of visit count exported to excel */
ods excel file="C:\Users\Leonard.Rule\PTVBC_summary(2).xlsx";
	proc means data=sandbox.lr_vbc_final3  n mean median mode stddev min max range ;
	var epi_totpay;
	WHERE epi_totpay = 0
	AND category = 'Surgery Code';
	run;
ods excel close;

/*Summary stats of visit count by age group*/
	ods excel file="C:\Users\Leonard.Rule\PTVBC_summary(6).xlsx";

	proc means data=sandbox.lr_vbc_final3 mean median mode stddev min max range ;
	var epi_totpay;
	class pain;
    WHERE epi_totpay>0
	AND epi_totpay <9000;
	run;

	ods excel close;

	proc export data=out
    outfile="C:\Users\Leonard.Rule\PTVBC_summary.xlsx"
    dbms=xlsx
    replace;
    sheet="summary";
run;



/* Change age to numeric*/
DATA lr_vbc_final3sas;
set sandbox.lr_vbc_finalv2;
agen = input(age, 4.);
drop age;
rename agen=age;
run;

/*Check correlation of age on total episode cost*/
PROC CORR data=lr_vbc_final4sas spearman ;
var age epi_totpay;
Where epi_totpay IS NOT NULL AND epi_totpay >0 AND epi_totpay < 9000;
run;

/*See if statistical differenc in epsiode cost by region of the country or surgical episodes*/
proc univariate data = lr_vbc_final4sas normal;
qqplot epi_totpay;
run;

PROC ANOVA data=lr_vbc_final4sas;
class region;
model epi_totpay=region;
means region / tukey cldiff;
WHERE post_surg = 'yes';
run;

proc glm data = lr_vbc_final4sas;
class post_surg;
model epi_totpay = post_surg ;
run;

DATA lr_vbc_final4sas;
Set lr_vbc_final3sas;
IF Category = 'Surgery Code' THEN post_surg= 'yes';
ELSE post_surg ='No';
run;

proc ttest data=lr_vbc_final4sas;
class post_surg;
var epi_totpay;
WHERE age>50;
run;

proc univariate data = lr_vbc_final4sas normal;
qqplot epi_totpay;
run;

/*Nnparmatirc ANOVA*/
proc npar1way data = lr_vbc_final4sas;
  class region;
  var epi_totpay;
run;
