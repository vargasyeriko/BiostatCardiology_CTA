*--------------------------------------MANAGING THE DATA--------------------------------------------;
proc import out= plaque
            datafile= "y:\data for ma studyplaque_gender_jb.xlsx" 
            dbms=xlsx replace;
			
run;

proc import out = patient
            datafile= "y:\data for ma studyplaque_gender_jb.xlsx"         
            dbms = xlsx replace;
            sheet = "patient level data";
run;


data plaque;
 	set plaque;
 	keep study__--gender; *exclude the variables that have all empty cells;
run;


proc sort data = plaque;
	by Study__ ;
run;

proc sort data = patient;
	by Study__;
run;
 
data ppD;
	merge plaque patient; *Merge the two sheets of the excel file in one data set;
	by Study__;
run;
*---------------------------------------VOLUME OF PLAQUE--------------------------------------------;


proc glimmix data=ppD plots= residualpanel;
	class gender;
	model plaq_vol = gender / ddfm =kr; *add covariates ;
	random int /sub= study__(gender);
	lsmeans gender / cl; run;


proc glimmix data=ppD;
	class gender dm;
	model plaq_vol = gender dm/ ddfm =kr ; 
	random int /sub= study__(gender);
	lsmeans gender / cl; run;

proc glimmix data=ppD;
	class gender htn;
	model plaq_vol = gender htn / ddfm =kr; 
	random int /sub= study__(gender);
	lsmeans gender /cl; run;

proc glimmix data=ppD;
	class gender smoking;
	model plaq_vol = gender smoking / ddfm =kr; 
	random int /sub= study__(gender);
	lsmeans gender /cl ; run;

proc glimmix data=ppD;
	class gender htn smoking dm;
	model plaq_vol = gender dm htn smoking / ddfm =kr ; 
	random int /sub= study__(gender);
	lsmeans gender /cl; run; 
*--------------------------------------VOL_LAP------------------------------------------------------------;

ods output SolutionR=vol_lap_blup; * random effects solution vectors;

proc glimmix data= ppD plots= residualpanel;
	class gender;
	model vol_lap= gender /  ddfm =kr solution;  *add covariates, kr option provides the Kenward-Roger Degrees of Freedom Approximation;
	random int /sub= study__(gender) s ; *random /s will give the solutions before using the ods output;
	output out=vol_lap_outp pred=p resid=r;
	lsmeans gender / cl; run;


proc univariate plot normal data=vol_lap_outp;
	var p;
run;

proc univariate plot normal data = vol_lap_blup;
	var Estimate; *plot the estimates ;
run;

proc glimmix data= ppD plots= residualpanel;
	class gender dm;
	model vol_lap= gender dm/ ddfm =kr;   
	random int /sub= study__(gender);
	lsmeans gender /cl; run;

proc glimmix data= ppD plots= residualpanel;
	class gender htn;
	model vol_lap= gender htn / ddfm =kr;  
	random int /sub= study__(gender);
	lsmeans gender / cl ; run;

proc glimmix data= ppD plots= residualpanel;
	class gender smoking;
	model vol_lap= gender smoking /ddfm =kr;  
	random int /sub= study__(gender);
	lsmeans gender / cl; run; 

proc glimmix data= ppD plots= residualpanel;
	class gender dm smoking htn ;
	model vol_lap= gender dm  htn smoking / ddfm =kr;  
	random int /sub= study__(gender);
	lsmeans gender / cl; run; 
*------------------------------------------# OF PLAQUES------------------------------------------------------------;

proc genmod data = ppD  ;
	class gender;
	model total_num_plaques = gender / dist = poisson lrci type3;
	lsmeans gender /exp diff cl ;
run;

proc genmod data = ppD  ;
	class gender dm;
	model total_num_plaques = gender dm /dist = poisson lrci type3;
	lsmeans gender /exp diff cl ;
run;

proc genmod data = ppD  ;
	class gender htn;
	model total_num_plaques = gender htn /dist = poisson lrci type3;
	lsmeans gender /exp diff cl ;
run;

proc genmod data = ppD  ;
	class gender smoking;
	model total_num_plaques = gender smoking /dist = poisson lrci type3;
	lsmeans gender /exp diff cl ;
run;

proc genmod data = ppD  ;
	class gender  dm htn smoking;
	model total_num_plaques = gender htn dm smoking /dist = poisson lrci type3;
	lsmeans gender /exp diff cl ;
run;
	

*----------------------------------------REMODELED INDEX----------------------------------------------------;

data ppD;
	set ppD;
		y = log(dx_vessel_area) - log((prox_ref_area + dist_ref_area)/2);
run;

proc glimmix data=ppD plots=residualpanel;
	class gender;
	model y = gender / ddfm =kr ; 
	random int /sub= study__(gender);
	lsmeans  gender  / cl ;
	lsmestimate gender  0 1 /exp cl; run;

proc glimmix data=ppD plots=residualpanel;
	class gender dm;
	model y = gender dm/ ddfm =kr ; 
	random int /sub= study__(gender);
	lsmeans gender /cl; 
	lsmestimate gender  0 1 /exp cl;  run;

proc glimmix data=ppD plots=residualpanel;
	class gender htn;
	model y = gender htn / ddfm =kr; 
	random int /sub= study__(gender);
	lsmeans gender / cl ;
	lsmestimate gender   0 1 / exp cl ;run;

proc glimmix data=ppD plots=residualpanel;
	class gender smoking;
	model y = gender smoking / ddfm =kr; 
	random int /sub= study__(gender);
	lsmeans gender / cl ; 
	lsmestimate gender 0 1 / exp cl ;run;

proc glimmix data=ppD plots=residualpanel;
	class gender dm htn smoking;
	model y = gender dm htn smoking / ddfm =kr; 
	random int /sub= study__(gender);
	lsmeans gender /cl; 
	lsmestimate gender 0 1  / exp cl ; run; * 1 for the exp and cl of the gender_ 0 and 0 1  for gender_1;

 *--------------------------------------POSITIVELY REMODELED-------------------------------------------------;

proc glimmix data = ppD ;
	class gender;
	model positively_remodeled(event='1')= gender / s  ddfm = kr dist = binary oddsratio;
	random int /sub= study__(gender); 
	lsmeans gender / odds oddsratio cl ; run;

proc glimmix data = ppD ;
	class gender dm;
	model positively_remodeled(event='1')= gender dm / ddfm =kr dist = binary oddsratio ;
	random int /sub= study__(gender); 
	lsmeans gender / odds oddsratio cl; run;

proc glimmix data = ppD ;
	class gender htn;
	model positively_remodeled(event='1')= gender htn / ddfm =kr dist = binary oddsratio;
	random int /sub= study__(gender); 
	lsmeans gender /odds oddsratio  cl; run;

proc glimmix data = ppD ;
	class gender smoking;
	model positively_remodeled(event='1')= gender smoking / ddfm =kr dist = binary oddsratio;
	random int /sub= study__(gender); 
	lsmeans gender /odds oddsratio cl ; run;

proc glimmix data = ppD ;
	class gender dm htn smoking;
	model positively_remodeled(event='1')= gender dm htn smoking /  ddfm =kr dist = binary oddsratio;
	random int /sub= study__(gender); 
	lsmeans gender /odds oddsratio cl; run;
*************;




	*--------------------------------------------------------------------------------------TYPE_1;
proc glimmix data = ppD ;
	class gender;
	model type_1(event='1')= gender / s  ddfm = kr dist = binary oddsratio;
	random int /sub= study__(gender); 
	lsmeans gender / odds oddsratio cl ; run;

proc glimmix data = ppD ;
	class gender dm;
	model type_1(event='1')= gender dm / ddfm =kr dist = binary oddsratio ;
	random int /sub= study__(gender); 
	lsmeans gender / odds oddsratio cl; run;

proc glimmix data = ppD ;
	class gender htn;
	model type_1(event='1')= gender htn / ddfm =kr dist = binary oddsratio;
	random int /sub= study__(gender); 
	lsmeans gender /odds oddsratio  cl; run;

proc glimmix data = ppD ;
	class gender smoking;
	model type_1(event='1')= gender smoking / ddfm =kr dist = binary oddsratio;
	random int /sub= study__(gender); 
	lsmeans gender /odds oddsratio cl ; run;

proc glimmix data = ppD ;
	class gender dm htn smoking;
	model type_1(event='1')= gender dm htn smoking /  ddfm =kr dist = binary oddsratio;
	random int /sub= study__(gender); 
	lsmeans gender /odds oddsratio cl; run;

*--------------------------------------------------------------------------------------TYPE_2;

proc glimmix data = ppD ;
	class gender;
	model type_2(event='1')= gender / s  ddfm = kr dist = binary oddsratio;
	random int /sub= study__(gender); 
	lsmeans gender / odds oddsratio cl ; run;

proc glimmix data = ppD ;
	class gender dm;
	model type_2(event='1')= gender dm / ddfm =kr dist = binary oddsratio ;
	random int /sub= study__(gender); 
	lsmeans gender / odds oddsratio cl; run;

proc glimmix data = ppD ;
	class gender htn;
	model type_2(event='1')= gender htn / ddfm =kr dist = binary oddsratio;
	random int /sub= study__(gender); 
	lsmeans gender /odds oddsratio  cl; run;

proc glimmix data = ppD ;
	class gender smoking;
	model type_2(event='1')= gender smoking / ddfm =kr dist = binary oddsratio;
	random int /sub= study__(gender); 
	lsmeans gender /odds oddsratio cl ; run;

proc glimmix data = ppD ;
	class gender dm htn smoking;
	model type_2(event='1')= gender dm htn smoking /  ddfm =kr dist = binary oddsratio;
	random int /sub= study__(gender); 
	lsmeans gender /odds oddsratio cl; run;

*--------------------------------------------------------------------------------------TYPE_3;

	proc glimmix data = ppD ;
	class gender;
	model type_3(event='1')= gender / s  ddfm = kr dist = binary oddsratio;
	random int /sub= study__(gender); 
	lsmeans gender / odds oddsratio cl ; run;

proc glimmix data = ppD ;
	class gender dm;
	model type_3(event='1')= gender dm / ddfm =kr dist = binary oddsratio ;
	random int /sub= study__(gender); 
	lsmeans gender / odds oddsratio cl; run;

proc glimmix data = ppD ;
	class gender htn;
	model type_3(event='1')= gender htn / ddfm =kr dist = binary oddsratio;
	random int /sub= study__(gender); 
	lsmeans gender /odds oddsratio  cl; run;

proc glimmix data = ppD ;
	class gender smoking;
	model type_3(event='1')= gender smoking / ddfm =kr dist = binary oddsratio;
	random int /sub= study__(gender); 
	lsmeans gender /odds oddsratio cl ; run;

proc glimmix data = ppD ;
	class gender dm htn smoking;
	model type_3(event='1')= gender dm htn smoking /  ddfm =kr dist = binary oddsratio;
	random int /sub= study__(gender); 
	lsmeans gender /odds oddsratio cl; run;

*--------------------------------------------------------------------------------------TYPE_4;

		proc glimmix data = ppD ;
	class gender;
	model type_4(event='1')= gender / s  ddfm = kr dist = binary oddsratio;
	random int /sub= study__(gender); 
	lsmeans gender / odds oddsratio cl ; run;

proc glimmix data = ppD ;
	class gender dm;
	model type_4(event='1')= gender dm / ddfm =kr dist = binary oddsratio ;
	random int /sub= study__(gender); 
	lsmeans gender / odds oddsratio cl; run;

proc glimmix data = ppD ;
	class gender htn;
	model type_4(event='1')= gender htn / ddfm =kr dist = binary oddsratio;
	random int /sub= study__(gender); 
	lsmeans gender /odds oddsratio  cl; run;

proc glimmix data = ppD ;
	class gender smoking;
	model type_4(event='1')= gender smoking / ddfm =kr dist = binary oddsratio;
	random int /sub= study__(gender); 
	lsmeans gender /odds oddsratio cl ; run;

proc glimmix data = ppD ;
	class gender dm htn smoking;
	model type_4(event='1')= gender dm htn smoking /  ddfm =kr dist = binary oddsratio;
	random int /sub= study__(gender); 
	lsmeans gender /odds oddsratio cl; run;


*--------------------------------;

*--------------------------------Categorical Logistic ;
*none;

proc glimmix data = ppD;
        class _Ctplaqtype gender ;
        model _Ctplaqtype( ref='2') = gender /
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;

proc glimmix data = ppD;
        class _Ctplaqtype gender ;
        model _Ctplaqtype( ref='3') = gender /
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;

proc glimmix data = ppD;
        class _Ctplaqtype gender ;
        model _Ctplaqtype( ref='4') = gender /
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;



*--------------------;
*dm;


proc glimmix data = ppD;
        class _Ctplaqtype gender DM;
        model _Ctplaqtype( ref='2') = gender DM /
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;

proc glimmix data = ppD;
        class _Ctplaqtype gender DM;
        model _Ctplaqtype( ref='3') = gender DM/
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;

proc glimmix data = ppD;
        class _Ctplaqtype gender DM;
        model _Ctplaqtype( ref='4') = gender DM /
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;

*-------------------------------;
*htn;

proc glimmix data = ppD;
        class _Ctplaqtype gender HTN;
        model _Ctplaqtype( ref='2') = gender HTN /
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;

proc glimmix data = ppD;
        class _Ctplaqtype gender HTN;
        model _Ctplaqtype( ref='3') = gender HTN/
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;

proc glimmix data = ppD;
        class _Ctplaqtype gender HTN;
        model _Ctplaqtype( ref='4') = gender HTN /
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;

*---------------------------------------------;
*smoking;

proc glimmix data = ppD;
        class _Ctplaqtype gender smoking;
        model _Ctplaqtype( ref='2') = gender smoking /
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;

proc glimmix data = ppD;
        class _Ctplaqtype gender smoking;
        model _Ctplaqtype( ref='3') = gender smoking/
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;

proc glimmix data = ppD;
        class _Ctplaqtype gender smoking;
        model _Ctplaqtype( ref='4') = gender smoking /
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;

*------------------------------------------------------------;
*all;

proc glimmix data = ppD;
        class _Ctplaqtype gender DM HTN smoking;
        model _Ctplaqtype( ref='2') = gender DM HTN smoking /
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;

proc glimmix data = ppD;
        class _Ctplaqtype gender DM HTN smoking;
        model _Ctplaqtype( ref='3') = gender DM HTN smoking/
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;

proc glimmix data = ppD;
        class _Ctplaqtype gender DM HTN smoking;
        model _Ctplaqtype( ref='4') = gender DM HTN smoking /
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;


*----------------------Adjusting------------------------------------;
*none;

proc import out = nonepval
            datafile= "y:\pvals.xlsx"         
            dbms = xlsx replace;
            sheet = "none";
run;






proc multtest inpvalues= panddf adaptiveholm bon holm;
run;


data panddf;
set panddf ;
sdb_t_quantiles= tinv(step_down_bon, DF);
run;

proc print data= panddf;
run;







x= tinv(.035, 198.8)

*--------------------------------------MANAGING THE DATA--------------------------------------------;
proc import out= plaque
            datafile= "y:\data for ma studyplaque_gender_jb.xlsx" 
            dbms=xlsx replace;
			
run;



proc import out = patient
            datafile= "y:\data for ma studyplaque_gender_jb.xlsx"         
            dbms = xlsx replace;
            sheet = "patient level data";
run;


data plaque;
 	set plaque;
 	keep study__--gender; *exclude the variables that have all empty cells;
run;


proc sort data = plaque;
	by Study__ ;
run;

proc sort data = patient;
	by Study__;
run;
 
data ppD;
	merge plaque patient; *Merge the two sheets of the excel file in one data set;
	by Study__;
run;

*--------------------------------Categorical Logistic ;
*none;

proc glimmix data = ppD;
        class _Ctplaqtype gender ;
        model _Ctplaqtype( ref='2') = gender /
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr
cl ;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;

proc glimmix data = ppD;
        class _Ctplaqtype gender ;
        model _Ctplaqtype( ref='3') = gender /
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr
cl;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;

proc glimmix data = ppD;
        class _Ctplaqtype gender ;
        model _Ctplaqtype( ref='4') = gender /
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;



*--------------------;
*dm;


proc glimmix data = ppD;
        class _Ctplaqtype gender DM;
        model _Ctplaqtype( ref='2') = gender DM /
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;

proc glimmix data = ppD;
        class _Ctplaqtype gender DM;
        model _Ctplaqtype( ref='3') = gender DM/
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;

proc glimmix data = ppD;
        class _Ctplaqtype gender DM;
        model _Ctplaqtype( ref='4') = gender DM /
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;

*-------------------------------;
*htn;

proc glimmix data = ppD;
        class _Ctplaqtype gender HTN;
        model _Ctplaqtype( ref='2') = gender HTN /
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;

proc glimmix data = ppD;
        class _Ctplaqtype gender HTN;
        model _Ctplaqtype( ref='3') = gender HTN/
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;

proc glimmix data = ppD;
        class _Ctplaqtype gender HTN;
        model _Ctplaqtype( ref='4') = gender HTN /
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;

*---------------------------------------------;
*smoking;

proc glimmix data = ppD;
        class _Ctplaqtype gender smoking;
        model _Ctplaqtype( ref='2') = gender smoking /
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;

proc glimmix data = ppD;
        class _Ctplaqtype gender smoking;
        model _Ctplaqtype( ref='3') = gender smoking/
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;

proc glimmix data = ppD;
        class _Ctplaqtype gender smoking;
        model _Ctplaqtype( ref='4') = gender smoking /
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;

*------------------------------------------------------------;
*all;

proc glimmix data = ppD;
        class _Ctplaqtype gender DM HTN smoking;
        model _Ctplaqtype( ref='2') = gender DM HTN smoking /
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;

proc glimmix data = ppD;
        class _Ctplaqtype gender DM HTN smoking;
        model _Ctplaqtype( ref='3') = gender DM HTN smoking/
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;

proc glimmix data = ppD;
        class _Ctplaqtype gender DM HTN smoking;
        model _Ctplaqtype( ref='4') = gender DM HTN smoking /
                        dist=multinomial
                        link=glogit
                        solution
                        ddfm = kr;
        random intercept / subject=study__(gender) group=_Ctplaqtype;
		output out = ref_1_none;
run;

*********************************Adjusting***********************************;
*----------------------------------NONE-------------------------------;


proc import out = nonepval
            datafile= "y:\pvals.xlsx"         
            dbms = xlsx replace;
            sheet = "none";
run;

proc multtest inpvalues= nonepval holm bon adaptiveholm;
run;

*none: confidence intervals ;

data t_nonepval;
set nonepval ;
bon_tquantiles = tinv((1-(.05/6)),DF);
proc print;
run;

data none_sdb;
none_sdb_1vs3 = tinv((1-(.05/(6-1+1))), 187.7);
none_sdb_1vs4 = tinv((1-(.05/(6-2+1))), 165.1);
none_sdb_2vs3 = tinv((1-(.05/(6-3+1))), 168.7);
none_sdb_2vs4 = tinv((1-(.05/(6-4+1))), 141.6);
none_sdb_1vs2 = tinv((1-(.05/(6-5+1))), 191.3);
none_sdb_3vs4 = tinv((1-(.05/(6-6+1))), 148.2);
proc print;
run;

*-------------------------------DM-------------------------------------;

proc import out = dmpval
            datafile= "y:\pvals.xlsx"         
            dbms = xlsx replace;
            sheet = "dm";
run;

proc multtest inpvalues= dmpval holm bon adaptiveholm;
run;

*dm: confidence intervals ;

data t_dmpval;
set dmpval ;
bon_tquantiles = tinv((1-(.05/6)),DF));
proc print;
run;

data dm_sdb;
dm_sdb_1vs3 = tinv((1-(.05/(6-1+1))), 186.5);
dm_sdb_1vs4 = tinv((1-(.05/(6-2+1))), 163.2);
dm_sdb_2vs3 = tinv((1-(.05/(6-3+1))), 168.5);
dm_sdb_2vs4 = tinv((1-(.05/(6-4+1))), 139.9);
dm_sdb_1vs2 = tinv((1-(.05/(6-5+1))), 190.7);
dm_sdb_3vs4 = tinv((1-(.05/(6-6+1))), 146.3);
proc print;
run;
*--------------------------------------------------------------------;
*htn;

proc import out = htnpval
            datafile= "y:\pvals.xlsx"         
            dbms = xlsx replace;
            sheet = "htn";
run;

proc multtest inpvalues= htnpval holm bon adaptiveholm;
run;

*htn: confidence intervals ;

data t_htnpval;
set htnpval ;
bon_tquantiles = tinv((1-(.05/6)),DF));
proc print;
run;

data dm_sdb;
htn_sdb_1vs3 = tinv((1-(.05/(6-1+1))), 186.5);
htn_sdb_1vs4 = tinv((1-(.05/(6-2+1))), 163.2);
htn_sdb_2vs3 = tinv((1-(.05/(6-3+1))), 168.5);
htn_sdb_2vs4 = tinv((1-(.05/(6-4+1))), 139.9);
htn_sdb_1vs2 = tinv((1-(.05/(6-5+1))), 190.7);
htn_sdb_3vs4 = tinv((1-(.05/(6-6+1))), 146.3);
proc print;
run;
*--------------------------------------------------------------------;
*smoking;

proc import out = smokingpval
            datafile= "y:\pvals.xlsx"         
            dbms = xlsx replace;
            sheet = "smoking";
run;

proc multtest inpvalues= smokingpval holm bon adaptiveholm;
run;

*smoking: confidence intervals ;

data t_smokingpval;
set smokingpval ;
stepdown_bon_tquantiles= tinv(Stepdown_Bonferroni, DF);
run;

proc print data= t_smokingpval;
run;
*--------------------------------------------------------------------;
*all;

proc import out = allpval
            datafile= "y:\pvals.xlsx"         
            dbms = xlsx replace;
            sheet = "all";
run;

proc multtest inpvalues= allpval holm bon adaptiveholm;
run;

*all: confidence intervals ;

data t_allpval;
set allpval ;
stepdown_bon_tquantiles= tinv(Stepdown_Bonferroni, DF);
run;

proc print data= t_allpval;
run;



*____________________;
