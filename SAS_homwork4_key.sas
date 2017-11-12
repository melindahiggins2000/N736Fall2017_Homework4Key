* =============================
  N736 Homework 4 Answer Key
  =============================;

* make a copy to WORK;
data helpmkh;
  set library.helpmkh;
  run;

* run regression of indtot versus mcs;

proc reg data=helpmkh;
  model indtot = mcs;
  plot indtot * mcs / pred;
  run;

* proc glm option to get
  type I and type III SS;

proc glm data=helpmkh;
  model indtot = mcs;
  run;

* recode race into 3 groups;

proc format;
   value race3f
      1 = 'Black'  
      2 = 'White' 
      3 = 'Hispanic/Other';
run;

data help2;
  set helpmkh;
  if racegrp="black" then race3=1;
  if racegrp="white" then race3=2;
  if racegrp="hispanic" then race3=3;
  if racegrp="other" then race3=3;
  format race3 race3f.;
  run;

proc freq data=help2;
  table racegrp;
  table race3;
  run;

* run ANOVA of INDTOT versus Race3;

proc ANOVA data=help2;
	title Oneway ANOVA of INDTOT by Race;
	class race3;
	model indtot = race3;
	means race3 / bon sidak hovtest welch;
	run;

* proc glm option to get
  type I and type III SS;

proc glm data=help2;
  class race3;
  model indtot = race3;
  means race3 / bon sidak hovtest welch;
  lsmeans race3 / pdiff=all adjust=tukey;
  run;
