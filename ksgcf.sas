

/*
 &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
 &                                                   &
 &      KS MACRO : SANKHA MUKHERJEE                  &
 &                                                   &
 &>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<&
 &>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<&
 &               INSTRUCTIONS:                       &
 &                                                   &
 &     DO NOT CHANGE ANYTHING IN THE MACRO           &
 &     INPUT ONLY THE MACRO PARAMETERS AS            &
 &     PER THE DEFINITIONS GIVEN BELOW               &
 &                                                   &
 &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
 &>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<&
 &               DEFINITIONS:MACRO PARAMETERS        &
 &                                                   &
 & SET   : name of data set which contains the score &
 & SCORE : probability(score) variable name          &
 & RESP  : name of the response variable             &
 & GROUP : number of equal groups in which you want  &
 &         to break the data set e.g : deciles       &
 & TITLE : any name that you want to give to the     &
 &         data set                                  &
 &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
*/

/*rsubmit;*/
%macro ksgcf(set,score,resp,group,title);
/*******Defining responders & nonresponders and creating Deciles**************/
data t1;
set &set(keep=&score &resp);
nonresp=1-&resp;
run;
proc sort data = t1 ;by descending &score;run;
data _null_;
set t1;
call symput('size',_n_);
run;
data t1;
set t1;
decile=floor(1+&group*(_n_-1)/&size);
run;
/********Finding no of responders, nonresponders, minscore, maxscore and meanscore in each decile********/
proc summary data = t1 nway missing;
var &score;
class decile;
output out=t2(drop=_TYPE_ _FREQ_) sum(nonresp &resp )=nonresp resp min(&score)=minscore max(&score)=maxscore
mean(&score)=meanscore;
run;

/********Finding minscore, maxscore and meanscore for overall data set********/
proc summary data = t1 nway missing;
var &score;
output out=t11(drop=_TYPE_ _FREQ_) min(&score)=minscore max(&score)=maxscore
mean(&score)=meanscore;
run;

data t2;
set t2 end=last;

/***Formatting minscore and maxscore**********/
minscore=minscore*1000;format minscore 8.0;
maxscore=maxscore*1000;format maxscore 8.0;

/****Calculating predicted response rate and actual response rate******/
predrespr=meanscore*100;format predrespr 8.1;
actrespr=(resp/(resp+nonresp))*100;format actrespr 8.1;

/******Calculating overall odds************/
overall_odds =nonresp/resp;

/********Defining macro vars to pick up total numbers************/
/******calculating cumulative no of nonresponders*******/
cumnonresp+nonresp;
/********calculating cumulative no of responders**********/
cumresp+resp;
/******calculating cumulative no of observations**********/
cumtotal=cumnonresp+cumresp;
/*********calculating cumulative response rate*******/
cumrespr=(cumresp/cumtotal)*100;format cumrespr 8.1;
/******calculating cumulative odds*********/
cumodds=cumnonresp/cumresp;format cumodds 8.2;
/******defining macro vars for storing sum total values*****/
if last then do;
call symput('sumodds',cumodds);
call symput('sumnonresp',cumnonresp);
call symput('sumresp',cumresp);
call symput('sumcumresprate',cumrespr);
end;
run;

data t3(drop=cumresp cumnonresp cumodds y cumgini);
set t2 end=final;
/*****calculating information odds**********/
info_odds=overall_odds/&sumodds;format info_odds 8.1;
/*******formatting overall odds***********/
format overall_odds 8.1;
/****calculating log odds****************/
log_odds=log(info_odds);format log_odds 8.1;
/*******calculating probability of non response in each decile********/
prob_nonresp=nonresp/(nonresp+resp);format prob_nonresp 8.2;
/*******calculating chi square*************/
chi_sq=((nonresp-(&sumnonresp/(&sumnonresp+&sumresp))*(nonresp+resp))**2)/((&sumnonresp/(&sumnonresp+&sumresp))
*(nonresp+resp))+((resp-(&sumresp/(&sumnonresp+&sumresp))*(nonresp+resp))**2)/((&sumresp/(&sumnonresp+&sumresp))
*(nonresp+resp));format chi_sq 8.1;
/********calculating cumulative chi square**********/
cumchi_sq+chi_sq;
/*******calculating percentage of nonresponders in each decile*********/
pernonresp=(nonresp/&sumnonresp)*100;format pernonresp 8.1;
/*******calculating percentage of responders in each decile*********/
perresp=(resp/&sumresp)*100;format perresp 8.1;
/*******calculating cumulative percentage of nonresponders in each decile*********/
cumpernonresp+pernonresp;format cumpernonresp 8.1;
/*******calculating cumulative percentage of responders in each decile*********/
cumperresp+perresp;format cumperresp 8.1;
/*********calculating percentage of observations in each decile************/
perobs=((nonresp+resp)/(&sumnonresp+&sumresp))*100;format perobs 8.0;
/*********calculating cumulative percentage of observations in each decile************/
cumperobs+perobs;format cumperobs 8.0;
/**********calculating individual lift obtained from each decile*********/
lift=(actrespr/&sumcumresprate)*100;format lift 8.0;
/********calculating cumulative lift*****************/
cumlift=(cumrespr/&sumcumresprate)*100;format cumlift 8.0;
/***********calculating KS***************/
ks=abs(cumperresp-cumpernonresp);format ks 8.1;
lg1=lag(resp);
if _n_=1 then flag=1;else flag=(resp<=lg1);
if flag=0 then break=_n_;
y=lag(cumperresp);

gini=((sum(cumperresp,y)/2)*pernonresp)/(100**2);format gini 8.2;
cumgini+gini;
if final then do;
call symput('sumgini',cumgini);
end;

predresp=meanscore*(resp+nonresp);
diff=resp-predresp; 
gofcell=(diff**2)/((resp+nonresp)*meanscore*(1-meanscore));
gof+gofcell;format gof 8.1;

meanscore=meanscore*1000;format meanscore 8.0;

run;

data t3;
set t3;
totalgini=&sumgini-0.5;format totalgini 8.4;
run;

proc transpose data =t3 out=t4;var break;run;

data t4(keep=sat_rank ranking);
set t4(drop=_NAME_);
array a{&group} col1 - col&group;
do i=1 to &group ;
if a{i}>0 then leave;
end;
if i=(&group+1) then sat_rank='all';else sat_rank=i-1;
if sat_rank='all' then ranking='SATISFACTORY        ';else ranking='NOT SATISFACTORY';
run;

proc transpose data = t3 out=t5;var ks ;run;
data t5(keep=ks maxksdec);
set t5(drop=_NAME_);
array ks1{&group} col1 - col&group;
format ks 8.1;
ks=0;
do i=1 to &group;
if ks1{i}> ks then ks=ks1{i};else leave;
end;
maxksdec=i-1;
run;

proc transpose data = t3 out=t6;var totalgini;run;
data t6;
set t6(keep=col1);
rename col1=gini;
run;

proc transpose data = t3 out=t7;var gof ;run;
data t7;
set t7(keep=col&group);
rename col&group=gof;
run;

data mrg;
merge t4 t5 t6 t7;
run;

data total(drop=cumrespr);
set t2(obs=1);
nonresp=&sumnonresp;
resp=&sumresp;
actrespr=(resp/(resp+nonresp))*100;format actrespr 8.1;
overall_odds =nonresp/resp;
info_odds=overall_odds/&sumodds;
format overall_odds 8.1;
format info_odds 8.1;
log_odds=log(info_odds);format log_odds 8.1;
prob_nonresp=nonresp/(nonresp+resp);format prob_nonresp 8.2;
pernonresp=(nonresp/nonresp)*100;format pernonresp 8.1;
perresp=(resp/resp)*100;format perresp 8.1;
perobs=((nonresp+resp)/(&sumnonresp+&sumresp))*100;format perobs 8.0;
run;
data total1;
set t11;
minscore=minscore*1000;format minscore 8.0;
maxscore=maxscore*1000;format maxscore 8.0;
predrespr=meanscore*100;format predrespr 8.1;
meanscore=meanscore*1000;format meanscore 8.0;
run;
data total11(keep=cumchi_sq cumgini);
set t3;
cumchisq+chi_sq;
cumgini+gini;
if _n_=&group;
run;
data total111;
set t5(keep=ks);
run;
data total1111;
set t7(keep=gof);
run;
data total;
merge total total1 total11 total111 total1111;
rename cumchi_sq=chi_sq;
rename cumgini=gini;
run;

data t3(drop=decile);
set t3 total ;
if _n_=&group+1 then decile=9999;
decile1=put(decile,8.);format decile1 $8.;
run;
data t3;
set t3;
rename decile1=decile;
decile1=left(trim(decile1));
if decile1="9999" then decile1="Total";
run;
proc print data = t3 noobs;
title "&title";
var decile nonresp resp actrespr predrespr minscore maxscore meanscore overall_odds info_odds log_odds
prob_nonresp chi_sq pernonresp perresp cumpernonresp cumperresp perobs cumperobs cumrespr lift cumlift gini ks gof;
run;

proc print data = mrg noobs;
var ranking sat_rank ks maxksdec gini gof;
run;

%mend;
/*%ksgcf(tmp,newpred,resp,10,KS Table);*/

