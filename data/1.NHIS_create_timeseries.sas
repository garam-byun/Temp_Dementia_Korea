/*-----------------------------------------------------------
  Build Case-Crossover Dataset for Dementia Admissions Only
  → Output: Daily province-level counts (Poisson regression-ready)
-----------------------------------------------------------*/

libname source '/userdata06/room051/data_source';
libname temp   '/userdata06/room051/working/temp';

/*-----------------------------------------------------------
  1. Load and Filter Admissions
-----------------------------------------------------------*/
data temp.gy20_t1_all;
  set source.nhis_senior_gy20_:_t1;
  where FORM_CD in ("02", "07", "10");  /* Admission only */
  date = input(RECU_FR_DT, yymmdd10.);
run;

/*-----------------------------------------------------------
  2. Identify Dementia Admissions (DM_def_1 and subtype)
-----------------------------------------------------------*/
data temp.dm_admit_raw;
  set temp.gy20_t1_all;

  length DM_def_1 3 subtype $2;
  DM_def_1 = 0; subtype = "";

  if substr(MAIN_SICK, 1, 3) in ("F00", "F01", "F02", "F03", "G30") or
     substr(MAIN_SICK, 1, 4) in ("G310", "G311", "G318") or
     substr(SUB_SICK, 1, 3) in ("F00", "F01", "F02", "F03", "G30") or
     substr(SUB_SICK, 1, 4) in ("G310", "G311", "G318") then do;

    DM_def_1 = 1;

    if substr(MAIN_SICK, 1, 3) in ("F00", "G30") or substr(SUB_SICK, 1, 3) in ("F00", "G30") then subtype = "AD";
    else if substr(MAIN_SICK, 1, 3) = "F01" or substr(SUB_SICK, 1, 3) = "F01" then subtype = "VS";
    else subtype = "OT";
  end;
run;

/*-----------------------------------------------------------
  3. Remove Repeat Admissions Within 31 Days
-----------------------------------------------------------*/
proc sort data=temp.dm_admit_raw; by PERSON_ID date; run;

data temp.dm_admit_clean;
  set temp.dm_admit_raw;
  format end_date lag_end date9.;
  end_date = date + VSCN;
  lag_end = lag(end_date);

  if PERSON_ID = lag(PERSON_ID) then do;
    gap = date - lag_end;
    if -99 <= gap <= 31 then delete;
  end;
run;

/*-----------------------------------------------------------
  4. Construct Case Days (DM Admissions Only)
-----------------------------------------------------------*/
data case;
  set temp.dm_admit_clean;
  event = 1;
  weekday = weekday(date);
run;

/*-----------------------------------------------------------
  5. Construct Matched Control Days (Same Month + Weekday)
-----------------------------------------------------------*/
data controls;
  set case;
  do control_date = intnx('month', date, 0, 'beginning') to intnx('month', date, 0, 'end');
    if weekday(control_date) = weekday and day(control_date) ne day(date) then do;
      date = control_date;
      event = 0;
      output;
    end;
  end;
  drop control_date;
run;

data temp.caco_dm_admit;
  set case controls;
run;

proc sort data=temp.caco_dm_admit; by SIDO date; run;

/*-----------------------------------------------------------
  6. Aggregate for Poisson Regression (Province × Date)
-----------------------------------------------------------*/
proc sql;
  create table temp.ts_dm as
  select SIDO, date,
    sum(event = 1 and DM_def_1 = 1) as DM_def_1_all,
    sum(event = 1 and DM_def_1 = 1 and subtype = "AD") as DM_def_1_AD,
    sum(event = 1 and DM_def_1 = 1 and subtype = "VS") as DM_def_1_VS,
    sum(event = 1 and DM_def_1 = 1 and subtype = "OT") as DM_def_1_OT
  from temp.caco_dm_admit
  group by SIDO, date;
quit;
