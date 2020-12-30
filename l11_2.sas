options nocenter nonumber nodate;
goptions reset=global gunit=pct border cback=white
         colors=(black blue green red)
         ftitle=swiss ftext=simplex htitle=6 htext=4; 

filename in1 'C:\Users\ariel\Documents\BU_RCode\BS820-RCode\data\Oscar01.xlsx';

proc format;
     value groupf 1 = 'Oscar Winners'
                  2 = 'Oscar Nominee'
                  3 = 'Control';
run;

proc import OUT= WORK.zero 
            DATAFILE= in1
            DBMS=xlsx;
run;  

ods rtf file=OSAnal bodytitle style=journal;

data one;
     set zero;

     if ((identity ne 1075) and (identity ne 1430));

     age_at_end = Final - Birth;
     age_at_first_film = Film - Birth;
     age_at_first_nom  = Nom - Birth;
     age_at_first_win  = Win - Birth;

     if (Wins ge 1) then group = 1;
     else if (Noms ge 1) then group = 2;
     else group = 3;

     if (born_USA eq 'Yes') then born_USA_n = 1;
     else if (born_USA eq 'No') then born_USA_n = 0;

     if (white eq 'Yes') then white_n = 1;
     else if (white eq 'No') then white_n = 0;

     if (name_change eq 'Yes') then name_change_n = 1;
     else if (name_change eq 'No') then name_change_n = 0;
      
     if (drama eq 'Yes') then drama_n = 1;
     else if (drama eq 'No') then drama_n = 0;
run;

proc phreg data=one;
     model age_at_end*alive(1) = win_time nom_time male born_USA_n 
           white_n name_change_n drama_n birth / ties=exact rl;

	 if (age_at_first_win eq .) then win_time = 0;
     else if (age_at_first_win > age_at_end) then win_time = 0;
     else win_time = 1;

     if (age_at_first_nom eq .) then nom_time = 0;
     else if (age_at_first_nom > age_at_end) then nom_time = 0;
     else nom_time = 1; 
	 
	 title1 'Time-Varying Oscar Status';
run;

proc phreg data=one;
     model (age_at_first_film, age_at_end)*alive(1) = win_time nom_time male born_USA_n 
           white_n name_change_n drama_n birth / ties=exact rl;

	 if (age_at_first_win eq .) then win_time = 0;
     else if (age_at_first_win > age_at_end) then win_time = 0;
     else win_time = 1;

     if (age_at_first_nom eq .) then nom_time = 0;
     else if (age_at_first_nom > age_at_end) then nom_time = 0;
     else nom_time = 1; 
	 
	 title1 'Time-Varying Oscar Status, Time in Films';
run;
