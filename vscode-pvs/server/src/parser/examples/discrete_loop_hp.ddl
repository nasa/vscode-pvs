discrete_loop_hp: THEORY
BEGIN

  IMPORTING dynamic_logic

  %--- Definition of syntactic sugar for variables and assignments
  x : nat = 0
  y : nat = 1
  %--- 
  
  %---------------------------------
  % Simple discrete loop program
  %
  % (x>=8) /\ (y>=0) /\ (y<=5)
  %   --> [(x:=x+y; y:=x-2y)*](x>=0)
  %
  %---------------------------------
   
  discrete_loop: PROBLEM
     (x>=8) AND (y>=0) AND (y<=5)
     |- [(x':=x+y; x':=x+y; x:=x+y; y:=x-2y)*] (x>=0)

%  discrete_loop: LEMMA
%     val(x) >= 8 AND val(y) >= 0 AND val(y) <= 5
%  |- ALLRUNS(STAR(SEQ(ASSIGN((:(x,val(x)+val(y)):)),
%                      ASSIGN((:(y,val(x)-2*val(y)):)))),
%     val(x) >= 0)


%|- discrete_loop : PROOF
%|- (spread (dl-loop "val(x) >= val(y) AND val(y) >= 0")
%|-  ((dl-grind) (dl-grind) (dl-assert)))
%|- QED

END discrete_loop_hp
