%-----------------------------
% examples from DDL/test.pvs
%-----------------------------

(::) |- SOMERUNS(SEQ(ANY(x, DLRANDOM), ANY(y, DLRANDOM)), val(x) = val(y))

(::) |- SOMERUNS(SEQ(ANY(x, DLRANDOM), ANY(y, DLRANDOM)), val(x) /= val(y))

(::) |- NOT ALLRUNS(SEQ(ANY(x, DLRANDOM), ANY(y, DLRANDOM)), val(x) = val(y))

DLFORALL(LAMBDA(X: real): val(x) >= X + cnst(1)) |- DLFORALL(LAMBDA(X: real): val(x) >= X)

DLEXISTS(LAMBDA(X: real): val(x) >= X + cnst(1)) |- DLEXISTS(LAMBDA(X: real): val(x) >= X) 

LET hp: HP = DIFF( (: (x, val(x)), (y, -val(y)) :), DLTRUE )
IN (val(x) * val(y) - 1 = 0) |- ALLRUNS(hp, val(x) * val(y) = 1)

val(x) > 0 |- ALLRUNS(DIFF((: (x, -val(x)) :)), val(x) > 0)


%-----------------------------
% additional examples
%-----------------------------

% Single assignment
%---------------------------------
%    x>=0 |- [x:=2x + 1] (x>=0)
%---------------------------------
val(x) >= cnst(0)
     |- ALLRUNS(ASSIGN((: (x, cnst(2) * x + cnst(1)) :)), (val(x) >= cnst(0)))


% Assignment with star
%---------------------------------
%    (x>=0) |- [(x:=x + 1)*] (x>=0)
%---------------------------------
(val(x) >= cnst(0))
     |- ALLRUNS(STAR(ASSIGN((: (x, val(x) + cnst(1)) :))), (val(x) >= cnst(0)))


% Assignment with test
%---------------------------------
%    (x=0) |- [?x>0;x:=x + 1] (x=0)
%---------------------------------
(val(x) = cnst(0))
     |- ALLRUNS(SEQ(TEST(val(x) > cnst(0)), ASSIGN((: (x, val(x) + cnst(1)) :))), (val(x) = cnst(0)))


% test with AND
%---------------------------------
%    (x > 0) AND (y > 0) |- [?x < 5 AND y < 3; x := x +1; y:= y + 1] (x > 0 AND y > 0)
%---------------------------------
(val(x) > cnst(0)) AND (val(y) > cnst(0))
     |- ALLRUNS(SEQ(TEST(val(x) < cnst(5) AND val(y) < cnst(3)), SEQ(ASSIGN((: (x, val(x) + cnst(1)) :)), ASSIGN((: (y, val(y) + cnst(1)) :)))), (val(x) > cnst(0) AND val(y) > cnst(0)))


% Unions
%---------------------------------
%    (x > 0) AND (y > 0) |- [(a:=1 ++ a:=-1 ++ a:=1; x := x +1; y:= 3y + 1)*] (x > 0 AND y > 0)
%---------------------------------
(val(x) > cnst(0)) AND (val(y) > cnst(0))
     |- ALLRUNS(STAR(SEQ(UNION(ASSIGN((: (a, cnst(1)) :)), UNION(ASSIGN((: (a, cnst(-1)) :)), ASSIGN((: (a, cnst(1)) :)))), SEQ(ASSIGN((: (x, val(x) + cnst(1)) :)), ASSIGN((: (y, cnst(3) * y + cnst(1)) :))))), (val(x) > cnst(0) AND val(y) > cnst(0)))


% Differential equations
%---------------------------------
%    (x > 0) AND (y > 0) |- [x'= y, y'=x*y] (x > 0 AND y >0)
%---------------------------------
(val(x) > cnst(0)) AND (val(y) > cnst(0))
     |- ALLRUNS(DIFF((: (x, val(y)), (y, val(x) * val(y)) :)), (val(x) > cnst(0) AND val(y) > cnst(0)))


% Paralllel assignment
%---------------------------------
%    (x > 0) AND (y > 0) |- [x := y, y := x*y] (x > 0 AND y >0)
%---------------------------------
(val(x) > cnst(0)) AND (val(y) > cnst(0))
     |- ALLRUNS(ASSIGN((: (x, val(y)), (y, val(x) * val(y)) :)), (val(x) > cnst(0) AND val(y) > cnst(0)))


% Test any
%---------------------------------
%   (x > 0) AND (y > 0) |- [w:=*; x'= x + w] (x > 0)
%---------------------------------
(val(x) > cnst(0)) AND (val(y) > cnst(0))
     |- ALLRUNS(SEQ(ANY(w, DLRANDOM), DIFF((: (x, val(x) + val(w)) :), DLTRUE)), (val(x) > cnst(0)))

