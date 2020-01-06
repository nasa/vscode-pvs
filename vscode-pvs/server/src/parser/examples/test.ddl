% simple assignment
%x := x + 1 

% parallel assignment
%x := y, y := x

% sequential assignment
%x := y; y := x; z := y

% loops
%(A)*
%[A]P
%<A>P
%(x:=x+y; y:=x-2*y)*

% any
%x := ANY(y: real | P(y))

% discrete program loop
(x>=8) AND (y>=0) AND (y<=5)
     |- [(x:=x+y; y:=x-2y)*](x>=0)