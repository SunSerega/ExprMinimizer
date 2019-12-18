uses EM;

begin
  ExprNode.otp_calc_v['a'] := Pi*2/5;
  ExprNode.otp_calc_v['x'] := sqrt(1 - 2*cos(Pi*2/5));
//  ExprNode.force_br := true;
  
  var e := ExprNode.FromStr(
    'sin(-1*a)*x*cos(a)/sqrt( 1+ x*x  ) + x*sin(a)/sqrt( 1+ x*x  ) + 4*sin(-0.5*a)*sin(-0.5*a)*x*sin(a)/(1+ x*x )/sqrt( 1+ x*x  ) - 2*sin(-0.5*a)*sin(-0.5*a)*x*sin(a)/sqrt( 1+ x*x  )'
  );
  
  e.Calc(ExprNode.otp_calc_v).Println;
  
end.