uses EM;

function PrintInsert<T>(self: T): T; extensionmethod;
begin
  Writeln('='*50);
  Result := self;
end;

begin
  try
    var s23 := Arr(
      ExprNode.FromStr('r.00*s12.x + r.01*s12.y + r.02*s12.z'),
      ExprNode.FromStr('r.10*s12.x + r.11*s12.y + r.12*s12.z'),
      ExprNode.FromStr('r.20*s12.x + r.21*s12.y + r.22*s12.z')
    );
    
    s23 := s23.ConvertAll(e->e
      .Println.PrintInsert
      
      .ReplaceVar('s12.x', 'x / sqrt(x*x+1)')
      .ReplaceVar('s12.y', '0')
      .ReplaceVar('s12.z', '1 / sqrt(x*x+1)')
      .Println.PrintInsert
      
      .ReplaceVar('r.00', '1 + k2*( -u.val2*u.val2 - u.val1*u.val1 )')
      .ReplaceVar('r.01', 'k1*u.val2 + k2*( u.val1*u.val0 )')
      .ReplaceVar('r.02', '-k1*u.val1 + k2*( u.val2*u.val0 )')
      .ReplaceVar('r.10', '-k1*u.val2 + k2*( u.val0*u.val1 )')
      .ReplaceVar('r.11', '1 + k2*( -u.val2*u.val2 - u.val0*u.val0 )')
      .ReplaceVar('r.12', 'k1*u.val0 + k2*( u.val2*u.val1 )')
      .ReplaceVar('r.20', 'k1*u.val1 + k2*( u.val0*u.val2 )')
      .ReplaceVar('r.21', '-k1*u.val0 + k2*( u.val1*u.val2 )')
      .ReplaceVar('r.22', '1 + k2*( -u.val1*u.val1 - u.val0*u.val0 )')
      .Println.PrintInsert
      
      .ReplaceVar('k1', 'sin(a)')
      .ReplaceVar('k2', '2*sqr(sin(a/2))')
      .Println.PrintInsert
      
      .ReplaceVar('u.val0', 'x/(x*x+1)*2')
      .ReplaceVar('u.val1', '0')
      .ReplaceVar('u.val2', '1/(x*x+1)*2 - 1')
      .Println.PrintInsert
      
      .FullOptimize
      .PrintInsert.PrintInsert.PrintInsert
    );
    
    var test := ExprNode.FromStr('sqr(v1)+sqr(v2)+sqr(v3)')
      .ReplaceVar('v1', s23[0])
      .ReplaceVar('v2', s23[1])
      .ReplaceVar('v3', s23[2])
    ;
    
    // проверка кода на баги
    // s23 нормализован, а значит квадрат его длины должен быть 1, не зависимо от значений a и x
    foreach var a in Range(0,30).Select(i->i/10) do
      foreach var x in Range(0,30).Select(i->i/10) do
      begin
        var res := test.Calc(
          ('a'+'', a),
          ('x'+'', x)
        );
        if abs(res-1) < 0.001 then continue;
        Writeln($'a = {a}');
        Writeln($'x = {x}');
        Writeln($'res = {res}');
        ''.PrintInsert;
      end;
    Writeln('done');
    
  except
    on e: Exception do
    begin
      Writeln(e);
      Readln;
    end;
  end;
end.