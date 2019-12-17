{$reference System.Windows.Forms.dll}
{$apptype windows}

{$region NodeTypes}

type
  ExprNode = abstract class
    k: real;
    
    static otp := true;
    static otp_x1 := true;
    static force_br := false;
    
    constructor(k: real) :=
    self.k := k;
    constructor := Create(1);
    
    function Clone: ExprNode; abstract;
    
    function Optimize: ExprNode; abstract;
    function FullOptimize: ExprNode;
    begin
      Result := self;
      var last: ExprNode;
      
      while not Result.IsSame(last) do
      begin
        {$region Debug}
        if otp then
        begin
          Result.Reconstruct.Println;
          if otp_x1 then
          begin
            otp := false;
            Result.ReplaceVar('x','1').FullOptimize.Reconstruct.Println;
            otp := true;
          end;
        end;
        {$endregion Debug}
        last := Result;
        Result := Result.Optimize;
      end;
      
    end;
    
    function IsSame(n: ExprNode): boolean; abstract;
    function IsContentSame(n: ExprNode): boolean; abstract;
    
    function Reconstruct(br_req: integer := 0): string; abstract;
    
    static function FromStr(s: string): ExprNode;
    
    function MltK(k2: real): ExprNode;
    begin
      Result := self.Clone;
      Result.k *= k2;
    end;
    
    function ConvK(f: real->real): ExprNode;
    begin
      Result := self.Clone;
      Result.k := f(Result.k);
    end;
    
    function LiteralEq(val: real); virtual := false;
    
    function ReplaceVar(vn: string; e: ExprNode): ExprNode; abstract;
    function ReplaceVar(vn: string; e: string) := ReplaceVar(vn, FromStr(e));
    
    property DebugStr: string read $'{self.GetType}[{Reconstruct()}]';
    
    function EnmrExpr: sequence of ExprNode; virtual :=
    new ExprNode[](self);
    
  end;
  
  BaseNode = sealed class(ExprNode)
    vname: string := nil;
    
    function Clone: ExprNode; override;
    begin
      var res := new BaseNode(self.k);
      res.vname := self.vname;
      Result := res;
    end;
    
    function Optimize: ExprNode; override := self;
    
    function IsSame(n: ExprNode): boolean; override :=
    (n is BaseNode(var tn)) and
    (tn.k=self.k) and (tn.vname=self.vname);
    
    function IsContentSame(n: ExprNode): boolean; override :=
    (n is BaseNode(var tn)) and
    (tn.vname=self.vname);
    
    function Reconstruct(br_req: integer := 0): string; override;
    begin
      var res := new List<string>;
      if k<>1 then res += FloatToStr(k);
      if vname<>nil then res += vname;
      Result := res.JoinIntoString('*');
      if Result='' then Result := '1';
    end;
    
    function LiteralEq(val: real): boolean; override :=
    (vname=nil) and (k=val);
    
    function ReplaceVar(vn: string; e: ExprNode): ExprNode; override :=
    self.vname=vn ? e.MltK(self.k) : self;
    
  end;
  
  ContNode = abstract class(ExprNode)
    pns := new List<ExprNode>;
    nns := new List<ExprNode>;
    
    procedure Add(positive: boolean; n: ExprNode); abstract;
    
    function IsSame(n: ExprNode): boolean; override :=
    (n?.GetType = self.GetType) and (n is ContNode(var tn)) and
    (tn.k = self.k) and
    tn.pns.ZipTuple(self.pns).All(t->t[0].IsSame(t[1])) and
    tn.nns.ZipTuple(self.nns).All(t->t[0].IsSame(t[1]));
    
    function IsContentSame(n: ExprNode): boolean; override;
    begin
      var tn := n as ContNode;
      if tn=nil then exit;
      if n?.GetType <> self.GetType then exit;
      if tn.pns.Count<>self.pns.Count then exit;
      if tn.nns.Count<>self.nns.Count then exit;
      
      var l := tn.pns.ToList;
      foreach var sn in self.pns do
      begin
        var ind := l.FindIndex(n2->sn.IsSame(n2));
        if ind=-1 then exit;
        l.RemoveAt(ind);
      end;
      
      l := tn.nns.ToList;
      foreach var sn in self.nns do
      begin
        var ind := l.FindIndex(n2->sn.IsSame(n2));
        if ind=-1 then exit;
        l.RemoveAt(ind);
      end;
      
      Result := true;
    end;
    
    function ReplaceVar(vn: string; e: ExprNode): ExprNode; override;
    begin
      var res := self.Clone as ContNode;
      res.pns.Transform(n->n.ReplaceVar(vn, e));
      res.nns.Transform(n->n.ReplaceVar(vn, e));
      Result := res;
    end;
    
    function EnmrExpr: sequence of ExprNode; override;
    begin
      yield self;
      yield sequence pns.SelectMany(n->n.EnmrExpr);
      yield sequence nns.SelectMany(n->n.EnmrExpr);
    end;
    
  end;
  
  SumNode = sealed class(ContNode)
    
    procedure Add(positive: boolean; n: ExprNode); override;
    begin
      if n.k=0 then exit;
      
      if n is SumNode(var s) then
      begin
        foreach var sn in s.pns do Add(positive, sn.MltK(n.k));
        foreach var sn in s.nns do Add(not positive, sn.MltK(n.k));
        exit;
      end;
      
      if n.k<0 then
      begin
        n := n.MltK(-1);
        positive := not positive;
      end;
      
      var main_lst := positive?pns:nns;
      var sec_lst := positive?nns:pns;
      
      var pn := main_lst.Find(n2->n.IsContentSame(n2));
      if pn<>nil then
      begin
        pn.k += n.k;
        exit;
      end;
      
      pn := sec_lst.Find(n2->n.IsContentSame(n2));
      if pn<>nil then
      begin
        pn.k -= n.k;
        exit;
      end;
      
      main_lst += n;
    end;
    
    function Clone: ExprNode; override;
    begin
      var res := new SumNode(self.k);
      foreach var sn in pns do res.pns += sn.Clone;
      foreach var sn in nns do res.nns += sn.Clone;
      Result := res;
    end;
    
    function Optimize: ExprNode; override;
    begin
      var res := new SumNode(self.k);
      foreach var n in self.pns do res.Add(true, n.Optimize);
      foreach var n in self.nns do res.Add(false, n.Optimize);
      
      res.pns.RemoveAll(n->n.LiteralEq(0));
      res.nns.RemoveAll(n->n.LiteralEq(0));
      
      var l := new BaseNode(0);
      for var i := res.pns.Count-1 downto 0 do
        if (res.pns[i] is BaseNode(var tn)) and (tn.vname=nil) then
        begin
          res.pns.RemoveAt(i);
          l.k += tn.k;
        end;
      if l.k<>0 then res.pns.Insert(0,l);
      
      if (res.pns.Count=0) and (res.nns.Count=0) then
        Result := new BaseNode(0) else
      if (res.pns.Count=1) and (res.nns.Count=0) then
        Result := res.pns.Single.MltK(res.k) else
      if (res.pns.Count=0) and (res.nns.Count=1) then
        Result := res.nns.Single.MltK(-res.k) else
        Result := res;
    end;
    
    function Reconstruct(br_req: integer := 0): string; override;
    begin
      var res := new StringBuilder;
      if k<>1 then
      begin
        res += FloatToStr(k);
        res += '*';
      end;
      var br := force_br or (br_req>0) or (k<>1);
      res += br ? '(' : ' ';
      res += pns.Select(n->n.Reconstruct).JoinIntoString('+');
      foreach var n in nns do
      begin
        res += '-';
        res += n.Reconstruct;
      end;
      res += br ? ')' : ' ';
      Result := res.ToString;
    end;
    
  end;
  
  MltNode = sealed class(ContNode)
    
    procedure Add(positive: boolean; n: ExprNode); override;
    begin
      n := n.Clone;
      if positive then
        self.k *= n.k else
        self.k /= n.k;
      if (n is BaseNode(var tn)) and (tn.vname=nil) then exit;
      n.k := 1;
      
      if n is MltNode(var m) then
      begin
        foreach var sn in m.pns do Add(positive, sn);
        foreach var sn in m.nns do Add(not positive, sn);
        exit;
      end;
      
      (positive?pns:nns).Add(n);
      
    end;
    
    function Clone: ExprNode; override;
    begin
      var res := new MltNode(self.k);
      foreach var sn in pns do res.pns += sn.Clone;
      foreach var sn in nns do res.nns += sn.Clone;
      Result := res;
    end;
    
    function Optimize: ExprNode; override;
    begin
      var res := new MltNode(self.k);
      foreach var n in self.pns do res.Add(true, n.Optimize);
      foreach var n in self.nns do res.Add(false, n.Optimize);
      
      if res.pns.Concat(res.nns).Any(n->n.LiteralEq(0)) then
      begin
        Result := new BaseNode(0);
        exit;
      end;
      
      res.pns.RemoveAll(n->n.LiteralEq(1));
      res.nns.RemoveAll(n->n.LiteralEq(1));
      
      if (res.pns.Count=0) and (res.nns.Count=0) then
        Result := new BaseNode(res.k) else
      if (res.pns.Count=1) and (res.nns.Count=0) then
        Result := res.pns.Single.MltK(res.k) else
      begin
        var ind := res.pns.FindIndex(n->n is SumNode);
        
        if ind<>-1 then
        begin
          var s := res.pns[ind] as SumNode;
          res.pns.RemoveAt(ind);
          var sres := new SumNode;
          foreach var n in s.pns do
          begin
            var mn := res.Clone as MltNode;
            mn.pns.Insert(ind,n);
            sres.Add(true,mn);
          end;
          foreach var n in s.nns do
          begin
            var mn := res.Clone as MltNode;
            mn.pns.Insert(ind,n);
            sres.Add(false,mn);
          end;
          Result := sres;
        end else
          Result := res;
        
      end;
    end;
    
    function Reconstruct(br_req: integer := 0): string; override;
    begin
      var res := new StringBuilder;
      res += force_br ? '(' : ' ';
      var sq := pns.Select(n->n.Reconstruct(1));
      if k<>1 then sq := sq.Prepend(FloatToStr(k));
      if sq.Any then
        res += sq.JoinIntoString('*') else
        res += '1';
      foreach var n in nns do
      begin
        res += '/';
        res += n.Reconstruct(1);
      end;
      res += force_br ? ')' : ' ';
      Result := res.ToString;
    end;
    
  end;
  
  FuncNode = sealed class(ContNode)
    fname: string;
    
    procedure Add(positive: boolean; n: ExprNode); override :=
    pns += n;
    
    function Clone: ExprNode; override;
    begin
      var res := new FuncNode(self.k);
      res.fname := self.fname;
      foreach var sn in pns do res.pns += sn.Clone;
      Result := res;
    end;
    
    function Optimize: ExprNode; override;
    begin
      var res := new FuncNode(self.k);
      res.fname := self.fname;
      foreach var n in self.pns do res.pns += n.Optimize;
      
      case res.fname.ToLower of
        
        'sqr':
        begin
          if res.pns.Count<>1 then raise new Exception($'invalid use of {res.fname}: >>> {res.Reconstruct()} <<<');
          var res2 := new MltNode(res.k);
          res2.Add(true,res.pns.Single.Clone);
          res2.Add(true,res.pns.Single);
          Result := res2;
          exit;
        end;
        
      end;
      
      Result := res;
    end;
    
    function Reconstruct(br_req: integer := 0): string; override;
    begin
      var res := new StringBuilder;
      if k<>1 then
      begin
        res += FloatToStr(k);
        res += '*';
      end;
      res += fname;
      res += '(';
      res += pns.Select(n->n.Reconstruct).JoinIntoString(',');
      res += ')';
      Result := res.ToString;
    end;
    
  end;
  
{$endregion NodeTypes}

{$region Parser}

{$string_nullbased+}
function FindOuterCh(self: string; params chs: array of char): sequence of integer; extensionmethod;
begin
  var bl := 0;
  for var i := 0 to self.Length-1 do
    if self[i]='(' then
      bl += 1 else
    if self[i]=')' then
      bl -= 1 else
    if (bl=0) and (self[i] in chs) then
      yield i;
end;

static function ExprNode.FromStr(s: string): ExprNode;
begin
//  Writeln($'+[{s}]');
  var inds := s.FindOuterCh('+','-');
  if inds.Any then
  begin
    var res := new SumNode;
    var last_ind := 0;
    var last_positive := true;
    foreach var ind in inds do
    begin
      var ss := s.Substring(last_ind,ind-last_ind);
      if not string.IsNullOrWhiteSpace(ss) then res.Add(last_positive, FromStr(ss));
      last_ind := ind+1;
      last_positive := s[ind] = '+';
    end;
    var ss := s.Substring(last_ind);
    if not string.IsNullOrWhiteSpace(ss) then res.Add(last_positive, FromStr(ss));
    Result := res;
//    Writeln($'-[{s}]');
    exit;
  end;
  
  inds := s.FindOuterCh('*','/');
  if inds.Any then
  begin
    var res := new MltNode;
    var last_ind := 0;
    var last_positive := true;
    foreach var ind in inds do
    begin
      var ss := s.Substring(last_ind,ind-last_ind);
      if not string.IsNullOrWhiteSpace(ss) then res.Add(last_positive, FromStr(ss));
      last_ind := ind+1;
      last_positive := s[ind] = '*';
    end;
    var ss := s.Substring(last_ind);
    if not string.IsNullOrWhiteSpace(ss) then res.Add(last_positive, FromStr(ss));
    Result := res;
//    Writeln($'-[{s}]');
    exit;
  end;
  
  s := s.Trim;
  if s[0]='(' then
  begin
    if not s.EndsWith(')') then raise new Exception(s);
    Result := FromStr(s.Substring(1,s.Length-2));
//    Writeln($'-[{s}]');
    exit;
  end;
  
  var ind := s.IndexOf('(');
  if ind<>-1 then
  begin
    if not s.EndsWith(')') then raise new Exception(s);
    var res := new FuncNode;
    res.fname := s.Remove(ind);
    foreach var ss in s.Substring(ind+1,s.Length-ind-2).Split(',') do
      res.Add(true, FromStr(ss));
    Result := res;
//    Writeln($'-[{s}]');
    exit;
  end;
  
  var res := new BaseNode;
  if s[0].IsDigit then
  begin
    res.k := s.ToReal;
    res.vname := nil;
  end else
  begin
    res.k := 1;
    res.vname := s;
  end;
  Result := res;
//  Writeln($'-[{s}]');
end;

{$endregion Parser}

begin
  try
//    ExprNode.otp := false;
//    ExprNode.FromStr('sqr(x/(x*x+1))+sqr(1/(x*x+1)-1)').ReplaceVar('x','1').FullOptimize.Reconstruct.Println;
//    exit;
    
//    var e := ExprNode.FromStr('+x*( 1-2*sqr(sin(a/2))*( u.val2*u.val2 + u.val1*u.val1 ) ) + 1*( -sin(a)*u.val1 + 2*sqr(sin(a/2))*( u.val2*u.val0 ) )');
    var e := ExprNode.FromStr('sqr(x*2/(x*x+1))+sqr(2/(x*x+1)-1)');
    e := e.FullOptimize;
    e.Reconstruct.Println;
    
    ExprNode.otp := false;
    
    Writeln('='*50);
    
//    foreach var se in e.EnmrExpr do
//      $'>>> {se.Reconstruct()} ::: {se.ReplaceVar(''x'',''1'').FullOptimize.Reconstruct()} <<<'.Println;
//    Writeln('='*50);
    
    e.ReplaceVar('x','1').FullOptimize.Reconstruct.Println;
    
    System.Windows.Forms.Clipboard.SetText(e.Reconstruct);
    System.Console.Beep;
  except
    on e: Exception do
    begin
      Writeln(e);
      Readln;
    end;
  end;
end.