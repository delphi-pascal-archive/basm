{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-* Win-Assembler *+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}

Unit StrTools;

Interface

FUNCTION RtJustify(s:String; L:Byte):String;
function LtJustify(s:String;n:byte):String;

Implementation

{##########################################################################}
FUNCTION RtJustify(s:String; L:Byte):String;
 Var    i   : byte;
begin
 if L > Length(s) then begin
   for i:=Length(s)+1 to L do S:=S+' ';
  End;
 RtJustify:=S;
end;

function LtJustify(s:String;n:byte):String;
 var i:byte;
begin
 result:='';
 for i:=1 to n-length(s) do result:=result+' ';
 result:=result+s;
end;

end.
