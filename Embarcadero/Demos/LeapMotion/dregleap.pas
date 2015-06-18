unit dregleap;

interface

Procedure Register;

implementation

uses classes,wsleap;

{$R dleapres.res}

Procedure Register;

begin
  RegisterComponents('Leap Motion',[TWebSocketLeapController])
end;

end.
 