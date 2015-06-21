unit fOpen;

interface

uses
{$IFDEF MSWINDOWS}
  Winapi.ShellAPI, Winapi.Windows;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  OpenViewUrl;
{$ENDIF POSIX}

type
  TMisc = class
    class procedure Open(sCommand: string);
  end;

implementation

class procedure TMisc.Open(sCommand: string);
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'OPEN', PChar(sCommand), '', '', SW_SHOWNORMAL);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
   OpenURL(sCommand);
{$ENDIF POSIX}
end;

end.