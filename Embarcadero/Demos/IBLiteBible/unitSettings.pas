unit unitSettings;

interface

type

  TSettings = class
  private
    FFontSize: Integer;
    procedure SetFontSize(const Value: Integer);
  public
    property FontSize : Integer read FFontSize write SetFontSize;
  end;

  TSettingsIniHelper = class helper for TSettings
    function IniFileName : string;
    procedure Save;
    procedure Load;
  end;


implementation

uses INIFiles, IOUtils, SysUtils;

{ TSettings }

procedure TSettings.SetFontSize(const Value: Integer);
begin
  FFontSize := Value;
end;

{ TSettingsHelper }

function TSettingsIniHelper.IniFileName: string;
begin
  Result := TPath.GetDocumentsPath+PathDelim+'bible.ini';
end;

procedure TSettingsIniHelper.Load;
var
  Ini : Tinifile;
begin
  Ini := TIniFile.Create(IniFileName);
  try
    FontSize := Ini.ReadInteger('Settings','FontSize',80);
  finally
    Ini.Free;
  end;
end;

procedure TSettingsIniHelper.Save;
var
  Ini : Tinifile;
begin
  Ini := TIniFile.Create(IniFileName);
  try
    Ini.WriteInteger('Settings','FontSize',FontSize);
  finally
    Ini.Free;
  end;
end;

end.
