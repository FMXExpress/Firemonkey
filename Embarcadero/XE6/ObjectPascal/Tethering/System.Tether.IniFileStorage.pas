unit System.Tether.IniFileStorage;

interface

uses
  System.Generics.Collections, System.IniFiles, System.SysUtils, System.Classes,
  System.Tether.Manager;

type

  TTetheringIniFileStorage = class(TTetheringCustomStorage)
  private
    const RemoteSection = 'Remote Managers';  // do not localize
    const LocalSection = 'Local Managers';  // do not localize
    const KeyIdentifier = 'Identifier';  // do not localize
  private
    FIniFile: TIniFile;
  public
    constructor Create(const AStorage: TFileName);
    destructor Destroy; override;
    function LoadManagerGUID: string; override;
    procedure SaveManagerGUID(const AIdentifier: string); override;
    procedure LoadRemoteManagersGUIDs(const AGUIDPassList: TStringList); override;
    procedure SaveRemoteManagersGUIDs(const AGUIDPassList: TStringList); override;
  end;

implementation

uses
  System.IOUtils;

{ TTetheringIniFileStorage }

constructor TTetheringIniFileStorage.Create(const AStorage: TFileName);
begin
  FIniFile := TIniFile.Create(string(AStorage));
end;

destructor TTetheringIniFileStorage.Destroy;
begin
  FIniFile.Free;
end;

function TTetheringIniFileStorage.LoadManagerGUID: string;
begin
  Result := FIniFile.ReadString(LocalSection, KeyIdentifier, '');  // do not localize
  if Result = '' then
  begin
    Result := TGUID.NewGuid.ToString;
    SaveManagerGUID(Result);
  end;
end;

procedure TTetheringIniFileStorage.LoadRemoteManagersGUIDs(const AGUIDPassList: TStringList);
begin
  FIniFile.ReadSectionValues(RemoteSection, AGUIDPassList);
end;

procedure TTetheringIniFileStorage.SaveManagerGUID(const AIdentifier: string);
begin
  FIniFile.WriteString(LocalSection, KeyIdentifier, AIdentifier); // do not localize
  FIniFile.UpdateFile;
end;

procedure TTetheringIniFileStorage.SaveRemoteManagersGUIDs(const AGUIDPassList: TStringList);
var
  I: Integer;
begin

  if AGUIDPassList <> nil then
  begin
    FIniFile.EraseSection(RemoteSection);
    for I := 0 to AGUIDPassList.Count - 1 do
      if AGUIDPassList.Names[I] <> '' then
       FIniFile.WriteString(RemoteSection, AGUIDPassList.Names[I], AGUIDPassList.ValueFromIndex[I]);
    FIniFile.UpdateFile;
  end;
end;

end.
