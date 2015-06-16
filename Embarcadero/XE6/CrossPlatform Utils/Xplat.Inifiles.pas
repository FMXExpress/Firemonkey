unit Xplat.Inifiles;

interface

uses
  Xplat.Services, System.Classes, System.IniFiles;

type
  TXplatIniFile = class(TInterfacedObject, IIniFileService)
  private
    FIniFile: TCustomIniFile;
    FFreeIni: Boolean;
    FUpdateAfterWrite: Boolean;
  public
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
    procedure WriteBool(const Section, Ident: string; Value: Boolean);
    function ReadString(const Section, Ident, Default: string): string;
    procedure WriteString(const Section, Ident, Value: String);
    function ReadInteger(const Section, Ident: string; Default: Integer): Integer;
    procedure WriteInteger(const Section, Ident: string; Value: Integer);
    function ReadDate(const Section, Ident: string; Default: TDateTime): TDateTime;
    procedure WriteDate(const Section, Ident: string; Value: TDateTime);
    function ReadDateTime(const Section, Ident: string; Default: TDateTime): TDateTime;
    procedure WriteDateTime(const Section, Ident: string; Value: TDateTime);
    function ReadTime(const Section, Ident: string; Default: TDateTime): TDateTime;
    procedure WriteTime(const Section, Ident: string; Value: TDateTime);
    function ReadFloat(const Section, Ident: string; Default: Double): Double;
    procedure WriteFloat(const Section, Ident: string; Value: Double);
    procedure ReadSection(const Section: string; Strings: TStrings);
    procedure ReadSections(Strings: TStrings);
    procedure ReadSectionValues(const Section: string; Strings: TStrings);
    procedure EraseSection(const Section: string);
    procedure DeleteKey(const Section, Ident: String);
    procedure UpdateFile;

    //The TIniFile implementation used by non-Windows compilers is actually
    //a TMemIniFile descendant. This means there is no explicit file write with
    //each call to Write*. To preserve the same behaviour as Windows, set
    //UpdateAfterWrite to True (which is the default).
    property UpdateAfterWrite: Boolean read FUpdateAfterWrite write FUpdateAfterWrite;

    constructor Create(AIniFile: TCustomIniFile; AFreeIni: Boolean = True;
      AUpdateAfterWrite: Boolean = {$IFDEF MSWINDOWS}False{$ELSE}True{$ENDIF}); overload;
    constructor Create; overload;
    destructor Destroy; override;
  end;


implementation

uses
  System.SysUtils, System.IOUtils;

constructor TXplatIniFile.Create(AIniFile: TCustomIniFile; AFreeIni: Boolean;
  AUpdateAfterWrite: Boolean);
begin
  inherited Create;
  FIniFile := AIniFile;
  FFreeIni := AFreeIni;
  FUpdateAfterWrite := AUpdateAfterWrite;
end;

constructor TXplatIniFile.Create;
const
  cnFileNameFmt = '%s%s%s.ini';
  cnDefaultPrefsName = 'Prefs';
var
  lFileName: string;
  lPrefsName: string;
begin
  lPrefsName := TPath.GetFileNameWithoutExtension(ParamStr(0));
  if lPrefsName = '' then
    lPrefsName := cnDefaultPrefsName;
  lFileName := Format(cnFileNameFmt, [TPath.GetDocumentsPath, TPath.DirectorySeparatorChar, lPrefsName]);
  Self.Create(TIniFile.Create(lFileName), True);
end;

procedure TXplatIniFile.DeleteKey(const Section, Ident: String);
begin
  FIniFile.DeleteKey(Section, Ident);
  if FUpdateAfterWrite then
    FIniFile.UpdateFile;
end;

destructor TXplatIniFile.Destroy;
begin
  if FFreeIni then
    FIniFile.Free;
  inherited;
end;

procedure TXplatIniFile.EraseSection(const Section: string);
begin
  FIniFile.EraseSection(Section);
  if FUpdateAfterWrite then
    FIniFile.UpdateFile;
end;

function TXplatIniFile.ReadBool(const Section, Ident: string;
  Default: Boolean): Boolean;
begin
  Result := FIniFile.ReadBool(Section, Ident, Default);
end;

function TXplatIniFile.ReadDate(const Section, Ident: string;
  Default: TDateTime): TDateTime;
begin
  Result := FIniFile.ReadDate(Section, Ident, Default);
end;

function TXplatIniFile.ReadDateTime(const Section, Ident: string;
  Default: TDateTime): TDateTime;
begin
  Result := FIniFile.ReadDateTime(Section, Ident, Default);
end;

function TXplatIniFile.ReadFloat(const Section, Ident: string;
  Default: Double): Double;
begin
  Result := FIniFile.ReadFloat(Section, Ident, Default);
end;

function TXplatIniFile.ReadInteger(const Section, Ident: string;
  Default: Integer): Integer;
begin
  Result := FIniFile.ReadInteger(Section, Ident, Default);
end;

procedure TXplatIniFile.ReadSection(const Section: string; Strings: TStrings);
begin
  FIniFile.ReadSection(Section, Strings);
end;

procedure TXplatIniFile.ReadSections(Strings: TStrings);
begin
  FIniFile.ReadSections(Strings);
end;

procedure TXplatIniFile.ReadSectionValues(const Section: string;
  Strings: TStrings);
begin
  FIniFile.ReadSectionValues(Section, Strings);
end;

function TXplatIniFile.ReadString(const Section, Ident,
  Default: string): string;
begin
  Result := FIniFile.ReadString(Section, Ident, Default);
end;

function TXplatIniFile.ReadTime(const Section, Ident: string;
  Default: TDateTime): TDateTime;
begin
  Result := FIniFile.ReadTime(Section, Ident, Default);
end;

procedure TXplatIniFile.UpdateFile;
begin
  FIniFile.UpdateFile;
end;

procedure TXplatIniFile.WriteBool(const Section, Ident: string;
  Value: Boolean);
begin
  FIniFile.WriteBool(Section, Ident, Value);
  if FUpdateAfterWrite then
    FIniFile.UpdateFile;
end;

procedure TXplatIniFile.WriteDate(const Section, Ident: string;
  Value: TDateTime);
begin
  FIniFile.WriteDate(Section, Ident, Value);
  if FUpdateAfterWrite then
    FIniFile.UpdateFile;
end;

procedure TXplatIniFile.WriteDateTime(const Section, Ident: string;
  Value: TDateTime);
begin
  FIniFile.WriteDateTime(Section, Ident, Value);
  if FUpdateAfterWrite then
    FIniFile.UpdateFile;
end;

procedure TXplatIniFile.WriteFloat(const Section, Ident: string;
  Value: Double);
begin
  FIniFile.WriteFloat(Section, Ident, Value);
  if FUpdateAfterWrite then
    FIniFile.UpdateFile;
end;

procedure TXplatIniFile.WriteInteger(const Section, Ident: string;
  Value: Integer);
begin
  FIniFile.WriteInteger(Section, Ident, Value);
  if FUpdateAfterWrite then
    FIniFile.UpdateFile;
end;

procedure TXplatIniFile.WriteString(const Section, Ident, Value: String);
begin
  FIniFile.WriteString(Section, Ident, Value);
  if FUpdateAfterWrite then
    FIniFile.UpdateFile;
end;

procedure TXplatIniFile.WriteTime(const Section, Ident: string;
  Value: TDateTime);
begin
  FIniFile.WriteTime(Section, Ident, Value);
  if FUpdateAfterWrite then
    FIniFile.UpdateFile;
end;


end.
