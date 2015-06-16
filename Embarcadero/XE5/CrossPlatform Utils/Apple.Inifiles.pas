unit Apple.Inifiles;

interface


uses
  System.Inifiles, System.Classes
  {$IFDEF MACOS}
  {$IFDEF IOS}
  , iOSapi.Foundation, iosAPI.CocoaTypes
  {$ELSE}
  , MacApi.Foundation
  {$ENDIF IOS}
  {$ENDIF MACOS}
  ;

type
  TUserInifile = class(TCustomIniFile)
  private
    FDefaults: NSUserDefaults;
    function GetDictionary(const Section: string): NSMutableDictionary;
    function ReadPointer(const Section, Ident: string): Pointer;
    procedure WritePointer(const Section, Ident: string; Value: Pointer);
  public
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean; overload; override;
    function ReadBool(const Ident: string; Default: Boolean): Boolean; reintroduce; overload;
    procedure WriteBool(const Section, Ident: string; Value: Boolean); overload; override;
    procedure WriteBool(const Ident: string; Value: Boolean); reintroduce; overload;

    function ReadString(const Section, Ident, Default: string): string; overload; override;
    function ReadString(const Ident, Default: string): string; reintroduce; overload;
    procedure WriteString(const Section, Ident, Value: String); overload; override;
    procedure WriteString(const Ident, Value: String); reintroduce; overload;

    function ReadInteger(const Section, Ident: string; Default: Integer): Integer; overload; override;
    function ReadInteger(const Ident: string; Default: Integer): Integer; reintroduce; overload;
    procedure WriteInteger(const Section, Ident: string; Value: Integer); overload; override;
    procedure WriteInteger(const Ident: string; Value: Integer); reintroduce; overload;

    function ReadDate(const Section, Ident: string; Default: TDateTime): TDateTime; overload; override;
    function ReadDate(const Ident: string; Default: TDateTime): TDateTime; reintroduce; overload;
    procedure WriteDate(const Section, Ident: string; Value: TDateTime); overload; override;
    procedure WriteDate(const Ident: string; Value: TDateTime); reintroduce; overload;

    function ReadDateTime(const Section, Ident: string; Default: TDateTime): TDateTime; overload; override;
    function ReadDateTime(const Ident: string; Default: TDateTime): TDateTime; reintroduce; overload;
    procedure WriteDateTime(const Section, Ident: string; Value: TDateTime); overload; override;
    procedure WriteDateTime(const Ident: string; Value: TDateTime); reintroduce; overload;

    function ReadFloat(const Section, Ident: string; Default: Double): Double; overload; override;
    function ReadFloat(const Ident: string; Default: Double): Double; reintroduce; overload;
    procedure WriteFloat(const Section, Ident: string; Value: Double); overload; override;
    procedure WriteFloat(const Ident: string; Value: Double); reintroduce; overload;

    function ReadTime(const Section, Ident: string; Default: TDateTime): TDateTime; overload; override;
    function ReadTime(const Ident: string; Default: TDateTime): TDateTime; reintroduce; overload;
    procedure WriteTime(const Section, Ident: string; Value: TDateTime); overload; override;
    procedure WriteTime(const Ident: string; Value: TDateTime); reintroduce; overload;

    procedure ReadSection(const Section: string; Strings: TStrings); override;
    procedure ReadSections(Strings: TStrings); override;
    procedure ReadSectionValues(const Section: string; Strings: TStrings); override;
    procedure EraseSection(const Section: string); override;

    procedure DeleteKey(const Section, Ident: String); overload; override;
    procedure DeleteKey(const Ident: String); reintroduce; overload;

    procedure UpdateFile; override;

    constructor Create;
  end;


resourcestring
  rsErrorSynchronizing = 'Error synchronizing user defaults';

implementation

uses
  Apple.Utils, System.SysUtils, System.DateUtils, FMX.Platform, Xplat.Services,
  Xplat.IniFiles;

{ TInifile }

constructor TUserInifile.Create;
begin
  inherited Create('');
  FDefaults := TNSUserDefaults.Wrap(TNSUserDefaults.OCClass.standardUserDefaults);
end;

procedure TUserInifile.DeleteKey(const Section, Ident: String);
var
  lDict: NSMutableDictionary;
begin
  if Section <> '' then
  begin
    lDict := GetDictionary(Section);
    if Assigned(lDict) then
    begin
      lDict.removeObjectForKey(NSStrPtr(Ident));
      FDefaults.setObject(PtrForObject(lDict), NSStr(Section));
    end;
  end
  else
    FDefaults.removeObjectForKey(NSStr(Ident));
end;

procedure TUserInifile.DeleteKey(const Ident: String);
begin
  DeleteKey('', Ident);
end;

procedure TUserInifile.EraseSection(const Section: string);
var
  lDict: NSDictionary;
  lName: NSString;
begin
  lName := NSStr(Section);
  lDict := FDefaults.dictionaryForKey(lName);
  if Assigned(lDict) then
    FDefaults.removeObjectForKey(lName);
end;

function TUserIniFile.GetDictionary(const Section: string): NSMutableDictionary;
var
  lDict: NSDictionary;
begin
  lDict := FDefaults.dictionaryForKey(NSStr(Section));
  if not Assigned(lDict) then
  begin
    Result := TNSMutableDictionary.Create;
    FDefaults.setObject(PtrForObject(Result), NSStr(Section));
  end
  else
    Result := TNSMutableDictionary.Wrap(TNSMutableDictionary.OCClass.dictionaryWithDictionary(lDict));
end;

function TUserInifile.ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
var
  lPtr: Pointer;
begin
  lPtr := ReadPointer(Section, Ident);
  if Assigned(lPtr) then
    Result := NSNumberToBool(lPtr)
  else
    Result := Default;
end;

function TUserInifile.ReadBool(const Ident: string; Default: Boolean): Boolean;
begin
  Result := ReadBool('', Ident, Default);
end;

function TUserInifile.ReadDate(const Section, Ident: string;
  Default: TDateTime): TDateTime;
begin
  Result := DateOf(ReadDateTime(Section, Ident, Default));
end;

function TUserInifile.ReadDate(const Ident: string;
  Default: TDateTime): TDateTime;
begin
  Result := ReadDate('', Ident, Default);
end;

function TUserInifile.ReadDateTime(const Ident: string;
  Default: TDateTime): TDateTime;
begin
  Result := ReadDateTime('', Ident, Default);
end;

function TUserInifile.ReadFloat(const Ident: string; Default: Double): Double;
begin
  Result := ReadFloat('', Ident, Default);
end;

function TUserInifile.ReadDateTime(const Section, Ident: string;
  Default: TDateTime): TDateTime;
var
  lPtr: Pointer;
begin
  lPtr := ReadPointer(Section, Ident);
  if Assigned(lPtr) then
    Result := NSDateToDateTime(lPtr)
  else
    Result := Default;
end;

function TUserInifile.ReadFloat(const Section, Ident: string;
  Default: Double): Double;
var
  lPtr: Pointer;
begin
  lPtr := ReadPointer(Section, Ident);
  if Assigned(lPtr) then
    Result := NSNumberToDouble(lPtr)
  else
    Result := Default;
end;

function TUserInifile.ReadInteger(const Ident: string;
  Default: Integer): Integer;
begin
  Result := ReadInteger('', Ident, Default);
end;

function TUserInifile.ReadInteger(const Section, Ident: string;
  Default: Integer): Integer;
var
  lPtr: Pointer;
begin
  lPtr := ReadPointer(Section, Ident);
  if Assigned(lPtr) then
    Result := NSNumberToInt(lPtr)
  else
    Result := Default;
end;

function TUserInifile.ReadPointer(const Section, Ident: string): Pointer;
var
  lDict: NSMutableDictionary;
begin
  if (Section <> '') then
  begin
    lDict := GetDictionary(Section);
    Result := lDict.valueForKey(NSStr(Ident));
  end
  else
    Result := FDefaults.objectForKey(NSStr(Ident));
end;

procedure TUserInifile.ReadSection(const Section: string; Strings: TStrings);
var
  lDict: NSMutableDictionary;
  lKeys: NSArray;
  I: Cardinal;
  lCount: Cardinal;
  lKey: Pointer;
  lObj: Pointer;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    lDict := GetDictionary(Section);
    lKeys := lDict.allKeys;
    lCount := lKeys.count;
    for I := 0 to lCount -1 do
    begin
      lKey := lKeys.objectAtIndex(I);
      lObj := lDict.objectForKey(lKey);
      Strings.Add(Format('%s=%s', [NSStringToString(lKey), NSObjectToString(lObj)]));
    end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TUserInifile.ReadSections(Strings: TStrings);
var
  lArray: NSArray;
  lCur: NSString;
  I: Integer;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    lArray := FDefaults.dictionaryRepresentation.allKeys;
    for I := 0 to lArray.Count -1 do
    begin
      lCur := TNSString.Wrap(lArray.objectAtIndex(I));
      if Assigned(FDefaults.dictionaryForKey(lCur)) then
        Strings.Add(NSStringToString(lCur));
    end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TUserInifile.ReadSectionValues(const Section: string;
  Strings: TStrings);
var
  lValues: TStringList;
  I: Integer;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    lValues := TStringList.Create;
    try
      ReadSection(Section, lValues);
      for I := 0 to lValues.Count -1 do
        Strings.Add(lValues.ValueFromIndex[I]);
    finally
      lValues.Free;
    end;
  finally
    Strings.EndUpdate;
  end;
end;

function TUserInifile.ReadString(const Ident, Default: string): string;
begin
  Result := ReadString('', Ident, Default);
end;

function TUserInifile.ReadTime(const Ident: string;
  Default: TDateTime): TDateTime;
begin
  Result := ReadTime('', Ident, Default);
end;

function TUserInifile.ReadString(const Section, Ident, Default: string): string;
var
  lPtr: Pointer;
begin
  lPtr := ReadPointer(Section, Ident);
  if Assigned(lPtr) then
    Result := NSStringToString(lPtr)
  else
    Result := Default;
end;

function TUserInifile.ReadTime(const Section, Ident: string;
  Default: TDateTime): TDateTime;
begin
 Result := TimeOf(ReadDateTime(Section, Ident, Default));
end;

procedure TUserInifile.UpdateFile;
begin
  if not FDefaults.synchronize then
    raise Exception.Create(rsErrorSynchronizing);
end;

procedure TUserInifile.WriteBool(const Section, Ident: string; Value: Boolean);
begin
  WritePointer(Section, Ident, NSNumberPtr(Value));
end;

procedure TUserInifile.WriteBool(const Ident: string; Value: Boolean);
begin
  WriteBool('', Ident, Value);
end;

procedure TUserInifile.WriteDate(const Section, Ident: string; Value: TDateTime);
begin
  WriteDateTime(Section, Ident, DateOf(Value));
end;

procedure TUserIniFile.WritePointer(const Section, Ident: string; Value: Pointer);
var
  lDict: NSMutableDictionary;
  lPtr: Pointer;
  lKey: NSString;
begin
  if Section <> '' then
  begin
    lDict := GetDictionary(Section);
    lDict.setValue(Value, NSStr(Ident));
    lPtr := PtrForObject(lDict);
    lKey := NSStr(Section);
  end
  else
  begin
    lPtr := Value;
    lKey := NSStr(Ident);
  end;
  FDefaults.setObject(lPtr, lKey);
end;

procedure TUserInifile.WriteString(const Ident, Value: String);
begin
  WriteString('', Ident, Value);
end;

procedure TUserInifile.WriteTime(const Ident: string; Value: TDateTime);
begin
  WriteTime('', Ident, Value);
end;

procedure TUserInifile.WriteDate(const Ident: string; Value: TDateTime);
begin
  WriteDate('', Ident, Value);
end;

procedure TUserInifile.WriteDateTime(const Ident: string; Value: TDateTime);
begin
  WriteDateTime('', Ident, Value);
end;

procedure TUserInifile.WriteFloat(const Ident: string; Value: Double);
begin
  WriteFloat('', Ident, Value);
end;

procedure TUserInifile.WriteDateTime(const Section, Ident: string;
  Value: TDateTime);
var
  lDate: NSDate;
begin
  lDate := DateTimeToNSDate(Value);
  try
    WritePointer(Section, Ident, PtrForObject(lDate));
  finally
    lDate.release;
  end;
end;

procedure TUserInifile.WriteFloat(const Section, Ident: string; Value: Double);
begin
  WritePointer(Section, Ident, NSNumberPtr(Value));
end;

procedure TUserInifile.WriteInteger(const Ident: string; Value: Integer);
begin
  WriteInteger('', Ident, Value);
end;

procedure TUserInifile.WriteInteger(const Section, Ident: string;
  Value: Integer);
begin
  WritePointer(Section, Ident, NSNumberPtr(Value));
end;

procedure TUserInifile.WriteString(const Section, Ident, Value: String);
begin
  WritePointer(Section, Ident, NSStrPtr(Value));
end;

procedure TUserInifile.WriteTime(const Section, Ident: string;
  Value: TDateTime);
begin
  //Need to ensure we're writing a TDateTime value that will correspond to
  //>= 1/1/1900, otherwise we get garbage back when reading the time
  WriteDateTime(Section, Ident, 2 + TimeOf(Value));
end;

initialization
  TPlatformServices.Current.AddPlatformService(IIniFileService,
    TXplatIniFile.Create(TUserIniFile.Create));

finalization
  TPlatformServices.Current.RemovePlatformService(IIniFileService);

end.
