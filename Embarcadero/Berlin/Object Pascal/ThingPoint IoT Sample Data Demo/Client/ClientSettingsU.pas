unit ClientSettingsU;
{*******************************************************}
{                                                       }
{             Delphi REST Client Framework              }
{                                                       }
{ Copyright(c) 2013-2015 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

interface

uses
  System.Generics.Collections;

const
  SETTINGSDBFILE = 'settings.dat';  // do not localize

type
  TSettingsList = class(TObject)
  private const
    cWidth = 'width';
    cWidthKey = 'widthkey';
  private
    FWidths: TDictionary<string, Integer>;
    FFilename : string;
  public
    constructor Create( const AFilename: string = ''); overload;
    destructor Destroy; override;

    procedure Clear;

    procedure AddWidth(const AKey: string; AWidth: Integer);
    function GetWidth(const AKey: string; out AWidth: Integer): Boolean;
    procedure SaveToFile(const AFilename: string = '');
    procedure LoadFromFile(const AFilename: string = '');

  end;

implementation

uses System.JSON, System.SysUtils, System.Classes;

{ TMRUList }

procedure TSettingsList.AddWidth(const AKey: string; AWidth: Integer);
begin
  FWidths.AddOrSetValue(AKey, AWidth);
end;

procedure TSettingsList.Clear;
begin
  FWidths.Clear;
end;

constructor TSettingsList.Create( const AFilename : string = '' );
begin
  inherited Create;
  FWidths := TDictionary<string, Integer>.Create;

  FFilename:= AFilename;

  if (FFilename <> '') AND FileExists(FFilename) then
    LoadFromFile( FFilename );
end;

destructor TSettingsList.Destroy;
begin
  FWidths.Free;

  inherited;
end;

function TSettingsList.GetWidth(const AKey: string;
  out AWidth: Integer): Boolean;
begin
  Result := FWidths.TryGetValue(AKey, AWidth)
end;

procedure TSettingsList.LoadFromFile(const AFilename: string);
var
  LStream : TStringStream;
  LRoot : TJSONArray;
  LFilename : string;
  i : integer;
  LValue: TJSONValue;
  LWidth: Integer;
  LKey: string;
begin
  Clear;

  if (AFilename <> '') then
    LFilename:= AFilename
  else
    LFilename:= FFilename;

  if (LFilename = '') OR (NOT FileExists(LFilename)) then
    exit;

  LRoot := nil;
  LStream:= TStringStream.Create;
  try
    LStream.LoadFromFile( LFilename );

    LRoot:= TJSONObject.ParseJSONValue(LStream.DataString) as TJSONArray;
    for I := 0 to LRoot.Count - 1 do
    begin
      LValue := LRoot.Items[I] as TJSONObject;
      if LValue.TryGetValue<string>(cWidthKey, LKey) and
         LValue.TryGetValue<Integer>(cWidth, LWidth) then
         FWidths.AddOrSetValue(LKey, LWidth);
    end;
  finally
    LStream.Free;
    LRoot.Free;
  end;
end;

procedure TSettingsList.SaveToFile( const AFilename : string = '' );
var
  LStream : TStringStream;
  LRoot : TJSONArray;
  LFilename : string;
  LPair: TPair<string, Integer>;
  LJSONObject: TJSONObject;
begin

  if (AFilename <> '') then
    LFilename:= AFilename
  else
    LFilename:= FFilename;

  if (LFilename = '') then
    exit;

  LRoot:= TJSONArray.Create;
  try
    for LPair in FWidths do
    begin
      LJSONObject := TJSONObject.Create;
      LJSONObject.AddPair(cWidthKey, TJSONString.Create(LPair.Key));
      LJSONObject.AddPair(cWidth, TJSONNumber.Create(LPair.Value));
      LRoot.AddElement(LJSONObject);
    end;

    LStream:= TStringStream.Create( LRoot.ToString );
    try
      LStream.SaveToFile( LFilename );
    finally
      LStream.Free;
    end;

  finally
    LRoot.Free;
  end;
end;

end.
