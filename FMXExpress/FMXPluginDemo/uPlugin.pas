unit uPlugin;

interface
uses
    System.JSON, System.StrUtils, System.Generics.Defaults, System.Generics.Collections,
    System.SysUtils, System.Math, System.Classes, System.IOUtils,
    FMX.Graphics, FMX.Features.BitmapHelper,
    uUtils;
type
    TProcInt = procedure (index : Integer) of object;

    TPlugin = class
    public
        FName : string;
        FPath : string;
        FType : Integer;
        FIcon : TBitmap;
        FIndex : Integer;
        FChanged : Boolean;
    public
        const
            INT_DEFAULT_INDEX : Integer = -1;
            ARRAY_PARSE : array of string = ['name', 'icon', 'type', 'path'];
        constructor Create;
        destructor Destroy; override;
        function LoadFromJson(AJSONValue : TJSONValue) : Boolean;
        function SaveToJson(var AJSONObject : TJSONObject) : Boolean;
    end;

    TPlugins = class(TList<TPlugin>)
    private
        FItemIndex : Integer;
        FOnChangeCurrent : TProcInt;
        FFileName : string;
        function GetCurrent: TPlugin;
        procedure SetCurrent(const Value: TPlugin);
        procedure SetItemIndex(const Value: Integer);
        procedure SetFileName(const Value: string);
    function GetIsEmpty: Boolean;
    public
        const
            INT_DEFAULT_INDEX : Integer = -1;
            ARRAY_TYPES : array of string = ['url', 'plugin'];//temp
        class var
            FTypes : TStringList;
    public
        constructor Create; overload;
        constructor Create(fileName : string); overload;
        destructor Destroy; override;

        function LoadFromJson(AJSONValue : TJSONValue) : Boolean; overload;
        function LoadFromJson(AJSONValue : string) : Boolean; overload;
        function LoadFromFile(fileName : string) : Boolean;
        function SaveToJson(var sJson : string) : Boolean;
        function SaveToArray : TArray<string>;
        function SaveToFile : Boolean;

        property Current : TPlugin read GetCurrent write SetCurrent;
        property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;
        property OnChangeCurrent : TProcInt read FOnChangeCurrent write FOnChangeCurrent;
        property FileName : string read FFileName write SetFileName;
        property IsEmpty : Boolean read GetIsEmpty;
    end;

implementation

{ TPlugins }
{------------------------------------------------------------------------------}
constructor TPlugins.Create;
var
    item : string;
begin
  inherited;
    ItemIndex := INT_DEFAULT_INDEX;
    FTypes := TStringList.Create;
    for item in ARRAY_TYPES do
        FTypes.Add(item);
end;
{------------------------------------------------------------------------------}
constructor TPlugins.Create(fileName: string);
begin
    Create;
    Self.FileName := fileName;
end;
{------------------------------------------------------------------------------}
destructor TPlugins.Destroy;
begin
    FreeAndNil(FTypes);
  inherited;
end;
{------------------------------------------------------------------------------}
function TPlugins.GetCurrent: TPlugin;
begin
    if (FItemIndex >= 0) and (FItemIndex < Count) then
        Result := Items[FItemIndex]
    else
        Result := nil;
end;
{------------------------------------------------------------------------------}
function TPlugins.GetIsEmpty: Boolean;
begin
    Result := Self.Count = 0;
end;
{------------------------------------------------------------------------------}
function TPlugins.LoadFromFile(fileName: string): Boolean;
var
    st : TStringList;
    text : string;
begin
    Result := False;
    st := TStringList.Create;
    try
        if not TFile.Exists(fileName) then
            Exit;
        st.LoadFromFile(fileName);
        text := AnsiReplaceText(st.Text, '\', '/');
        Result := self.LoadFromJson(text);
    finally
        FreeAndNil(st);
    end;
end;
{------------------------------------------------------------------------------}
function TPlugins.LoadFromJson(AJSONValue: string): Boolean;
begin
    Result := LoadFromJson(TJSONObject.ParseJSONValue(AJSONValue));
end;
{------------------------------------------------------------------------------}
function TPlugins.LoadFromJson(AJSONValue: TJSONValue): Boolean;
var
    Enums: TJSONArrayEnumerator;
    tempJson : TJSONArray;
    plugin : TPlugin;
    temp : string;
begin
    Result := False;
    try
        if uUtils.ValidateJSONArray(AJSONValue, tempJson) then
            Exit;
        Clear;
        Enums := tempJson.GetEnumerator;
        try
            while Enums.MoveNext do
            begin
                plugin := TPlugin.Create;
                plugin.LoadFromJson(Enums.Current);
                Self.Add(plugin);
            end;
            Result := True
        finally
            FreeAndNil(Enums);
        end;
    except
        on e : Exception do
            temp := e.Message;
    end;
end;
{------------------------------------------------------------------------------}
function TPlugins.SaveToArray: TArray<string>;
var
    sArray: TArray<string>;
    plugin : TPlugin;
    i : integer;
begin
    sArray := TArray<string>.Create('1');
    SetLength(sArray, self.Count);
    for i := 0 to self.Count - 1 do
    begin
        plugin := self[i];
        sArray[i] := plugin.FName;
    end;
    Result := sArray;
end;
{------------------------------------------------------------------------------}
function TPlugins.SaveToFile: Boolean;
var
    st : TStringList;
    dirName, fName, text : string;
begin
    Result := False;
    st := TStringList.Create;
    try
        if fileName.IsEmpty or not SaveToJson(text) then
            Exit;
        st.Text := text;
        dirName := System.IOUtils.TPath.GetDirectoryName(fileName);
        if ForceDirectories(dirName) then
        begin
            st.SaveToFile(fileName);
            Result := True;
        end;
    finally
        FreeAndNil(st);
    end;
end;
{------------------------------------------------------------------------------}
function TPlugins.SaveToJson(var sJson: string): Boolean;
var
    JsonArray: TJSONArray;
    plugin : TPlugin;
    temp : TJSONObject;
begin
    Result := false;
    JsonArray := TJSONArray.Create;
    try
        for plugin in Self do
            if plugin.SaveToJson(temp) then
                JsonArray.Add(temp);
        sJson := JsonArray.ToString;
        Result := True;
    finally
        FreeAndNil(JsonArray);
    end;
end;
{------------------------------------------------------------------------------}
procedure TPlugins.SetCurrent(const Value: TPlugin);
begin
    if not Assigned(Value) then
        ItemIndex := INT_DEFAULT_INDEX
    else
        ItemIndex := IndexOf(Value);
end;
{------------------------------------------------------------------------------}
procedure TPlugins.SetFileName(const Value: string);
begin
    if FFileName.Equals(Value) then
        Exit;
    FFileName := Value;
    LoadFromFile(Value);
end;
{------------------------------------------------------------------------------}
procedure TPlugins.SetItemIndex(const Value: Integer);
var
    prevItemIndex : integer;
begin
    prevItemIndex := FItemIndex;
    FItemIndex := Value;
    if prevItemIndex <> ItemIndex then
        if Assigned(OnChangeCurrent) then
            OnChangeCurrent(ItemIndex);
end;
{------------------------------------------------------------------------------}
{ TPlugin }
{------------------------------------------------------------------------------}
constructor TPlugin.Create;
begin
    FIcon := TBitmap.Create(0,0);
end;
{------------------------------------------------------------------------------}
destructor TPlugin.Destroy;
begin
    FreeAndNil(FIcon);
  inherited;
end;
{------------------------------------------------------------------------------}
function TPlugin.LoadFromJson(AJSONValue: TJSONValue): Boolean;
var
    Enums: TJSONPairEnumerator;
    tempJson : TJSONObject;
    FoundIndex : Integer;
    decodeIcon : string;
begin
    try
        if uUtils.ValidateJSONObject(AJSONValue, tempJson) then
            Exit(False);
        tempJson.TryGetValue<string>(ARRAY_PARSE[0], self.FName);
        if tempJson.TryGetValue<string>(ARRAY_PARSE[1], decodeIcon) then
            self.FIcon.Base64 := decodeIcon;
        tempJson.TryGetValue<Integer>(ARRAY_PARSE[2], self.FType);
        tempJson.TryGetValue<string>(ARRAY_PARSE[3], self.FPath);
        FIndex := INT_DEFAULT_INDEX;
        FChanged := True;
        Result := true;
    except
        Result := false;
    end;
end;
{------------------------------------------------------------------------------}
function TPlugin.SaveToJson(var AJSONObject: TJSONObject): Boolean;
var
    encodeIcon : string;
begin
    Result := False;
    encodeIcon := self.FIcon.Base64;
    AJSONObject:= TJSONObject.Create;
    AJSONObject.AddPair(ARRAY_PARSE[0], FName);
    AJSONObject.AddPair(ARRAY_PARSE[2], integer.ToString(FType));
    AJSONObject.AddPair(ARRAY_PARSE[3], FPath);
    AJSONObject.AddPair(ARRAY_PARSE[1], encodeIcon); //Convenient viewing file
    Result := True;
end;
{------------------------------------------------------------------------------}
end.
