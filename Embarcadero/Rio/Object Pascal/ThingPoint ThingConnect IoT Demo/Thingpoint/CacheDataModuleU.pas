//---------------------------------------------------------------------------
// Copyright (c) 2016 Embarcadero Technologies, Inc. All rights reserved.  
//
// This software is the copyrighted property of Embarcadero Technologies, Inc. 
// ("Embarcadero") and its licensors. You may only use this software if you 
// are an authorized licensee of Delphi, C++Builder or RAD Studio 
// (the "Embarcadero Products").  This software is subject to Embarcadero's 
// standard software license and support agreement that accompanied your 
// purchase of the Embarcadero Products and is considered a Redistributable, 
// as such term is defined thereunder. Your use of this software constitutes 
// your acknowledgement of your agreement to the foregoing software license 
// and support agreement. 
//---------------------------------------------------------------------------
unit CacheDataModuleU;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.JSON;

type

  TCache = class
  public type
    TItem = record
    private
      FTime: TDateTime;
      FDevice: string;
      FValue: string;
      FPerson: string;
    public
      constructor Create(ATime: TDateTime; const ADevice, APerson: string;
        const AValue: TJSONObject);
      property Person: string read FPerson;
      property Device: string read FDevice;
      property Time: TDateTime read FTime;
      property Value: string read FValue;
    end;
    TEnumCallback = reference to function(const AItem: TItem): Boolean;
  private
    FList: TThreadList<TItem>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearDevice(const ADevice: string);
    procedure Save(const AItem: TItem);
    function EnumValues(const ACallback: TEnumCallback): Boolean;
  end;

  TCacheDataModule = class(TDataModule)
  private
    FCache: TCache;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SaveDeviceData(const ADevice: string; ATime: TDateTime; AData: TJSONObject);
    procedure ClearDeviceData(const ADevice: string);
    function TryGetRecentDeviceData(const ADevice: string; out ATime: TDateTime; const AData: TJSONObject): Boolean;
    function TryGetDeviceDataStats(const ADevice: string; out ATime: TDateTime; const AData: TJSONObject): Boolean;
  end;

var
  CacheDataModule: TCacheDataModule;

implementation

uses System.Math;

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TCacheDataModule }


constructor TCacheDataModule.Create(AOwner: TComponent);
begin
  inherited;
  FCache := TCache.Create;

end;

destructor TCacheDataModule.Destroy;
begin
  FCache.Free;
  inherited;
end;


procedure TCacheDataModule.SaveDeviceData(const ADevice: string; ATime: TDateTime; AData: TJSONObject);
var
  LItem: TCache.TItem;
begin
  LItem := TCache.TItem.Create(ATime, ADevice, '', AData);
  FCache.Save(LItem);
end;

procedure TCacheDataModule.ClearDeviceData(const ADevice: string);
begin
  FCache.ClearDevice(ADevice);
end;

function TCacheDataModule.TryGetRecentDeviceData(const ADevice: string; out ATime: TDateTime; const AData: TJSONObject): Boolean;
var
  LValue: TCache.TItem;
  LJSON: TJSONObject;
  LPair: TJSONPair;
begin
  Result := FCache.EnumValues(
    function(const AValue: TCache.TItem): Boolean
    begin
      Result := AValue.Device = ADevice;
      if Result then
        LValue := AValue;
    end);

  if Result then
  begin
    ATime := LValue.Time;
    LJSON := TJSONObject.ParseJSONValue(LValue.Value) as TJSONObject;
    try
      Assert(LJSON <> nil);
      for LPair in LJSON do
        AData.AddPair(LPair.Clone as TJSONPair);
    finally
      LJSON.Free;
    end;
  end;
end;


// TODO: Cache most recent calculation and time
function TCacheDataModule.TryGetDeviceDataStats(const ADevice: string; out ATime: TDateTime; const AData: TJSONObject): Boolean;
var
  LJSONObject: TJSONObject;
  LDataLists: TDictionary<string, TList<Double>>;
  LTime: TDateTime;
  LPair: TPair<string, TList<Double>>;
  LMean: Double;
  LStdDev: Double;
  LList: TList<Double>;
  LResult: Boolean;
begin
  LDataLists := TObjectDictionary<string, TList<Double>>.Create([doOwnsValues]);
  try
    FCache.EnumValues(
      function(const AValue: TCache.TItem): Boolean
      var
        LJSONPair: TJSONPair;
      begin
        Result := False; // Keep going
        if AValue.Device = ADevice then
        begin
          if not LResult then
            LTime := AValue.Time;
          LResult := True; // Found
          LJSONObject := TJSONObject.ParseJSONValue(AValue.Value) as TJSONObject;
          try
            Assert(LJSONObject <> nil);
            for LJSONPair in LJSONObject do
            begin
              if LJSONPair.JsonValue is TJSONNumber then
              begin
               if not LDataLists.TryGetValue(LJSONPair.JsonString.Value, LList) then
               begin
                 LList := TList<Double>.Create;
                 LDataLists.Add(LJSONPair.JsonString.Value, LList);
               end;
               LList.Add(TJSONNumber(LJSONPair.JsonValue).AsDouble)
              end;
            end;
          finally
            LJSONObject.Free;
          end;
        end;
      end);

    if LResult then
    begin
      ATime := LTime;
      for LPair in  LDataLists do
      begin
        LMean := 0;
        LStdDev := 0;
        LList := LPair.Value;
        System.Math.MeanAndStdDev(LList.ToArray, LMean, LStdDev);
        LJSONObject := TJSONObject.Create;
        AData.AddPair(LPair.Key, LJSONObject);
        LJSONObject.AddPair('last', TJSONNumber.Create(LPair.Value[0]));
        LJSONObject.AddPair('mean', TJSONNumber.Create(LMean));
        LJSONObject.AddPair('stddev', TJSONNumber.Create(LStdDev));
      end;
    end;
  finally
    LDataLists.Free;
  end;
  Result := LResult;
end;

{ TCache }

constructor TCache.Create;
begin
  inherited;
  FList := TThreadList<TCache.TItem>.Create;

end;

destructor TCache.Destroy;
begin
  FList.Free;
  inherited;
end;

function TCache.EnumValues(const ACallback: TEnumCallback): Boolean;
var
  I: Integer;
  LList: TList<TItem>;
  LResult: Boolean;
begin
  LResult := False;
  LList := FList.LockList;
  try
    for I := 0 to LList.Count - 1 do
    begin
      LResult := ACallback(LList[I]);
      if LResult then
        break;
    end;
  finally
    FList.UnlockList;
  end;
  Result := LResult;
end;

procedure TCache.Save(const AItem: TItem);
var
  I: Integer;
  Index: Integer;
  LList: TList<TItem>;
begin
  LList := FList.LockList;
  try
    Index := -1;
    for I := 0 to LList.Count - 1 do
    begin
      if AItem.Time > LList.Items[I].Time then
      begin
        Index := I;
        break;
      end;
    end;
    if Index >= 0 then
      LList.Insert(Index, AItem)
    else
      LList.Add(AItem);
  finally
    FList.UnlockList;
  end;
end;

procedure TCache.ClearDevice(const ADevice: string);
var
  I: Integer;
  LList: TList<TItem>;
begin
  LList := FList.LockList;
  try
    for I := LList.Count - 1 downto 0 do
      if LList.Items[I].Device = ADevice then
        LList.Delete(I);
  finally
    FList.UnlockList;
  end;
end;

{ TCache.TItem }

constructor TCache.TItem.Create(ATime: TDateTime; const ADevice, APerson: string;
  const AValue: TJSONObject);
begin
  FTime := ATime;
  FDevice := ADevice;
  FPerson := APerson;
  FValue := AValue.ToJson;
end;

end.
