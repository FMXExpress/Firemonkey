
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit BindScopeDBDictExt;

interface

uses
  System.Classes, Data.Bind.DBScope,
  System.Bindings.EvalProtocol, Data.Bind.Components;

type
  TScopeMappings = class;

  TScopeMappingItem = class(TCollectionItem)
  private
    FComponent: TComponent;
    FAlias: string;
    function GetScopeMappings: TScopeMappings;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
    procedure SetComponent(const Value: TComponent);
  public
    constructor Create(Collection: TCollection); override;
    property ScopeMappings: TScopeMappings read GetScopeMappings;
  published
    property Alias: string read FAlias write FAlias;
    property Component: TComponent read FComponent write SetComponent;
  end;

  TScopeMappingsEnumerator =  class(TCollectionEnumerator)
  public
    function GetCurrent: TScopeMappingItem; inline;
    property Current: TScopeMappingItem read GetCurrent;
  end;

  TScopeMappings = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TScopeMappingItem;
    procedure SetItem(Index: Integer; const Value: TScopeMappingItem);
  protected
    function GetAttrCount: Integer; override;
    function GetAttr(Index: Integer): string; override;
    function GetItemAttr(Index, ItemIndex: Integer): string; override;
  public
    function GetEnumerator: TScopeMappingsEnumerator;
    property Items[Index: Integer]: TScopeMappingItem read GetItem write SetItem; default;
  end;

  TCustomBindScopeDBDictionary = class(TBindScopeDB, IScopeRecordEnumerable)
  private
    FScopeMappings: TScopeMappings;
  protected
    procedure SetScopeMappings(const Value: TScopeMappings);
    function GetScope: IScope; override;
    procedure AddExpression(AExpression: TBasicBindComponent); override;
    procedure RemoveExpression(AExpression: TBasicBindComponent); override;
    property ScopeMappings: TScopeMappings read FScopeMappings write SetScopeMappings;
    { IScopeRecordEnumerable }
    function GetEnumerator(const AMemberName: string; ABufferCount: Integer): IScopeRecordEnumerator;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TBindScopeDBDictionary = class(TCustomBindScopeDBDictionary)
  published
    property ScopeMappings;
  end;

  TCustomBindScopeDict = class(TBindScope, IScopeRecordEnumerable)
  private
    FScopeMappings: TScopeMappings;
  protected
    function GetValue: TObject; override;
    procedure SetScopeMappings(const Value: TScopeMappings);
    function GetScope: IScope; override;
    procedure AddExpression(AExpression: TBasicBindComponent); override;
    procedure RemoveExpression(AExpression: TBasicBindComponent); override;
    property ScopeMappings: TScopeMappings read FScopeMappings write SetScopeMappings;
    { IScopeRecordEnumerable }
    function GetEnumerator(const AMemberName: string; ABufferCount: Integer): IScopeRecordEnumerator;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TBindScopeDict = class(TCustomBindScopeDict)
  published
    property ScopeMappings;
  end;

  TScopeDictionaryEnumerableWrapper = class(TInterfacedObject, IScopeRecordEnumerator)
  private
    FDelegateEnumerator: IScopeRecordEnumerator;
    FDictScope: IScope;
  public
    constructor Create(ADictScope: IScope; ADelegateEnumerator: IScopeRecordEnumerator);
    destructor Destroy; override;
    procedure First;
    function GetCurrent: IScope;
    function GetMemberCurrent(const AMemberName: string): IScope;
    function MoveNext: Boolean;
    property Current: IScope read GetCurrent;
  end;

procedure Register;

implementation

uses
  System.SysUtils, System.Bindings.ObjEval,
  System.Bindings.EvalSys, Data.Bind.Consts;

{ TScopeMappingItem }

procedure TScopeMappingItem.SetComponent(const Value: TComponent);
begin
  if FComponent <> Value then
  begin
    FComponent := Value;
  end;
end;

procedure TScopeMappingItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TScopeMappingItem then
  begin
    TScopeMappingItem(Dest).Alias := Alias;
    TScopeMappingItem(Dest).Component := Component;
  end
  else
    inherited;
end;

constructor TScopeMappingItem.Create(Collection: TCollection);
begin
  inherited;
end;

function TScopeMappingItem.GetDisplayName: string;
begin
  Result := '';
end;

function TScopeMappingItem.GetScopeMappings: TScopeMappings;
begin
  Result := TScopeMappings(Collection);
end;

{ TScopeMappingsEnumerator }

function TScopeMappingsEnumerator.GetCurrent: TScopeMappingItem;
begin
  Result := TScopeMappingItem(inherited GetCurrent);
end;

{ TScopeMappings }

function TScopeMappings.GetEnumerator: TScopeMappingsEnumerator;
begin
  Result := TScopeMappingsEnumerator.Create(Self);
end;

function TScopeMappings.GetItem(Index: Integer): TScopeMappingItem;
begin
  Result := TScopeMappingItem(inherited Items[Index]);
end;

function TScopeMappings.GetAttr(Index: Integer): string;
begin
  case Index of
    0: Result := 'ID';
    1: Result := 'Name';
    2: Result := 'Source Component';
  else
    Result := ''; { do not localize }
  end;
end;

function TScopeMappings.GetAttrCount: Integer;
begin
  Result := 3;
end;

function TScopeMappings.GetItemAttr(Index, ItemIndex: Integer): string;
begin
  case Index of
    0: begin
         Result := IntToStr(ItemIndex);
       end;
    1: Result := Items[ItemIndex].Alias;
    2: begin
         if Items[ItemIndex].Component = nil then
           Result := ''
         else
           Result := Items[ItemIndex].Component.Name;
       end;
  else
    Result := '';
  end;
end;

procedure TScopeMappings.SetItem(Index: Integer; const Value: TScopeMappingItem);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;

{ TCustomBindScopeDBDictionary }

procedure TCustomBindScopeDBDictionary.AddExpression(
  AExpression: TBasicBindComponent);
var
  LScopeMappingItem: TScopeMappingItem;
  LScopeExpressions: IScopeExpressions;
begin
  inherited;

  //IScopeExpressions
  for LScopeMappingItem in ScopeMappings do
  begin
    if Supports(LScopeMappingItem.Component, IScopeExpressions, LScopeExpressions) then
      LScopeExpressions.AddExpression(AExpression);
  end;
end;

constructor TCustomBindScopeDBDictionary.Create(AOwner: TComponent);
begin
  inherited;
  FScopeMappings := TScopeMappings.Create(nil, TScopeMappingItem);
end;

destructor TCustomBindScopeDBDictionary.Destroy;
begin
  FreeAndNil(FScopeMappings);
  inherited;
end;

function TCustomBindScopeDBDictionary.GetEnumerator(const AMemberName: string;
  ABufferCount: Integer): IScopeRecordEnumerator;
var
  LBaseRecordEnumerator: IScopeRecordEnumerator;
begin
  LBaseRecordEnumerator := inherited GetEnumerator(AMemberName, ABufferCount);
  Result := TScopeDictionaryEnumerableWrapper.Create(GetScope, LBaseRecordEnumerator);
end;

function TCustomBindScopeDBDictionary.GetScope: IScope;
var
  LDictScope: TDictionaryScope;
  LScopeMappingItem: TScopeMappingItem;
begin
  Result := inherited;

  if FScopeMappings.Count > 0 then
  begin
    LDictScope := TDictionaryScope.Create;
    for LScopeMappingItem in FScopeMappings do
    begin
      if LScopeMappingItem.Component <> nil then
        LDictScope.Map.Add(LScopeMappingItem.Alias, WrapObject(LScopeMappingItem.Component))
    end;
    //make sure Self scope comes last to ensure proper evaluation (inner looked up first)
    if LDictScope.Map.Count > 0 then
      Result := TNestedScope.Create(Result, LDictScope);
  end;
end;

procedure TCustomBindScopeDBDictionary.RemoveExpression(
  AExpression: TBasicBindComponent);
var
  LScopeMappingItem: TScopeMappingItem;
  LScopeExpressions: IScopeExpressions;
begin
  inherited;

  //IScopeExpressions
  if ScopeMappings <> nil then
    for LScopeMappingItem in ScopeMappings do
    begin
      if Supports(LScopeMappingItem.Component, IScopeExpressions, LScopeExpressions) then
        LScopeExpressions.RemoveExpression(AExpression);
    end;
end;

procedure TCustomBindScopeDBDictionary.SetScopeMappings(const Value: TScopeMappings);
begin
  if Value <> FScopeMappings then
  begin
    if FScopeMappings <> nil then
      FScopeMappings.Free;
    FScopeMappings := Value;
  end;
end;

{ TCustomBindScopeDict }

procedure TCustomBindScopeDict.AddExpression(
  AExpression: TBasicBindComponent);
var
  LScopeMappingItem: TScopeMappingItem;
  LScopeExpressions: IScopeExpressions;
begin
  inherited;

  //IScopeExpressions
  for LScopeMappingItem in ScopeMappings do
  begin
    if Supports(LScopeMappingItem.Component, IScopeExpressions, LScopeExpressions) then
      LScopeExpressions.AddExpression(AExpression);
  end;
end;

constructor TCustomBindScopeDict.Create(AOwner: TComponent);
begin
  inherited;
  FScopeMappings := TScopeMappings.Create(nil, TScopeMappingItem);
end;

destructor TCustomBindScopeDict.Destroy;
begin
  FreeAndNil(FScopeMappings);
  inherited;
end;

function TCustomBindScopeDict.GetEnumerator(const AMemberName: string;
  ABufferCount: Integer): IScopeRecordEnumerator;
var
  LBaseRecordEnumerator: IScopeRecordEnumerator;
begin
  LBaseRecordEnumerator := inherited GetEnumerator(AMemberName, ABufferCount);
  Result := TScopeDictionaryEnumerableWrapper.Create(GetScope, LBaseRecordEnumerator);
end;

function TCustomBindScopeDict.GetScope: IScope;
var
  LDictScope: TDictionaryScope;
  LScopeMappingItem: TScopeMappingItem;
begin
  Result := WrapObject(Self);

  if FScopeMappings.Count > 0 then
  begin
    LDictScope := TDictionaryScope.Create;
    for LScopeMappingItem in FScopeMappings do
    begin
      if LScopeMappingItem.Component <> nil then
        LDictScope.Map.Add(LScopeMappingItem.Alias, WrapObject(LScopeMappingItem.Component))
    end;
    //make sure Self scope comes last to ensure proper evaluation (inner looked up first)
    if LDictScope.Map.Count > 0 then
    begin
      if Result <> nil then
        Result := TNestedScope.Create(Result, LDictScope)
      else
        Result := LDictScope;
    end;
  end;
end;

function TCustomBindScopeDict.GetValue: TObject;
begin
  Result := nil;
end;

procedure TCustomBindScopeDict.RemoveExpression(
  AExpression: TBasicBindComponent);
var
  LScopeMappingItem: TScopeMappingItem;
  LScopeExpressions: IScopeExpressions;
begin
  inherited;

  //IScopeExpressions
  if ScopeMappings <> nil then
    for LScopeMappingItem in ScopeMappings do
    begin
      if Supports(LScopeMappingItem.Component, IScopeExpressions, LScopeExpressions) then
        LScopeExpressions.RemoveExpression(AExpression);
    end;
end;

procedure TCustomBindScopeDict.SetScopeMappings(const Value: TScopeMappings);
begin
  if Value <> FScopeMappings then
  begin
    if FScopeMappings <> nil then
      FScopeMappings.Free;
    FScopeMappings := Value;
  end;
end;

procedure Register;
begin
  RegisterComponents(SBindingComponentsCategory, [TBindScopeDBDictionary, TBindScopeDict]);
end;

{ TScopeMappingEnumerableWrapper }

constructor TScopeDictionaryEnumerableWrapper.Create(ADictScope: IScope; ADelegateEnumerator: IScopeRecordEnumerator);
begin
  FDelegateEnumerator := ADelegateEnumerator;
  FDictScope := ADictScope;
end;

destructor TScopeDictionaryEnumerableWrapper.Destroy;
begin

  inherited;
end;

procedure TScopeDictionaryEnumerableWrapper.First;
begin
  if Assigned(FDelegateEnumerator) then
    FDelegateEnumerator.First;
end;

function TScopeDictionaryEnumerableWrapper.GetCurrent: IScope;
var
  LCurrent: IScope;
begin
  if Assigned(FDictScope) then
    Result := FDictScope
  else
    Result := nil;
  if Assigned(FDelegateEnumerator) then
    LCurrent := FDelegateEnumerator.Current
  else
    LCurrent := nil;
  if Assigned(Result) and Assigned(LCurrent) then
    Result := TNestedScope.Create(LCurrent, Result)
  else
    Result := LCurrent;
end;

function TScopeDictionaryEnumerableWrapper.GetMemberCurrent(
  const AMemberName: string): IScope;
begin
  if Assigned(FDelegateEnumerator) then
    Result := FDelegateEnumerator.GetMemberCurrent(AMemberName)
end;

function TScopeDictionaryEnumerableWrapper.MoveNext: Boolean;
begin
  Result := False;
  if Assigned(FDelegateEnumerator) then
    Result := FDelegateEnumerator.MoveNext;
end;

end.
