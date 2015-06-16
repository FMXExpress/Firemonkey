
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit BindListUtils;

// Utility methods to populate TListBox and TComboBox controls using TBindList
// When using with FireMonkey, the project must use FMX.Bind.Editors
// When using with VCL, the project must use VCL.Bind.Editor

interface

uses System.Classes, Data.Bind.Components, Data.Db;

// Clear a list
procedure ClearList(AControl: TComponent);

// Fill a list with an enumerable object such as TList<>, using one or more binding expressions
procedure FillList(AControl: TComponent; const AControlExpressions: TArray<string>;
  ASource: TObject; const ASourceExpressions: TArray<string>; const ASourceMemberName: string = ''); overload;

// Fill a list with an enumerable object such as TList<>, using a single binding expression
procedure FillList(AControl: TComponent; const AControlExpression: string;
  ASource: TObject; const ASourceExpression: string; const ASourceMemberName: string = ''); overload;

// Fill a list from a binding scope, using one or more binding expressions
procedure FillList(AControl: TComponent; const AControlExpressions: TArray<string>;
  ASource: TBaseBindScopeComponent; const ASourceExpressions: TArray<string>; const ASourceMemberName: string = ''); overload;

// Fill a list from a binding scope, using a single binding expression
procedure FillList(AControl: TComponent; const AControlExpression: string;
  ASource: TBaseBindScopeComponent; const ASourceExpression: string; const ASourceMemberName: string = ''); overload;

// Fill a list from a TDataSet, using one or more binding expression.  Field name is optional.
procedure FillList(AControl: TComponent; const AControlExpressions: TArray<string>;
  ASource: TDataSet; const ASourceExpressions: TArray<string>; const AFieldName: string = ''); overload;

// Fill a list from a TDataSet, using a single binding expression. Field name is optional.
procedure FillList(AControl: TComponent; const AControlExpression: string;
  ASource: TDataSet; const ASourceExpression: string; const AFieldName: string = ''); overload;

implementation

type
  // Enumerate the records in a TDataSet.  Current is TDataSet
  TDataSetEnumerator = class
  private
    FDataSet: TDataSet;
    FFirst: Boolean;
  public
    constructor Create(ADataSet: TDataSet);
    function MoveNext: Boolean;
    property Current: TDataSet read FDataSet;
  end;

  // Enumerate the records in a TDataSet.  Current is a TField
  TDataSetFieldEnumerator = class
  private
    FField: TField;
    FFirst: Boolean;
  public
    constructor Create(AField: TField);
    function MoveNext: Boolean;
    property Current: TField read FField;
  end;

  // Present a TDataSet as an enumerable type
  TDataSetEnumerable = class
  private
    FDataSet: TDataSet;
  public
    constructor Create(ADataSet: TDataSet);
    function GetEnumerator: TDataSetEnumerator;
  end;

  // Present a TDataSet as an enumerable type
  TDataSetFieldEnumerable = class
  private
    FField: TField;
  public
    constructor Create(AField: TField);
    function GetEnumerator: TDataSetFieldEnumerator;
  end;

procedure ClearList(AControl: TComponent);
var
  LBindList: TBindList;
begin
  LBindList := TBindList.Create(nil);
  try
    // Turn off auto properties.
    LBindList.AutoFill := False;
    LBindList.AutoActivate := False;
    LBindList.ControlComponent := AControl;
    LBindList.ClearList;
  finally
    LBindList.Free;
  end;
end;

procedure FillList(AControl: TComponent; const AControlExpression: string;
  ASource: TBaseBindScopeComponent; const ASourceExpression, ASourceMemberName: string); overload;
begin
  FillList(AControl, TArray<string>.Create(AControlExpression),
    ASource, TArray<string>.Create(ASourceExpression), ASourceMemberName);
end;

procedure FillList(AControl: TComponent; const AControlExpressions: TArray<string>;
  ASource: TBaseBindScopeComponent; const ASourceExpressions: TArray<string>; const ASourceMemberName: string); overload;
var
  LBindList: TBindList;
  I: Integer;
begin
  LBindList := TBindList.Create(nil);
  try
    // Turn off auto properties.
    LBindList.AutoFill := False;
    LBindList.AutoActivate := False;
    LBindList.ControlComponent := AControl;
    LBindList.SourceComponent := ASource;
    LBindList.SourceMemberName := ASourceMemberName;
    for I := Low(AControlExpressions) to High(AControlExpressions) do
      with LBindList.FormatExpressions.AddExpression do
      begin
        SourceExpression := ASourceExpressions[I];
        ControlExpression := AControlExpressions[I];
      end;
    LBindList.FillList;
  finally
    LBindList.Free;
  end;
end;

procedure FillList(AControl: TComponent; const AControlExpression: string;
  ASource: TObject; const ASourceExpression, ASourceMemberName: string); overload;
begin
  FillList(AControl, TArray<string>.Create(AControlExpression),
    ASource, TArray<string>.Create(ASourceExpression), ASourceMemberName);
end;

procedure FillList(AControl: TComponent; const AControlExpressions: TArray<string>;
  ASource: TObject; const ASourceExpressions: TArray<string>; const ASourceMemberName: string); overload;
var
  LScope: TBindScope;
begin
  LScope := TBindScope.Create(nil);
  try
    LScope.DataObject := ASource;
    FillList(AControl, AControlExpressions, LScope, ASourceExpressions, ASourceMemberName);
  finally
    LScope.Free;
  end;
end;

procedure FillList(AControl: TComponent; const AControlExpression: string;
  ASource: TDataSet; const ASourceExpression, AFieldName: string); overload;
begin
  FillList(AControl, TArray<string>.Create(AControlExpression),
    ASource, TArray<string>.Create(ASourceExpression), AFieldName);
end;

procedure FillList(AControl: TComponent; const AControlExpressions: TArray<string>;
  ASource: TDataSet; const ASourceExpressions: TArray<string>; const AFieldName: string); overload;
var
  LEnumerable: TObject;
  LScope: TBindScope;
begin
  LEnumerable := nil;
  LScope := nil;
  try
    if AFieldName = '' then
      // Current will be TDataSet
      LEnumerable := TDataSetEnumerable.Create(ASource)
    else
      // Current will be TField
      LEnumerable := TDataSetFieldEnumerable.Create(ASource.FieldByName(AFieldName));
    LScope := TBindScope.Create(nil);
    LScope.DataObject := LEnumerable;
    FillList(AControl, AControlExpressions,
      LScope, ASourceExpressions);
  finally
    LScope.Free;
    LEnumerable.Free;
  end;
end;

procedure FillListBox(AControl: TComponent;
  ASource: TObject; const ASourceExpression: string);
begin
  FillList(AControl, 'Text', ASource, ASourceExpression);
end;

{ TDataSetEnumerable }

constructor TDataSetEnumerable.Create(ADataSet: TDataSet);
begin
  FDataSet := ADataSet;
end;

function TDataSetEnumerable.GetEnumerator: TDataSetEnumerator;
begin
  Result := TDataSetEnumerator.Create(FDataSet);
end;


{ TDataSetEnumerator }

constructor TDataSetEnumerator.Create(ADataSet: TDataSet);
begin
  FDataSet := ADataSet;
  FFirst := True;
end;

function TDataSetEnumerator.MoveNext: Boolean;
begin
  if FFirst then
  begin
    FFirst := False;
    FDataSet.First;
  end
  else
    FDataSet.Next;
  Result := not FDataSet.Eof;
end;

{ TDataSetFieldEnumerable }

constructor TDataSetFieldEnumerable.Create(AField: TField);
begin
  FField := AField;
end;

function TDataSetFieldEnumerable.GetEnumerator: TDataSetFieldEnumerator;
begin
  Result := TDataSetFieldEnumerator.Create(FField);
end;

{ TDataSetFieldEnumerator }

constructor TDataSetFieldEnumerator.Create(AField: TField);
begin
  FFirst := True;
  FField := AField;
end;

function TDataSetFieldEnumerator.MoveNext: Boolean;
begin
  if FFirst then
  begin
    FFirst := False;
    FField.DataSet.First;
  end
  else
    FField.DataSet.Next;
  Result := not FField.DataSet.Eof;
end;

end.
