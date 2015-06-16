
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit SimpleListCollectionSampleFormUnit1;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Dialogs, System.Rtti, FMX.Edit, Data.Bind.Components, Data.Bind.EngExt, FMX.Menus,
  Fmx.Bind.DBEngExt, System.Bindings.Outputs, FMX.ListBox, FMX.Layouts, SampleCollections,
  Generics.Collections, FMX.StdCtrls,
  FMX.Bind.Editors; // Used by TBindList to populate a TListBox

type
  TForm1 = class(TForm)
    BindingsList1: TBindingsList;
    EditSourceExpression: TEdit;
    ButtonFill: TButton;
    EditSourceComponentName: TEdit;
    EditControlComponent: TEdit;
    EditControlExpression: TEdit;
    BindList1: TBindList;
    ListBox1: TListBox;
    BindScope1: TBindScope;
    ButtonClear: TButton;
    ButtonFillFromScratch: TButton;
    ButtonClearFromScratch: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    procedure ButtonFillClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure BindList1EvalError(Sender: TObject; AException: Exception);
    procedure ButtonFillFromScratchClick(Sender: TObject);
    procedure ButtonClearFromScratchClick(Sender: TObject);
  private
    FListData: TObject;
  public
    destructor Destroy; override;
    procedure UpdateDisplayFields;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

type
  TListObject = class
  private
    FStringField: string;
    FIntegerField: Integer;
  public
    constructor Create(const AString: string; AInteger: Integer);
    property StringField: string read FStringField write FStringField;
    property IntegerField: Integer read FIntegerField write FIntegerField;
  end;

{ TListObject }

constructor TListObject.Create(const AString: string; AInteger: Integer);
begin
  FStringField := AString;
  FIntegerField := AInteger;
end;

procedure TForm1.BindList1EvalError(Sender: TObject; AException: Exception);
begin
  // Generate a new exception with more information
  raise TBindCompException.CreateFmt(
    'Evaluation Exception'#13#10 +
    'Component Name: %s'#13#10 +
    'Exception Class: %s'#13#10 +
    'Exception Message: %s',
    [TComponent(Sender).Name, AException.ClassName, AException.Message]);
end;

procedure TForm1.ButtonClearClick(Sender: TObject);
begin
  BindList1.ClearList;
end;

procedure TForm1.ButtonClearFromScratchClick(Sender: TObject);
var
  LBindList: TBindList;
begin
  // This example calls FillList so no need for for autoactivate and autofill.
  LBindList := TBindList.Create(nil);
//  LBindList.AutoActivate := True;
//  LBindList.AutoFill := False;
  try
    LBindList.ControlComponent := ListBox1;
    LBindList.ClearList;
  finally
    LBindList.Free;
  end;
end;

procedure TForm1.ButtonFillClick(Sender: TObject);
var
  LData: TList<TListObject>;
  I: Integer;
begin
  // This example calls FillList so no need for for autoactivate and autofill.
  BindList1.AutoActivate := True;
  BindList1.AutoFill := False;
  LData := TObjectList<TListObject>.Create;
  try
    for I := 1 to 10 do
      LData.Add(TListObject.Create('Item' + IntToStr(I), I));
    BindScope1.DataObject := LData;
    try
      BindList1.FillList;
    finally
      BindScope1.DataObject := nil;  // clear before free data object
    end;
  finally
    LData.Free;
  end;
end;

procedure TForm1.ButtonFillFromScratchClick(Sender: TObject);
var
  LData: TList<TListObject>;
  I: Integer;
  LBindList: TBindList;
  LBindScope: TBindScope;
begin
  LBindList := TBindList.Create(nil);
  // This example calls FillList so no need for for autoactivate and autofill.
  LBindList.AutoActivate := True;
  LBindList.AutoFill := False;
  LBindScope := TBindScope.Create(nil);
  try
    LData := TObjectList<TListObject>.Create;
    try
      for I := 1 to 10 do
        LData.Add(TListObject.Create('Item' + IntToStr(I), I));
      LBindList.ControlComponent := ListBox1;
      LBindList.SourceComponent := LBindScope;
      with LBindList.FormatExpressions.AddExpression do
      begin
        SourceExpression := 'ToStr(Current.IntegerField) + ": " + Current.StringField';
        ControlExpression := 'Text';
      end;
      LBindScope.DataObject := LData;
      LBindList.FillList;
      LBindScope.DataObject := nil;
    finally
      LData.Free;
    end;
  finally
    LBindList.Free;
    LBindScope.Free;
  end;
end;

destructor TForm1.Destroy;
begin
  inherited;
end;

//  Display information about binding
procedure TForm1.UpdateDisplayFields;
var
  LSourceExpression: string;
  LControlExpression: string;
  LSourceComponent: string;
  LControlComponent: string;
begin
  if BindList1.FormatExpressions.Count > 0 then
  begin
    LSourceExpression := BindList1.FormatExpressions[0].SourceExpression;
    LControlExpression := BindList1.FormatExpressions[0].ControlExpression;
  end;
  if BindList1.ControlComponent <> nil then
    LControlComponent := BindList1.ControlComponent.ClassName;
  if BindList1.SourceComponent <> nil then
  begin
    LSourceComponent := BindList1.SourceComponent.ClassName;
    if BindList1.SourceComponent is TBindScope then
      with TBindScope(BindList1.SourceComponent) do
        if DataObject <> nil then
          LSourceComponent := LSourceComponent + ' (' +
            DataObject.ClassName + ')'
      else if Component <> nil then
        LSourceComponent := LSourceComponent + ' (' +
          Component.ClassName + ')';
  end;
  EditSourceExpression.Text := LSourceExpression;
  EditControlExpression.Text := LControlExpression;
  EditControlComponent.Text := LControlComponent;
  EditSourceComponentName.Text := LSourceComponent;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  UpdateDisplayFields;
end;


end.
