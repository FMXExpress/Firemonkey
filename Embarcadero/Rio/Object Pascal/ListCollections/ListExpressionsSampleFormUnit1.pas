//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit ListExpressionsSampleFormUnit1;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Dialogs, System.Rtti, FMX.Edit, Data.Bind.Components, Data.Bind.EngExt, FMX.Menus,
  Fmx.Bind.DBEngExt, System.Bindings.Outputs, FMX.ListBox, FMX.Layouts, SampleCollections,
  Generics.Collections,
  FMX.Bind.Editors, FMX.StdCtrls, FMX.Controls.Presentation; // Used by TBindList to populate a TListBox

type
  TForm1 = class(TForm)
    BindingsList1: TBindingsList;
    EditSourceExpression: TEdit;
    ButtonEvaluate: TButton;
    CheckBoxActive: TCheckBox;
    EditSourceComponentName: TEdit;
    EditControlComponent: TEdit;
    EditControlExpression: TEdit;
    BindList1: TBindList;
    ListBox1: TListBox;
    ComboBox1: TComboBox;
    CheckBoxAutoFill: TCheckBox;
    ButtonClear: TButton;
    Label1: TLabel;
    CheckBoxAutoActivate: TCheckBox;
    procedure ButtonEvaluateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBoxActiveChange(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure BindList1Activating(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure CheckBoxAutoFillChange(Sender: TObject);
    procedure CheckBoxAutoActivateChange(Sender: TObject);
    procedure BindList1EvalError(Sender: TObject; AException: Exception);
  private
    FChecking: Boolean;
    FDataObjects: TDictionary<TCollectionFactory, TObject>;
    FFactory: TCollectionFactory;
    FChanging: Boolean;
    FBindScope1: TBindScope;
  public
    destructor Destroy; override;
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure UpdateDisplayFields;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}
{$R *.iPhone.fmx IOS}
{$R *.SmXhdpiPh.fmx ANDROID}
{$R *.iPad.fmx IOS}
{$R *.LgXhdpiTb.fmx ANDROID}

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

procedure TForm1.BindList1Activating(Sender: TObject);
begin
  if not FChanging then
    if BindList1.FormatExpressions.Count > 0 then
      BindList1.FormatExpressions[0].SourceExpression := EditSourceExpression.Text;
end;

procedure TForm1.ButtonClearClick(Sender: TObject);
begin
  BindList1.ClearList;
end;

procedure TForm1.ButtonEvaluateClick(Sender: TObject);
begin
  BindList1Activating(Self); // Update expression
  BindList1.FillList;
end;

procedure TForm1.CheckBoxActiveChange(Sender: TObject);
begin
  if not FChecking then
    BindList1.Active := CheckBoxActive.IsChecked;
end;

procedure TForm1.CheckBoxAutoActivateChange(Sender: TObject);
begin
  if not FChecking then
    BindList1.AutoActivate := CheckBoxAutoActivate.IsChecked;
end;

procedure TForm1.CheckBoxAutoFillChange(Sender: TObject);
begin
  if not FChecking then
    BindList1.AutoFill := CheckBoxAutoFill.IsChecked;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
var
  LDataObject: TObject;
begin
  FChanging := True;
  try
    if ComboBox1.ItemIndex <> -1 then
    begin
      FFactory := ComboBox1.ListBox.Selected.Data as TCollectionFactory;
    end
    else
    begin
      FFactory := nil;
    end;
    if FFactory <> nil then
    begin
      if BindList1.FormatExpressions.Count = 0 then
        BindList1.FormatExpressions.Add;
      BindList1.FormatExpressions[0].SourceExpression := FFactory.GetExpression;
      BindList1.FormatExpressions[0].ControlExpression := 'Text';
      LDataObject := FFactory.CreateCollection;
      FDataObjects.AddOrSetValue(FFactory, LDataObject);  // Track objects in order to free when not in use
      // Set DataObject last because this can trigger activation and auto fill
      FBindScope1.DataObject := LDataObject;
    end
    else
      FBindScope1.DataObject := nil;
  finally
    FChanging := False;
    UpdateDisplayFields;
  end;
end;

destructor TForm1.Destroy;
begin
  FDataObjects.Free;
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
var
  LFactory: TCollectionFactory;
  LListItem: TListBoxItem;
begin
  FBindScope1 := TBindScope.Create(Self);
  BindList1.SourceComponent := FBindScope1;
  FDataObjects := TObjectDictionary<TCollectionFactory, TObject>.Create([doOwnsValues]);
  // List test data in combobox
  for LFactory in GetCollectionFactories do
  begin
    LListItem := TListBoxItem.Create(ComboBox1);
    ComboBox1.AddObject(LListItem);
    LListItem.Text := LFactory.DisplayName;
    LListItem.Data := LFactory;
  end;
  Application.OnIdle := OnIdle;
  UpdateDisplayFields;
end;

procedure TForm1.OnIdle(Sender: TObject; var Done: Boolean);
begin
  FChecking := True;
  try
    CheckBoxActive.IsChecked := BindList1.Active;
    CheckBoxAutoFill.IsChecked := BindList1.AutoFill;
    CheckBoxAutoActivate.IsChecked := BindList1.AutoActivate;
  finally
    FChecking := False;
  end;
end;

end.
