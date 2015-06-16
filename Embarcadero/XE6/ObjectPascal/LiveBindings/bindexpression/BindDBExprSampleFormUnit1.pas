
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit BindDBExprSampleFormUnit1;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Dialogs, System.Rtti, FMX.Edit, FMX.StdCtrls, FMX.Styles,
  Data.Bind.Components, Data.Bind.DBScope, FMX.Layouts, FMX.ListBox, Data.DB,
  Datasnap.DBClient, Data.Bind.EngExt, FMX.Menus, Fmx.Bind.DBEngExt,
  Fmx.Bind.Editors, System.Bindings.Outputs;

type
  TForm1 = class(TForm)
    BindingsList1: TBindingsList;
    BindEdits: TBindExpression;
    EditOutput: TEdit;
    EditSourceExpression: TEdit;
    LabelEditOutput: TLabel;
    ButtonEvaluate: TButton;
    CheckBoxActive: TCheckBox;
    BindSourceDB1: TBindSourceDB;
    LabelSourceExpressions: TLabel;
    ButtonPrior: TButton;
    ButtonNext: TButton;
    ListBoxFieldNames: TListBox;
    Label3: TLabel;
    Label5: TLabel;
    ListBoxPersistentFields: TListBox;
    ClientDataSetDataSource1: TDataSource;
    ClientDataSet1: TClientDataSet;
    CategoryField: TStringField;
    SpeciesNameField: TStringField;
    LengthCmField: TFloatField;
    LengthInField: TFloatField;
    CommonNameField: TStringField;
    NotesField: TMemoField;
    GraphicField: TBlobField;
    GroupBox1: TGroupBox;
    LabelControlExpression: TLabel;
    EditControlExpression: TEdit;
    LabelControlComponent: TLabel;
    EditControlComponent: TEdit;
    EditSourceComponent: TEdit;
    LabelSourceComponent: TLabel;
    LabelClickNextPrior: TLabel;
    procedure ButtonEvaluateClick(Sender: TObject);
    procedure CheckBoxActiveClick(Sender: TObject);
    procedure EditSourceExpressionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonPriorClick(Sender: TObject);
    procedure ButtonNextClick(Sender: TObject);
    procedure ClientDataSetDataSource1DataChange(Sender: TObject;
      Field: TField);
    procedure BindEditsActivating(Sender: TObject);
    procedure BindEditsEvalError(Sender: TObject; AException: Exception);
  private
    FChecking: Boolean;
    FLoaded: Boolean;
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  public
  end;

var
  Form1: TForm1;

implementation

uses System.Bindings.Helper;

{$R *.fmx}

procedure TForm1.BindEditsEvalError(Sender: TObject; AException: Exception);
begin
  // Generate a new exception with more information
  raise TBindCompException.CreateFmt(
    'Evaluation Exception'#13#10 +
    'Component Name: %s'#13#10 +
    'Exception Class: %s'#13#10 +
    'Exception Message: %s',
    [TComponent(Sender).Name, AException.ClassName, AException.Message]);
end;


procedure TForm1.ButtonEvaluateClick(Sender: TObject);
begin
  // Toggle causes BindEditsActivating to get called
  BindEdits.Active := False;
  BindEdits.Active := True;
end;

procedure TForm1.BindEditsActivating(Sender: TObject);
begin
  // Update expression before activate
  if FLoaded then
    BindEdits.SourceExpression := EditSourceExpression.Text;
end;

procedure TForm1.ButtonNextClick(Sender: TObject);
begin
  BindSourceDB1.DataSource.DataSet.Next;
end;

procedure TForm1.ButtonPriorClick(Sender: TObject);
begin
  BindSourceDB1.DataSource.DataSet.Prior;
end;

procedure TForm1.CheckBoxActiveClick(Sender: TObject);
begin
  if not FChecking then
    ClientDataSetDataSource1.Enabled := not ClientDataSetDataSource1.Enabled;
end;

procedure TForm1.ClientDataSetDataSource1DataChange(Sender: TObject;
  Field: TField);
var
  LField: TField;
begin
  // Call notify to cause expressions, which depend on fields, to be evaluated
  if Field <> nil then
    BindingsList1.Notify(Field, '')
  else
  begin
    Assert(Sender is TDataSource);
    for LField in (Sender as TDataSource).DataSet.Fields do
      BindingsList1.Notify(LField, '');
  end;

end;

procedure TForm1.EditSourceExpressionChange(Sender: TObject);
begin
  if BindEdits.Active then
    ButtonEvaluateClick(Self);
end;


procedure TForm1.FormCreate(Sender: TObject);
var
  LField: TField;
begin
  FLoaded := True;
  Application.OnIdle := OnIdle;
  ClientDataSetDataSource1.Enabled := False;
  EditSourceExpression.Text := Trim(BindEdits.SourceExpression);
  EditControlExpression.Text := BindEdits.ControlExpression;
  EditSourceComponent.Text := BindEdits.SourceComponent.Name;
  EditControlComponent.Text := BindEdits.ControlComponent.Name;
  EditOutput.Text := '';
  with ClientDataSetDataSource1.DataSet do
  begin

    // List field names as a convenience for typing in expressions which use fields
    for LField in Fields do
    begin
      ListBoxFieldNames.AddObject(TListBoxItem.Create(ListBoxFieldNames));
      ListBoxFieldNames.Items[ListBoxFieldNames.Count-1] := LField.FieldName;

      ListBoxPersistentFields.AddObject(TListBoxItem.Create(ListBoxPersistentFields));
      ListBoxPersistentFields.Items[ListBoxPersistentFields.Count-1] := LField.Name;
    end;
  end;
end;

procedure TForm1.OnIdle(Sender: TObject; var Done: Boolean);
begin
  FChecking := True;
  try
    CheckBoxActive.IsChecked := ClientDataSetDataSource1.Enabled;
    ButtonPrior.Enabled := not BindSourceDB1.DataSource.DataSet.BOF;
    ButtonNext.Enabled := not BindSourceDB1.DataSource.DataSet.EOF;
    LabelClickNextPrior.Visible := ClientDataSetDataSource1.Enabled;
  finally
    FChecking := False;
  end;
end;

end.
