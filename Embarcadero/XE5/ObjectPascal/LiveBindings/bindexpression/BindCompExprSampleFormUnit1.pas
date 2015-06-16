
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit BindCompExprSampleFormUnit1;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Dialogs, System.Rtti, FMX.Edit, Data.Bind.Components, FMX.Layouts, FMX.ListBox,
  Data.Bind.EngExt, FMX.Menus, Fmx.Bind.DBEngExt, FMX.Memo, FMX.StdCtrls, FMX.Styles,
  System.Bindings.Outputs, Fmx.Bind.Editors;

type
  TForm1 = class(TForm)
    BindingsList1: TBindingsList;
    BindEdits: TBindExpression;
    EditOutput: TEdit;
    EditSource: TEdit;
    LabelEditControl: TLabel;
    LabelEditSource: TLabel;
    ButtonEvaluate: TButton;
    CheckBoxActive: TCheckBox;
    BindScopeComponent1: TBindScope;
    ListBoxComponents: TListBox;
    Label1: TLabel;
    ListBox1: TListBox;
    LabelListBox: TLabel;
    LabelSourceExpressions: TLabel;
    LabelControlExpression: TLabel;
    EditControlExpression: TEdit;
    LabelControlComponent: TLabel;
    EditControlComponent: TEdit;
    LabelSourceComponent: TLabel;
    EditSourceComponentName: TEdit;
    LabelEditEnter: TLabel;
    LabelClickList: TLabel;
    MemoSourceExpression: TMemo;
    procedure ButtonEvaluateClick(Sender: TObject);
    procedure EditSourceExpressionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EditSourceChange(Sender: TObject);
    procedure ListBox1Change(Sender: TObject);
    procedure BindEditsActivating(Sender: TObject);
    procedure CheckBoxActiveChange(Sender: TObject);
    procedure BindEditsEvalError(Sender: TObject; AException: Exception);
  private
    FChecking: Boolean;
    FLoaded: Boolean;
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

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
  // Toggle active to use new expression (see BindEditsActivating)
  BindScopeComponent1.Active := False;
  BindScopeComponent1.Active := True;
end;

procedure TForm1.BindEditsActivating(Sender: TObject);
begin
  if FLoaded then
    BindEdits.SourceExpression := MemoSourceExpression.Text;
end;

procedure TForm1.CheckBoxActiveChange(Sender: TObject);
begin
  BindScopeComponent1.Active := CheckBoxActive.IsChecked;
end;

procedure TForm1.EditSourceExpressionChange(Sender: TObject);
begin
  if BindScopeComponent1.Active then
    ButtonEvaluateClick(Self);
end;

procedure TForm1.EditSourceChange(Sender: TObject);
begin
  // Notify so expressions referencing EditSource get reevaluated
  BindingsList1.Notify(Sender, '');

end;

procedure TForm1.ListBox1Change(Sender: TObject);
begin
  // Notify so expressions referencing ListBox1 get reevaluated
  BindingsList1.Notify(ListBox1, '');
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
  LComponent: TComponent;
begin
  FLoaded := True;
  MemoSourceExpression.Text := BindEdits.SourceExpression;
  EditSourceComponentName.Text := BindEdits.SourceComponent.Name;
  EditControlComponent.Text := BindEdits.ControlComponent.Name;
  EditControlExpression.Text := BindEdits.ControlExpression;
  Application.OnIdle := OnIdle;
  // List component names, as information for use in writing expressions
  for I := 0 to Self.ComponentCount - 1 do
  begin
    LComponent := Self.Components[I];
    ListBoxComponents.AddObject(TListBoxItem.Create(ListBoxComponents));
    ListBoxComponents.Items[ListBoxComponents.Count-1] := LComponent.Name;
  end;

  // List sample values
  for I := 0 to 6 - 1 do
  begin
    ListBox1.AddObject(TListBoxItem.Create(ListBox1));
    ListBox1.Items[ListBox1.Count-1] := 'Item' + IntToStr(I);
  end;
  ListBox1.ItemIndex := 0;
end;

procedure TForm1.OnIdle(Sender: TObject; var Done: Boolean);
begin
  FChecking := True;
  try
    CheckBoxActive.IsChecked := BindScopeComponent1.Active;
    LabelEditEnter.Visible := BindScopeComponent1.Active;
    LabelClickList.Visible := BindScopeComponent1.Active;
  finally
    FChecking := False;
  end;

end;

end.
