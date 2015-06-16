
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit BindExpressionSampleFormUnit1;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Dialogs, System.Rtti, FMX.Edit, Data.Bind.Components, Data.Bind.EngExt, FMX.Menus, FMX.StdCtrls, FMX.Styles,
  Fmx.Bind.DBEngExt, System.Bindings.Outputs, Fmx.Bind.Editors;

type
  TForm1 = class(TForm)
    BindingsList1: TBindingsList;
    BindEdits: TBindExpression;
    EditOutput: TEdit;
    EditSource: TEdit;
    EditSourceExpression: TEdit;
    LabelEditOutput: TLabel;
    LabelEditSource: TLabel;
    ButtonEvaluate: TButton;
    CheckBoxActive: TCheckBox;
    EditSourceComponentName: TEdit;
    EditControlComponent: TEdit;
    EditControlExpression: TEdit;
    LabelPressEnter: TLabel;
    procedure ButtonEvaluateClick(Sender: TObject);
    procedure EditSourceExpressionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EditSourceChange(Sender: TObject);
    procedure CheckBoxActiveChange(Sender: TObject);
    procedure BindEditsActivating(Sender: TObject);
    procedure BindEditsEvalError(Sender: TObject; AException: Exception);
  private
    FChecking: Boolean;
    FLoaded: Boolean;
    procedure OnIdle(Sender: TObject; var Done: Boolean);
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
  BindEdits.Active := False;
  BindEdits.Active := True;
end;

procedure TForm1.BindEditsActivating(Sender: TObject);
begin
  // Update expression before evaluation
  if FLoaded then
    BindEdits.SourceExpression := EditSourceExpression.Text;
end;


procedure TForm1.CheckBoxActiveChange(Sender: TObject);
begin
  if not FChecking then
    BindEdits.Active := CheckBoxActive.IsChecked;
end;

procedure TForm1.EditSourceExpressionChange(Sender: TObject);
begin
  if BindEdits.Active then
    ButtonEvaluateClick(Self);
end;

procedure TForm1.EditSourceChange(Sender: TObject);
begin
  // Re-evaluate expressions which depend on the control
  BindingsList1.Notify(Sender, '');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FLoaded := True;
  EditSourceExpression.Text := BindEdits.SourceExpression;
  EditSourceComponentName.Text := BindEdits.SourceComponent.Name;
  EditControlComponent.Text := BindEdits.ControlComponent.Name;
  EditControlExpression.Text := BindEdits.ControlExpression;
  Application.OnIdle := OnIdle;
end;

procedure TForm1.OnIdle(Sender: TObject; var Done: Boolean);
begin
  FChecking := True;
  try
    CheckBoxActive.IsChecked := BindEdits.Active;
    LabelPressEnter.Visible := BindEdits.Active;
  finally
    FChecking := False;
  end;
end;

end.
