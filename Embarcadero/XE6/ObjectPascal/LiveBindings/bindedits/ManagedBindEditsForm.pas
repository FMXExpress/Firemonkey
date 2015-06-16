
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
// Use components to created a managed binding between Edit1.Text and Edit2.Text.
// Show the managed bindings created in the expression engine.
// Show the assignment that occur when the expression engine re-evaluates an expression when notified of a property change.
// Allow the user to change the direction of the binding at runtime.
//
// Also see the SimpleManagedBindEdits project, which sets up the same binding but doesn't
// display as much information.

unit ManagedBindEditsForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, Data.Bind.EngExt, FMX.StdCtrls, FMX.Styles,
  Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs, Data.Bind.Components,
  FMX.Edit, FMX.Layouts, FMX.ListBox, Fmx.Bind.Editors;

type
  TEditsForm1 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    BindingsList1: TBindingsList;
    BindExpressionEdit11: TBindExpression;
    ListBoxBindingExpressions: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    CheckBoxActive: TCheckBox;
    GroupBox1: TGroupBox;
    RadioButtonSourceToControl: TRadioButton;
    RadioButtonControlToSource: TRadioButton;
    RadioButtonBidirectional: TRadioButton;
    ListBoxAssigningValues: TListBox;
    Label4: TLabel;
    ButtonClear: TButton;
    LabelSource: TLabel;
    LabelControl: TLabel;
    ButtonResetValues: TButton;
    LabelHint: TLabel;
    ButtonNotifyAll: TButton;
    procedure EditChange(Sender: TObject);
    procedure BindExpressionEdit11Activated(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBoxActiveChange(Sender: TObject);
    procedure RadioButtonSourceToControlChange(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure BindExpressionEdit11AssignedValue(Sender: TObject;
      AssignValueRec: TBindingAssignValueRec; const Value: TValue);
    procedure ButtonResetValuesClick(Sender: TObject);
    procedure ButtonNotifyAllClick(Sender: TObject);
  private
    FChecking: Boolean;
    FEdit1Text: string;
    FEdit2Text: string;
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure ExpressionDeactivated;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  EditsForm1: TEditsForm1;

implementation

{$R *.fmx}

uses  System.Bindings.EvalProtocol, System.TypInfo,
  System.Bindings.EvalSys,
  System.Bindings.Expression,
  System.Bindings.Methods,
  System.Bindings.ObjEval,
  System.Bindings.Helper,
  System.Bindings.Consts,
  System.Bindings.Manager,
  System.Bindings.Factories;

type
  TBindingExpressionCracker = class(TBindingExpression);

// Write descriptions of managed bindings to TStrings
procedure GetManagedBindings(AStrings: TStrings);
var
  I: Integer;
  LExpression: TBindingExpression;
  LOutputExpr: string;
  LInputExpr: string;
  LManager: TBindingManager;
  LLocation: ILocation;
  LChild: IChild;
  LScope: IScope;
  LScopeSelf: IScopeSelf;
  LSelf: IInterface;
  LSourceObject: TObject;
begin
  LSourceObject := nil;
  AStrings.Clear;
  LManager := TBindingManagerFactory.AppManager;
  for I := 0 to LManager.ExprCount - 1 do
  begin
    LExpression := LManager.Expressions[i];
    LInputExpr := LExpression.Source;
    LOutputExpr := LExpression.Outputs.Destinations.ToArray[0].Value.Value;
    LLocation := LExpression.Outputs.Destinations.ToArray[0].Key;
    LChild := LLocation as IChild;
    for LScope in TBindingExpressionCracker(LExpression).FScopes do
    begin
      if Supports(LScope, IScopeSelf, LScopeSelf) then
      begin
        LSelf := LScopeSelf.GetSelf;
        LSourceObject := (LSelf as IValue).GetValue.AsObject;
        break;
      end;
    end;
    Assert(LSourceObject is TComponent);
    Assert(LChild.Parent is TComponent);
    AStrings.Add(Format('[%d] %s.%s => %s.%s', [I,
      (LSourceObject as TComponent).Name, LInputExpr, (LChild.Parent as TComponent).Name, LOutputExpr]));
  end;
end;

// Call notify on source component of all managed bindings
// Assumes scope contains a TComponent qualifed by "Self"
procedure NotifyAll;
var
  I: Integer;
  LExpression: TBindingExpression;
  LManager: TBindingManager;
  LScope: IScope;
  LScopeSelf: IScopeSelf;
  LSelf: IInterface;
  LSourceObject: TObject;
begin
  LSourceObject := nil;
  LManager := TBindingManagerFactory.AppManager;
  for I := 0 to LManager.ExprCount - 1 do
  begin
    LExpression := LManager.Expressions[i];
    for LScope in TBindingExpressionCracker(LExpression).FScopes do
    begin
      if Supports(LScope, IScopeSelf, LScopeSelf) then
      begin
        LSelf := LScopeSelf.GetSelf;
        LSourceObject := (LSelf as IValue).GetValue.AsObject;
        break;
      end;
    end;
    Assert(LSourceObject is TComponent);
    TBindings.Notify(LSourceObject as TComponent, '');
  end;
end;

// Write a description of a TValue to TStrings
procedure ListValue(AType: PTypeInfo; AValue: TValue; AStrings: TStrings);
begin
  if AValue.IsEmpty then
    AStrings.Add('  Empty')
  else
  begin
    if AValue.IsObject then
      AStrings.Add(Format('  ClassName: %s', [(AValue.AsObject as TComponent).ClassName]))
    else
      try
        if AType <> nil then
          AStrings.Add(Format('  ToString: "%s"', [AType.Name, AValue.ToString]))
        else
          AStrings.Add(Format('  ToString: "%s"', [AValue.ToString]))
      except
        on E: Exception do
          AStrings.Add(Format('  %s: %s', [E.ClassName,E.Message]));
      end;
  end;
end;

// Write a description of an expression engine assignment operation to TStrings
procedure ListAssignedValue(
  AssignValueRec: TBindingAssignValueRec; const Value: TValue; AStrings: TStrings);
var
  LBindingExpression: TBindingExpression;
  LScope: IScope;
  LScopeSelf: IScopeSelf;
  LSelf: IInterface;
  LSourceObject: TObject;
begin
  LSourceObject := nil;
  LBindingExpression := AssignValueRec.Expression as TBindingExpression;
  for LScope in TBindingExpressionCracker(LBindingExpression).FScopes do
  begin
    if Supports(LScope, IScopeSelf, LScopeSelf) then
    begin
      LSelf := LScopeSelf.GetSelf;
      LSourceObject := (LSelf as IValue).GetValue.AsObject;
      break;
    end;
  end;
  Assert(LSourceObject is TComponent);
  AStrings.Add(Format('Source Object: %s', [(LSourceObject as TComponent).Name]));
  AStrings.Add(Format('Source Expression: %s', [LBindingExpression.Source]));
  ListValue(nil, Value, AStrings);
  AStrings.Add(Format('Out Object: %s', [(AssignValueRec.OutObj as TComponent).Name]));
  AStrings.Add(Format('Out Property: %s', [AssignValueRec.OutProp]));
  AStrings.Add('');
end;

// Update GUI when the expression is deactivated
procedure TEditsForm1.ExpressionDeactivated;
begin
  GetManagedBindings(ListBoxBindingExpressions.Items);
  ListBoxAssigningValues.Clear;
end;

procedure TEditsForm1.BindExpressionEdit11Activated(Sender: TObject);
begin
  GetManagedBindings(ListBoxBindingExpressions.Items);
end;

procedure TEditsForm1.BindExpressionEdit11AssignedValue(Sender: TObject;
  AssignValueRec: TBindingAssignValueRec; const Value: TValue);
begin
  ListAssignedValue(AssignValueRec, Value, ListBoxAssigningValues.Items);
end;

procedure TEditsForm1.ButtonClearClick(Sender: TObject);
begin
  ListBoxAssigningValues.Clear;
end;

procedure TEditsForm1.ButtonNotifyAllClick(Sender: TObject);
begin
  NotifyAll;
end;

procedure TEditsForm1.ButtonResetValuesClick(Sender: TObject);
begin
  Edit1.Text := FEdit1Text;
  Edit2.Text := FEdit2Text;
end;

procedure TEditsForm1.CheckBoxActiveChange(Sender: TObject);
begin
  if not FChecking then
  begin
    BindExpressionEdit11.Active := not BindExpressionEdit11.Active;
    if not BindExpressionEdit11.Active  then
      ExpressionDeactivated;
  end;
end;


procedure TEditsForm1.FormCreate(Sender: TObject);
begin
  Application.OnIdle := OnIdle;
  FEdit1Text := Edit1.Text;
  FEdit2Text := Edit2.Text;
  LabelSource.Text := 'Source: ' + BindExpressionEdit11.SourceComponent.Name;
  LabelControl.Text := 'Control: ' + BindExpressionEdit11.ControlComponent.Name;
end;

procedure TEditsForm1.OnIdle(Sender: TObject; var Done: Boolean);
var
  LControlName: string;
begin
  FChecking := True;
  try
    CheckBoxActive.IsChecked := BindExpressionEdit11.Active;
    case BindExpressionEdit11.Direction of
      dirSourceToControl:
      begin
        LControlName :=  BindExpressionEdit11.SourceComponent.Name;
        RadioButtonSourceToControl.IsChecked := True;
      end;
      dirControlToSource:
      begin
        LControlName :=  BindExpressionEdit11.ControlComponent.Name;
        RadioButtonControlToSource.IsChecked := True;
      end;
      dirBidirectional:
      begin
        LControlName := BindExpressionEdit11.SourceComponent.Name + ' or ' +
           BindExpressionEdit11.ControlComponent.Name;
        RadioButtonBidirectional.IsChecked := True;
      end;
    end;
    if BindExpressionEdit11.Active then
    begin
      LabelHint.Text := 'Type into ' + LControlName + ' and press <enter>'
    end
    else
      LabelHint.Text := 'Check "Active" to bind controls';

  finally
    FChecking := False;
  end;
end;

procedure TEditsForm1.RadioButtonSourceToControlChange(Sender: TObject);
begin
  if not FChecking then
  begin
    BindExpressionEdit11.Active := False;
    ExpressionDeactivated;
    if RadioButtonSourceToControl.IsChecked then
      BindExpressionEdit11.Direction := dirSourceToControl
    else if RadioButtonControlToSource.IsChecked then
      BindExpressionEdit11.Direction := dirControlToSource
    else
      BindExpressionEdit11.Direction := dirBidirectional
  end;
end;

// Notify when Edit1 or Edit2 text changes.  This is the only code needed to make
// the managed bindings work once the component is Active.  The other code in this unit is to support the GUI for setting binding properties,
// showing managed bindings, showing assignments, etc.
procedure TEditsForm1.EditChange(Sender: TObject);
begin
  BindingsList1.Notify(Sender, '');
end;


end.
