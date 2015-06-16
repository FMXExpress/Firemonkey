
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit SimpleManagedBindEditsForm;

interface
// Use components to created a managed binding between Edit1.Text and Edit2.Text.
// Run the app then type into Edit1 and press <enter> to see the Edti1 is updated.
//
// To change the direction of the binding, double click BindingsList1, select BindExpressionEdit11 and modify
// the Direction property.
// Also see the ManagedBindEdits project, which show information about the bindings and can change the direction at
// runtime.

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, System.Rtti, System.Bindings.Outputs, Data.Bind.Components,
  FMX.StdCtrls, FMX.Styles, FMX.Edit, FMX.Layouts, FMX.ListBox, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  Fmx.Bind.Editors;

type
  TEditsForm1 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    BindingsList1: TBindingsList;
    BindExpressionEdit11: TBindExpression;
    LabelHint: TLabel;
    LabelControl: TLabel;
    LabelSource: TLabel;
    LabelDirection: TLabel;
    procedure EditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  EditsForm1: TEditsForm1;

implementation

{$R *.fmx}

procedure TEditsForm1.FormCreate(Sender: TObject);
var
  LHint: string;
  LDirection: string;
begin
  // Display information about the managed binding
  LabelSource.Text := 'Source: ' + BindExpressionEdit11.SourceComponent.Name;
  LabelControl.Text := 'Control: ' + BindExpressionEdit11.ControlComponent.Name;
  case BindExpressionEdit11.Direction of
    dirSourceToControl:
    begin
      LHint := 'Type into ' + BindExpressionEdit11.SourceComponent.Name + ' and press <enter>';
      LDirection := 'SourceToControl';
    end;
    dirControlToSource:
    begin
      LHint := 'Type into ' + BindExpressionEdit11.ControlComponent.Name + ' and press <enter>';
      LDirection := 'ControlToSource';
    end;
    dirBidirectional:
    begin
      LHint := 'Type into ' + BindExpressionEdit11.SourceComponent.Name + ' or ' +
         BindExpressionEdit11.ControlComponent.Name + ' and press <enter>';
      LDirection := 'Bidirectional';
    end;
  end;
  if BindExpressionEdit11.Active then
    LabelHint.Text := LHint
  else
    LabelHint.Text := '';
  LabelDirection.Text := 'Direction: ' + LDirection;
end;


// Notify when Edit1 or Edit2 text changes.
procedure TEditsForm1.EditChange(Sender: TObject);
begin
  BindingsList1.Notify(Sender, '');
end;


end.
