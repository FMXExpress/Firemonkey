
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit SynchControlsSampleForm;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Dialogs, FMX.Edit, FMX.Layouts, FMX.ListBox, FMX.ExtCtrls, Data.Bind.EngExt,
  Data.Bind.Components, FMX.StdCtrls, FMX.Styles, Fmx.DateTimeCtrls,
  Fmx.Bind.Editors, Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs;

type
  TSynchControlsForm = class(TForm)
    BindingsList1: TBindingsList;
    Root1: TLayout;
    Panel1Edit1: TEdit;
    Panel1Label1: TLabel;
    Panel1TrackBar1: TTrackBar;
    Panel3Edit1: TEdit;
    Panel3Label1: TLabel;
    Panel4NumberBox1: TNumberBox;
    Panel4Label1: TLabel;
    Panel2Tracker1: TTrackBar;
    Panel2Tracker2: TTrackBar;
    ToolBar1: TToolBar;
    Grid1: TGridLayout;
    ValueLabel1: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Label4: TLabel;
    Panel2Label1: TLabel;
    Panel5Edit1: TEdit;
    Panel5Edit2: TEdit;
    Panel5: TPanel;
    Panel6ListBox1: TListBox;
    Panel6Edit1: TEdit;
    Panel6: TPanel;
    Panel7ComboBox1: TComboBox;
    Panel7Label1: TLabel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel8TrackBar1: TTrackBar;
    Panel8ProgressBar1: TProgressBar;
    Panel9: TPanel;
    Panel9Calendar1: TCalendar;
    Panel10: TPanel;
    ValueLabel4: TLabel;
    Panel5EditExpressions: TBindExprItems;
    Panel5CheckBox: TCheckBox;
    Panel2TrackerExpressions: TBindExprItems;
    Panel2CheckBox: TCheckBox;
    Panel1TrackerEditExpressions: TBindExprItems;
    Panel1CheckBox1: TCheckBox;
    Panel3LabelEditExpressions: TBindExprItems;
    Panel3CheckBox1: TCheckBox;
    Panel4NumberBoxLabelExpressions: TBindExprItems;
    Panel4CheckBox1: TCheckBox;
    Panel6EditListBoxExpressions: TBindExprItems;
    Panel6CheckBox1: TCheckBox;
    Panel7CheckBox1: TCheckBox;
    Panel8CheckBox1: TCheckBox;
    Panel10CheckBox1: TCheckBox;
    Panel7ComboBoxLabelExpressions: TBindExprItems;
    Panel8TrackBarProgressBarExpressions: TBindExprItems;
    Panel9CalendarTextControlExpressions: TBindExprItems;
    procedure FormCreate(Sender: TObject);
    procedure OnControlChange(Sender: TObject);
    procedure Panel6ListBox1Change(Sender: TObject);
    procedure Panel7ComboBox1Change(Sender: TObject);
    procedure Panel1CheckBox1Change(Sender: TObject);
  private
    FChecking: Boolean;
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SynchControlsForm: TSynchControlsForm;

implementation

{$R *.fmx}


procedure TSynchControlsForm.FormCreate(Sender: TObject);
  procedure AddString(AListBox: TControl; const AText: string);
  var
    AItem: TListboxItem;
  begin
    AItem := TListBoxItem.Create(AListBox);
    AItem.Text := AText;
    AListBox.AddObject(AItem);
  end;
begin
  // TODO: Remove when TComboBox has Items property (like TListBox)
  AddString(Panel7ComboBox1, 'cb one');
  AddString(Panel7ComboBox1, 'cb two');
  AddString(Panel7ComboBox1, 'cb three');
  Application.OnIdle := OnIdle;
end;

procedure TSynchControlsForm.OnIdle(Sender: TObject; var Done: Boolean);
begin
  FChecking := True;
  try
    Panel1CheckBox1.IsChecked := Panel1TrackerEditExpressions.Active;
    Panel2CheckBox.IsChecked := Panel2TrackerExpressions.Active;
    Panel3CheckBox1.IsChecked := Panel3LabelEditExpressions.Active;
    Panel4CheckBox1.IsChecked := Panel4NumberBoxLabelExpressions.Active;
    Panel5CheckBox.IsChecked := Panel5EditExpressions.Active;
    Panel6CheckBox1.IsChecked := Panel6EditListBoxExpressions.Active;
    Panel7CheckBox1.IsChecked := Panel7ComboBoxLabelExpressions.Active;
    Panel8CheckBox1.IsChecked := Panel8TrackBarProgressBarExpressions.Active;
    Panel10CheckBox1.IsChecked := Panel9CalendarTextControlExpressions.Active;
  finally
    FChecking := False;
  end;
end;

procedure TSynchControlsForm.Panel1CheckBox1Change(Sender: TObject);
begin
  if not FChecking then
  begin
    Panel1TrackerEditExpressions.Active := Panel1CheckBox1.IsChecked;
    Panel2TrackerExpressions.Active := Panel2CheckBox.IsChecked;
    Panel3LabelEditExpressions.Active := Panel3CheckBox1.IsChecked;
    Panel4NumberBoxLabelExpressions.Active := Panel4CheckBox1.IsChecked;
    Panel5EditExpressions.Active := Panel5CheckBox.IsChecked;
    Panel6EditListBoxExpressions.Active := Panel6CheckBox1.IsChecked;
    Panel7ComboBoxLabelExpressions.Active := Panel7CheckBox1.IsChecked;
    Panel8TrackBarProgressBarExpressions.Active := Panel8CheckBox1.IsChecked;
    Panel9CalendarTextControlExpressions.Active := Panel10CheckBox1.IsChecked;
  end;
end;

var
  FNotifying: Integer;
procedure TSynchControlsForm.OnControlChange(Sender: TObject);
begin
  // Some controls send notifications when setting properties,
  // like TTrackBar
  if FNotifying = 0 then
  begin
    Inc(FNotifying);
    // Send notification to cause expression re-evaluation of dependent expressions
    try
      BindingsList1.Notify(Sender, '');
    finally
      Dec(FNotifying);
    end;
  end
end;

procedure TSynchControlsForm.Panel7ComboBox1Change(Sender: TObject);
begin
  // Can't use OnControlChange directly for ComboBox because Sender is TListItem
  OnControlChange(Panel7ComboBox1);
end;

procedure TSynchControlsForm.Panel6ListBox1Change(Sender: TObject);
begin
  // Can't use OnControlChange directly for ListBox because Sender is TListItem
  OnControlChange(Panel6ListBox1);
end;


end.
