//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit MouseoverHintsForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Menus,
  FMX.Edit, System.Actions, FMX.ActnList;

type
  TMouseoverHintForm = class(TForm)
    Button1: TButton;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    StatusBar1: TStatusBar;
    Label1: TLabel;
    Panel1: TPanel;
    Button5: TButton;
    Button6: TButton;
    ActionList1: TActionList;
    Action1: TAction;
    HintEdit: TEdit;
    Label2: TLabel;
    ApplicationShowHint: TCheckBox;
    FormShowHint: TCheckBox;
    StatusBarAutohintCheckBox: TCheckBox;
    Action2: TAction;
    ApplicationShortCutsInHintsCheckBox: TCheckBox;
    Button2: TButton;
    Action3: TAction;
    Button3: TButton;
    Button4: TButton;
    EnableActionOnHintCheckBox: TCheckBox;
    procedure StatusBar1Hint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HintEditChange(Sender: TObject);
    procedure ApplicationShowHintChange(Sender: TObject);
    procedure FormShowHintChange(Sender: TObject);
    procedure StatusBarAutohintCheckBoxChange(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
    procedure Action2Execute(Sender: TObject);
    procedure Action2Hint(var HintStr: string; var CanShow: Boolean);
    procedure ApplicationShortCutsInHintsCheckBoxChange(Sender: TObject);
    procedure Action3Execute(Sender: TObject);
    procedure Action3Hint(var HintStr: string; var CanShow: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MouseoverHintForm: TMouseoverHintForm;

implementation

uses
  FMX.Utils;

{$R *.fmx}

procedure TMouseoverHintForm.Action1Execute(Sender: TObject);
begin
//
end;

procedure TMouseoverHintForm.Action2Execute(Sender: TObject);
begin
//
end;

procedure TMouseoverHintForm.Action2Hint(var HintStr: string; var CanShow: Boolean);
begin
  HintStr := 'Hint by Code';
  CanShow := EnableActionOnHintCheckBox.IsChecked;
end;

procedure TMouseoverHintForm.Action3Execute(Sender: TObject);
begin
//
end;

procedure TMouseoverHintForm.Action3Hint(var HintStr: string; var CanShow: Boolean);
begin
  HintStr := HintStr + ' a tail';
  CanShow := EnableActionOnHintCheckBox.IsChecked;
end;

procedure TMouseoverHintForm.ApplicationShortCutsInHintsCheckBoxChange(Sender: TObject);
begin
  Application.HintShortCuts := ApplicationShortCutsInHintsCheckBox.IsChecked;
end;

procedure TMouseoverHintForm.ApplicationShowHintChange(Sender: TObject);
begin
  Application.ShowHint := ApplicationShowHint.IsChecked;
end;

procedure TMouseoverHintForm.FormCreate(Sender: TObject);
begin
  HintEdit.Text := Button1.Hint;
  ApplicationShowHint.IsChecked := Application.ShowHint;
  FormShowHint.IsChecked := ShowHint;
  StatusBarAutohintCheckBox.IsChecked := StatusBar1.AutoHint;
  ApplicationShortCutsInHintsCheckBox.IsChecked := Application.HintShortCuts;
end;

procedure TMouseoverHintForm.FormShowHintChange(Sender: TObject);
begin
  ShowHint := FormShowHint.IsChecked;
end;

procedure TMouseoverHintForm.HintEditChange(Sender: TObject);
begin
  Button1.Hint := HintEdit.Text;
end;

procedure TMouseoverHintForm.StatusBar1Hint(Sender: TObject);
begin
  Label1.Text := GetLongHint(Application.Hint);
end;

procedure TMouseoverHintForm.StatusBarAutohintCheckBoxChange(Sender: TObject);
begin
  StatusBar1.AutoHint := StatusBarAutohintCheckBox.IsChecked;
end;

end.

