unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Edit, FMX.StdCtrls, FMX.Platform,
  FMX.Notification, FMX.ListBox, FMX.Layouts, FMX.Controls.Presentation,
  FMX.EditBox, FMX.NumberBox;

type
  TSettingBadgeNumberForm = class(TForm)
    btnSetBadgeNumber: TButton;
    ToolBar1: TToolBar;
    Label1: TLabel;
    ListBox1: TListBox;
    ListBoxItem1: TListBoxItem;
    nbBadgeNumber: TNumberBox;
    btnBadgeNumberDown: TButton;
    btnBadgeNumberUp: TButton;
    btnResetBadgeNumber: TButton;
    NotificationC: TNotificationCenter;
    procedure btnSetBadgeNumberClick(Sender: TObject);
    procedure btnBadgeNumberDownClick(Sender: TObject);
    procedure btnBadgeNumberUpClick(Sender: TObject);
    procedure btnResetBadgeNumberClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetBadgeNumber: single;
  public
    { Public declarations }
  end;

var
  SettingBadgeNumberForm: TSettingBadgeNumberForm;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure TSettingBadgeNumberForm.btnSetBadgeNumberClick(Sender: TObject);
begin
  { set Icon Badge Number }

  if NotificationC.Supported then
    NotificationC.ApplicationIconBadgeNumber := Trunc(nbBadgeNumber.Value);
end;

procedure TSettingBadgeNumberForm.FormCreate(Sender: TObject);
begin
  { display current Icon Badge Number }
  nbBadgeNumber.Value := GetBadgeNumber;
end;

procedure TSettingBadgeNumberForm.btnBadgeNumberDownClick(Sender: TObject);
begin
  nbBadgeNumber.Value := nbBadgeNumber.Value - 1;
end;

procedure TSettingBadgeNumberForm.btnBadgeNumberUpClick(Sender: TObject);
begin
  nbBadgeNumber.Value := nbBadgeNumber.Value + 1;
end;

procedure TSettingBadgeNumberForm.btnResetBadgeNumberClick(Sender: TObject);
begin
  { reset Icon Badge Number }
  if NotificationC.Supported then
    NotificationC.ApplicationIconBadgeNumber := 0;

  nbBadgeNumber.Value := 0;
end;


function TSettingBadgeNumberForm.GetBadgeNumber;
begin
  Result:= NotificationC.ApplicationIconBadgeNumber;
end;

end.
