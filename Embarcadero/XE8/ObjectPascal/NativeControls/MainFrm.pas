unit MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Advertising, FMX.Layouts, FMX.ListBox, FMX.StdCtrls,
  FMX.Calendar, FMX.Controls.Presentation, FMX.Edit;

type
  TForm1 = class(TForm)
    ToolBar1: TToolBar;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Label3: TLabel;
    Edit2: TEdit;
    Calendar1: TCalendar;
    Label4: TLabel;
    Label5: TLabel;
    Edit3: TEdit;
    Edit4: TEdit;
    Label6: TLabel;
    Layout1: TLayout;
    ClearEditButton1: TClearEditButton;
    ClearEditButton2: TClearEditButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
{$R *.iPhone4in.fmx IOS}

end.
