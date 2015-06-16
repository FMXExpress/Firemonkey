unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.UILabel,
  DPF.iOS.BaseControl, DPF.iOS.UIButton, DPF.iOS.UIToolbar, iOSAPI.UIKIT;

type
  TForm2 = class(TForm)
    DPFLabel1: TDPFLabel;
    DPFLabel2: TDPFLabel;
    DPFToolbar1: TDPFToolbar;
    DPFButton1: TDPFButton;
    procedure DPFToolbar1BarItems0Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses uMain, Unit1;

procedure TForm2.DPFToolbar1BarItems0Click(Sender: TObject);
begin
  TUIView.OCClass.beginAnimations(nil, nil);
  TUIView.OCClass.setAnimationDuration(0.4);
  fMain.Frame2.SetBounds(fMain.MainView.WIdth, 0, fMain.MainView.Width, fMain.MainView.Height);
  fMain.Frame1.SetBounds(0, 0, fMain.MainView.Width, fMain.MainView.Height);
  TUIView.OCClass.commitAnimations;
end;

end.
