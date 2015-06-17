unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,GuideFrame,uUIFunction;

type
  TfrmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  ShowFrame(TFrame(GlobalGuideFrame),TFrameGuide,Self,nil,nil,nil,Self);
  GlobalGuideFrame.FrameHistroy:=CurrentFrameHistroy;
end;

end.
