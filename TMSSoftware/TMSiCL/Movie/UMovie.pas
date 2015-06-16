unit UMovie;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSNativeUIBaseControl,
  FMX.TMSNativeMPMoviePlayerViewController;

type
  TForm1073 = class(TForm)
    TMSFMXNativeMPMoviePlayerViewController1: TTMSFMXNativeMPMoviePlayerViewController;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1073: TForm1073;

implementation

{$R *.fmx}

procedure TForm1073.FormCreate(Sender: TObject);
begin
  TMSFMXNativeMPMoviePlayerViewController1.Location := ExtractFilePath(ParamStr(0)) + 'Nature.mp4';
end;

end.
