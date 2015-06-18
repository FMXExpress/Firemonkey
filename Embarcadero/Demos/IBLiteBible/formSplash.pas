unit formSplash;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls;

//  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Types,
//  FMX.Controls, FMX.StdCtrls, FMX.Forms;

type
  TfrmSplash = class(TForm)
    Label2: TLabel;
    Label3: TLabel;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSplash: TfrmSplash;

implementation

{$R *.fmx}
uses FormMain, dmMain, System.Threading;

procedure TfrmSplash.FormShow(Sender: TObject);
var
  Task: ITask;
begin
  Application.ProcessMessages;

  Task := TTask.Create(procedure ()
            begin
              Sleep(100);
              dtmdlMain := TdtmdlMain.Create(Application);

              TThread.Synchronize(nil, procedure() begin
                                    try
                                      frmMain := TfrmMain.Create(Application);
                                      frmMain.Show;
                                      Self.Hide;
                                    except
                                      on e:exception do begin
                                        ShowMessage(e.message);
                                        Application.Terminate;
                                      end;
                                    end;
                                  end);
            end);
  Task.Start;
end;

end.
