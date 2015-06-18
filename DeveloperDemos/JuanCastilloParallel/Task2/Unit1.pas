unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  System.Threading;

type
  TForm1 = class(TForm)
    btnTask1: TButton;
    lblTask1: TLabel;
    AniIndicator1: TAniIndicator;
    Button1: TButton;
    lblTask2: TLabel;
    Button2: TButton;
    lblTask3: TLabel;
    Button3: TButton;
    lblTask4: TLabel;
    Button4: TButton;
    lblTask5: TLabel;
    btnTask1And2: TButton;
    lblTarea1y2: TLabel;
    procedure btnTask1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure btnTask1And2Click(Sender: TObject);
  private
    FTasks: array[0..4] of ITask;
    function RunTask(aLabel: TLabel): ITask;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.btnTask1And2Click(Sender: TObject);
var
  Tarea1y2: array[0..1] of ITask;
begin
  lblTarea1y2.Text := '--';
  Tarea1y2[0] := FTasks[0];
  Tarea1y2[1] := FTasks[1];
  TTask.Run(
    procedure
    begin
      TTask.WaitForAll(Tarea1y2);
      TThread.Synchronize(nil,
        procedure
        begin
          lblTarea1y2.Text :=
            lblTask1.Text + ' + ' + lblTask2.Text;
        end);
    end);
end;

procedure TForm1.btnTask1Click(Sender: TObject);
begin
  FTasks[0] := RunTask(lblTask1);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FTasks[1] := RunTask(lblTask2);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FTasks[2] := RunTask(lblTask3);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  FTasks[3] := RunTask(lblTask4);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  FTasks[4] := RunTask(lblTask5);
end;

function TForm1.RunTask(aLabel: TLabel): ITask;
begin
  aLabel.Text := '--';
  Result := TTask.Run(
    procedure
    begin
      Sleep(5000);
      TThread.Synchronize(nil,
        procedure
        begin
          aLabel.Text := Random(50).ToString;
        end
      );
    end
  );
end;

end.
