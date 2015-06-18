unit UnitFormMain;

{===============================================================================
 CodeRage 9 - Demo for Task Chaining using WaitForAll

 This code shows how to chain Tasks together (Fork / Join pattern)

 Using the ITask interface you can let new Tasks wait for results from previous
 Tasks.

 Be careful when using Synchronize in a TTask.Run combined with a WaitForAll or
 WaitForAny in the main thread. These may cause a deadlock, because the Task
 is waiting for the main thread to perform the Synchronize and the main thread
 is waiting for the task to complete in WaitForAll or WaitForAny. It is safe to
 use Synchronize if you do the WaitForAll from inside the TTask.Run as in this
 example.

 Author: Danny Wind
===============================================================================}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  System.Threading, FMX.Ani;

type
  TFormMain = class(TForm)
    ButtonTask1: TButton;
    ButtonTask2: TButton;
    ButtonTask3: TButton;
    ButtonTask4: TButton;
    ButtonTask5: TButton;
    ButtonTask1Plus2: TButton;
    ButtonTask4Plus5: TButton;
    ButtonTaskSumAll: TButton;
    ScrollBarActivity: TScrollBar;
    FloatAnimationActivity: TFloatAnimation;
    LabelTask1: TLabel;
    LabelTask2: TLabel;
    LabelTask3: TLabel;
    LabelTask4: TLabel;
    LabelTask5: TLabel;
    LabelTask1Plus2: TLabel;
    LabelTask4Plus5: TLabel;
    LabelTaskSumAll: TLabel;
    procedure ButtonTask1Click(Sender: TObject);
    procedure ButtonTask2Click(Sender: TObject);
    procedure ButtonTask3Click(Sender: TObject);
    procedure ButtonTask4Click(Sender: TObject);
    procedure ButtonTask5Click(Sender: TObject);
    procedure ButtonTaskSumAllClick(Sender: TObject);
    procedure ButtonTask1Plus2Click(Sender: TObject);
    procedure ButtonTask4Plus5Click(Sender: TObject);
  private
    { Private declarations }
    AllTasks: array [0..4] of ITask;
  public
    { Public declarations }
    procedure RunTask(aLabel: TLabel;var aTask: ITask);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.ButtonTask1Click(Sender: TObject);
begin
  LabelTask1.Text := '--';
  RunTask(LabelTask1, AllTasks[0]);
end;

procedure TFormMain.ButtonTask2Click(Sender: TObject);
begin
  LabelTask2.Text := '--';
  RunTask(LabelTask2, AllTasks[1]);
end;

procedure TFormMain.ButtonTask3Click(Sender: TObject);
begin
  LabelTask3.Text := '--';
  RunTask(LabelTask3, AllTasks[2]);
end;

procedure TFormMain.ButtonTask4Click(Sender: TObject);
begin
  LabelTask4.Text := '--';
  RunTask(LabelTask4, AllTasks[3]);
end;

procedure TFormMain.ButtonTask5Click(Sender: TObject);
begin
  LabelTask5.Text := '--';
  RunTask(LabelTask5, AllTasks[4]);
end;

procedure TFormMain.ButtonTask4Plus5Click(Sender: TObject);
var
  Tasks4to5: array[0..1] of ITask;
begin
  LabelTask4Plus5.Text := '--';
  Tasks4to5[0] := AllTasks[3];
  Tasks4to5[1] := AllTasks[4];
  TTask.Run( procedure
             begin
               TTask.WaitForAll(Tasks4to5);
               TThread.Synchronize(nil,
                 procedure
                 begin
                   LabelTask4Plus5.Text :=
                     LabelTask4.Text + ' + ' + LabelTask5.Text;
                 end);
             end );
end;

procedure TFormMain.ButtonTaskSumAllClick(Sender: TObject);
begin
  LabelTaskSumAll.Text := '--';
  TTask.Run( procedure
             var
               lSumAll: Integer;
             begin
               TTask.WaitForAll(AllTasks);
               TThread.Synchronize(nil,
                 procedure
                 begin
                   lSumAll := LabelTask1.Text.ToInteger
                             + LabelTask2.Text.ToInteger
                             + LabelTask3.Text.ToInteger
                             + LabelTask4.Text.ToInteger
                             + LabelTask5.Text.ToInteger;
                   LabelTaskSumAll.Text := lSumAll.ToString;
                 end);
             end );
end;

procedure TFormMain.ButtonTask1Plus2Click(Sender: TObject);
var
  Tasks1to2: array[0..1] of ITask;
begin
  LabelTask1Plus2.Text := '--';
  Tasks1to2[0] := AllTasks[0];
  Tasks1to2[1] := AllTasks[1];
  TTask.Run( procedure
             begin
               TTask.WaitForAll(Tasks1to2);
               TThread.Synchronize(nil,
                 procedure
                 begin
                   LabelTask1Plus2.Text :=
                     LabelTask1.Text + ' + ' + LabelTask2.Text;
                 end);
             end );
end;

procedure TFormMain.RunTask(aLabel: TLabel;var aTask: ITask);
begin
  aTask := TTask.Run(
             procedure
               var
                 lValue: Integer;
               begin
                 {Some calculation that takes time}
                 Sleep(3000);
                 lValue := Random(10);
                 TThread.Synchronize(nil, procedure
                                          begin
                                            aLabel.Text := lValue.ToString;
                                          end);
               end );
end;


end.
