unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Touch.GestureCtrls, Vcl.ComCtrls,
  System.Actions, Vcl.ActnList, Vcl.Touch.GestureMgr;

type
  TForm1 = class(TForm)
    GestureManager1: TGestureManager;
    ActionList1: TActionList;
    GestureListView1: TGestureListView;
    GesturePreview1: TGesturePreview;
    GestureRecorder1: TGestureRecorder;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
