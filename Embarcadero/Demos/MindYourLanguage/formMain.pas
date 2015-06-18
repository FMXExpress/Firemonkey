unit formMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, unitMyClasses, FMX.Edit,
  FMX.Layouts, FMX.Memo, contnrs, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    btnListFee: TButton;
    btnAddFee: TButton;
    Edit1: TEdit;
    btnAddFoo: TButton;
    SpinBox1: TSpinBox;
    btnListFoo: TButton;
    btnListInterface: TButton;
    btnRTTIAsString: TButton;
    Button1: TButton;
    procedure btnAddFeeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddFooClick(Sender: TObject);
    procedure btnListFeeClick(Sender: TObject);
    procedure btnListFooClick(Sender: TObject);
    procedure btnListInterfaceClick(Sender: TObject);
    procedure btnRTTIAsStringClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FMyList : TMyBaseList;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
uses RTTI;

procedure TForm1.btnAddFeeClick(Sender: TObject);
begin
  FMyList.Add(TMyFee.Create(Trunc(SpinBox1.Value),Edit1.Text));
end;

procedure TForm1.btnAddFooClick(Sender: TObject);
begin
  FMyList.Add(TMyFoo.Create(Trunc(SpinBox1.Value),Edit1.Text));
end;

procedure TForm1.btnListFeeClick(Sender: TObject);
var
  MyFee: TMyFee;
  Obj : TMyBaseObject;
begin
  Memo1.Lines.Clear;
  for Obj in FMyList do begin
    if not (Obj is TMyFee) then
      Continue;

    MyFee := TMyFee(Obj);
    Memo1.Lines.Add(Format('%S : [%d]',[MyFee.Fee, MyFee.ID]));
  end;
end;

procedure TForm1.btnListFooClick(Sender: TObject);
var
  MyFoo: TMyFoo;
  Obj : TMyBaseObject;
  I: Integer;
begin
  Memo1.Lines.Clear;
  for I := 0 to Pred(FMyList.Count) do begin
    Obj := FMyList.Items[I];
    if not (Obj is TMyFoo) then
      Continue;

    MyFoo := TMyFoo(Obj);
    Memo1.Lines.Add(Format('%S : [%d]',[MyFoo.Foo, MyFoo.ID]));
  end;
end;


procedure TForm1.btnListInterfaceClick(Sender: TObject);
var
  MyI: IMyInterface;
  BaseObj : TMyBaseObject;
begin
  Memo1.Lines.Clear;
  for BaseObj in FMyList do begin
    if BaseObj.GetInterface(IMyInterface, MyI) then begin
      Memo1.Lines.Add(Format('%S : [%d]',[MyI.GetAsString, BaseObj.ID]));
    end;
  end;
end;

procedure TForm1.btnRTTIAsStringClick(Sender: TObject);
var
  ctx : TRttiContext;
  t : TRttiType;
  p : TRttiProperty;
  v : TValue;
  BaseObj: TMyBaseObject;
begin
  Memo1.Lines.Clear;

  ctx := TRttiContext.Create;
  try
    for BaseObj in FMyList do begin
      t := ctx.GetType(BaseObj.ClassType); // Create a RTTI Type of the class type.
      p := t.GetProperty('AsString');    // Check if the class has a specific public / published method
      if p <> nil then begin
        v := p.GetValue(BaseObj);      // if So, get a pointer to the value of a specific object
        Memo1.Lines.Add(Format('%S : [%d]',[v.AsString, BaseObj.ID]));
      end;
    end;

  finally
    ctx.Free;
  end;

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FMyList := TMyBaseList.Create(True);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FMyList.Free;
end;

end.
