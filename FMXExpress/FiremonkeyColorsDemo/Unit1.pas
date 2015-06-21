unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani,
  FMX.TabControl, FMX.Objects, FMX.StdCtrls, FMX.Layouts, FMX.Memo;

type
  TForm1 = class(TForm)
    ToolBar1: TToolBar;
    StyleBook1: TStyleBook;
    ToolBar2: TToolBar;
    Rectangle1: TRectangle;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    Rectangle2: TRectangle;
    Rectangle3: TRectangle;
    Rectangle4: TRectangle;
    GradientAnimation1: TGradientAnimation;
    Label1: TLabel;
    FloatAnimation1: TFloatAnimation;
    Label2: TLabel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
FloatAnimation1.StartValue := Rectangle1.Width;
FloatAnimation1.StopValue := 0-Label1.Width;
FloatAnimation1.Enabled := True;
end;

end.
