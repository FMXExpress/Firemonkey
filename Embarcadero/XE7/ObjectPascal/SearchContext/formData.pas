unit formData;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.Components, FMX.Layouts, FMX.Memo, FMX.Edit, Fmx.Bind.Navigator,
  Fmx.Bind.DBLinks, Data.Bind.DBScope, Data.Bind.DBLinks, FMX.StdCtrls,
  Data.Bind.Controls, FMX.Controls.Presentation;

type
  TfrmManageData = class(TForm)
    BindNavigator1: TBindNavigator;
    ImageControl1: TImageControl;
    Edit1: TEdit;
    Edit2: TEdit;
    Memo1: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    BindingsList1: TBindingsList;
    BindExpressionLabel11: TBindExpression;
    BindScopeDB1: TBindScopeDB;
    DBLinkEdit1Category1: TBindDBEditLink;
    DBLinkEdit2Description1: TBindDBEditLink;
    DBLinkMemo1SearchTerms1: TBindDBMemoLink;
    DBLinkImageControl1Icon1: TBindDBImageLink;
    BindExpressionLabel21: TBindExpression;
    BindExpressionLabel41: TBindExpression;
    BindExpressionLabel31: TBindExpression;
    OpenDialog1: TOpenDialog;
    procedure ImageControl1DblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmManageData: TfrmManageData;

implementation

uses dataData;

{$R *.fmx}

procedure TfrmManageData.ImageControl1DblClick(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    ImageControl1.Bitmap.LoadFromFile(OpenDialog1.FileName);
  end;
end;

end.
