unit frmAddContact;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit;

type
  TFormAddContact = class(TForm)
    Label1: TLabel;
    edJID: TEdit;
    Label2: TLabel;
    edName: TEdit;
    Label3: TLabel;
    edGroup: TEdit;
    Button1: TButton;
    Button2: TButton;
    Label4: TLabel;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  FormAddContact: TFormAddContact;

implementation

{$R *.fmx}

end.
