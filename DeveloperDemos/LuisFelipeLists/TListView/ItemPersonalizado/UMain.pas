unit UMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListView.Types, Data.DB, Datasnap.DBClient, FMX.ListView, System.Rtti,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Data.Bind.Components, Data.Bind.DBScope, FMX.Objects;

type
  TForm9 = class(TForm)
    ToolBar1: TToolBar;
    Label1: TLabel;
    ListView1: TListView;
    ClientDataSet1: TClientDataSet;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkFillControlToField1: TLinkFillControlToField;
    Image1: TImage;
    procedure ListView1UpdateObjects(const Sender: TObject;
      const AItem: TListViewItem);
    procedure LinkFillControlToField1FilledListItem(Sender: TObject;
      const AEditor: IBindListEditorItem);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form9: TForm9;

implementation

{$R *.fmx}

procedure TForm9.LinkFillControlToField1FilledListItem(Sender: TObject;
  const AEditor: IBindListEditorItem);
var
  AListItem : TListViewItem;
  AItemText : TListItemText;
  AItemImage : TListItemImage;
  AField : TField;
begin
  // Otro Evento que debo implementar
  if(AEditor.CurrentIndex >= 0)then
  begin
    AListItem := ListView1.Items[AEditor.CurrentIndex];

    AItemText := AListItem.Objects.FindObject('Salary') as TListItemText;
    AField := BindSourceDB1.DataSet.FieldByName('Salary');
    if(AItemText <> nil) and (AField <> nil)then
    begin
      AItemText.Text := 'Bs. ' + FormatFloat('#,#00.00', AField.AsFloat);
      if(AField.AsFloat >= 45000)then
      begin
        AItemText.TextColor := TAlphaColorRec.Blue;
      end
      else
      begin
        AItemText.TextColor := TAlphaColorRec.Red;
      end;
    end;
    AItemText := AListItem.Objects.FindObject('HireDate') as TListItemText;
    AField := BindSourceDB1.DataSet.FieldByName('HireDate');
    if(AItemText <> nil) and (AField <> nil)then
    begin
      AItemText.Text := FormatDateTime('dd/MM/yyy', AField.AsDateTime);
    end;

    AItemImage := AListItem.Objects.FindObject('MiImagen') as TListItemImage;
    AItemImage.Bitmap := Image1.Bitmap;
  end;
end;

procedure TForm9.ListView1UpdateObjects(const Sender: TObject;
  const AItem: TListViewItem);
var
  AItemText : TListItemText;
  AItemImage : TListItemImage;
begin
  // Uno de los Eventos dque debo implementar
  AItemText := AItem.Objects.FindObject('Salary') as TListItemText;
  if(AItemText = nil)then
  begin
    AItemText := TListItemText.Create(AItem);
    AItemText.Name := 'Salary';
    AItemText.Align := TListItemAlign.Trailing;
    AItemText.PlaceOffset.X := -30;
    AItemText.Width := 120;
  end;
  AItemText := AItem.Objects.FindObject('HireDate') as TListItemText;
  if(AItemText = nil)then
  begin
    AItemText := TListItemText.Create(AItem);
    AItemText.Name := 'HireDate';
    AItemText.Align := TListItemAlign.Trailing;
    AItemText.PlaceOffset.X := -30;
    AItemText.PlaceOffset.Y := 20;
    AItemText.Width := 120;
  end;
  AItemImage := AItem.Objects.FindObject('MiImagen') as TListItemImage;
  if(AItemImage = nil)then
  begin
    AItemImage := TListItemImage.Create(AItem);
    AItemImage.Name := 'MiImagen';
    AItemImage.Align := TListItemAlign.Trailing;
    AItemImage.VertAlign := TListItemAlign.Center;
    AItemImage.PlaceOffset.X := -200;
    AItemImage.ScalingMode := TImageScalingMode.Stretch;
    AItemImage.Width := 24;
    AItemImage.Height := 24;
  end;
end;

end.
