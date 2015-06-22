unit ConversationListComponent;

interface

uses  FMX.Layouts, Fmx.graphics, FMX.Objects, FMX.Types, System.Classes,
System.UITypes, System.Math, System.Types, CommonHeader;

Type
  TConversationList = class(TVertScrollBox)
    private
      fPeople1Name : string;
      fPeople2Name : string;
      procedure AddMessage(AlignRight : Boolean;  AMessage : String);
      procedure OnLabelPaintProc(ASender : TObject; Aanvas : TCanvas; const ARect : TRectF);
   public
      Constructor Create(AOwner : TComponent; APeople1Name, APeople2Name: string);
      destructor destroy;override;
      procedure AddMessageFromPeople1(AMessage : string);
      procedure AddMessageFromPeople2(AMessage : string);

  end;


implementation

{ TConversationList }

uses
  frmConnect;

procedure TConversationList.AddMessage(AlignRight: Boolean; AMessage : String);
var
  wCalloutR: TCalloutRectangle;
  wMessage: TText;
  wTmpImg: TImage;
  wcolor : TColors;
begin
  wCalloutR := TCalloutRectangle.Create(Self);
  wCalloutR.Parent := Self;
  wCalloutR.Align := TAlignLayout.alTop;
  wCalloutR.CalloutPosition := TCalloutPosition.cpLeft;
  wCalloutR.Margins.Top := 10;
  wCalloutR.Margins.Bottom := 10;
  wCalloutR.Margins.Right := 5;
  wCalloutR.Height := 75;
  wCalloutR.Fill.Color := wcolor.White;

  wMessage := TText.Create(Self);
  wMessage.Parent := wCalloutR;
  wMessage.Align := TAlignLayout.alClient;
  wMessage.Text := AMessage;
  wMessage.Margins.Left := 15;
  wMessage.Margins.Right := 5;
  wMessage.Width := wCalloutR.Width-20;

  wMessage.WordWrap := True;
  wMessage.AutoSize := True;

  //wCalloutR.Height := 75;
  wMessage.OnPaint := OnLabelPaintProc;

  wTmpImg := TImage.Create(Self);
  wTmpImg.Parent := wCalloutR;

  if AlignRight then
  begin
    wTmpImg.Align :=  TAlignLayout.alRight;
    wTmpImg.Margins.right := 12;
    wTmpImg.Bitmap.Assign(FormConnect.HimImage.Bitmap); //LoadFromFile(GetImageFilename('HimAvatar.png'));
  end
  else begin
    wTmpImg.Align :=  TAlignLayout.alLeft;
    wTmpImg.Margins.Left := 12;
    wTmpImg.Bitmap.Assign(FormConnect.MeImage.Bitmap); //LoadFromFile(GetImageFilename('MeAvatar.png'));
  end;
  wTmpImg.Width := 55;

end;

procedure TConversationList.AddMessageFromPeople1(AMessage: string);
begin
  AddMessage(False, AMessage);
end;

procedure TConversationList.AddMessageFromPeople2(AMessage: string);
begin
    AddMessage(True, AMessage);
end;

constructor TConversationList.Create(AOwner : TComponent; APeople1Name, APeople2Name: string);
begin
  inherited Create(AOwner);
  fPeople1Name := APeople1Name;
  fPeople2Name := APeople2Name;

end;

destructor TConversationList.destroy;
begin
  inherited;
end;

procedure TConversationList.OnLabelPaintProc(ASender : TObject; Aanvas : TCanvas; const ARect : TRectF);
begin
  TText(ASender).Height := ARect.Height;
  TCalloutRectangle(TText(ASender).Parent).height := IFTHEN(Arect.Height < 50,50 ,Arect.Height);
end;

end.
