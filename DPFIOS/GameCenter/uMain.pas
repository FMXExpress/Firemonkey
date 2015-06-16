// ------------------------------------------------------------------------------
// 1.	In iTunes connect ensure that you have a unique App ID
// 2.	Create a new application and update application information. You can know more about this in apple add new apps documentation.
// 3.	Setup a leader board in Manage Game Center of your application's page where add a single leaderboard and give leaderboard ID and score Type. Here we give leader board ID as DPFLeaderboard.
// 4.	Setup a Achievement in Manage Game Center of your application's page where add a single Achievement and give leaderboard ID and score Type. Here we give Achievement ID as DPFAchievementID.
// 5.	Enter the bundle identifier is the identifier specified in iTunes connect. (very important)
// ------------------------------------------------------------------------------

unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,

  DPF.iOS.BaseControl,
  DPF.iOS.UIView,
  DPF.iOS.GameKit,
  DPF.iOS.UIButton,
  DPF.iOS.UIViewController,
  DPF.iOS.UILabel,
  DPF.iOS.UITextField,
  DPF.iOS.GameCenterManager,
  DPF.iOS.Keyboard;

type
  TFGameCenter = class( TForm )
    DPFUIView1: TDPFUIViewController;
    DPFButtonShowGameCenter: TDPFButton;
    DPFTextField1: TDPFTextField;
    DPFLabel1: TDPFLabel;
    DPFGameCenterManager1: TDPFGameCenterManager;
    DPFButtonIncreaseScore: TDPFButton;
    DPFButtonSubmitScore: TDPFButton;
    DPFButtonSubmitAchievement: TDPFButton;
    DPFKeyboard1: TDPFKeyboard;
    DPFButtonConnect: TDPFButton;
    DPFLabelSubmitScore: TDPFLabel;
    DPFLabelSubmitAchievement: TDPFLabel;
    DPFLabelConnect: TDPFLabel;
    DPFButtonGetScore: TDPFButton;
    DPFLabelGetScore: TDPFLabel;
    DPFButtonAchievement: TDPFButton;
    DPFLabelAchievement: TDPFLabel;
    DPFButtonMatchmaker: TDPFButton;
    DPFButtonFriendRequest: TDPFButton;
    procedure DPFButtonShowGameCenterClick( Sender: TObject );
    procedure DPFButtonIncreaseScoreClick( Sender: TObject );
    procedure DPFButtonSubmitScoreClick( Sender: TObject );
    procedure DPFButtonSubmitAchievementClick( Sender: TObject );
    procedure DPFButtonConnectClick( Sender: TObject );

    procedure DPFGameCenterManager1Authenticate( Sender: TObject; const isAuthenticated: Boolean; error: string; const ErrorCode: NativeUInt );
    procedure DPFGameCenterManager1ReportAchievement( Sender: TObject; const Submitted: Boolean; error: string; const ErrorCode: NativeUInt );
    procedure DPFGameCenterManager1ReportScrore( Sender: TObject; const Submitted: Boolean; error: string; const ErrorCode: NativeUInt );
    procedure DPFButtonGetScoreClick( Sender: TObject );
    procedure DPFGameCenterManager1GetScores( Sender: TObject; const Scores: TScores; const error: string; const ErrorCode: NativeUInt );
    procedure DPFButtonAchievementClick( Sender: TObject );
    procedure DPFGameCenterManager1GetAchievements( Sender: TObject; const Achievements: TAchievements; const error: string; const ErrorCode: NativeUInt );
    procedure DPFButtonMatchmakerClick( Sender: TObject );
    procedure DPFButtonFriendRequestClick( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FGameCenter: TFGameCenter;

implementation

{$R *.fmx}

procedure TFGameCenter.DPFButtonShowGameCenterClick( Sender: TObject );
begin
  DPFGameCenterManager1.ShowGameCenter( 'DPFLeaderboard' );
end;

procedure TFGameCenter.DPFButtonAchievementClick( Sender: TObject );
begin
  DPFLabelAchievement.Text := 'Wait...';
  DPFGameCenterManager1.RequestAchievements;
end;

procedure TFGameCenter.DPFButtonConnectClick( Sender: TObject );
begin
  if not DPFGameCenterManager1.InitPlayer then
  begin
    DPFLabelConnect.TextColor := TAlphaColors.Black;
    DPFLabelConnect.Text      := 'Wait...'
  end;
end;

procedure TFGameCenter.DPFButtonFriendRequestClick( Sender: TObject );
begin
  DPFGameCenterManager1.ShowFriendRequest;
end;

procedure TFGameCenter.DPFButtonGetScoreClick( Sender: TObject );
begin
  DPFLabelGetScore.Text := 'Wait...';
  DPFGameCenterManager1.RequestScores( 'DPFLeaderboard' );
end;

procedure TFGameCenter.DPFButtonIncreaseScoreClick( Sender: TObject );
begin
  DPFTextField1.Text := IntToStr( StrToInt64Def( DPFTextField1.Text, 0 ) + 1 );
end;

procedure TFGameCenter.DPFButtonMatchmakerClick( Sender: TObject );
begin
  DPFGameCenterManager1.ShowMatchmaker;
end;

procedure TFGameCenter.DPFButtonSubmitAchievementClick( Sender: TObject );
begin
  DPFLabelSubmitAchievement.TextColor := TAlphaColors.Black;
  DPFLabelSubmitAchievement.Text      := 'Wait...';

  DPFGameCenterManager1.ReportAchievement( 'DPFAchievementID', StrToFloat( DPFTextField1.Text ), true );
end;

procedure TFGameCenter.DPFButtonSubmitScoreClick( Sender: TObject );
begin
  DPFLabelSubmitScore.TextColor := TAlphaColors.Black;
  DPFLabelSubmitScore.Text      := 'Wait...';

  DPFGameCenterManager1.ReportScore( 'DPFLeaderboard', StrToInt64( DPFTextField1.Text ) );
end;

procedure TFGameCenter.DPFGameCenterManager1Authenticate( Sender: TObject; const isAuthenticated: Boolean; error: string; const ErrorCode: NativeUInt );
begin
  DPFButtonSubmitScore.Enabled       := isAuthenticated;
  DPFButtonSubmitAchievement.Enabled := isAuthenticated;
  DPFButtonShowGameCenter.Enabled    := isAuthenticated;
  DPFButtonGetScore.Enabled          := isAuthenticated;
  DPFButtonAchievement.Enabled       := isAuthenticated;
  DPFButtonMatchmaker.Enabled        := isAuthenticated;
  DPFButtonFriendRequest.Enabled     := isAuthenticated;

  if isAuthenticated then
  begin
    DPFLabelConnect.TextColor := TAlphaColors.Green;
    DPFLabelConnect.Text      := 'Connected.'
  end
  else
  begin
    DPFLabelConnect.TextColor := TAlphaColors.Red;
    DPFLabelConnect.Text      := Error;
  end;
end;

procedure TFGameCenter.DPFGameCenterManager1GetAchievements( Sender: TObject; const Achievements: TAchievements; const error: string; const ErrorCode: NativeUInt );
begin
  if Length( Achievements ) > 0 then
  begin
    DPFLabelAchievement.Text := 'V: ' + Achievements[0].percentComplete.ToString( );
  end
  else
    DPFLabelAchievement.Text := 'No Achievement';
end;

procedure TFGameCenter.DPFGameCenterManager1GetScores( Sender: TObject; const Scores: TScores; const error: string; const ErrorCode: NativeUInt );
begin
  if Length( Scores ) > 0 then
  begin
    DPFLabelGetScore.Text := 'V: ' + Scores[0].Value.ToString( ) + ' - R: ' + Scores[0].Rank.ToString;
  end
  else
    DPFLabelGetScore.Text := 'No Score';
end;

procedure TFGameCenter.DPFGameCenterManager1ReportAchievement( Sender: TObject; const Submitted: Boolean; error: string; const ErrorCode: NativeUInt );
begin
  if Submitted then
  begin
    DPFLabelSubmitAchievement.TextColor := TAlphaColors.Green;
    DPFLabelSubmitAchievement.Text      := 'Submitted'
  end
  else
  begin
    DPFLabelSubmitAchievement.TextColor := TAlphaColors.Red;
    DPFLabelSubmitAchievement.Text      := Error;
  end;
end;

procedure TFGameCenter.DPFGameCenterManager1ReportScrore( Sender: TObject; const Submitted: Boolean; error: string; const ErrorCode: NativeUInt );
begin
  if Submitted then
  begin
    DPFLabelSubmitScore.TextColor := TAlphaColors.Green;
    DPFLabelSubmitScore.Text      := 'Submitted'
  end
  else
  begin
    DPFLabelSubmitScore.TextColor := TAlphaColors.Red;
    DPFLabelSubmitScore.Text      := Error;
  end;
end;

procedure TFGameCenter.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
