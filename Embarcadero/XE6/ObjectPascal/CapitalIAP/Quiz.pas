unit Quiz;

//{$DEFINE USE_IB}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Objects, System.Generics.Collections;

type
  TCapital = record
    State: string;
    City: string;
    OtherCities: TArray<string>;
    Region: string;
    Used: Boolean;
  end;

  TQuizForm = class(TForm)
    ToolBar1: TToolBar;
    lCurrentQuestion: TLabel;
    lQuestion: TLabel;
    Answer1: TButton;
    Answer2: TButton;
    Answer3: TButton;
    Answer4: TButton;
    bHome: TButton;
    procedure AnswerClick(Sender: TObject);
    procedure bHomeClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  protected
    FNumberOfStates: Integer;
    FNumberOfQuestions: Integer;
    FAnswerState: TCapital;
    FCurrentQuestion: Integer;
    FCorrectAnswers: Integer;
    FStateList: TList<TCapital>;
    procedure GetStates;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    procedure NewQuiz(NumberOfQuestions: Integer);
    destructor Destroy; override;
    procedure ShowQuestion(QuestionNumber: Integer);
    procedure GoHome;
    procedure ReviewedAnswer;
  end;

var
  QuizForm: TQuizForm = nil;
  States: array[1..100, 1..7] of string =
(('Alabama', 'Montgomery', 'c', 'Birmingham', 'Mobile', 'Huntsville', 'Tuscaloosa'),
('Alaska' ,'Anchorage', 'w', 'Fairbanks', 'Juneau', 'Sitka', 'Ketchikan'),
('Arizona', 'Phoenix', 'w', 'Tucson', 'Mesa', 'Chandler', 'Glendale'),
('Arkansas', 'Little Rock', 'c', 'Fort Smith', 'North Little Rock', 'Fayetteville', 'Springdale'),
('California', 'Sacramento', 'w', 'Los Angeles', 'San Diego', 'San Jose', 'San Francisco'),
('Colorado', 'Denver', 'w', 'Colorado Springs', 'Aurora', 'Fort Collins', 'Lakewood'),
('Connecticut', 'Hartford', 'e', 'Bridgeport', 'New Haven', 'Stamford', 'Waterbury'),
('Delaware', 'Dover', 'e', 'Wilmington', 'Newark', 'Middletown', 'Smyrna'),
('Florida', 'Tallahassee', 'e', 'Jacksonville', 'Miami', 'Tampa', 'St. Petersburg'),
('Georgia', 'Atlanta', 'e', 'Augusta-Richmond', 'Columbus', 'Macon-Bibb', 'Savannah'),
('Hawaii', 'Honolulu', 'w', 'Hilo', 'Kailua', 'Kapolei', 'Kaneohe'),
('Idaho', 'Boise', 'w', 'Nampa', 'Meridian', 'Idaho Falls', 'Pocatello'),
('Illinois', 'Springfield', 'c', 'Chicago', 'Aurora', 'Rockford', 'Joliet'),
('Indiana', 'Indianapolis', 'e', 'Fort Wayne', 'Evansville', 'South Bend', 'Carmel'),
('Iowa', 'Des Moines', 'c', 'Cedar Rapids', 'Davenport', 'Sioux City', 'Waterloo'),
('Kansas', 'Wichita', 'c', 'Overland Park', 'Kansas City', 'Topeka', 'Olathe'),
('Kentucky', 'Frankfort', 'c', 'Louisville', 'Lexington', 'Bowling Green', 'Owensboro'),
('Louisiana', 'Baton Rouge', 'c', 'New Orleans', 'Shreveport', 'Lafayette', 'Lake Charles'),
('Maine', 'Augusta', 'e', 'Portland', 'Lewiston', 'Bangor', 'South Portland'),
('Maryland', 'Annapolis', 'e', 'Baltimore', 'Frederick', 'Rockville', 'Gaithersburg'),
('Massachusetts', 'Boston', 'e', 'Worcester', 'Springfield', 'Lowell', 'Cambridge'),
('Michigan', 'Lansing', 'e', 'Detroit', 'Grand Rapids', 'Warren', 'Sterling Heights'),
('Minnesota', 'Saint Paul', 'c', 'Minneapolis', 'Rochester', 'Duluth', 'Bloomington'),
('Mississippi', 'Jackson', 'c', 'Gulfport', 'Hattiesburg', 'Southaven', 'Biloxi'),
('Missouri', 'Jefferson City', 'c', 'Kansas City', 'Saint Louis', 'Springfield', 'Independence'),
('Montana', 'Helena', 'w', 'Billings', 'Missoula', 'Great Falls', 'Bozeman'),
('Nebraska', 'Lincoln', 'c', 'Omaha', 'Bellevue', 'Grand Island', 'Kearney'),
('Nevada', 'Carson City', 'w', 'Las Vegas', 'Henderson', 'North Las Vegas', 'Reno'),
('New Hampshire', 'Concord', 'e', 'Manchester', 'Nashua', 'Derry', 'Rochester'),
('New Jersey', 'Trenton', 'e', 'Newark', 'Jersey City', 'Paterson', 'Elizabeth'),
('New Mexico', 'Santa Fe', 'w', 'Albuquerque', 'Las Cruces', 'Rio Rancho', 'Roswell'),
('New York', 'Albany', 'e', 'New York', 'Buffalo', 'Rochester', 'Syracuse'),
('North Carolina', 'Raleigh', 'e', 'Charlotte', 'Greensboro', 'Durham', 'Winston-Salem'),
('North Dakota', 'Bismarck', 'c', 'Fargo', 'Grand Forks', 'Minot', 'West Fargo'),
('Ohio', 'Columbus', 'e', 'Cleveland', 'Cincinnati', 'Toledo', 'Akron'),
('Oklahoma', 'Oklahoma City', 'c', 'Tulsa', 'Norman', 'Broken Arrow', 'Lawton'),
('Oregon', 'Salem', 'w', 'Portland', 'Eugene', 'Corvallis', 'Hillsboro'),
('Pennsylvania', 'Harrisburg', 'e', 'Philadelphia', 'Pittsburgh', 'Allentown', 'Erie'),
('Rhode Island', 'Providence', 'e', 'Warwick', 'Cranston', 'Pawtucket', 'East Providence'),
('South Carolina', 'Columbia', 'e', 'Charleston', 'North Charleston', 'Mount Pleasant', 'Rock Hill'),
('South Dakota', 'Pierre', 'c', 'Sioux Falls', 'Rapid City', 'Aberdeen', 'Brookings'),
('Tennessee', 'Nashville', 'c', 'Memphis', 'Knoxville', 'Chattanooga', 'Clarksville'),
('Texas', 'Houston', 'c', 'San Antonio', 'Dallas', 'Austin', 'Fort Worth'),
('Utah', 'Salt Lake City', 'w', 'West Valley City', 'Provo', 'West Jordan', 'Orem'),
('Vermont', 'Montpelier', 'e', 'Burlington', 'Essex', 'South Burlington', 'Colchester'),
('Virginia', 'Richmond', 'e', 'Virginia Beach', 'Norfolk', 'Chesapeake', 'Newport News'),
('Washington', 'Olympia', 'w', 'Seattle', 'Spokane', 'Tacoma', 'Vancouver'),
('West Virginia', 'Charleston', 'e', 'Huntington', 'Parkersburg', 'Morgantown', 'Wheeling'),
('Wisconsin', 'Madison', 'c', 'Milwaukee', 'Green Bay', 'Kenosha', 'Racine'),
('Wyoming', 'Cheyenne', 'w', 'Casper', 'Laramie', 'Gillette', 'Rock Springs'),
('Albania', 'Tirana', 'eu', 'Berat', 'Fier', 'Elbasan', 'Durres'),
('Andorra', 'Andorra la Vella', 'eu', 'Escaldes-Engordany', 'Encamp', 'La Massana', 'Santa Coloma'),
('Armenia', 'Yerevan', 'eu', 'Ashtarak', 'Armavir', 'Vanadzor', 'Kepan'),
('Austria', 'Vienna', 'eu', 'Salzburg', 'Linz', 'Bregenz', 'Innsbruck'),
('Azerbaijan', 'Baku', 'eu', 'Ganja', 'Lankaran', 'Shirvan', 'Barda'),
('Belarus', 'Minsk', 'eu', 'Gomel', 'Mogilev', 'Vitebsk', 'Hrodna'),
('Belgium', 'Brussels', 'eu', 'Antwert', 'Charleroi', 'Liege', 'Namur'),
('Bosnia and Herzegovina', 'eu', 'Sarajevo', 'Mostar', 'Banja Luka', 'Bihac', 'Jajce'),
('Bulgaria', 'Sofia', 'eu', 'Varna', 'Vidin', 'Lovech', 'Pleven'),
('Croatia', 'Zagreb', 'eu', 'Zadar', 'Karlovac', 'Sisak', 'Varazdin'),
('Cyprus', 'Nicosia', 'eu', 'Limassol', 'Strovolos', 'Larnaca', 'Lakatamia'),
('Czech Republic', 'Prague', 'eu', 'Brno', 'Ostrova', 'Plzen', 'Liberec'),
('Denmark', 'Copenhagen', 'eu', 'Aarhus', 'Aalborg', 'Roskilde', 'Odense'),
('Estonia', 'Tallinn', 'eu', 'Tartu', 'Narva', 'Parnu', 'Kohtla-Jarve'),
('Finland', 'Helsinki', 'eu', 'Espoo', 'Tampere', 'Vantaa', 'Oulu'),
('France', 'Paris', 'eu', 'Lyon', 'Marseille', 'Lille', 'Bourdeaux'),
('Georgia', 'Tbilisi', 'eu', 'Kutaisi', 'Batumi', 'Rustavi', 'Zugdidi'),
('Germany', 'Berlin', 'eu', 'Hamburg', 'Munich', 'Cologne', 'Frankfurt'),
('Greece', 'Athens', 'eu', 'Thessaloniki', 'Patras', 'Larissa', 'Volos'),
('Hungary', 'Budapest', 'eu', 'Debrecen', 'Szeged', 'Miskolc', 'Pecs'),
('Iceland', 'Reykjavik', 'eu', 'Kopavogur', 'Hafnarfjorour', 'Akureyri', 'Arborg'),
('Ireland', 'Dublin', 'eu', 'Cork', 'Limerick', 'Galway', 'Waterford'),
('Italy', 'Rome', 'eu', 'Milan', 'Naples', 'Turin', 'Palermo'),
('Kazakhstan', 'Astana', 'eu', 'Almaty', 'Shymkent', 'Karagandy', 'Taraz'),
('Latvia', 'Riga', 'eu', 'Valmiera', 'Ventspils', 'Jelgava', 'Jurmala'),
('Liechtenstein', 'Vaduz', 'eu', 'Balzers', 'Planken', 'Schaan', 'Triesen'),
('Lithuania', 'Vilnius', 'eu', 'Kaunas', 'Klaipede', 'Siauliai', 'Alytus'),
('Luxembourg', 'Luxembourg', 'eu', 'Wiltz', 'Petange', 'Esch', 'Differdange'),
('Macedonia', 'Skopje', 'eu', 'Bitola', 'Kumanovo', 'Prilep', 'Tetovo'),
('Malta', 'Valletta', 'eu', 'Gozo', 'Comino', 'Rabat', 'Marsaxlokk'),
('Moldova', 'Chisinau', 'eu', 'Tiraspol', 'Balti', 'Bender', 'Ungheni'),
('Monaco', 'Monaco', 'eu', 'Fontvieille', 'La Condamine', 'Monte Carlo', 'La Colle'),
('Montenegro', 'Podgorica', 'eu', 'Bijelo Polje', 'Pljevja', 'Herceg Novi', 'Bar'),
('Netherlands', 'Amsterdam', 'eu', 'Rotterdam', 'The Hague', 'Utrecht', 'Eindhoven'),
('Norway', 'Oslo', 'eu', 'Bergen', 'Stavanger', 'Trondheim', 'Drammen'),
('Poland', 'Warsaw', 'eu', 'Krakow', 'Poznan', 'Gdansk', 'Szczecin'),
('Portugal', 'Lisbon', 'eu', 'Sintra', 'Porto', 'Braga', 'Amadora'),
('Romania', 'Bucharest', 'eu', 'Cluj-Napoca', 'Timisoara', 'Iasi', 'Constanta'),
('Russia', 'Moscow', 'eu', 'Saint Petersburg', 'Novosibirsk', 'Samara', 'Kazan'),
('San Marino', 'San Marino', 'eu', 'Serravalle', 'Borgo MAggiore', 'Domagnano', 'Fiorentino'),
('Serbia', 'Belgrade', 'eu', 'Novi Sad', 'Nis', 'Kragujevac', 'Zrenjanin'),
('Slovakia', 'Bratislava', 'eu', 'Presov', 'Zilina', 'Nitra', 'Martin'),
('Slovenia', 'Ljubljana', 'eu', 'Maribor', 'Celje', 'Kranj', 'Velenje'),
('Spain', 'Madrid', 'eu', 'Barcelona', 'Valencia', 'Seville', 'Malaga'),
('Sweden', 'Stockholm', 'eu', 'Gothenburg', 'Malmo', 'Uppsala', 'Linkoping'),
('Switzerland', 'Bern', 'eu', 'Aarau', 'Basel', 'Vaud', 'Zug'),
('Turkey', 'Ankara', 'eu', 'Istanbul', 'Izmir', 'Bursa', 'Adana'),
('Ukraine', 'Kiev', 'eu', 'Kharkiv', 'Odessa', 'Donetsk', 'Lviv'),
('United Kingdom', 'London', 'eu', 'Manchester', 'Belfast', 'Liverpool', 'Dublin'),
('Vatican City', 'Vatican City', 'eu', '', ' ', ' ', ' '));

procedure CreateQuiz;
procedure StartQuiz(NumberOfQuestions: Integer);

implementation

uses Answered, Score, Main, System.IOUtils, Posix.StdLib;

{$R *.fmx}

procedure CreateQuiz;
begin
  if not Assigned(QuizForm) then
    QuizForm := TQuizForm.Create(Application);
end;

procedure StartQuiz(NumberOfQuestions: Integer);
begin
  Randomize;
  CreateQuiz;
  QuizForm.NewQuiz(NumberOfQuestions);
  QuizForm.Show;
end;

{ TQuizForm }

procedure TQuizForm.AnswerClick(Sender: TObject);
var
  Correct: Boolean;
begin
  Inc(FCurrentQuestion);
  Correct := FAnswerState.City = TButton(Sender).Text;
  if Correct then
    Inc(FCorrectAnswers);
  ShowAnswer(Self, Correct, FAnswerState.State, FAnswerState.City);
end;

procedure TQuizForm.bHomeClick(Sender: TObject);
begin
  GoHome;
end;

constructor TQuizForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TQuizForm.Destroy;
begin
  FStateList.DisposeOf;
  inherited;
end;

procedure TQuizForm.FormActivate(Sender: TObject);
begin
  Log.d('Setting ad parent to quiz form');
  MainForm.TakeAdvertFromMainForm(Self);
end;

procedure TQuizForm.FormDeactivate(Sender: TObject);
begin
  Log.d('Setting ad parent back to main form');
  MainForm.PlaceAdvertOnMainForm;
end;

procedure TQuizForm.FormShow(Sender: TObject);
begin
end;

{$IFDEF USE_IB}
procedure TQuizForm.GetStates;
  procedure SetupConnection;
  begin
    DataModule1.SQLConnection1.Params.Values['Database'] := TPath.GetDocumentsPath + '/CAPITALS.GDB';
    DataModule1.SQLConnection1.Params.Values['User_Name'] := 'sysdba';
    DataModule1.SQLConnection1.Params.Values['password'] := 'masterkey';
// Remove encryption password
//    DataModule1.SQLConnection1.Params.Values['SEP'] := 'masterkey';
  end;
var
  Capital: TCapital;

  I: Integer;
begin

  SetupConnection;
  FStateList := TList<TCapital>.Create;
  DataModule1.ClientDataSet1.Active := True;
  FNumberOfStates := 0;
  I := 0;
  while not DataModule1.ClientDataSet1.Eof do
  begin
    Inc(I);
    Capital.State := DataModule1.ClientDataSet1.FieldByName('STATE').AsString;
    Capital.City := DataModule1.ClientDataSet1.FieldByName('CITY').AsString;
    Capital.Region := DataModule1.ClientDataSet1.FieldByName('REGION').AsString;
    Capital.Used := False;
    SetLength(Capital.OtherCities, 4);
    Capital.OtherCities[0] := States[I, 4];
    Capital.OtherCities[1] := States[I, 5];
    Capital.OtherCities[2] := States[I, 6];
    Capital.OtherCities[3] := States[I, 7];
    if MainForm.East and (Capital.Region = 'e') then
    begin
      FStateList.Add(Capital);
      Inc(FNumberOfStates);
    end
    else if MainForm.Central and (Capital.Region = 'c') then
    begin
      FStateList.Add(Capital);
      Inc(FNumberOfStates);
    end
    else if MainForm.West and (Capital.Region = 'w') then
    begin
      FStateList.Add(Capital);
      Inc(FNumberOfStates);
    end
    else if MainForm.Europe and (Capital.Region = 'eu') then
    begin
      FStateList.Add(Capital);
      Inc(FNumberOfStates);
    end;

    DataModule1.ClientDataSet1.Next;
  end;
  DataModule1.ClientDataSet1.Active := False;
end;

{$ELSE}

procedure TQuizForm.GetStates;
var
  Capital: TCapital;
  I: Integer;

  procedure Assign;
  begin
    Capital.State := States[I, 1];
    Capital.City := States[I, 2];
    Capital.Region := States[I, 3];
    SetLength(Capital.OtherCities, 4);
    Capital.OtherCities[0] := States[I, 4];
    Capital.OtherCities[1] := States[I, 5];
    Capital.OtherCities[2] := States[I, 6];
    Capital.OtherCities[3] := States[I, 7];
    Capital.Used := False;
  end;
begin
  FStateList := TList<TCapital>.Create;
  FNumberOfStates := 0;
  for I := 1 to 50 do
  begin
    if MainForm.East and (States[I, 3] = 'e') then
    begin
      Assign;
      FStateList.Add(Capital);
      Inc(FNumberOfStates);
    end
    else if MainForm.Central and (States[I, 3] = 'c') then
    begin
      Assign;
      FStateList.Add(Capital);
      Inc(FNumberOfStates);
    end
    else if MainForm.West and (States[I, 3] = 'w') then
    begin
      Assign;
      FStateList.Add(Capital);
      Inc(FNumberOfStates);
    end
    else if MainForm.Europe and (Capital.Region = 'eu') then
    begin
      FStateList.Add(Capital);
      Inc(FNumberOfStates);
    end;
  end;
end;
{$ENDIF}

procedure TQuizForm.GoHome;
begin
  Close;
end;

procedure TQuizForm.NewQuiz(NumberOfQuestions: Integer);
begin
  GetStates;
  FNumberOfQuestions := NumberOfQuestions;
  FCurrentQuestion := 1;
  ShowQuestion(FCurrentQuestion);
  FCorrectAnswers := 0;
end;

procedure TQuizForm.ReviewedAnswer;
begin
  if FCurrentQuestion > FNumberOfQuestions then
  begin
    ShowScore(Self, FNumberOfQuestions, FCorrectAnswers);
    Close;
  end
  else
    ShowQuestion(FCurrentQuestion);
end;

procedure TQuizForm.ShowQuestion(QuestionNumber: Integer);
const
  cQuestion = 'What is the capital of %s?';
  cCurrent = 'Question #%d';
var
  I, R: Integer;
  List: TList<string>;
begin
  List := TList<string>.Create;

  lCurrentQuestion.Text := Format(cCurrent, [FCurrentQuestion]);
  R := Random(FStateList.Count);

  while FStateList[R].Used do
    R := Random(FStateList.Count);

  FAnswerState := FStateList[R];
  FAnswerState.Used := True;
  FStateList[R] := FAnswerState;

  lQuestion.Text := Format(cQuestion, [FAnswerState.State]);

  List.Add(FAnswerState.City);

  List.Add(FAnswerState.OtherCities[Random(4)]);

  List.Add(FStateList[Random(FNumberOfStates)].City);
  while (List[2] = List[0]) or (List[2] = List[1]) do
    List[2] := FStateList[Random(FNumberOfStates)].City;

  List.Add(FStateList[Random(FNumberOfStates)].City);
  while (List[3] = List[0]) or (List[3] = List[1]) or (List[3] = List[2]) do
    List[3] := FStateList[Random(FNumberOfStates)].City;

  I := Random(4);
  Answer1.Text := List[I];
  List.Delete(I);

  I := Random(3);
  Answer2.Text := List[I];
  List.Delete(I);

  I := Random(2);
  Answer3.Text := List[I];
  List.Delete(I);

  Answer4.Text := List[0];
end;

end.
