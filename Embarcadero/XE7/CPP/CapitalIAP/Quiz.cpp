//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "Quiz.h"
#include "Answered.h"
#include "Score.h"
#include "Main.h"
#include "System.IOUtils.hpp"
#include "Posix.StdLib.hpp"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TQuizForm *QuizForm = NULL;

String States[100][7] = {
	{"Alabama", "Montgomery", "c", "Birmingham", "Mobile", "Huntsville", "Tuscaloosa"},
	{"Alaska" ,"Anchorage", "w", "Fairbanks", "Juneau", "Sitka", "Ketchikan"},
	{"Arizona", "Phoenix", "w", "Tucson", "Mesa", "Chandler", "Glendale"},
	{"Arkansas", "Little Rock", "c", "Fort Smith", "North Little Rock", "Fayetteville", "Springdale"},
	{"California", "Sacramento", "w", "Los Angeles", "San Diego", "San Jose", "San Francisco"},
	{"Colorado", "Denver", "w", "Colorado Springs", "Aurora", "Fort Collins", "Lakewood"},
	{"Connecticut", "Hartford", "e", "Bridgeport", "New Haven", "Stamford", "Waterbury"},
	{"Delaware", "Dover", "e", "Wilmington", "Newark", "Middletown", "Smyrna"},
	{"Florida", "Tallahassee", "e", "Jacksonville", "Miami", "Tampa", "St. Petersburg"},
	{"Georgia", "Atlanta", "e", "Augusta-Richmond", "Columbus", "Macon-Bibb", "Savannah"},
	{"Hawaii", "Honolulu", "w", "Hilo", "Kailua", "Kapolei", "Kaneohe"},
	{"Idaho", "Boise", "w", "Nampa", "Meridian", "Idaho Falls", "Pocatello"},
	{"Illinois", "Springfield", "c", "Chicago", "Aurora", "Rockford", "Joliet"},
	{"Indiana", "Indianapolis", "e", "Fort Wayne", "Evansville", "South Bend", "Carmel"},
	{"Iowa", "Des Moines", "c", "Cedar Rapids", "Davenport", "Sioux City", "Waterloo"},
	{"Kansas", "Wichita", "c", "Overland Park", "Kansas City", "Topeka", "Olathe"},
	{"Kentucky", "Frankfort", "c", "Louisville", "Lexington", "Bowling Green", "Owensboro"},
	{"Louisiana", "Baton Rouge", "c", "New Orleans", "Shreveport", "Lafayette", "Lake Charles"},
	{"Maine", "Augusta", "e", "Portland", "Lewiston", "Bangor", "South Portland"},
	{"Maryland", "Annapolis", "e", "Baltimore", "Frederick", "Rockville", "Gaithersburg"},
	{"Massachusetts", "Boston", "e", "Worcester", "Springfield", "Lowell", "Cambridge"},
	{"Michigan", "Lansing", "e", "Detroit", "Grand Rapids", "Warren", "Sterling Heights"},
	{"Minnesota", "Saint Paul", "c", "Minneapolis", "Rochester", "Duluth", "Bloomington"},
	{"Mississippi", "Jackson", "c", "Gulfport", "Hattiesburg", "Southaven", "Biloxi"},
	{"Missouri", "Jefferson City", "c", "Kansas City", "Saint Louis", "Springfield", "Independence"},
	{"Montana", "Helena", "w", "Billings", "Missoula", "Great Falls", "Bozeman"},
	{"Nebraska", "Lincoln", "c", "Omaha", "Bellevue", "Grand Island", "Kearney"},
	{"Nevada", "Carson City", "w", "Las Vegas", "Henderson", "North Las Vegas", "Reno"},
	{"New Hampshire", "Concord", "e", "Manchester", "Nashua", "Derry", "Rochester"},
	{"New Jersey", "Trenton", "e", "Newark", "Jersey City", "Paterson", "Elizabeth"},
	{"New Mexico", "Santa Fe", "w", "Albuquerque", "Las Cruces", "Rio Rancho", "Roswell"},
	{"New York", "Albany", "e", "New York", "Buffalo", "Rochester", "Syracuse"},
	{"North Carolina", "Raleigh", "e", "Charlotte", "Greensboro", "Durham", "Winston-Salem"},
	{"North Dakota", "Bismarck", "c", "Fargo", "Grand Forks", "Minot", "West Fargo"},
	{"Ohio", "Columbus", "e", "Cleveland", "Cincinnati", "Toledo", "Akron"},
	{"Oklahoma", "Oklahoma City", "c", "Tulsa", "Norman", "Broken Arrow", "Lawton"},
	{"Oregon", "Salem", "w", "Portland", "Eugene", "Corvallis", "Hillsboro"},
	{"Pennsylvania", "Harrisburg", "e", "Philadelphia", "Pittsburgh", "Allentown", "Erie"},
	{"Rhode Island", "Providence", "e", "Warwick", "Cranston", "Pawtucket", "East Providence"},
	{"South Carolina", "Columbia", "e", "Charleston", "North Charleston", "Mount Pleasant", "Rock Hill"},
	{"South Dakota", "Pierre", "c", "Sioux Falls", "Rapid City", "Aberdeen", "Brookings"},
	{"Tennessee", "Nashville", "c", "Memphis", "Knoxville", "Chattanooga", "Clarksville"},
	{"Texas", "Houston", "c", "San Antonio", "Dallas", "Austin", "Fort Worth"},
	{"Utah", "Salt Lake City", "w", "West Valley City", "Provo", "West Jordan", "Orem"},
	{"Vermont", "Montpelier", "e", "Burlington", "Essex", "South Burlington", "Colchester"},
	{"Virginia", "Richmond", "e", "Virginia Beach", "Norfolk", "Chesapeake", "Newport News"},
	{"Washington", "Olympia", "w", "Seattle", "Spokane", "Tacoma", "Vancouver"},
	{"West Virginia", "Charleston", "e", "Huntington", "Parkersburg", "Morgantown", "Wheeling"},
	{"Wisconsin", "Madison", "c", "Milwaukee", "Green Bay", "Kenosha", "Racine"},
	{"Wyoming", "Cheyenne", "w", "Casper", "Laramie", "Gillette", "Rock Springs"},
	{"Albania", "Tirana", "eu", "Berat", "Fier", "Elbasan", "Durres"},
	{"Andorra", "Andorra la Vella", "eu", "Escaldes-Engordany", "Encamp", "La Massana", "Santa Coloma"},
	{"Armenia", "Yerevan", "eu", "Ashtarak", "Armavir", "Vanadzor", "Kepan"},
	{"Austria", "Vienna", "eu", "Salzburg", "Linz", "Bregenz", "Innsbruck"},
	{"Azerbaijan", "Baku", "eu", "Ganja", "Lankaran", "Shirvan", "Barda"},
	{"Belarus", "Minsk", "eu", "Gomel", "Mogilev", "Vitebsk", "Hrodna"},
	{"Belgium", "Brussels", "eu", "Antwert", "Charleroi", "Liege", "Namur"},
	{"Bosnia and Herzegovina", "eu", "Sarajevo", "Mostar", "Banja Luka", "Bihac", "Jajce"},
	{"Bulgaria", "Sofia", "eu", "Varna", "Vidin", "Lovech", "Pleven"},
	{"Croatia", "Zagreb", "eu", "Zadar", "Karlovac", "Sisak", "Varazdin"},
	{"Cyprus", "Nicosia", "eu", "Limassol", "Strovolos", "Larnaca", "Lakatamia"},
	{"Czech Republic", "Prague", "eu", "Brno", "Ostrova", "Plzen", "Liberec"},
	{"Denmark", "Copenhagen", "eu", "Aarhus", "Aalborg", "Roskilde", "Odense"},
	{"Estonia", "Tallinn", "eu", "Tartu", "Narva", "Parnu", "Kohtla-Jarve"},
	{"Finland", "Helsinki", "eu", "Espoo", "Tampere", "Vantaa", "Oulu"},
	{"France", "Paris", "eu", "Lyon", "Marseille", "Lille", "Bourdeaux"},
	{"Georgia", "Tbilisi", "eu", "Kutaisi", "Batumi", "Rustavi", "Zugdidi"},
	{"Germany", "Berlin", "eu", "Hamburg", "Munich", "Cologne", "Frankfurt"},
	{"Greece", "Athens", "eu", "Thessaloniki", "Patras", "Larissa", "Volos"},
	{"Hungary", "Budapest", "eu", "Debrecen", "Szeged", "Miskolc", "Pecs"},
	{"Iceland", "Reykjavik", "eu", "Kopavogur", "Hafnarfjorour", "Akureyri", "Arborg"},
	{"Ireland", "Dublin", "eu", "Cork", "Limerick", "Galway", "Waterford"},
	{"Italy", "Rome", "eu", "Milan", "Naples", "Turin", "Palermo"},
	{"Kazakhstan", "Astana", "eu", "Almaty", "Shymkent", "Karagandy", "Taraz"},
	{"Latvia", "Riga", "eu", "Valmiera", "Ventspils", "Jelgava", "Jurmala"},
	{"Liechtenstein", "Vaduz", "eu", "Balzers", "Planken", "Schaan", "Triesen"},
	{"Lithuania", "Vilnius", "eu", "Kaunas", "Klaipede", "Siauliai", "Alytus"},
	{"Luxembourg", "Luxembourg", "eu", "Wiltz", "Petange", "Esch", "Differdange"},
	{"Macedonia", "Skopje", "eu", "Bitola", "Kumanovo", "Prilep", "Tetovo"},
	{"Malta", "Valletta", "eu", "Gozo", "Comino", "Rabat", "Marsaxlokk"},
	{"Moldova", "Chisinau", "eu", "Tiraspol", "Balti", "Bender", "Ungheni"},
	{"Monaco", "Monaco", "eu", "Fontvieille", "La Condamine", "Monte Carlo", "La Colle"},
	{"Montenegro", "Podgorica", "eu", "Bijelo Polje", "Pljevja", "Herceg Novi", "Bar"},
	{"Netherlands", "Amsterdam", "eu", "Rotterdam", "The Hague", "Utrecht", "Eindhoven"},
	{"Norway", "Oslo", "eu", "Bergen", "Stavanger", "Trondheim", "Drammen"},
	{"Poland", "Warsaw", "eu", "Krakow", "Poznan", "Gdansk", "Szczecin"},
	{"Portugal", "Lisbon", "eu", "Sintra", "Porto", "Braga", "Amadora"},
	{"Romania", "Bucharest", "eu", "Cluj-Napoca", "Timisoara", "Iasi", "Constanta"},
	{"Russia", "Moscow", "eu", "Saint Petersburg", "Novosibirsk", "Samara", "Kazan"},
	{"San Marino", "San Marino", "eu", "Serravalle", "Borgo MAggiore", "Domagnano", "Fiorentino"},
	{"Serbia", "Belgrade", "eu", "Novi Sad", "Nis", "Kragujevac", "Zrenjanin"},
	{"Slovakia", "Bratislava", "eu", "Presov", "Zilina", "Nitra", "Martin"},
	{"Slovenia", "Ljubljana", "eu", "Maribor", "Celje", "Kranj", "Velenje"},
	{"Spain", "Madrid", "eu", "Barcelona", "Valencia", "Seville", "Malaga"},
	{"Sweden", "Stockholm", "eu", "Gothenburg", "Malmo", "Uppsala", "Linkoping"},
	{"Switzerland", "Bern", "eu", "Aarau", "Basel", "Vaud", "Zug"},
	{"Turkey", "Ankara", "eu", "Istanbul", "Izmir", "Bursa", "Adana"},
	{"Ukraine", "Kiev", "eu", "Kharkiv", "Odessa", "Donetsk", "Lviv"},
	{"United Kingdom", "London", "eu", "Manchester", "Belfast", "Liverpool", "Dublin"},
	{"Vatican City", "Vatican City", "eu", "", " ", " ", " "}
};

//---------------------------------------------------------------------------
void CreateQuiz(void)
{
	if (QuizForm == NULL) {
        QuizForm = new TQuizForm(Application);
	}
}
//---------------------------------------------------------------------------
void StartQuiz(int NumberOfQuestions)
{
	Randomize();
	CreateQuiz();
	QuizForm->NewQuiz(NumberOfQuestions);
	QuizForm->Show();
}
//---------------------------------------------------------------------------
TCapital * AssignCapital(const int I)
{
	TCapital * cap = new TCapital();
	cap->State = States[I][0];
	cap->City = States[I][1];
	cap->Region = States[I][2];
	cap->OtherCities.push_back(States[I][3]);
	cap->OtherCities.push_back(States[I][4]);
	cap->OtherCities.push_back(States[I][5]);
	cap->OtherCities.push_back(States[I][6]);
	cap->Used = false;
	return cap;
}
//---------------------------------------------------------------------------
void __fastcall TQuizForm::GetStates(void)
{
	FNumberOfStates = 0;
	for (int i = 0; i < 50; i++) {
		TCapital * cap = NULL;
		if((MainForm->East) && (States[i][2] == "e")) {
			cap = AssignCapital(i);
			FStateList.push_back(cap);
			FNumberOfStates++;
		}
		else if((MainForm->Central) && (States[i][2] == "c")) {
			TCapital * cap = AssignCapital(i);
			FStateList.push_back(cap);
			FNumberOfStates++;
		}
		else if((MainForm->West) && (States[i][2] == "w")) {
			TCapital * cap = AssignCapital(i);
			FStateList.push_back(cap);
			FNumberOfStates++;
		}
		else if((MainForm->Europe) && (cap->Region == "eu")) {
			FStateList.push_back(cap);
			FNumberOfStates++;
		}
	}
}
//---------------------------------------------------------------------------
void __fastcall TQuizForm::ShowQuestion(int QuestionNumber)
{
	const String cQuestion = "What is the capital of %s?";
	const String cCurrent = "Question #%d";
	std::vector<String> List;

	lCurrentQuestion->Text = Format(cCurrent, ARRAYOFCONST((FCurrentQuestion)));
	int R;
	do
	{
		R = Random(FStateList.size());
	}
	while (FStateList[R]->Used);

	FStateList[R]->Used = true;
	FAnswerState = FStateList[R];

	lQuestion->Text = Format(cQuestion, ARRAYOFCONST((FAnswerState->State)));

	List.push_back(FAnswerState->City);

	List.push_back(FAnswerState->OtherCities[Random(4)]);

	List.push_back(FStateList[Random(FNumberOfStates)]->City);

	while((List[2] == List[0]) || (List[2] == List[1])) {
		List[2] = FStateList[Random(FNumberOfStates)]->City;
	}

	List.push_back(FStateList[Random(FNumberOfStates)]->City);

	while((List[3] == List[0]) || (List[3] == List[1]) || (List[3] == List[2])) {
		List[3] = FStateList[Random(FNumberOfStates)]->City;
	}

	int I = Random(4);
	Answer1->Text = List[I];
	std::vector<String>::iterator iter =
		std::find(List.begin(), List.end(), Answer1->Text);
	List.erase(iter);

	I = Random(3);
	Answer2->Text = List[I];
	iter = std::find(List.begin(), List.end(), Answer2->Text);
	List.erase(iter);

	I = Random(2);
	Answer3->Text = List[I];
	iter = std::find(List.begin(), List.end(), Answer3->Text);
	List.erase(iter);

	Answer4->Text = *List.begin();
}
//---------------------------------------------------------------------------
void __fastcall TQuizForm::GoHome(void)
{
	Close();
}
//---------------------------------------------------------------------------
void __fastcall TQuizForm::ReviewedAnswer(void)
{
	if (FCurrentQuestion > FNumberOfQuestions) {
		ShowScore(this, FNumberOfQuestions, FCorrectAnswers);
		Close();
	}
	else {
        ShowQuestion(FCurrentQuestion);
	}
}
//---------------------------------------------------------------------------
void __fastcall TQuizForm::NewQuiz(int NumberOfQuestions)
{
	GetStates();
	FNumberOfQuestions = NumberOfQuestions;
	FCurrentQuestion = 1;
	ShowQuestion(FCurrentQuestion);
	FCorrectAnswers = 0;
}
//---------------------------------------------------------------------------
__fastcall TQuizForm::TQuizForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TQuizForm::FormActivate(TObject *Sender)
{
	Log::d("Setting ad parent to quiz form");
	MainForm->TakeAdvertFromMainForm(this);
}
//---------------------------------------------------------------------------

void __fastcall TQuizForm::FormDeactivate(TObject *Sender)
{
	Log::d("Setting ad parent back to main form");
	MainForm->PlaceAdvertOnMainForm();
}
//---------------------------------------------------------------------------
void __fastcall TQuizForm::Answer1Click(TObject *Sender)
{
	FCurrentQuestion++;
	bool Correct = FAnswerState->City == static_cast<TButton*>(Sender)->Text;
	if(Correct) {
		FCorrectAnswers++;
	}
	ShowAnswer(this, Correct, FAnswerState->State, FAnswerState->City);
}
//---------------------------------------------------------------------------

void __fastcall TQuizForm::bHomeClick(TObject *Sender)
{
	Close();
}
//---------------------------------------------------------------------------

