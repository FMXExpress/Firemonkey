//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "Quiz.h"
#include "Answered.h"
#include "Score.h"
#include "Main.h"
#include "System.IOUtils.hpp"
#include <random>
#include <sstream>

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TQuizForm *QuizForm = NULL;

namespace {

const StateInfo centralStates[] = {
	{ "Connecticut", "Hartford", { "Bridgeport", "New Haven", "Stamford", "Waterbury" } },
	{ "Delaware", "Dover", { "Wilmington", "Newark", "Middletown", "Smyrna" } },
	{ "Florida", "Tallahassee", { "Jacksonville", "Miami", "Tampa", "St. Petersburg" } },
	{ "Georgia", "Atlanta", { "Augusta-Richmond", "Columbus", "Macon-Bibb", "Savannah" } },
	{ "Indiana", "Indianapolis", { "Fort Wayne", "Evansville", "South Bend", "Carmel" } },
	{ "Maine", "Augusta", { "Portland", "Lewiston", "Bangor", "South Portland" } },
	{ "Maryland", "Annapolis", { "Baltimore", "Frederick", "Rockville", "Gaithersburg" } },
	{ "Massachusetts", "Boston", { "Worcester", "Springfield", "Lowell", "Cambridge" } },
	{ "Michigan", "Lansing", { "Detroit", "Grand Rapids", "Warren", "Sterling Heights" } },
	{ "New Hampshire", "Concord", { "Manchester", "Nashua", "Derry", "Rochester" } },
	{ "New Jersey", "Trenton", { "Newark", "Jersey City", "Paterson", "Elizabeth" } },
	{ "New York", "Albany", { "New York", "Buffalo", "Rochester", "Syracuse" } },
	{ "North Carolina", "Raleigh", { "Charlotte", "Greensboro", "Durham", "Winston-Salem" } },
	{ "Ohio", "Columbus", { "Cleveland", "Cincinnati", "Toledo", "Akron" } },
	{ "Pennsylvania", "Harrisburg", { "Philadelphia", "Pittsburgh", "Allentown", "Erie" } },
	{ "Rhode Island", "Providence", { "Warwick", "Cranston", "Pawtucket", "East Providence" } },
	{ "South Carolina", "Columbia", { "Charleston", "North Charleston", "Mount Pleasant", "Rock Hill" } },
	{ "Vermont", "Montpelier", { "Burlington", "Essex", "South Burlington", "Colchester" } },
	{ "Virginia", "Richmond", { "Virginia Beach", "Norfolk", "Chesapeake", "Newport News" } },
	{ "West Virginia", "Charleston", { "Huntington", "Parkersburg", "Morgantown", "Wheeling" } } };

const StateInfo euStates[] = {
    { "Albania", "Tirana", { "Berat", "Fier", "Elbasan", "Durres" } },
    { "Andorra", "Andorra la Vella", { "Escaldes-Engordany", "Encamp", "La Massana", "Santa Coloma" } },
    { "Armenia", "Yerevan", { "Ashtarak", "Armavir", "Vanadzor", "Kepan" } },
    { "Austria", "Vienna", { "Salzburg", "Linz", "Bregenz", "Innsbruck" } },
    { "Azerbaijan", "Baku", { "Ganja", "Lankaran", "Shirvan", "Barda" } },
    { "Belarus", "Minsk", { "Gomel", "Mogilev", "Vitebsk", "Hrodna" } },
    { "Belgium", "Brussels", { "Antwert", "Charleroi", "Liege", "Namur" } },
    { "Bosnia and Herzegovina", "Sarajevo", { "Mostar", "Banja Luka", "Bihac", "Jajce" } },
    { "Bulgaria", "Sofia", { "Varna", "Vidin", "Lovech", "Pleven" } },
    { "Croatia", "Zagreb", { "Zadar", "Karlovac", "Sisak", "Varazdin" } },
    { "Cyprus", "Nicosia", { "Limassol", "Strovolos", "Larnaca", "Lakatamia" } },
    { "Czech Republic", "Prague", { "Brno", "Ostrova", "Plzen", "Liberec" } },
    { "Denmark", "Copenhagen", { "Aarhus", "Aalborg", "Roskilde", "Odense" } },
    { "Estonia", "Tallinn", { "Tartu", "Narva", "Parnu", "Kohtla-Jarve" } },
    { "Finland", "Helsinki", { "Espoo", "Tampere", "Vantaa", "Oulu" } },
    { "France", "Paris", { "Lyon", "Marseille", "Lille", "Bourdeaux" } },
    { "Georgia", "Tbilisi", { "Kutaisi", "Batumi", "Rustavi", "Zugdidi" } },
    { "Germany", "Berlin", { "Hamburg", "Munich", "Cologne", "Frankfurt" } },
    { "Greece", "Athens", { "Thessaloniki", "Patras", "Larissa", "Volos" } },
    { "Hungary", "Budapest", { "Debrecen", "Szeged", "Miskolc", "Pecs" } },
    { "Iceland", "Reykjavik", { "Kopavogur", "Hafnarfjorour", "Akureyri", "Arborg" } },
    { "Ireland", "Dublin", { "Cork", "Limerick", "Galway", "Waterford" } },
    { "Italy", "Rome", { "Milan", "Naples", "Turin", "Palermo" } },
    { "Kazakhstan", "Astana", { "Almaty", "Shymkent", "Karagandy", "Taraz" } },
    { "Latvia", "Riga", { "Valmiera", "Ventspils", "Jelgava", "Jurmala" } },
    { "Liechtenstein", "Vaduz", { "Balzers", "Planken", "Schaan", "Triesen" } },
    { "Lithuania", "Vilnius", { "Kaunas", "Klaipede", "Siauliai", "Alytus" } },
    { "Luxembourg", "Luxembourg", { "Wiltz", "Petange", "Esch", "Differdange" } },
    { "Macedonia", "Skopje", { "Bitola", "Kumanovo", "Prilep", "Tetovo" } },
    { "Malta", "Valletta", { "Gozo", "Comino", "Rabat", "Marsaxlokk" } },
    { "Moldova", "Chisinau", { "Tiraspol", "Balti", "Bender", "Ungheni" } },
    { "Monaco", "Monaco", { "Fontvieille", "La Condamine", "Monte Carlo", "La Colle" } },
    { "Montenegro", "Podgorica", { "Bijelo Polje", "Pljevja", "Herceg Novi", "Bar" } },
	{ "Netherlands", "Amsterdam", { "Rotterdam", "The Hague", "Utrecht", "Eindhoven" } },
    { "Norway", "Oslo", { "Bergen", "Stavanger", "Trondheim", "Drammen" } },
    { "Poland", "Warsaw", { "Krakow", "Poznan", "Gdansk", "Szczecin" } },
    { "Portugal", "Lisbon", { "Sintra", "Porto", "Braga", "Amadora" } },
    { "Romania", "Bucharest", { "Cluj-Napoca", "Timisoara", "Iasi", "Constanta" } },
    { "Russia", "Moscow", { "Saint Petersburg", "Novosibirsk", "Samara", "Kazan" } },
    { "San Marino", "San Marino", { "Serravalle", "Borgo MAggiore", "Domagnano", "Fiorentino" } },
    { "Serbia", "Belgrade", { "Novi Sad", "Nis", "Kragujevac", "Zrenjanin" } },
    { "Slovakia", "Bratislava", { "Presov", "Zilina", "Nitra", "Martin" } },
    { "Slovenia", "Ljubljana", { "Maribor", "Celje", "Kranj", "Velenje" } },
    { "Spain", "Madrid", { "Barcelona", "Valencia", "Seville", "Malaga" } },
    { "Sweden", "Stockholm", { "Gothenburg", "Malmo", "Uppsala", "Linkoping" } },
    { "Switzerland", "Bern", { "Aarau", "Basel", "Vaud", "Zug" } },
    { "Turkey", "Ankara", { "Istanbul", "Izmir", "Bursa", "Adana" } },
    { "Ukraine", "Kiev", { "Kharkiv", "Odessa", "Donetsk", "Lviv" } },
    { "United Kingdom", "London", { "Manchester", "Belfast", "Liverpool", "Dublin" } } };

const StateInfo westStates[] = {
    { "Alaska", "Anchorage", { "Fairbanks", "Juneau", "Sitka", "Ketchikan" } },
    { "Arizona", "Phoenix", { "Tucson", "Mesa", "Chandler", "Glendale" } },
    { "California", "Sacramento", { "Los Angeles", "San Diego", "San Jose", "San Francisco" } },
    { "Colorado", "Denver", { "Colorado Springs", "Aurora", "Fort Collins", "Lakewood" } },
    { "Hawaii", "Honolulu", { "Hilo", "Kailua", "Kapolei", "Kaneohe" } },
    { "Idaho", "Boise", { "Nampa", "Meridian", "Idaho Falls", "Pocatello" } },
    { "Montana", "Helena", { "Billings", "Missoula", "Great Falls", "Bozeman" } },
    { "Nevada", "Carson City", { "Las Vegas", "Henderson", "North Las Vegas", "Reno" } },
    { "New Mexico", "Santa Fe", { "Albuquerque", "Las Cruces", "Rio Rancho", "Roswell" } },
    { "Oregon", "Salem", { "Portland", "Eugene", "Corvallis", "Hillsboro" } },
    { "Utah", "Salt Lake City", { "West Valley City", "Provo", "West Jordan", "Orem" } },
    { "Washington", "Olympia", { "Seattle", "Spokane", "Tacoma", "Vancouver" } },
    { "Wyoming", "Cheyenne", { "Casper", "Laramie", "Gillette", "Rock Springs" } } };

const StateInfo eastStates[] = {
    { "Connecticut", "Hartford", { "Bridgeport", "New Haven", "Stamford", "Waterbury" } },
    { "Delaware", "Dover", { "Wilmington", "Newark", "Middletown", "Smyrna" } },
    { "Florida", "Tallahassee", { "Jacksonville", "Miami", "Tampa", "St. Petersburg" } },
    { "Georgia", "Atlanta", { "Augusta-Richmond", "Columbus", "Macon-Bibb", "Savannah" } },
    { "Indiana", "Indianapolis", { "Fort Wayne", "Evansville", "South Bend", "Carmel" } },
    { "Maine", "Augusta", { "Portland", "Lewiston", "Bangor", "South Portland" } },
    { "Maryland", "Annapolis", { "Baltimore", "Frederick", "Rockville", "Gaithersburg" } },
    { "Massachusetts", "Boston", { "Worcester", "Springfield", "Lowell", "Cambridge" } },
    { "Michigan", "Lansing", { "Detroit", "Grand Rapids", "Warren", "Sterling Heights" } },
    { "New Hampshire", "Concord", { "Manchester", "Nashua", "Derry", "Rochester" } },
    { "New Jersey", "Trenton", { "Newark", "Jersey City", "Paterson", "Elizabeth" } },
    { "New York", "Albany", { "New York", "Buffalo", "Rochester", "Syracuse" } },
    { "North Carolina", "Raleigh", { "Charlotte", "Greensboro", "Durham", "Winston-Salem" } },
    { "Ohio", "Columbus", { "Cleveland", "Cincinnati", "Toledo", "Akron" } },
    { "Pennsylvania", "Harrisburg", { "Philadelphia", "Pittsburgh", "Allentown", "Erie" } },
    { "Rhode Island", "Providence", { "Warwick", "Cranston", "Pawtucket", "East Providence" } },
    { "South Carolina", "Columbia", { "Charleston", "North Charleston", "Mount Pleasant", "Rock Hill" } },
    { "Vermont", "Montpelier", { "Burlington", "Essex", "South Burlington", "Colchester" } },
    { "Virginia", "Richmond", { "Virginia Beach", "Norfolk", "Chesapeake", "Newport News" } },
    { "West Virginia", "Charleston", { "Huntington", "Parkersburg", "Morgantown", "Wheeling" } } };

std::random_device randomDevice;
std::mt19937 randomFunc(randomDevice());

std::vector<Question> CreateQuestions(int nQuestions)
{
	std::vector<const StateInfo*> states;
	if (MainForm->East) {
		for (auto& state : eastStates)
			states.push_back(&state);
	}
	if (MainForm->Central) {
		for (auto& state : centralStates)
			states.push_back(&state);
	}
	if (MainForm->West) {
		for (auto& state: westStates)
			states.push_back(&state);
	}
	if (MainForm->Europe) {
		for (auto& state : euStates)
			states.push_back(&state);
	}

	typedef	std::uniform_int_distribution<> Dist;
	Dist otherCitiesDist(0, 4);
	Dist otherStatesDist(1, states.size() - 1);
	Dist otherStatesDist2(1, states.size() - 2);

	std::vector<Question> result(nQuestions);
	for (int i = 0; i < nQuestions; ++i) {
		Dist dist(i, states.size() - 1);  // Let's select a new state randomly
		std::swap(states[dist(randomFunc)], states[i]);
		std::swap(states[0], states[i]);  // Now a new state is in the front
		auto& newState = states[0];

		auto& dst = result[i];
		dst.state = newState;
		dst.candidates[0] = newState->capital;  // The first candidate is a capital
		dst.candidates[1] = newState->candidates[otherCitiesDist(randomFunc)];  // The second one is another city from this state
		auto candidateStateId1 = otherStatesDist(randomFunc);  // The last two are the capitals of another states
		auto candidateStateId2 = otherStatesDist2(randomFunc);
		if (candidateStateId2 >= candidateStateId1) {
			++candidateStateId2;  // This ensures they will be different
		}
		dst.candidates[2] = states[candidateStateId1]->capital;
		dst.candidates[3] = states[candidateStateId2]->capital;
		std::shuffle(dst.candidates, dst.candidates + 4, randomFunc);
	}
	return result;
}

}  // namespace

//---------------------------------------------------------------------------
void CreateQuiz()
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

void __fastcall TQuizForm::ShowQuestion(int QuestionNumber)
{
	std::ostringstream captionSS;
	captionSS << "Question " << FCurrentQuestion;
	lCurrentQuestion->Text = captionSS.str().c_str();

	std::ostringstream questionSS;
	auto& question = questions[FCurrentQuestion - 1];
	questionSS << "What is the capital of " << question.state->name;
	lQuestion->Text = questionSS.str().c_str();

	Answer1->Text = question.candidates[0];
	Answer2->Text = question.candidates[1];
	Answer3->Text = question.candidates[2];
	Answer4->Text = question.candidates[3];
}
//---------------------------------------------------------------------------
void __fastcall TQuizForm::GoHome()
{
	Close();
}
//---------------------------------------------------------------------------
void __fastcall TQuizForm::ReviewedAnswer()
{
	if (FCurrentQuestion > questions.size()) {
		ShowScore(this, questions.size(), FCorrectAnswers);
		Close();
	} else {
        ShowQuestion(FCurrentQuestion);
	}
}
//---------------------------------------------------------------------------
void __fastcall TQuizForm::NewQuiz(int NumberOfQuestions)
{
	questions = CreateQuestions(NumberOfQuestions);
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
__fastcall TQuizForm::~TQuizForm() {}

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
	auto& state = questions[FCurrentQuestion - 1].state;
	bool Correct = state->capital == static_cast<TButton*>(Sender)->Text;
	if (Correct) {
		++FCorrectAnswers;
	}
	ShowAnswer(this, Correct, state->name, state->capital);
	++FCurrentQuestion;
}
//---------------------------------------------------------------------------
void __fastcall TQuizForm::bHomeClick(TObject *Sender)
{
	Close();
}

