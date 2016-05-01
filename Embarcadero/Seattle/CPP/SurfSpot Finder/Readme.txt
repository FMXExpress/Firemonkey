---SurfSpot Finder Sample Application---

SurfSpot Finder is a master/detail application that shows you how to integrate REST services in your application by using the Spitcast API.
The application consists of a TTabcontrol with three invisible tabs (Tabcontrol tpPosition = tpNone). 
Each tab item has a toolbar with a TLabel parented to it to display the title of each tab.
Tab1 includes a TListView that is aligned to the client. For TListview, SearchVisible and 
SearchAlwaysOnTop are set to True in the Object Inspector to allow for list filtering. Tab1 also has a TSpeedButton parented to it for accessing the Favorites list. 
Tab2 includes a TListBox with 3 listbox items, displaying the longitude, latitude and county name of the selected surf spot. It also includes a bottom aligned TWebBrowser component for displaying the location on a map.
To use the built-in slide transition action for transitioning between tabs, a TActionList component has been added
to the form and the ChangeTabAction has been assigned to the back button. 
When clicking the plus icon, the current surf spot gets saved to a text file and added to the ListBox on the Favorites tab (Tab3).

RESTRequest1’s Response property has been set to api/spot/all and RESTClient1’s BaseURL to http://api.spitcast.com
A TRestReponse component has been added to the form. 
RESTRequest1’s Response property has been set to RESTResponse1. A TRestReponseDataSetAdapter was also placed onto the form and the Response field was set to RESTResponse1. Next, a TFDMemTable component was placed onto the form. 
To execute the request, right-click on RESTRequest1 and select ‘Execute’.  Set TRESTResponseDataSetAdapter’s Dataset field to FDMemTable1. Then, right-click on FDMemTable, select ‘Fields Editor’, right-click inside the Fields Editor and select ‘Add Fields’. You should now see 5 available fields, including ‘county_name’, ‘latitude’, ‘longitude’, ’spot_id’ and ’spot_name’.

Via the LiveBindings Designer (View->LiveBindings Designer), bind spot_name to Item.Text and Sync to * by dragging a line between the two components. 

To data bind to each of the Listbox items, click on the … for each listbox item in the LiveBindings Designer. When the ‘Bindable Members’ dialog appears, select the ItemData.Text checkbox. This now exposes a bindable member.

To show data at Design Time, right-click on RestRequest1 and select 'Execute'.

For additional information on how to use the REST components, please visit:

http://docwiki.embarcadero.com/CodeExamples/XE6/en/RESTDemo_Sample

Third Party Notice:
Public distribution of Spitcast API content must acknowledge Spitcast as the content source, and provide a link to Spitcast.com.
The Spitcast API is available for non-commercial use. Commercial use is possible by prior arrangement.
 

