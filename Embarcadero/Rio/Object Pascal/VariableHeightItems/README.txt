This demo shows how to calculate dimensions of text and how to dyunamically 
adjust item height in a ListView so that the text fits inside. 

When ListView items are just being created, the drawable objects that
comprise the visual presentation of the content are not created immediately.
This usually happens when the ListView is being painted for the first time.

Appearances specify views and their properties that are common for all items.
However, to modify individual items, the drawable objects that form the views
should be manipulated directly. They can be accessed at any time, however 
trying to manipulate them before the appearance has done its job would be 
futile because all changes made before that will be overwritten by the 
appearance.

In order to access the drawable objects at a correct time, there is an event
called TListView.OnUpdateObjects which is called for every item after its view
has been updated. This demo implements a handler for it and calcualtes item
dimensions based on available width and length of the text that the item
contains. To liven up the visual aspect of the demo, the size and weight of the
font is also altered in the same handler.

The demo uses DynamicAppearance for the items, but same approach can be used
using the classic appearances, predefined or user defined.


