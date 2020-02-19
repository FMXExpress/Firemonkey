This demo shows how to use TListView together with TImageList.

ImageList is an efficient way of storing images that may have multiple
in the application. TListView items with images can use Bitmaps directly, 
bitmaps referenced from elsewhere or bitmaps specified as indices in 
an ImageList.

Depending on the kind of appearance used, there are different ways to select
ImageIndex.

If one of the classic appearances is used, TListViewItem.ImageIndex property
can be set. This is illustrated in the first Tab. Likewise, a LiveBinding
link can be drawn directly to Item.ImageIndex field for a similar effect.

In case of a dynamic appearance, there can be more than one image and
ListView performs no implicit actions on the items. It would be possible to
hook up on OnUpdateObjects event and set ImageIndex property of individual
drawable objects. However there is a more simple way of doing that. When
data members that correspond to TListItemImage objects are assigned an integer,
it is interpreted as ImageIndex. The second pair of tabs illustrates that.

