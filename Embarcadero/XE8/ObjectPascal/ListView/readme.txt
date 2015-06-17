Customize TListView appearance sample projects

- SampleListViewMultiDetailAppearancePackage.dpk

Registers "MultiDetailItem" appearance.  This appearance includes three detail text items.

- SampleListViewMultiDetailAppearanceProject.dpr

Uses the "MultiDetailItem" appearance.  To see the form at design time, the SampleListViewMultiDetailAppearancePackagepackage must be installed.

- SampleListViewRatingsAppearancePackage.dpk

Registers "RatingsListItem" appearance.  This appearance displays an image that represents a rating (such as 0 through 5 stars).  The application must provide the image.

- SampleListViewRatingsAppearanceProject.dpr

Uses the "RatingsListItem" appearance.  To see the form at design time, the SampleListViewRatingsAppearancePackage package must be installed.

- SampleListViewAddThumbAndCaptionProject.dpr

This project adds a small image and caption to each list item by overriding event handlers and creating TListView rendering objects.

- SampleListViewCustomBottomDetailProject.dpr

This projects modifies the "Custom" appearance to dispay image with text and bottom detail.  The properties of the custom appearance item and itemedit have been configured in the designer.  This sample also shows how to set the appearance properties in code.


For each <ProjName>*Package.dpk project, you must load it, right click on the main project node in the Project Manager, and 
select Install to install the component package, before attempting to load <ProjName>.dproj. 


