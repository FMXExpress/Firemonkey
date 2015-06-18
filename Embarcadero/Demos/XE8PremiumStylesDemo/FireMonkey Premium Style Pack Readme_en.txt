This Readme outlines the steps to add custom styles to your multi-device application with Delphi, C++Builder and RAD Studio XE8.

1. With the Master view selected, add a TStyleBook component to your form

2. On the Master view, select a Master style from the toolbar drop-down menu and then load the associated premium style from the style pack. For example, if you selected ‘Android’ as the Master style, load and assign the AndroidCoralCrystal.style file to your StyleBook on the Master view. When working with custom styles, each view must have a style, including the Master view.

3. Switch to each of your created views, select the TStyleBook component on that view and load the custom style related to that platform  (Windows style for "Windows Desktop" view, Android style for "Android..." view, Mac style for  "OS X Desktop" view, iOS style for "iPad" and for "iPhone" views). Note: If you have different views for iPad and iPhone, you need to load the same iOS style for each view.

4. If your application consists of multiple forms, you can set TStyleBook.UseStyleManager = True in each view in order to use the same custom styles for all other forms at runtime. If TStyleBook.UseStyleManager = True is set, then custom styles fully override system styles in all other forms (Form2, Form3etc.)  that are part of your application for that particular platform. If  TStyleBook.UseStyleManager = False is set, then new forms (Form2, Form3 etc.) will use the default platform style and for customization, you must add TStyleBook to Form2's "Master" view and load each custom style again for all created views of the additional forms that are part of your application.
