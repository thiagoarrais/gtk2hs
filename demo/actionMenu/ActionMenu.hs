import Graphics.UI.Gtk

uiDef =
  "<ui>\
  \  <menubar>\
  \    <menu name=\"File\" action=\"FileAction\">\
  \      <menuitem name=\"New\" action=\"NewAction\" />\
  \      <menuitem name=\"Open\" action=\"OpenAction\" />\
  \      <menuitem name=\"Save\" action=\"SaveAction\" />\
  \      <menuitem name=\"SaveAs\" action=\"SaveAsAction\" />\
  \      <separator/>\
  \      <menuitem name=\"Exit\" action=\"ExitAction\"/>\
  \      <placeholder name=\"FileMenuAdditions\" />\
  \    </menu>\
  \    <menu name=\"Edit\" action=\"EditAction\">\
  \      <menuitem name=\"Cut\" action=\"CutAction\"/>\
  \      <menuitem name=\"Copy\" action=\"CopyAction\"/>\
  \      <menuitem name=\"Paste\" action=\"PasteAction\"/>\
  \    </menu>\
  \  </menubar>\
  \  <toolbar>\
  \    <placeholder name=\"FileToolItems\">\
  \      <separator/>\
  \      <toolitem name=\"New\" action=\"NewAction\"/>\
  \      <toolitem name=\"Open\" action=\"OpenAction\"/>\
  \      <toolitem name=\"Save\" action=\"SaveAction\"/>\
  \      <separator/>\
  \    </placeholder>\
  \    <placeholder name=\"EditToolItems\">\
  \      <separator/>\
  \      <toolitem name=\"Cut\" action=\"CutAction\"/>\
  \      <toolitem name=\"Copy\" action=\"CopyAction\"/>\
  \      <toolitem name=\"Paste\" action=\"PasteAction\"/>\
  \      <separator/>\
  \    </placeholder>\
  \  </toolbar>\
  \</ui>"

main = do
  initGUI

  -- Create the menus
  fileAct <- actionNew "FileAction" "File" Nothing Nothing
  editAct <- actionNew "EditAction" "Edit" Nothing Nothing

  -- Create menu items
  newAct <- actionNew "NewAction" "New"
	    (Just "Clear the spreadsheet area.")
	    (Just stockNew)
  newAct `onActionActivate` putStrLn "New activated."
  openAct <- actionNew "OpenAction" "Open"
	    (Just "Open an existing spreadsheet.")
	    (Just stockOpen)
  openAct `onActionActivate` putStrLn "Open activated."
  saveAct <- actionNew "SaveAction" "Save"
	    (Just "Save the current spreadsheet.")
	    (Just stockSave)
  saveAct `onActionActivate` putStrLn "Save activated."
  saveAsAct <- actionNew "SaveAsAction" "SaveAs"
	    (Just "Save spreadsheet under new name.")
	    (Just stockSaveAs)
  saveAsAct `onActionActivate` putStrLn "SaveAs activated."
  exitAct <- actionNew "ExitAction" "Exit"
	    (Just "Exit this application.")
	    (Just stockSaveAs)
  exitAct `onActionActivate` mainQuit 
  cutAct <- actionNew "CutAction" "Cut"
	    (Just "Cut out the current selection.")
	    (Just stockCut)
  cutAct `onActionActivate` putStrLn "Cut activated."
  copyAct <- actionNew "CopyAction" "Copy"
	    (Just "Copy the current selection.")
	    (Just stockCopy)
  copyAct `onActionActivate` putStrLn "Copy activated."
  pasteAct <- actionNew "PasteAction" "Paste"
	    (Just "Paste the current selection.")
	    (Just stockPaste)
  pasteAct `onActionActivate` putStrLn "Paste activated."
  
  standardGroup <- actionGroupNew "standard"
  mapM_ (actionGroupAddAction standardGroup) [fileAct, editAct]
  mapM_ (\act -> actionGroupAddActionWithAccel standardGroup act Nothing)
    [newAct, openAct, saveAct, saveAsAct, exitAct, cutAct, copyAct, pasteAct]
  
  ui <- uiManagerNew
  mid <- uiManagerAddUiFromString ui uiDef
  uiManagerInsertActionGroup ui standardGroup 0

  win <- windowNew
  win `onDestroy` mainQuit
  win `onSizeRequest` return (Requisition 200 100)
  (Just menuBar) <- uiManagerGetWidget ui "/ui/menubar"
  (Just toolBar) <- uiManagerGetWidget ui "/ui/toolbar"
 
  edit <- textViewNew 
  vBox <- vBoxNew False 0
  set vBox [boxHomogeneous := False]
  boxPackStart vBox menuBar PackNatural 0
  boxPackStart vBox toolBar PackNatural 0
  boxPackStart vBox edit PackGrow 0

  containerAdd win vBox
  widgetShowAll win

  mainGUI
