#include "box.ch"
#include "inkey.ch"

#include "rowbrowse.ch"

FUNCTION create_initial_config_hash()

    LOCAL hConfig := hb_Hash(;
                            ;/*** FOOTERS ***/
                            'ProgramFirstFooter', 'ESC - quit  DEL - delete  F1 - reorder  F2 - create  F3 - add  F4 - preview  F5 - fast edit  F6 - clone  F7 - change ID';
                            , 'DisplayFormFooter', 'Press any key to quit the preview';
                            , 'ReorderDisplayForm', 'ALT+ENTER - READ  another key - quit';
                            , 'CreateNewFormFooter', "Enter the form's unique language and ID";
                            , 'ChangeIDFooter', "Enter the form's unique language and/or ID";
                            , 'CloneFooter', "Enter the form's unique language and/or ID";
                            , 'ReorderFooter', 'ESC - quit  DEL - delete  F2 - rebuild  F3 - change  F4 - edit  F5 - move  F6/F7 - preeview line/form  F8/F9 - move down/up  F10 - add window';
                            , 'CreatorWindowFooter', "Arrows - move the window's corner  Enter - fast edit  ALT+A - change the corner  ALT+ENTER - READ  ESC - quit"; 
                            , 'CreatorBoxFooter', "Arrows - move the window's corner  Enter - fast edit  ALT+A - change the corner  ALT+ENTER - READ  ESC - quit"; 
                            , 'CreatorSayFooter', "Arrows - move the window's corner  Enter - fast edit  ALT+ENTER - READ  ESC - quit"; 
                            , 'CreatorGetFooter', "Arrows - move the window's corner  Enter - fast edit  ALT+ENTER - READ  ESC - quit"; 
                            , 'CreatorCheckboxFooter', "Arrows - move the window's corner  Enter - fast edit  ALT+ENTER - READ  ESC - quit"; 
                            , 'CreatorListboxFooter', "Arrows - move the window's corner  Enter - fast edit  ALT+A - change the corner  ALT+Z - expand the list  ALT+ENTER - READ  ESC - quit"; 
                            , 'CreatorRadiogroupFooter', "Arrows - move the window's corner  Enter - fast edit  ALT+A - change the corner  ALT+ENTER - READ  ESC - quit"; 
                            , 'CreatorPushbuttonFooter', "Arrows - move the window's corner  Enter - fast edit  ALT+ENTER - READ  ESC - quit"; 
                            , 'MenuDefaultFooter', 'Select the item';
                            , 'SaveFooter', 'SPACE - change the save method  ENTER - change the variable name';
                            , 'MemoEditFooter', 'ESC - quit without save  CTRL+W - save  INS - insert/overwrite mode';
                            ;/*** HEADERS ***/
                            , 'ProgramFirstHeader', 'Welcome back! Select the action you are interested in';
                            , 'DisplayFormHeader', 'Form preview';
                            , 'CreateNewFormHeader', 'Creating a new form';
                            , 'AddToFormHeader', 'Adding items to the form';
                            , 'ChangeIDHeader', "Changing the form's ID";
                            , 'CloneHeader', 'The form cloning';
                            , 'ReorderHeader', 'Line-by-line preview of the form with the option of editing and moving';
                            , 'CreatorWindowHeader', 'Window wizard';
                            , 'CreatorBoxHeader', 'Box wizard';
                            , 'CreatorSayHeader', 'Say wizard';
                            , 'CreatorGetHeader', 'Get wizard';
                            , 'CreatorCheckboxHeader', 'Checkbox wizard';
                            , 'CreatorListboxHeader', 'Listbox wizard';
                            , 'CreatorRadiogroupHeader', 'Buttons wizard';
                            , 'CreatorPushbuttonHeader', 'Pushbutton wizard';
                            , 'FormFastEditHeader', 'Fast edit mode';
                            , 'SaveHeader', 'Save the form';
                            , 'MemoEditHeader', 'Edit the text';
                            ;/*** TITLES ***/
                            , 'MainRowBrowseTitle', ' Forms ';
                            , 'ReorderRowBrowseTitle', ' Reorder ';
                            , 'SaveTitle', ' Save the form ';
                            ;/*** DIALOGS ***/
                            , 'NoVariableFileDialog', "There isn't the variable file. Create it?";
                            , 'NoVariableFileInform', "There isn't the variable file. The program is going to be closed";
                            , 'InitFilesFailure', 'Files initialization failed!';
                            , 'InitConfigFailure', 'Configuration initialization failed!';
                            , 'QuitQuestion', 'Are you sure to quit?';
                            , 'DoReadOrder', 'Execute the READ order?';
                            , 'CreateForm', 'Create a new form?';
                            , 'InformRecordExists', 'The record already exists!';
                            , 'YesNoSave', 'Save?';
                            , 'YesNoDeleteForm', 'Are you sure to delete the form?';
                            , 'YesNoDeleteRow', 'Are you sure to delete the row?';
                            , 'YesNoFormIdLanguageChange', 'Are you sure to change the language or the ID of the form?';
                            , 'YesNoBreakEdition', 'Are you sure to break the edition?';
                            , 'DialogWhatShouldIDo', 'What should I do?';
                            , 'Continue', 'Continue';
                            , 'Save', 'Save';
                            , 'Quit', 'Quit';
                            , 'YesNoBreakEditionLostChanges', 'Are you sure to break the edition without save?';
                            , 'BrokenFormWhatShouldIDo', 'Warning! The form is broken!;What should I do?';
                            , 'DialogRemoveSpaces', 'Should I remove spaces?';
                            , 'FromLeft', 'Yes, from left';
                            , 'FromRight', 'Yes, from right';
                            , 'FromBoth', 'Yes, from both sides';
                            , 'SaveVariableEditing', 'Should I finish the edition and save all changes?';
                            , 'ChangeVariableValue', 'Should I change the value of the variable?';
                            , 'IncorrectValueVariable', 'Incorrect value of the variable';
                            , 'ChangeVariableDataType', 'Should I change the type of the variable?';
                            , 'DistinctNameFirstPart', 'Name ';
                            , 'DistinctNameSecondPart', ' is in use. Choose another one.';
                            , 'ChangeVariableName', 'Should I rename the variable?';
                            , 'IncorrectValues', 'Incorrect values;The operation is going to be terminated';
                            , 'CreateWithoutWindow', 'Should I create the form without a window?';
                            , 'NoRecordSelected', 'No record selected';
                            , 'ItIsVariable', "The value represents the form's variable and can't be saved as a constant!";
                            , 'ItIsNotVariable', "The value represents the form's constant! You have to change it to a variable before executing the operation";
                            , 'CantCreateEmptyForm', "The form's ID and the form's language can't be empty!";
                            , 'CriticalError', 'Critical error! Program is going to be closed!';
                            , 'ImportantForm', "The selected form is crucial for the program. It's modification or deletion may cause irreparable damage. Backup is recommended. Continue anyway?";
                            , 'OnlyOneWindowAllowed', 'Only one window per form is allowed';
                            ;/*** INNER LIB ***/
                            , 'Title', 'Forms v0.1 eLama';
                            , 'CantCreateConfigFile', 'Creating of the configuration file has failed';
                            , 'CorruptionDetected', 'Data is broken!';
                            , 'VariableRepeating', 'Variable is in use multiple times';
                            , 'IncorrectDataType', 'Incorrect data type';
                            , 'IncorrectValue', 'Incorrect value';
                            , 'IncorrectDimensions', 'The dimensions are not in the correct order';
                            , 'DefaultPrintMessageOption', 'Ok';
                            , 'DefaultYes', 'Yes';
                            , 'DefaultNo', 'No';
                            , 'DefaultYesNoAllowMove', .T.;
                            , 'DefaultDialogAllowMove', .T.;
                            , 'DefaultInformAllowMove', .T.;
                            , 'RowBrowseDefaultBox', HB_B_SINGLE_DOUBLE_UNI;
                            , 'DefaultBox', HB_B_DOUBLE_UNI;
                            , 'WindowBorder', HB_B_SINGLE_UNI;
                            ;/*** OTHER ***/
                            , 'DefaultWindowCreatorColor', 'R/W,N/W,N/W,N/W,N/W';
                            , 'DefaultWindowCreatorBox', HB_B_DOUBLE_UNI;
                            , 'DefaultWindowCreatorShadow', 'N+';
                            , 'DefaultBoxCreatorBox', HB_B_DOUBLE_UNI;
                            , 'DefaultBoxCreatorColor', 'R/W,N/W,N/W,N/W,N/W';
                            , 'DefaultSayCreatorColor', 'N/W,W/N,N/W,N/W,N/W';
                            , 'DefaultGetSayCreatorColor', 'N/W,W/N,N/W,N/W,N/W';
                            , 'DefaultGetGetCreatorColor', 'N/W,W/N,GR/B,G/B,BG/G';
                            , 'DefaultCheckboxCreatorColor', 'GR/B,RB/G,BG/R,B/R,';
                            , 'DefaultListboxCreatorColor', 'RB/G,R/B,BG/R,BG/G,G/B,N/W,W/N,';
                            , 'DefaultRadiogroupCreatorColor', 'GR/B,RB/B,BG/R,,B/R';
                            , 'DefaultPushbuttonCreatorColor', 'GR/B,RB/G,BG/R,B/R,N/W';
                            , 'SaveColor', 'W/N,N/W,W*/N,N*/W';
                            , 'VariablesDefinitions', 'dbVariables.dbf';
                            , 'dbfPath', 'dbf/';
                            , 'ntxPath', 'ntx/';
                            , 'SaveAsConstant', 'constant';
                            , 'SaveAsVariable', 'variable';
                            , 'Language', 'ENGLISH';
                            , 'Found', 'Found: ';
                            , 'Code', ' Code ';
                            , 'CantLock', 'Somebody is using this record at the moment';
                            )
RETURN hConfig

FUNCTION row_browse_main_search(oRowBrowse, nKey)

    LOCAL nOldRecNo := RecNo()
    LOCAL cCurrentString := oRowBrowse:search_keys()
    LOCAL nReturn := ROWBROWSE_NOTHING

    IF AScan({K_DOWN, K_UP, K_HOME, K_END, K_PGUP, K_PGDN}, nKey) != 0
        oRowBrowse:search_keys('')
        oRowBrowse:draw_border()
        oRowBrowse:print_title()
    ELSEIF nKey == K_BS
        cCurrentString := Left(cCurrentString, Len(cCurrentString) - 1)
        oRowBrowse:search_keys(cCurrentString)
        oRowBrowse:draw_border()
        oRowBrowse:print_title()

        IF Len(cCurrentString) > 0
            @ oRowBrowse:bottom(), oRowBrowse:left() SAY Config():get_config('Found') + cCurrentString
        ENDIF
    ELSEIF !hb_hHasKey(oRowBrowse:keys_map(), nKey) .AND. !Empty(IndexKey())
        IF oRowBrowse:search(cCurrentString + Upper(Chr(nKey)))
            nReturn := ROWBROWSE_SEARCH
            oRowBrowse:search_keys(cCurrentString + Upper(Chr(nKey)))
            oRowBrowse:draw_border()
            oRowBrowse:print_title()
            @ oRowBrowse:bottom(), oRowBrowse:left() SAY Config():get_config('Found') + cCurrentString + Upper(Chr(nKey))
        ELSE
            GO nOldRecNo
        ENDIF
    ENDIF

RETURN nReturn
