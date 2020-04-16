#include "inkey.ch"

#include "functions.ch"

#define COLOR_FIELD_SIZE 40
#define PATH_FIELD_SIZE 100

#define ROW_BROWSE_EDITABLE {'menu', 'reorder', 'save'}
#define ROW_BROWSE_EDITABLE_STRING 'menu;reorder;save'

PROCEDURE settings()

    LOCAL cQuestion := Config():get_config('DialogWhatShouldIDo')
    LOCAL cRestore := Config():get_config('RestoreSettings')
    LOCAL cChange := Config():get_config('ChangeSettings')
    LOCAL cQuit := Config():get_config('Quit')
    LOCAL cRestart := Config():get_config('NecessaryRestart')
    LOCAL lRestart := .F.

    SWITCH Dialog(cQuestion, {cRestore, cChange, cQuit})
        CASE 1
            lRestart := restore_previous()
            EXIT
        CASE 2
            lRestart := change_settings()
            EXIT
    ENDSWITCH

    IF lRestart
        Inform(cRestart)

        KEYBOARD Chr(K_ESC) + Chr(K_ENTER)
    ENDIF

RETURN

STATIC FUNCTION restore_previous()

    LOCAL hVariables := create_initial_config_hash()
    LOCAL lChanged := .F.
    LOCAL cKey

    IF YesNo(Config():get_config('YesNoRestoreSettings'))

        FOR EACH cKey IN hb_hKeys(hVariables)
            Config():set_config(cKey, hVariables[cKey])
        NEXT

        Config():save_config()

        lChanged := .T.
    ENDIF


RETURN lChanged

STATIC FUNCTION change_settings()

    MEMVAR GETLIST

    LOCAL cOldHeader := Window():header(Config():get_config('SettingsHeader'))
    LOCAL cOldFooter := Window():footer(Config():get_config('SettingsFooter'))
    LOCAL cOldFilter := dbFilter()
    LOCAL hVariables := hb_Hash('DefaultYesNoAllowMove';
                                , Config():get_config('DefaultYesNoAllowMove');
                                , 'DefaultDialogAllowMove';
                                , Config():get_config('DefaultDialogAllowMove');
                                , 'DefaultInformAllowMove';
                                , Config():get_config('DefaultInformAllowMove');
                                , 'RowBrowseDefaultBox';
                                , hb_Translate(Config():get_config('RowBrowseDefaultBox'), 'EN', hb_cdpSelect());
                                , 'DefaultBox';
                                , hb_Translate(Config():get_config('DefaultBox'), 'EN', hb_cdpSelect());
                                , 'WindowBorder';
                                , hb_Translate(Config():get_config('WindowBorder'), 'EN', hb_cdpSelect());
                                , 'DefaultWindowCreatorColor';
                                , PadR(Config():get_config('DefaultWindowCreatorColor'), COLOR_FIELD_SIZE);
                                , 'DefaultWindowCreatorBox';
                                , hb_Translate(Config():get_config('DefaultWindowCreatorBox'), 'EN', hb_cdpSelect());
                                , 'DefaultWindowCreatorShadow';
                                , PadR(Config():get_config('DefaultWindowCreatorShadow'), COLOR_FIELD_SIZE);
                                , 'DefaultBoxCreatorBox';
                                , hb_Translate(Config():get_config('DefaultBoxCreatorBox'), 'EN', hb_cdpSelect());
                                , 'DefaultBoxCreatorColor';
                                , PadR(Config():get_config('DefaultBoxCreatorColor'), COLOR_FIELD_SIZE);
                                , 'DefaultSayCreatorColor';
                                , PadR(Config():get_config('DefaultSayCreatorColor'), COLOR_FIELD_SIZE);
                                , 'DefaultGetSayCreatorColor';
                                , PadR(Config():get_config('DefaultGetSayCreatorColor'), COLOR_FIELD_SIZE);
                                , 'DefaultGetGetCreatorColor';
                                , PadR(Config():get_config('DefaultGetGetCreatorColor'), COLOR_FIELD_SIZE);
                                , 'DefaultCheckboxCreatorColor';
                                , PadR(Config():get_config('DefaultCheckboxCreatorColor'), COLOR_FIELD_SIZE);
                                , 'DefaultListboxCreatorColor';
                                , PadR(Config():get_config('DefaultListboxCreatorColor'), COLOR_FIELD_SIZE);
                                , 'DefaultRadiogroupCreatorColor';
                                , PadR(Config():get_config('DefaultRadiogroupCreatorColor'), COLOR_FIELD_SIZE);
                                , 'DefaultPushbuttonCreatorColor';
                                , PadR(Config():get_config('DefaultPushbuttonCreatorColor'), COLOR_FIELD_SIZE);
                                , 'SaveColor';
                                , PadR(Config():get_config('SaveColor'), COLOR_FIELD_SIZE);
                                , 'dbfPath';
                                , PadR(Config():get_config('dbfPath'), PATH_FIELD_SIZE);
                                , 'ntxPath';
                                , PadR(Config():get_config('ntxPath'), PATH_FIELD_SIZE);
                                , 'ShowCrucialForms';
                                , Config():get_config('ShowCrucialForms');
                               )
    LOCAL lChanged := .F.
    LOCAL cKey
    LOCAL axOldKeys
    LOCAL cOldScreen

    ZAP KEYS TO axOldKeys
    SAVE SCREEN TO cOldScreen

    Window():refresh_header_footer()

    SET FILTER TO

    add_row_browse_widths(hVariables)

    IF Parser():prepare_form_from_database(Config():get_config('Language'), 'SETTINGS', hVariables)
        
        SET KEY K_F2 TO select_color()
        SET KEY K_F3 TO select_box()

        READ
        WClose()
    ELSE
        throw(Config():get_config('CriticalError'))
    ENDIF

    IF LastKey() == K_CTRL_W .AND. YesNo(Config():get_config('YesNoSave'))
        hVariables := change_codepages(Parser():get_answers())

        FOR EACH cKey IN hb_hKeys(hVariables)

            IF ValType(hVariables[cKey]) == 'C'
                hVariables[cKey] := RTrim(hVariables[cKey])
            ENDIF

            IF Left(cKey, Len(cKey) - 1) $ ROW_BROWSE_EDITABLE_STRING
                change_row_browse(cKey, hVariables[cKey])
            ELSE
                Config():set_config(cKey, hVariables[cKey])
            ENDIF
        NEXT

        Config():save_config()

        lChanged := .T.
    ENDIF

    SET FILTER TO &(cOldFilter)

    Window():header(cOldHeader)
    Window():footer(cOldFooter)
    RESTORE SCREEN FROM cOldScreen
    RESTORE KEYS FROM axOldKeys

RETURN lChanged

STATIC FUNCTION change_codepages(hHash)

    hHash['RowBrowseDefaultBox'] := hb_Translate(hHash['RowBrowseDefaultBox'], hb_cdpSelect(), 'EN')
    hHash['DefaultBox'] := hb_Translate(hHash['DefaultBox'], hb_cdpSelect(), 'EN')
    hHash['WindowBorder'] := hb_Translate(hHash['WindowBorder'], hb_cdpSelect(), 'EN')
    hHash['DefaultWindowCreatorBox'] := hb_Translate(hHash['DefaultWindowCreatorBox'], hb_cdpSelect(), 'EN')
    hHash['DefaultBoxCreatorBox'] := hb_Translate(hHash['DefaultBoxCreatorBox'], hb_cdpSelect(), 'EN')

RETURN hHash

STATIC PROCEDURE add_row_browse_widths(hVariables)

    LOCAL nOldSelect := Select()
    LOCAL cId

    SELECT dbRowBrowse

    FOR EACH cId IN ROW_BROWSE_EDITABLE
        SEEK cId

        DO WHILE RTrim(field->id) == cId
            hVariables[cId + LTrim(Str(field->col_nr))] := field->width
            SKIP
        ENDDO
    NEXT

    SELECT (nOldSelect)

RETURN 

STATIC PROCEDURE change_row_browse(cKey, nNewWidth)

    LOCAL nOldSelect := Select()
    LOCAL cId := Left(cKey, Len(cKey) - 1)
    LOCAL nColumn := Val(Right(cKey, 1))

    SELECT dbRowBrowse

    SEEK PadR(cId, Len(field->id))

    DO WHILE RTrim(field->id) == cId
        IF field->col_nr == nColumn
            IF RLock()
                field->width := nNewWidth
                COMMIT
                UNLOCK
            ELSE
                Inform(Config():get_config('CantLock'))
                Inform(Config():get_config('CantSaveEverything'))
            ENDIF

            EXIT
        ENDIF
        SKIP
    ENDDO

    SELECT (nOldSelect)

RETURN
