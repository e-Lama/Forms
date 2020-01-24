#include "fileio.ch"
#include "inkey.ch"
#include "box.ch"

#include "functions.ch"
#include "rowbrowse.ch"
#include "parser.ch"

REQUEST HB_CODEPAGE_UTF8EX
//REQUEST HB_CODEPAGE_UTF8

PROCEDURE main()

    MEMVAR GETLIST, oErr

    LOCAL oRowBrowse

    PUBLIC GETLIST

    BEGIN SEQUENCE

        //ErrorBlock({| oError | standard_error_handler(oError)})

        IF Config():init_config(create_initial_config_hash()) .AND. Variable():handle_variables()

            SELECT dbForms
            SET RELATION TO field->language + field->id INTO dbVariables

            SET KEY K_F1 TO change_order()
            SET KEY K_F2 TO create_new_form()
            SET KEY K_F3 TO add_to_form()
            SET KEY K_F4 TO display_form()
            SET KEY K_F5 TO fast_edit()
            SET KEY K_F6 TO clone()
            SET KEY K_F7 TO change_id()
            SET KEY K_DEL TO ask_delete_form()
            SET KEY K_ESC TO quit_program()

            Window():header(Config():get_config('ProgramFirstHeader'))
            Window():footer(Config():get_config('ProgramFirstFooter'))
            Window():title(Config():get_config('Title'))
            Window():apply_config()
            Window():refresh_window()

            @ Window():get_top() + 1, Window():get_left() + 1, Window():get_bottom() - 1, Window():get_right() - 1;
              ROWBROWSE oRowBrowse ID 'menu' TITLE Config():get_config('MainRowBrowseTitle');
              ACTION {| oRowBrowse, nKey | row_browse_main_search(oRowBrowse, nKey)} BORDER Config():get_config('RowBrowseDefaultBox');
              COLOR Config():get_config('DefaultColor')

            oRowBrowse:display()
        ELSE
            Config():get_config('InitConfigFailure')
        ENDIF
    RECOVER USING oErr
        standard_error_handler(oErr)
    END SEQUENCE

RETURN

INIT PROCEDURE prepare()

    SET CURSOR OFF
    SET CONFIRM ON
    SET EXACT ON
    SET OPTIMIZE ON
    SET CENTURY ON
    SET DELETED ON
    SET INTENSITY ON
    SET EVENTMASK TO INKEY_ALL
    SET DATE TO GERMAN
    SET SCOREBOARD OFF
    hb_cdpSelect('UTF8EX') 
    //SetCancel(.F.)

    CLS

RETURN

EXIT PROCEDURE finish()
    
    MEMVAR GETLIST

    CLOSE ALL
    CLEAR
    CLS

RETURN

PROCEDURE quit_program()

    LOCAL axOldKeys

    ZAP KEYS TO axOldKeys

    IF YesNo(Config():get_config('QuitQuestion'))
        QUIT
    ENDIF

    RESTORE KEYS FROM axOldKeys

RETURN

FUNCTION important_form(cID)

    LOCAL acImportantForms := {'CREATE_FORM', 'SET_DISTINCT_NAME', 'WHERE_MOVE', 'SWAP'}

RETURN AScan(acImportantForms, AllTrim(cId)) != 0

PROCEDURE display_form()

    MEMVAR GETLIST

    LOCAL cOldColor := SetColor()
    LOCAL cOldFooter := Window():footer(Config():get_config('DisplayFormFooter'))
    LOCAL cOldHeader := Window():header(Config():get_config('DisplayFormHeader'))
    LOCAL hJson
    LOCAL cOldScreen
    LOCAL axOldKeys

    ZAP KEYS TO axOldKeys

    IF EoF()
        Inform(Config():get_config('NoRecordSelected'))
        RESTORE KEYS FROM axOldKeys
        RETURN
    ENDIF

    SAVE SCREEN TO cOldScreen

    hJson := hb_JsonDecode(dbVariables->json)

    IF ValType(hJson) != 'H'
        Inform(Config():get_config('CorruptionDetected'))
        RESTORE KEYS FROM axOldKeys
        RETURN
    ENDIF

    IF !Parser():prepare_form_from_database(field->language, field->id, hJson) 
        Inform(Parser():log())
    ELSE
        Window():refresh_header()
        Window():refresh_footer()
        IF Len(GETLIST) > 0 .AND. NoYes(Config():get_config('DoReadOrder'))
            READ
        ELSE
            Inkey(0)
        ENDIF
    ENDIF

    WClose()
    Window():header(cOldHeader)
    Window():footer(cOldFooter)
    RESTORE SCREEN FROM cOldScreen
    RESTORE KEYS FROM axOldKeys
    SET COLOR TO (cOldColor)
    CLEAR GETS

RETURN

PROCEDURE create_new_form()

    MEMVAR GETLIST

    LOCAL cOldHeader := Window():header(Config():get_config('CreateNewFormHeader'))
    LOCAL cOldFooter := Window():footer(Config():get_config('CreateNewFormFooter'))
    LOCAL nOldRecNo := RecNo()
    LOCAL cNewId := Space(Len(field->id))
    LOCAL cNewLanguage := Space(Len(field->language))
    LOCAL hVariables := hb_Hash('cLanguage';
                                , PadR(Config():get_config('Language'), Len(field->language));
                                , 'cID';
                                , PadR('ID', Len(field->id));
                               )
    LOCAL axOldKeys
    LOCAL cOldScreen

    SAVE SCREEN TO cOldScreen
    ZAP KEYS TO axOldKeys

    IF YesNo(Config():get_config('CreateForm'))

        Window():refresh_header()
        Window():refresh_footer()

        IF Parser():prepare_form_from_database(Config():get_config('Language'), 'CREATE_FORM', hVariables)
            READ
            hVariables := Parser():get_answers()
            cNewId := AllTrim(hVariables['cID'])
            cNewLanguage := AllTrim(hVariables['cLanguage'])
            WClose()
        ELSE
            throw(Config():get_config('CriticalError'))
        ENDIF

        IF Empty(cNewLanguage) .OR. Empty(cNewId)
            Inform(Config():get_config('CantCreateEmptyForm'))
        ELSE

            SEEK PadR(cNewLanguage, Len(field->language)) + PadR(cNewId, Len(field->id))

            IF Found()
                Inform(Config():get_config('InformRecordExists'))
            ELSE
                APPEND BLANK
                field->id := cNewId
                field->language := cNewLanguage
                SELECT dbVariables
                APPEND BLANK
                field->id := cNewId
                field->language := cNewLanguage
                SELECT dbForms
                COMMIT

                IF YesNo(Config():get_config('CreateWithoutWindow')) .OR. Creator_window():edit_form()
                    add_to_form(.T.)
                ELSE
                    WClose()
                    delete_form()
                    GO nOldRecNo
                ENDIF
            ENDIF
        ENDIF
    ENDIF

    Window():header(cOldHeader)
    Window():footer(cOldFooter)
    RESTORE SCREEN FROM cOldScreen
    RESTORE KEYS FROM axOldKeys

RETURN

PROCEDURE add_to_form(lFromCreateForm)

    MEMVAR GETLIST

    LOCAL cOldHeader := Window():header(Config():get_config('AddToFormHeader'))
    LOCAL cOldFooter := Window():footer(Config():get_config('AddToFormFooter'))
    LOCAL aoOldGetList := AClone(GETLIST)
    LOCAL acMenuItems := {'BOX', 'SAY', 'GET', 'CHECKBOX', 'LISTBOX', 'RADIOGROUP'}
    LOCAL nChoose := 1
    LOCAL hJson
    LOCAL axOldKeys
    LOCAL cOldScreen

    ZAP KEYS TO axOldKeys

    IF EoF()
        Inform(Config():get_config('NoRecordSelected'))
        RESTORE KEYS FROM axOldKeys
        RETURN
    ENDIF

    SAVE SCREEN TO cOldScreen

    Window():refresh_header()
    Window():refresh_footer()

    IF !Empty(field->code) .AND. (ValType(lFromCreateForm) != 'L' .OR. (ValType(lFromCreateForm) == 'L' .AND. !lFromCreateForm))
        field->code := field->code + OBJECT_SEPARATOR
    ENDIF

    DO WHILE nChoose != 0

        IF WSelect() == 0
            RESTORE SCREEN FROM cOldScreen
        ELSE
            WClose()
        ENDIF

        GETLIST := {}
        
        hJson := hb_JsonDecode(dbVariables->json)
        IF ValType(hJson) != 'H'
            hJson := hb_Hash()
        ENDIF
        Parser():prepare_form_from_database(field->language, field->id, hJson)

        nChoose := display_menu_center_autosize(Window():center_row(), Window():center_col(), acMenuItems, .T.;
                                               , 'menu_search_allow_exit', 1, Config():get_config('DefaultMenuColor');
                                               , Config():get_config('DefaultBox');
                                               )
        IF nChoose > 0
            &('Creator_' + Lower(acMenuItems[nChoose]) + '():edit_form()')
        ENDIF
    ENDDO

    IF Right(dbForms->code, Len(OBJECT_SEPARATOR)) == OBJECT_SEPARATOR
        dbForms->code := Left(dbForms->code, Len(dbForms->code) - Len(OBJECT_SEPARATOR))
    ENDIF

    WClose()
    Window():header(cOldHeader)
    Window():footer(cOldFooter)
    RESTORE SCREEN FROM cOldScreen
    RESTORE KEYS FROM axOldKeys
    GETLIST := aoOldGetList

RETURN

PROCEDURE fast_edit()

    LOCAL nOldCursor := Set(_SET_CURSOR)
    LOCAL nTop := Window():get_top() + 1
    LOCAL nLeft := Window():get_left() + 1
    LOCAL nBottom := Window():get_bottom() - 1
    LOCAL nRight := Window():get_right() - 1
    LOCAL axOldKeys
    LOCAL cOldScreen

    ZAP KEYS TO axOldKeys

    IF EoF()
        Inform(Config():get_config('NoRecordSelected'))
        RESTORE KEYS FROM axOldKeys
        RETURN
    ENDIF

    SAVE SCREEN TO cOldScreen

    @ nTop, nLeft, nBottom, nRight BOX B_SINGLE
    @ nTop, Int((nRight + nLeft - Len(' Kod ')) / 2) SAY ' Kod '
    field->code := MemoEdit(field->code, nTop + 1, nLeft + 1, nBottom - 1, nRight - 1)
    @ nTop, nLeft, nBottom, nRight BOX B_SINGLE
    @ nTop, Int((nRight + nLeft - Len(' JSON ')) / 2) SAY ' JSON '
    dbVariables->json := MemoEdit(dbVariables->json, nTop + 1, nLeft + 1, nBottom - 1, nRight - 1)

    SET CURSOR (cast(nOldCursor, 'L'))

    RESTORE KEYS FROM axOldKeys
    RESTORE SCREEN FROM cOldScreen

RETURN

PROCEDURE ask_delete_form()

    LOCAL axOldKeys

    ZAP KEYS TO axOldKeys

    IF EoF()
        Inform(Config():get_config('NoRecordSelected'))
        RESTORE KEYS FROM axOldKeys
        RETURN
    ENDIF

    IF NoYes(Config():get_config('YesNoDeleteForm'))
        IF important_form(field->id) 
            IF NoYes(Config():get_config('ImportantForm'))
                delete_form()
            ENDIF
        ELSE
            delete_form()
        ENDIF
    ENDIF

    RESTORE KEYS FROM axOldKeys

RETURN

PROCEDURE delete_form()

    LOCAL nOldSelect := Select()

    SELECT dbForms
    DELETE
    SELECT dbVariables
    DELETE
    PACK
    SELECT dbForms
    PACK

    SELECT (nOldSelect)
    SKIP

    IF EoF()
        SKIP -1
    ENDIF

RETURN

PROCEDURE change_id()

    MEMVAR GETLIST

    LOCAL nOldRecNo := RecNo()
    LOCAL cOldHeader := Window():header(Config():get_config('ChangeIDHeader'))
    LOCAL cOldFooter := Window():footer(Config():get_config('ChangeIDFooter'))
    LOCAL hVariables := hb_Hash('cLanguage';
                                , field->language;
                                , 'cID';
                                , field->id;
                               )
    LOCAL lSuccess := .F.
    LOCAL cNewLanguage
    LOCAL cNewId
    LOCAL cOldScreen
    LOCAL axOldKeys

    ZAP KEYS TO axOldKeys

    IF EoF()
        Inform(Config():get_config('NoRecordSelected'))
        RESTORE KEYS FROM axOldKeys
        RETURN
    ENDIF
    
    SAVE SCREEN TO cOldScreen

    IF NoYes(Config():get_config('YesNoFormIdLanguageChange'))

        Window():refresh_header()
        Window():refresh_footer()

        DO WHILE !lSuccess
            IF Parser():prepare_form_from_database(Config():get_config('Language'), 'CREATE_FORM', hVariables)
                READ
                hVariables := Parser():get_answers()
                cNewId := AllTrim(hVariables['cID'])
                cNewLanguage := AllTrim(hVariables['cLanguage'])
                WClose()
            ELSE
                throw(Config():get_config('CriticalError'))
            ENDIF

            IF LastKey() == K_ESC 
                lSuccess := NoYes(Config():get_config('YesNoBreakEdition'))
            ELSEIF Empty(cNewLanguage) .OR. Empty(cNewId)
                Inform(Config():get_config('CantCreateEmptyForm'))
            ELSE

                SEEK PadR(cNewLanguage, Len(field->language)) + PadR(cNewId, Len(field->id))

                IF LastKey() == K_ESC 
                    lSuccess := NoYes(Config():get_config('YesNoBreakEdition'))
                ELSEIF Found()
                    Inform(Config():get_config('InformRecordExists'))
                ELSEIF YesNo(Config():get_config('YesNoSave'))
                    lSuccess := .T.
                    GO nOldRecNo
                    SET RELATION TO 
                    dbForms->id := cNewId
                    dbVariables->id := cNewId
                    dbForms->language := cNewLanguage
                    dbVariables->language := cNewLanguage
                    SELECT dbForms
                    SET RELATION TO field->language + field->id INTO dbVariables
                ENDIF
            ENDIF
        ENDDO

    ENDIF

    Window():header(cOldHeader)
    Window():footer(cOldFooter)
    RESTORE SCREEN FROM cOldScreen
    RESTORE KEYS FROM axOldKeys

RETURN

PROCEDURE clone()

    MEMVAR GETLIST

    LOCAL cOldHeader := Window():header(Config():get_config('CloneHeader'))
    LOCAL cOldFooter := Window():footer(Config():get_config('CloneFooter'))
    LOCAL cNewId := field->id
    LOCAL cNewLanguage := field->language
    LOCAL cCode := field->code
    LOCAL cJson := dbVariables->json
    LOCAL hVariables := hb_Hash('cLanguage';
                                , field->language;
                                , 'cID';
                                , field->id;
                               )
    LOCAL lSuccess := .F.
    LOCAL cOldScreen
    LOCAL axOldKeys

    ZAP KEYS TO axOldKeys

    IF EoF()
        Inform(Config():get_config('NoRecordSelected'))
        RESTORE KEYS FROM axOldKeys
        RETURN
    ENDIF

    SAVE SCREEN TO cOldScreen

    Window():refresh_header()
    Window():refresh_footer()

    DO WHILE !lSuccess
        IF Parser():prepare_form_from_database(Config():get_config('Language'), 'CREATE_FORM', hVariables)
            READ
            hVariables := Parser():get_answers()
            cNewId := AllTrim(hVariables['cID'])
            cNewLanguage := AllTrim(hVariables['cLanguage'])
            WClose()
        ELSE
            throw(Config():get_config('CriticalError'))
        ENDIF

        IF LastKey() == K_ESC 
            lSuccess := NoYes(Config():get_config('YesNoBreakEdition'))
        ELSEIF Empty(cNewLanguage) .OR. Empty(cNewId)
            Inform(Config():get_config('CantCreateEmptyForm'))
        ELSE

            SEEK PadR(cNewLanguage, Len(field->language)) + PadR(cNewId, Len(field->id))

            IF Found()
                Inform(Config():get_config('InformRecordExists'))
            ELSEIF YesNo(Config():get_config('YesNoSave'))
                lSuccess := .T.
                APPEND BLANK
                field->id := cNewId
                field->language := cNewLanguage
                field->code := cCode
                SELECT dbVariables
                APPEND BLANK
                field->id := cNewId
                field->language := cNewLanguage
                field->json := cJson
                SELECT dbForms
                COMMIT
            ENDIF
        ENDIF
    ENDDO

    Window():header(cOldHeader)
    Window():footer(cOldFooter)
    RESTORE SCREEN FROM cOldScreen
    RESTORE KEYS FROM axOldKeys

RETURN
