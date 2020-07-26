#include "fileio.ch"
#include "inkey.ch"
#include "box.ch"
#include "hbgtinfo.ch"

#include "functions.ch"
#include "rowbrowse.ch"
#include "parser.ch"

#define INITIALIZATION_FAILED 'Configuration initialization failed!'

REQUEST HB_CODEPAGE_UTF8EX

PROCEDURE main()

    MEMVAR GETLIST, oError

    LOCAL oRowBrowse

    PUBLIC GETLIST

    hb_gtInfo(HB_GTI_WINTITLE, 'Forms')

    BEGIN SEQUENCE

        ErrorBlock({| oError | standard_error_handler(oError)})

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
            SET KEY K_F8 TO settings()
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

            IF !Config():get_config('ShowCrucialForms')
                SET FILTER TO !important_form(field->id)
                GO TOP
            ENDIF

            oRowBrowse:display()
        ELSE
            Inform(INITIALIZATION_FAILED)
        ENDIF
    RECOVER USING oError
        standard_error_handler(oError)
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
    SetCancel(.F.)

    CLS

RETURN

EXIT PROCEDURE finish()
    
    MEMVAR GETLIST

    CLOSE ALL
    CLEAR
    CLS

RETURN

STATIC PROCEDURE quit_program()

    LOCAL axOldKeys

    ZAP KEYS TO axOldKeys

    IF YesNo(Config():get_config('QuitQuestion'))
        QUIT
    ENDIF

    RESTORE KEYS FROM axOldKeys

RETURN

STATIC PROCEDURE display_form()

    MEMVAR GETLIST

    LOCAL cOldColor := SetColor()
    LOCAL cOldFooter := Window():footer(Config():get_config('DisplayFormFooter'))
    LOCAL cOldHeader := Window():header(Config():get_config('DisplayFormHeader'))
    LOCAL cOldScreen
    LOCAL axOldKeys

    ZAP KEYS TO axOldKeys
    SAVE SCREEN TO cOldScreen

    IF EoF()
        Inform(Config():get_config('NoRecordSelected'))
    ELSEIF ValType(hb_JsonDecode(dbVariables->json)) != 'H'
        Inform(Config():get_config('CorruptionDetected'))
    ELSEIF validate()
        
        Window():refresh_header_footer()

        IF prepare_form()
            IF Len(GETLIST) > 0 .AND. NoYes(Config():get_config('DoReadOrder'))
                READ
            ELSE
                Inkey(0)
            ENDIF
        ELSE
            Inform(Parser():log(''))
        ENDIF
    ELSE
        Inform(Parser():log(''))
    ENDIF

    WClose()
    Window():header(cOldHeader)
    Window():footer(cOldFooter)
    RESTORE SCREEN FROM cOldScreen
    RESTORE KEYS FROM axOldKeys
    SET COLOR TO (cOldColor)
    CLEAR GETS

RETURN

STATIC PROCEDURE create_new_form()

    MEMVAR GETLIST

    LOCAL cOldHeader := Window():header(Config():get_config('CreateNewFormHeader'))
    LOCAL cOldFooter := Window():footer(Config():get_config('CreateNewFormFooter'))
    LOCAL cOldFilter := dbFilter()
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
    SET FILTER TO

    IF YesNo(Config():get_config('CreateForm'))

        Window():refresh_header_footer()

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
                IF append()
                    field->id := cNewId
                    field->language := cNewLanguage

                    SELECT dbVariables

                    IF append()
                        field->id := cNewId
                        field->language := cNewLanguage

                        SELECT dbForms
                        COMMIT

                        IF YesNo(Config():get_config('CreateWithoutWindow')) .OR. Creator_window():edit_form()
                            add_to_form()
                        ELSE
                            WClose()
                            delete_form()
                            GO nOldRecNo
                        ENDIF
                    ELSE
                        SELECT dbForms
                        DELETE
                        PACK
                        Inform(Config():get_config('CantLock'))
                    ENDIF
                ELSE
                    Inform(Config():get_config('CantLock'))
                ENDIF
            ENDIF
        ENDIF
    ENDIF

    UNLOCK ALL
    SET FILTER TO &(cOldFilter)
    Window():header(cOldHeader)
    Window():footer(cOldFooter)
    RESTORE SCREEN FROM cOldScreen
    RESTORE KEYS FROM axOldKeys

RETURN

STATIC PROCEDURE add_to_form()

    MEMVAR GETLIST

    LOCAL cOldHeader := Window():header(Config():get_config('AddToFormHeader'))
    LOCAL cOldFooter := Window():footer(Config():get_config('MenuDefaultFooter'))
    LOCAL aoOldGetList := AClone(GETLIST)
    LOCAL acMenuItems := {'BOX', 'SAY', 'GET', 'CHECKBOX', 'LISTBOX', 'RADIOGROUP', 'PUSHBUTTON'}
    LOCAL nChoose := 1
    LOCAL axOldKeys
    LOCAL cOldScreen

    ZAP KEYS TO axOldKeys

    IF EoF()
        Inform(Config():get_config('NoRecordSelected'))
        RESTORE KEYS FROM axOldKeys
        RETURN
    ELSEIF important_form(field->id) .AND. !NoYes(Config():get_config('ImportantForm'))
        RESTORE KEYS FROM axOldKeys
        RETURN
    ELSEIF !dbRLock(RecNo()) .AND. !dbVariables->(dbRLock(RecNo()))
        Inform(Config():get_config('CantLock'))
        UNLOCK ALL
        RESTORE KEYS FROM axOldKeys
        RETURN
    ENDIF

    Window():refresh_header_footer()
    SAVE SCREEN TO cOldScreen

    DO WHILE nChoose != 0

        IF WSelect() == 0
            RESTORE SCREEN FROM cOldScreen
        ELSE
            WClose()
        ENDIF

        CLEAR GETS
        
        IF prepare_form() 

            nChoose := display_menu_center_autosize(Window():center_row(), Window():center_col(), acMenuItems, .T.;
                                                   , 'menu_search_allow_exit_move', 1, Config():get_config('DefaultMenuColor');
                                                   , Config():get_config('DefaultBox');
                                                   )
            IF nChoose > 0
                &('Creator_' + Lower(acMenuItems[nChoose]) + '():edit_form()')
            ENDIF
        ELSE
            Inform(Parser():log(''))
            nChoose := 0
        ENDIF
    ENDDO

    IF Right(dbForms->code, Len(OBJECT_SEPARATOR)) == OBJECT_SEPARATOR
        dbForms->code := Left(dbForms->code, Len(dbForms->code) - Len(OBJECT_SEPARATOR))
    ENDIF

    UNLOCK ALL
    WClose()
    Window():header(cOldHeader)
    Window():footer(cOldFooter)
    RESTORE SCREEN FROM cOldScreen
    Window():refresh_header_footer()
    RESTORE KEYS FROM axOldKeys
    GETLIST := aoOldGetList

RETURN

STATIC PROCEDURE fast_edit()

    LOCAL cOldHeader := Window():header(Config():get_config('MemoEditHeader'))
    LOCAL cOldFooter := Window():footer(Config():get_config('MemoEditFooter'))
    LOCAL nOldCursor := Set(_SET_CURSOR)
    LOCAL nTop := Window():get_top() + 1
    LOCAL nLeft := Window():get_left() + 1
    LOCAL nBottom := Window():get_bottom() - 1
    LOCAL nRight := Window():get_right() - 1
    LOCAL axOldKeys
    LOCAL cOldScreen

    ZAP KEYS TO axOldKeys
    SAVE SCREEN TO cOldScreen

    IF EoF()
        Inform(Config():get_config('NoRecordSelected'))
    ELSEIF important_form(field->id) .AND. !NoYes(Config():get_config('ImportantForm'))
        //...
    ELSEIF !dbRLock(RecNo()) .OR. !dbVariables->(dbRLock(RecNo()))
        Inform(Config():get_config('CantLock'))
    ELSE

        Window():refresh_header_footer()

        @ nTop, nLeft, nBottom, nRight BOX B_SINGLE
        @ nTop, Int((nRight + nLeft - Len(Config():get_config('Code'))) / 2) SAY Config():get_config('Code')
        field->code := MemoEdit(field->code, nTop + 1, nLeft + 1, nBottom - 1, nRight - 1)
        @ nTop, nLeft, nBottom, nRight BOX B_SINGLE
        @ nTop, Int((nRight + nLeft - Len(' JSON ')) / 2) SAY ' JSON '
        dbVariables->json := MemoEdit(dbVariables->json, nTop + 1, nLeft + 1, nBottom - 1, nRight - 1)

        UNLOCK ALL
    ENDIF

    UNLOCK ALL
    Window():header(cOldHeader)
    Window():footer(cOldFooter)
    RESTORE SCREEN FROM cOldScreen
    Window():refresh_header_footer()
    SET CURSOR (cast(nOldCursor, 'L'))
    RESTORE KEYS FROM axOldKeys

RETURN

STATIC PROCEDURE ask_delete_form()

    LOCAL axOldKeys

    ZAP KEYS TO axOldKeys

    IF EoF()
        Inform(Config():get_config('NoRecordSelected'))
    ELSEIF !dbRLock(RecNo()) .OR. !dbVariables->(dbRLock(RecNo()))
        Inform(Config():get_config('CantLock'))
    ELSE
        IF NoYes(Config():get_config('YesNoDeleteForm'))
            IF important_form(field->id) 
                IF NoYes(Config():get_config('ImportantForm'))
                    delete_form()
                ENDIF
            ELSE
                delete_form()
            ENDIF
        ENDIF
    ENDIF

    UNLOCK ALL
    RESTORE KEYS FROM axOldKeys

RETURN

STATIC PROCEDURE delete_form()

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

STATIC PROCEDURE change_id()

    MEMVAR GETLIST

    LOCAL nOldRecNo := RecNo()
    LOCAL cOldHeader := Window():header(Config():get_config('ChangeIDHeader'))
    LOCAL cOldFooter := Window():footer(Config():get_config('ChangeIDFooter'))
    LOCAL cOldFilter := dbFilter()
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
    SAVE SCREEN TO cOldScreen
    SET FILTER TO

    IF EoF() 
        Inform(Config():get_config('NoRecordSelected'))
    ELSEIF important_form(field->id) .AND. !NoYes(Config():get_config('ImportantForm'))
        //...
    ELSEIF !dbRLock(RecNo()) .OR. !dbVariables->(dbRLock(RecNo()))
        Inform(Config():get_config('CantLock'))
    ELSEIF NoYes(Config():get_config('YesNoFormIdLanguageChange'))

        Window():refresh_header_footer()

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

    UNLOCK ALL
    SET FILTER TO &(cOldFilter)
    Window():header(cOldHeader)
    Window():footer(cOldFooter)
    RESTORE SCREEN FROM cOldScreen
    RESTORE KEYS FROM axOldKeys

RETURN

STATIC PROCEDURE clone()

    MEMVAR GETLIST

    LOCAL cOldHeader := Window():header(Config():get_config('CloneHeader'))
    LOCAL cOldFooter := Window():footer(Config():get_config('CloneFooter'))
    LOCAL cOldFilter := dbFilter()
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
    SET FILTER TO

    Window():refresh_header_footer()

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

                IF append()
                    field->id := cNewId
                    field->language := cNewLanguage
                    field->code := cCode

                    SELECT dbVariables

                    IF append()
                        field->id := cNewId
                        field->language := cNewLanguage
                        field->json := cJson

                        SELECT dbForms
                        COMMIT
                    ELSE
                        SELECT dbForms
                        DELETE
                        PACK
                        Inform(Config():get_config('CantLock'))
                    ENDIF
                ELSE
                    Inform(Config():get_config('CantLock'))
                ENDIF
            ENDIF
        ENDIF
    ENDDO

    UNLOCK ALL
    SET FILTER TO &(cOldFilter)
    Window():header(cOldHeader)
    Window():footer(cOldFooter)
    RESTORE SCREEN FROM cOldScreen
    RESTORE KEYS FROM axOldKeys

RETURN
