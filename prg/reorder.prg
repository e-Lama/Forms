#include "fileio.ch"
#include "inkey.ch"
#include "box.ch"

#include "functions.ch"
#include "rowbrowse.ch"
#include "parser.ch"

STATIC PROCEDURE delete_row()

    LOCAL nOldLineNr := field->line_nr
    LOCAL axOldKeys

    ZAP KEYS TO axOldKeys

    IF !NoYes(Config():get_config('YesNoDeleteRow'))
        RESTORE KEYS FROM axOldKeys
        RETURN
    ENDIF

    DELETE
    PACK

    renumber()
    
    SEEK nOldLineNr

    RESTORE KEYS FROM axOldKeys

RETURN

STATIC PROCEDURE renumber()

    LOCAL nOldRecNo := RecNo()
    LOCAL n := 1

    GO TOP
    DO WHILE !EoF()
        field->line_nr := n
        ++n
        SKIP
    ENDDO
    GO nOldRecNo

RETURN

STATIC PROCEDURE move_row_down()

    LOCAL nActualRecNo := RecNo()

    IF EoF() .OR. BoF()
        RETURN
    ENDIF

    SKIP 1

    IF EoF() .OR. BoF()
        RETURN
    ENDIF

    field->line_nr := field->line_nr - 1
    GO nActualRecNo
    field->line_nr := field->line_nr + 1

    SKIP -1

RETURN

STATIC PROCEDURE move_row_up()

    LOCAL nActualRecNo := RecNo()

    IF EoF() .OR. BoF()
        RETURN
    ENDIF

    SKIP -1

    IF EoF() .OR. BoF()
        RETURN
    ENDIF

    field->line_nr := field->line_nr + 1
    GO nActualRecNo 
    field->line_nr := field->line_nr - 1

RETURN

STATIC PROCEDURE swap()

    MEMVAR GETLIST

    LOCAL nOldLineNr := field->line_nr
    LOCAL nFirst := field->line_nr
    LOCAL nSecond := field->line_nr
    LOCAL hVariables := hb_Hash('nFirst';
                                , nFirst;
                                , 'nSecond';
                                , nSecond;
                               )
    LOCAL axOldKeys

    ZAP KEYS TO axOldKeys

    IF Parser():prepare_form_from_database(Config():get_config('Language'), 'SWAP', hVariables)
        READ
        hVariables := Parser():get_answers()
        nFirst := hVariables['nFirst']
        nSecond := hVariables['nSecond']
        WClose()
    ELSE
        throw(Config():get_config('CriticalError'))
    ENDIF
    
    IF nFirst == nSecond .OR. nFirst < 1 .OR. nFirst > LastRec() .OR. nSecond < 1 .OR. nSecond > LastRec()
        Inform(Config():get_config('IncorrectValues'))
        RESTORE KEYS FROM axOldKeys
        RETURN
    ENDIF

    SEEK nFirst
    field->line_nr := nSecond
    SEEK nSecond
    field->line_nr := nFirst
    SEEK nOldLineNr

    RESTORE KEYS FROM axOldKeys

RETURN

STATIC PROCEDURE move()

    MEMVAR GETLIST

    LOCAL nOldRecNo := RecNo()
    LOCAL hVariables := hb_Hash('nWhere', field->line_nr)
    LOCAL hReorder := hb_Hash()
    LOCAL nWasLineNr := field->line_nr
    LOCAL nRecNo
    LOCAL nTo
    LOCAL axOldKeys

    ZAP KEYS TO axOldKeys

    IF Parser():prepare_form_from_database(Config():get_config('Language'), 'WHERE_MOVE', hVariables)
        READ
        hVariables := Parser():get_answers()
        nTo := hVariables['nWhere']
        WClose()
    ELSE
        throw(Config():get_config('CriticalError'))
    ENDIF

    IF nTo == field->line_nr .OR. nTo < 1 .OR. nTo > LastRec()
        Inform(Config():get_config('IncorrectValues'))
        RESTORE KEYS FROM axOldKeys
        RETURN
    ENDIF

    SEEK nTo

    IF nWasLineNr < nTo
        DO WHILE field->line_nr > nWasLineNr
            hReorder[RecNo()] := field->line_nr - 1    
            SKIP -1
        ENDDO
    ELSE
        DO WHILE field->line_nr < nWasLineNr
            hReorder[RecNo()] := field->line_nr + 1
            SKIP
        ENDDO
    ENDIF

    FOR EACH nRecNo IN hb_hKeys(hReorder)
        GO nRecNo
        field->line_nr := hReorder[nRecNo]
    NEXT

    GO nOldRecNo
    field->line_nr := nTo

    RESTORE KEYS FROM axOldKeys

RETURN

STATIC PROCEDURE edit()

    LOCAL nOldCursor := Set(_SET_CURSOR)
    LOCAL nTop := Window():get_top() + 1
    LOCAL nLeft := Window():get_left() + 1
    LOCAL nBottom := Window():get_bottom() - 1
    LOCAL nRight := Window():get_right() - 1
    LOCAL cOldScreen
    LOCAL axOldKeys

    ZAP KEYS TO axOldKeys

    IF EoF()
        Inform(Config():get_config('NoRecordSelected'))
        RESTORE KEYS FROM axOldKeys
        RETURN
    ENDIF

    SAVE SCREEN TO cOldScreen

    @ nTop, nLeft, nBottom, nRight BOX B_SINGLE
    @ nTop, Int(nRight + nLeft - Len(Config():get_config('Code')) / 2) SAY Config():get_config('Code')
    field->code := MemoEdit(RTrim(field->code), nTop + 1, nLeft + 1, nBottom - 1, nRight - 1)
    @ nTop, nLeft, nBottom, nRight BOX B_SINGLE
    @ nTop, Int((nRight + nLeft - Len(' JSON ')) / 2) SAY ' JSON '
    dbVariables->json := MemoEdit(dbVariables->json, nTop + 1, nLeft + 1, nBottom - 1, nRight - 1)

    SET CURSOR (cast(nOldCursor, 'L'))

    RESTORE SCREEN FROM cOldScreen
    RESTORE KEYS FROM axOldKeys

RETURN

STATIC PROCEDURE save()

    LOCAL nOldRecNo := RecNo()
    LOCAL cNewCode := ''
    LOCAL hJson
    LOCAL axOldKeys

    ZAP KEYS TO axOldKeys

    GO TOP
    DO WHILE !EoF()
        cNewCode += RTrim(field->code) + OBJECT_SEPARATOR
        SKIP
    ENDDO

    cNewCode := Left(cNewCode, Len(cNewCode) - Len(OBJECT_SEPARATOR))

    hJson := hb_JsonDecode(dbVariables->json)
    IF Parser():check_correctness(hb_ATokens(cNewCode, OBJECT_SEPARATOR), hJson)
        dbForms->code := cNewCode
        COMMIT
    ELSE
        Inform(Parser():log(''))
    ENDIF

    GO nOldRecNo

    RESTORE KEYS FROM axOldKeys

RETURN

STATIC PROCEDURE rebuild()

    MEMVAR GETLIST
    
    LOCAL nOldWindow := WSelect()
    LOCAL nOldRecNo := RecNo()
    LOCAL hMenuItems := hb_Hash(OBJECT_WINDOW, 'WINDOW';
                                , OBJECT_BOX, 'BOX';
                                , OBJECT_SAY, 'SAY';
                                , OBJECT_GET, 'GET';
                                , OBJECT_CHECKBOX, 'CHECKBOX';
                                , OBJECT_LISTBOX, 'LISTBOX';
                                , OBJECT_RADIOGROUP, 'RADIOGROUP';
                               )
    LOCAL cType := hMenuItems[hb_ATokens(RTrim(field->code), LINE_SEPARATOR)[1]]
    LOCAL nLineNr := field->line_nr
    LOCAL acFormCode := Array(0)
    LOCAL nGetPos := 0
    LOCAL hJson
    LOCAL cOldScreen
    LOCAL axOldKeys

    SAVE SCREEN TO cOldScreen
    ZAP KEYS TO axOldKeys

    hJson := hb_JsonDecode(dbVariables->json)

    GO TOP
    DO WHILE !EoF()
        AAdd(acFormCode, RTrim(field->code))
        IF field->line_nr <= nLineNr
            IF AScan({'WINDOW', 'BOX', 'SAY'}, hMenuItems[hb_ATokens(RTrim(field->code), LINE_SEPARATOR)[1]]) == 0
                ++nGetPos
            ENDIF
        ENDIF
        SKIP
    ENDDO
    GO nOldRecNo

    IF validate(acFormCode, hJson)

        IF cType == 'WINDOW'
            IF prepare_form(acFormCode, hJson) 
                EVal(&('{| acFormCode, nGetPos | Creator_' + Lower(cType) + '():edit_form(acFormCode, nGetPos)}'), acFormCode, nGetPos)
            ELSE
                Inform(Parser():log(''))
            ENDIF
        ELSE
            IF prepare_form(hb_ADel(AClone(acFormCode), nLineNr, .T.), hJson) 
                EVal(&('{| acFormCode, nGetPos | Creator_' + Lower(cType) + '():edit_form(acFormCode, nGetPos)}'), acFormCode, nGetPos)
            ELSE
                Inform(Parser():log(''))
            ENDIF
        ENDIF

        IF WSelect() != nOldWindow
            WClose()
        ENDIF
    ELSE
        Inform(Parser():log(''))
    ENDIF

    CLEAR GETS
    RESTORE KEYS FROM axOldKeys
    RESTORE SCREEN FROM cOldScreen

RETURN

STATIC PROCEDURE display_line()

    MEMVAR GETLIST

    LOCAL aoOldGetList := AClone(GETLIST)
    LOCAL nOldWindow := WSelect()
    LOCAL cOldFooter := Window():footer(Config():get_config('ReorderDisplayForm'))
    LOCAL acCode := hb_ATokens(RTrim(field->code), OBJECT_SEPARATOR)
    LOCAL hJson
    LOCAL cOldScreen
    LOCAL axOldKeys

    SAVE SCREEN TO cOldScreen
    ZAP KEYS TO axOldKeys

    hJson := hb_JsonDecode(dbVariables->json)

    IF ValType(hJson) != 'H'
        Inform(Config():get_config('CorruptionDetected'))
    ELSEIF Parser():check_correctness(acCode, hJson)

        Window():refresh_footer()

        Parser():prepare_form_from_record(acCode, hJson)

        IF Inkey(0) == K_ALT_ENTER
            READ
        ENDIF

        IF WSelect() != nOldWindow
            WClose()
        ENDIF
    ELSE
        Inform(Parser():log(''))
    ENDIF

    GETLIST := aoOldGetList
    Window():footer(cOldFooter)
    RESTORE KEYS FROM axOldKeys
    RESTORE SCREEN FROM cOldScreen

RETURN

STATIC PROCEDURE display_form()

    MEMVAR GETLIST

    LOCAL aoOldGetList := AClone(GETLIST)
    LOCAL nOldRecNo := RecNo()
    LOCAL nOldWindow := WSelect()
    LOCAL cOldFooter := Window():footer(Config():get_config('ReorderDisplayForm'))
    LOCAL cNewCode := ''
    LOCAL hJson
    LOCAL cOldScreen
    LOCAL axOldKeys

    SAVE SCREEN TO cOldScreen
    ZAP KEYS TO axOldKeys

    Window():refresh_footer()

    GO TOP
    DO WHILE !EoF()
        cNewCode += RTrim(field->code) + OBJECT_SEPARATOR
        SKIP
    ENDDO

    cNewCode := Left(cNewCode, Len(cNewCode) - Len(OBJECT_SEPARATOR))

    hJson := hb_JsonDecode(dbVariables->json)

    IF ValType(hJson) != 'H'
        Inform(Config():get_config('CorruptionDetected'))
    ELSEIF Parser():prepare_form_from_record(hb_ATokens(cNewCode, OBJECT_SEPARATOR), hJson)

        IF Inkey(0) == K_ALT_ENTER
            READ
        ENDIF

        IF WSelect() != nOldWindow
            WClose()
        ENDIF
    ELSE
        Inform(Parser():log(''))
    ENDIF

    GO nOldRecNo

    GETLIST := aoOldGetList
    Window():footer(cOldFooter)
    RESTORE SCREEN FROM cOldScreen
    RESTORE KEYS FROM axOldKeys

RETURN

STATIC PROCEDURE add_window()

    LOCAL axOldKeys
    LOCAL nOldRecNo := RecNo()

    ZAP KEYS TO axOldKeys

    GO TOP

    IF Left(field->code, Len(OBJECT_WINDOW)) != OBJECT_WINDOW

        GO BOTTOM

        DO WHILE !BoF()
            field->line_nr := field->line_nr + 1
            SKIP -1
        ENDDO

        APPEND BLANK

        field->line_nr := 1
        field->code := OBJECT_WINDOW + LINE_SEPARATOR;
                       + CONSTANT + 'N' + '0' + LINE_SEPARATOR;
                       + CONSTANT + 'N' + '0' + LINE_SEPARATOR;
                       + CONSTANT + 'N' + LTrim(Str(MaxRow())) + LINE_SEPARATOR;
                       + CONSTANT + 'N' + LTrim(Str(MaxCol())) + LINE_SEPARATOR;
                       + CONSTANT + 'C' + Config():get_config('DefaultWindowCreatorBox') + LINE_SEPARATOR;
                       + CONSTANT + 'C' + Config():get_config('DefaultWindowCreatorColor') + LINE_SEPARATOR;
                       + CONSTANT + 'C' + Config():get_config('DefaultWindowCreatorShadow')
    ELSE
        Inform(Config():get_config('OnlyOneWindowAllowed'))
    ENDIF

    GO nOldRecNo
    RESTORE KEYS FROM axOldKeys

RETURN

PROCEDURE change_order()

    LOCAL nOldSelect := Select()
    LOCAL nOldRecNo := RecNo()
    LOCAL cOldFooter := Window():footer(Config():get_config('ReorderFooter'))
    LOCAL cOldHeader := Window():header(Config():get_config('ReorderHeader'))
    LOCAL acRows := hb_ATokens(field->code, OBJECT_SEPARATOR)
    LOCAL axStructure := {{'line_nr', 'N', 4, 0}, {'code', 'C', 2056, 0}} 
    LOCAL lChanged := .F.
    LOCAL lContinue := .T.
    LOCAL lCorrect := .T.
    LOCAL oRowBrowse
    LOCAL cOldScreen
    LOCAL axOldKeys
    LOCAL i

    ZAP KEYS TO axOldKeys

    IF EoF()
        Inform(Config():get_config('NoRecordSelected'))
        lContinue := .F.
    ELSEIF important_form(field->id) .AND. !NoYes(Config():get_config('ImportantForm'))
        lContinue := .F.
    ENDIF

    dbCreate('mem:dbReorder', axStructure, hb_Memio(), .T., 'dbReorder')

    IF lContinue
        FOR i := 1 TO Len(acRows)
            APPEND BLANK
            field->line_nr := i

            IF Empty(acRows[i])
                lCorrect := .F.
                EXIT
            ELSE
                field->code := acRows[i]
            ENDIF
        NEXT
    ENDIF

    IF lCorrect .AND. lContinue

        SAVE SCREEN TO cOldScreen

        SET KEY K_DEL TO delete_row()
        SET KEY K_F2 TO rebuild()
        SET KEY K_F3 TO swap()
        SET KEY K_F4 TO edit()
        SET KEY K_F5 TO move()
        SET KEY K_F6 TO display_line()
        SET KEY K_F7 TO display_form()
        SET KEY K_F8 TO move_row_down()
        SET KEY K_F9 TO move_row_up()
        SET KEY K_F10 TO add_window()

        Window():refresh_header()
        Window():refresh_footer()

        INDEX ON field->line_nr TO mem:dbReorderInd
        GO TOP

        @ Window():get_top() + 1, Window():get_left() + 1, Window():get_bottom() - 1, Window():get_right() - 1;
          ROWBROWSE oRowBrowse ID 'reorder' COLOR Config():get_config('DefaultColor') BORDER Config():get_config('RowBrowseDefaultBox');
          TITLE Config():get_config('ReorderRowBrowseTitle') ACTION {| oRowBrowse, nKey | row_browse_reorder_search(oRowBrowse, nKey)}

        oRowBrowse:display()

        GO TOP

        FOR i := 1 TO Len(acRows)
            IF field->code != acRows[i]
                lChanged := .T.
                EXIT
            ENDIF
            SKIP
        NEXT

        IF lChanged .AND. YesNo(Config():get_config('YesNoSave'))
            save(nOldRecNo)
        ENDIF
    ELSEIF !lCorrect
        Inform(Config():get_config('CorruptionDetected'))
    ENDIF

    CLOSE
    dbDrop('mem:dbReorder')
    SELECT (nOldSelect)
    Window():header(cOldHeader)
    Window():footer(cOldFooter)
    RESTORE SCREEN FROM cOldScreen
    RESTORE KEYS FROM axOldKeys

RETURN

FUNCTION row_browse_reorder_search(oRowBrowse, nKey)

    LOCAL cCurrentString := LTrim(oRowBrowse:search_keys())
    LOCAL nReturn := ROWBROWSE_NOTHING
    LOCAL nOldRecNo := RecNo()

    IF AScan({K_DOWN, K_UP, K_HOME, K_END, K_PGUP, K_PGDN}, nKey) != 0
        oRowBrowse:search_keys('')
        oRowBrowse:draw_border()
        oRowBrowse:print_title()
    ELSEIF nKey == K_BS
        cCurrentString := Left(cCurrentString, Len(cCurrentString) - 1)
        oRowBrowse:search_keys(PadL(cCurrentString, 4))
        oRowBrowse:draw_border()
        oRowBrowse:print_title()

        IF Len(cCurrentString) > 0
            @ oRowBrowse:bottom(), oRowBrowse:left() SAY 'Found: ' + cCurrentString
        ENDIF
    ELSE
        IF oRowBrowse:search(PadL(cCurrentString + Chr(nKey), 4))
            nReturn := ROWBROWSE_SEARCH
            oRowBrowse:search_keys(PadL(cCurrentString + Chr(nKey), 4))
            oRowBrowse:draw_border()
            oRowBrowse:print_title()
            @ oRowBrowse:bottom(), oRowBrowse:left() SAY 'Found: ' + cCurrentString + Chr(nKey)
        ELSE
            GO nOldRecNo
        ENDIF
    ENDIF

RETURN nReturn
