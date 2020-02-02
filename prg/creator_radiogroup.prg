#include "hbclass.ch"
#include "inkey.ch"

#include "creator.ch"

CLASS Creator_radiogroup INHERIT Creator

EXPORTED:

    METHOD edit_form(xFormCode, xGetPos)

ENDCLASS LOCK

METHOD edit_form(xFormCode, xGetPos) CLASS Creator_radiogroup

    MEMVAR GETLIST

    LOCAL cOldHeader := Window():header(Config():get_config('CreatorRadiogroupHeader'))
    LOCAL cOldFooter := Window():footer(Config():get_config('CreatorRadiogroupFooter'))
    LOCAL nOldWindow := WSelect()
    LOCAL nTopLimit := IF(WSelect() == 0, Window():get_top(), 0)
    LOCAL nLeftLimit := IF(WSelect() == 0, Window():get_left(), 0)
    LOCAL nBottomLimit := IF(WSelect() == 0, Window():get_bottom(), MaxRow() - 1)
    LOCAL nRightLimit := IF(WSelect() == 0, Window():get_right(), MaxCol() - 1)
    LOCAL lActiveUpperLeftCorner := .T.
    LOCAL lFinish := .F.
    LOCAL nTop := WRow()
    LOCAL nLeft := WCol()
    LOCAL nBottom := WLastRow()
    LOCAL nRight := WLastCol()
    LOCAL lSave := .F.
    LOCAL aoWasGetList
    LOCAL cScreen
    LOCAL nKey

    IF nOldWindow > 0
        WSelect(0)
        cScreen := SaveScreen(nTop, nLeft, nBottom, nRight)
        WSelect(nOldWindow)

        Window():refresh_header()
        Window():refresh_footer()

        WClose()
    ELSE
        Window():refresh_header()
        Window():refresh_footer()

        CLEAR GETS

        IF ValType(xFormCode) == 'A'
            SAVE SCREEN TO cScreen
        ENDIF

        IF !prepare_form(xFormCode)
            lFinish := .T.
            Inform(Parser():log(''))
        ELSEIF ValType(xFormCode) != 'A'
            SAVE SCREEN TO cScreen
        ENDIF
    ENDIF

    IF !lFinish
        ::set_type(OBJECT_RADIOGROUP)
        CLEAR GETS

        IF !prepare_form(xFormCode)
            lFinish := .T.
            Inform(Parser():log(''))
        ELSE
            ::make_form_array(xFormCode)

            IF Alias() == 'DBREORDER'
                GETLIST[xGetPos] := __objClone(ATail(GETLIST))
                GETLIST := ASize(GETLIST, Len(GETLIST) - 1)
            ENDIF
        ENDIF
    ENDIF

    DO WHILE !lFinish

        IF Alias() == 'DBREORDER'
            
            CLEAR GETS

            IF WSelect() > 0
                WClose()
            ELSE
                RESTORE SCREEN FROM cScreen
            ENDIF

            prepare_form(ACopy(xFormCode, Array(field->line_nr - 1), 1, field->line_nr - 1))
            ::display_form()
            prepare_form(ACopy(xFormCode, Array(Len(xFormCode) - field->line_nr), field->line_nr + 1))
        ELSE
            IF WSelect() > 0
                WSelect(0)
                RestScreen(nTop, nLeft, nBottom, nRight, cScreen)
                WSelect(nOldWindow)
            ELSE
                RESTORE SCREEN FROM cScreen
            ENDIF

            ::display_form()
        ENDIF

        IF ValType(aoWasGetList) == 'A' .AND. Len(aoWasGetList) != 0 .AND. Len(GETLIST) != 0
            IF ValType(xFormCode) == 'A'
                aoWasGetList[xGetPos] := __objClone(GETLIST[xGetPos])
            ELSE
                aoWasGetList[Len(aoWasGetList)] := __objClone(ATail(GETLIST))
            ENDIF
        ELSE
            aoWasGetList := clone_objects_array(GETLIST)
        ENDIF
        
        GETLIST := ASize(GETLIST, Len(GETLIST) - 1)

        nKey := Inkey(0)

        DO CASE
            CASE nKey == K_ALT_A
                lActiveUpperLeftCorner := !lActiveUpperLeftCorner
            CASE nKey == K_UP
                IF lActiveUpperLeftCorner
                    IF ::get_value(N_TOP_RGB) - 1 < ::get_value(N_BOTTOM_RGB).AND. ::get_value(N_TOP_RGB) >= nTopLimit
                        ::decrement(N_TOP_RGB)
                    ENDIF
                ELSE
                    IF ::get_value(N_TOP_RGB) < ::get_value(N_BOTTOM_RGB) - 1 .AND. ::get_value(N_BOTTOM_RGB) >= nTopLimit
                        ::decrement(N_BOTTOM_RGB)
                    ENDIF
                ENDIF    
            CASE nKey == K_LEFT
                IF lActiveUpperLeftCorner
                    IF ::get_value(N_LEFT_RGB) - 1 < ::get_value(N_RIGHT_RGB) .AND. ::get_value(N_LEFT_RGB) >= nLeftLimit
                        ::decrement(N_LEFT_RGB)
                    ENDIF
                ELSE
                    IF ::get_value(N_LEFT_RGB) < ::get_value(N_RIGHT_RGB) - 1 .AND. ::get_value(N_RIGHT_RGB) >= nLeftLimit
                        ::decrement(N_RIGHT_RGB)
                    ENDIF
                ENDIF
            CASE nKey == K_DOWN
                IF lActiveUpperLeftCorner
                    IF ::get_value(N_TOP_RGB) + 1 < ::get_value(N_BOTTOM_RGB) .AND. ::get_value(N_TOP_RGB) <= nBottomLimit
                        ::increment(N_TOP_RGB)
                    ENDIF
                ELSE
                    IF ::get_value(N_TOP_RGB) < ::get_value(N_BOTTOM_RGB) + 1 .AND. ::get_value(N_BOTTOM_RGB) <= nBottomLimit
                        ::increment(N_BOTTOM_RGB)
                    ENDIF
                ENDIF
            CASE nKey == K_RIGHT
                IF lActiveUpperLeftCorner
                    IF ::get_value(N_LEFT_RGB) + 1 < ::get_value(N_RIGHT_RGB) .AND. ::get_value(N_LEFT_RGB) <= nRightLimit
                        ::increment(N_LEFT_RGB)
                    ENDIF
                ELSE
                    IF ::get_value(N_LEFT_RGB) < ::get_value(N_RIGHT_RGB) + 1 .AND. ::get_value(N_RIGHT_RGB) <= nRightLimit
                        ::increment(N_RIGHT_RGB)
                    ENDIF
                ENDIF
            CASE nKey == K_ENTER
                ::form_fast_edit(nTop, nLeft, nBottom, nRight, cScreen, xFormCode, xGetPos)
            CASE nKey == K_ALT_ENTER
                IF YesNo(Config():get_config('DoReadOrder'))
                    ReadModal(aoWasGetList)
                ENDIF
            CASE nKey == K_ESC
                IF YesNo(Config():get_config('YesNoBreakEdition'))
                    IF YesNo(Config():get_config('YesNoSave'))
                        IF ::save_form()
                            lFinish := .T.
                            lSave := .T.
                        ENDIF
                    ELSE 
                        lFinish := .T.
                    ENDIF
                ENDIF
        ENDCASE
    ENDDO

    Window():header(cOldHeader)
    Window():footer(cOldFooter)
    Window():refresh_header()
    Window():refresh_footer()

RETURN lSave
