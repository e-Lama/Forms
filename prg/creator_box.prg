#include "hbclass.ch"
#include "inkey.ch"

#include "creator.ch"

CLASS Creator_box INHERIT Creator

EXPORTED:

    METHOD edit_form(xFormCode, xGetPos)

ENDCLASS LOCK

METHOD edit_form(xFormCode, xGetPos) CLASS Creator_box

    MEMVAR GETLIST

    LOCAL cOldHeader := Window():header(Config():get_config('CreatorBoxHeader'))
    LOCAL cOldFooter := Window():footer(Config():get_config('CreatorBoxFooter'))
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
    LOCAL cScreen
    LOCAL nKey

    IF nOldWindow > 0
        WSelect(0)
        cScreen := SaveScreen(nTop, nLeft, nBottom, nRight)
        WSelect(nOldWindow)

        Window():refresh_header_footer()

        WClose()
    ELSE
        Window():refresh_header_footer()

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
        ::set_type(OBJECT_BOX)
        CLEAR GETS

        IF !prepare_form(xFormCode)
            lFinish := .T.
            Inform(Parser():log(''))
        ELSE
            ::make_form_array(xFormCode)
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

        nKey := Inkey(0)

        DO CASE
            CASE nKey == K_ALT_A
                lActiveUpperLeftCorner := !lActiveUpperLeftCorner
            CASE nKey == K_UP
                IF lActiveUpperLeftCorner
                    IF ::get_value(N_TOP_BOX) - 1 <= ::get_value(N_BOTTOM_BOX).AND. ::get_value(N_TOP_BOX) >= nTopLimit
                        ::decrement(N_TOP_BOX)
                    ENDIF
                ELSE
                    IF ::get_value(N_TOP_BOX) <= ::get_value(N_BOTTOM_BOX) - 1 .AND. ::get_value(N_BOTTOM_BOX) >= nTopLimit
                        ::decrement(N_BOTTOM_BOX)
                    ENDIF
                ENDIF    
            CASE nKey == K_LEFT
                IF lActiveUpperLeftCorner
                    IF ::get_value(N_LEFT_BOX) - 1 <= ::get_value(N_RIGHT_BOX) .AND. ::get_value(N_LEFT_BOX) >= nLeftLimit
                        ::decrement(N_LEFT_BOX)
                    ENDIF
                ELSE
                    IF ::get_value(N_LEFT_BOX) <= ::get_value(N_RIGHT_BOX) - 1 .AND. ::get_value(N_RIGHT_BOX) >= nLeftLimit
                        ::decrement(N_RIGHT_BOX)
                    ENDIF
                ENDIF
            CASE nKey == K_DOWN
                IF lActiveUpperLeftCorner
                    IF ::get_value(N_TOP_BOX) + 1 <= ::get_value(N_BOTTOM_BOX) .AND. ::get_value(N_TOP_BOX) <= nBottomLimit
                        ::increment(N_TOP_BOX)
                    ENDIF
                ELSE
                    IF ::get_value(N_TOP_BOX) <= ::get_value(N_BOTTOM_BOX) + 1 .AND. ::get_value(N_BOTTOM_BOX) <= nBottomLimit
                        ::increment(N_BOTTOM_BOX)
                    ENDIF
                ENDIF
            CASE nKey == K_RIGHT
                IF lActiveUpperLeftCorner
                    IF ::get_value(N_LEFT_BOX) + 1 <= ::get_value(N_RIGHT_BOX) .AND. ::get_value(N_LEFT_BOX) <= nRightLimit
                        ::increment(N_LEFT_BOX)
                    ENDIF
                ELSE
                    IF ::get_value(N_LEFT_BOX) <= ::get_value(N_RIGHT_BOX) + 1 .AND. ::get_value(N_RIGHT_BOX) <= nRightLimit
                        ::increment(N_RIGHT_BOX)
                    ENDIF
                ENDIF
            CASE nKey == K_ENTER
                ::form_fast_edit(nTop, nLeft, nBottom, nRight, cScreen, xFormCode, xGetPos)
            CASE nKey == K_ALT_ENTER
                IF YesNo(Config():get_config('DoReadOrder'))
                    ReadModal(GETLIST)
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
    Window():refresh_header_footer()

RETURN lSave
