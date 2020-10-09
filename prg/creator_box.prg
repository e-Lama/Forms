#include "hbclass.ch"
#include "inkey.ch"

#include "functions.ch"
#include "menu.ch"

#include "creator.ch"

CLASS Creator_box INHERIT Creator

EXPORTED:

    METHOD edit_form(xFormCode, xGetPos)

HIDDEN:

    METHOD __refresh_and_display(nTop, nLeft, nBottom, nRight, nOldWindow, xFormCode, cScreen)
    //METHOD __display_menu(nTop, nLeft, nBottom, nRight, cScreen, xFormCode, xGetPos, lActiveUpperLeftCorner, lFinish, lSave)

ENDCLASS LOCK

/*
METHOD __display_menu(nTop, nLeft, nBottom, nRight, cScreen, xFormCode, xGetPos, lActiveUpperLeftCorner, lFinish, lSave) CLASS Creator_box

    MEMVAR GETLIST

    LOCAL nOldWindow := WSelect()
    LOCAL acMenuItems := Config():get_config('CreatorBoxMenuItems')
    LOCAL nWidth := max_of_array(length_array(acMenuItems)) + 5
    LOCAL nHeight := 6
    LOCAL nRow
    LOCAL nCol
    LOCAL nResult
    LOCAL axOldKeys

    WSelect(0)
    nRow := MRow()
    nCol := MCol()
    WSelect(nOldWindow)

    ZAP KEYS TO axOldKeys

    IF nRow + nHeight > Window():get_bottom()
        nRow -= nHeight
    ENDIF

    IF nCol + nWidth > Window():get_right()
        nCol -= nWidth
    ENDIF

    @ nRow, nCol, nRow + nHeight, nCol + nWidth MENU TO nResult;
      ITEMS acMenuItems SELECTABLE .T. MOUSABLE;
      FUNCTION 'menu_search_allow_exit_move';
      COLOR Config():get_config('DefaultMenuColor');
      BORDER Config():get_config('DefaultBox') SCROLLABLE;
      KEYS {K_ENTER, {K_ESC, K_RBUTTONDOWN}, K_ALT_UP, K_ALT_LEFT, K_ALT_DOWN, K_ALT_RIGHT, K_ALT_ENTER, K_LBUTTONUP, K_LBUTTONDOWN}

    DO CASE
        CASE nResult == 1
            ::_form_fast_edit(nTop, nLeft, nBottom, nRight, cScreen, xFormCode, xGetPos)
        CASE nResult == 2
            lActiveUpperLeftCorner := !lActiveUpperLeftCorner
        CASE nResult == 3
            IF YesNo(Config():get_config('DoReadOrder'))
                ReadModal(GETLIST)
            ENDIF
        CASE nResult == 4
            ::_next_stiffed()
        CASE nResult == 5
            IF YesNo(Config():get_config('YesNoBreakEdition'))
                IF YesNo(Config():get_config('YesNoSave')) 
                    IF ::_save_form()
                        lFinish := .T.
                        lSave := .T.
                    ENDIF
                ELSE
                    lFinish := .T.
                ENDIF
            ENDIF
    ENDCASE

    RESTORE KEYS FROM axOldKeys

RETURN NIL
*/

METHOD __refresh_and_display(nTop, nLeft, nBottom, nRight, nOldWindow, xFormCode, cScreen) CLASS Creator_box

    MEMVAR GETLIST

    DispBegin()

    IF Alias() == 'DBREORDER'

        CLEAR GETS

        IF WSelect() > 0
            WClose()
        ELSE
            RESTORE SCREEN FROM cScreen
        ENDIF

        prepare_form(ACopy(xFormCode, Array(field->line_nr - 1), 1, field->line_nr - 1))
        ::_display_form()
        prepare_form(ACopy(xFormCode, Array(Len(xFormCode) - field->line_nr), field->line_nr + 1))
    ELSE
        IF WSelect() > 0
            WSelect(0)
            RestScreen(nTop, nLeft, nBottom, nRight, cScreen)
            WSelect(nOldWindow)
        ELSE
            RESTORE SCREEN FROM cScreen
        ENDIF

        ::_display_form()
    ENDIF

    DispEnd()

RETURN NIL

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
    LOCAL lRefresh
    LOCAL nMouseRow
    LOCAL nMouseCol
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
        ::_set_type(OBJECT_BOX)
        CLEAR GETS

        IF !prepare_form(xFormCode)
            lFinish := .T.
            Inform(Parser():log(''))
        ELSE
            ::_make_form_array(xFormCode)
        ENDIF
    ENDIF

    DO WHILE !lFinish

        ::__refresh_and_display(nTop, nLeft, nBottom, nRight, nOldWindow, xFormCode, cScreen)

        nKey := Inkey(0)

        DO CASE
            CASE nKey == K_ALT_A
                lActiveUpperLeftCorner := !lActiveUpperLeftCorner
            CASE nKey == K_UP
                IF lActiveUpperLeftCorner
                    IF ::_get_value(N_TOP_BOX) - 1 <= ::_get_value(N_BOTTOM_BOX).AND. ::_get_value(N_TOP_BOX) >= nTopLimit
                        IF ::_get_stiffed() != STIFFED_VERTICALLY
                            ::_decrement(N_TOP_BOX)
                        ENDIF
                    ENDIF
                ELSE
                    IF ::_get_value(N_TOP_BOX) <= ::_get_value(N_BOTTOM_BOX) - 1 .AND. ::_get_value(N_BOTTOM_BOX) >= nTopLimit
                        IF ::_get_stiffed() != STIFFED_VERTICALLY
                            ::_decrement(N_BOTTOM_BOX)
                        ENDIF
                    ENDIF
                ENDIF    
            CASE nKey == K_LEFT
                IF lActiveUpperLeftCorner
                    IF ::_get_value(N_LEFT_BOX) - 1 <= ::_get_value(N_RIGHT_BOX) .AND. ::_get_value(N_LEFT_BOX) >= nLeftLimit
                        IF ::_get_stiffed() != STIFFED_HORIZONTALLY
                            ::_decrement(N_LEFT_BOX)
                        ENDIF
                    ENDIF
                ELSE
                    IF ::_get_value(N_LEFT_BOX) <= ::_get_value(N_RIGHT_BOX) - 1 .AND. ::_get_value(N_RIGHT_BOX) >= nLeftLimit
                        IF ::_get_stiffed() != STIFFED_HORIZONTALLY
                            ::_decrement(N_RIGHT_BOX)
                        ENDIF
                    ENDIF
                ENDIF
            CASE nKey == K_DOWN
                IF lActiveUpperLeftCorner
                    IF ::_get_value(N_TOP_BOX) + 1 <= ::_get_value(N_BOTTOM_BOX) .AND. ::_get_value(N_TOP_BOX) <= nBottomLimit
                        IF ::_get_stiffed() != STIFFED_VERTICALLY
                            ::_increment(N_TOP_BOX)
                        ENDIF
                    ENDIF
                ELSE
                    IF ::_get_value(N_TOP_BOX) <= ::_get_value(N_BOTTOM_BOX) + 1 .AND. ::_get_value(N_BOTTOM_BOX) <= nBottomLimit
                        IF ::_get_stiffed() != STIFFED_VERTICALLY
                            ::_increment(N_BOTTOM_BOX)
                        ENDIF
                    ENDIF
                ENDIF
            CASE nKey == K_RIGHT
                IF lActiveUpperLeftCorner
                    IF ::_get_value(N_LEFT_BOX) + 1 <= ::_get_value(N_RIGHT_BOX) .AND. ::_get_value(N_LEFT_BOX) <= nRightLimit
                        IF ::_get_stiffed() != STIFFED_HORIZONTALLY
                            ::_increment(N_LEFT_BOX)
                        ENDIF
                    ENDIF
                ELSE
                    IF ::_get_value(N_LEFT_BOX) <= ::_get_value(N_RIGHT_BOX) + 1 .AND. ::_get_value(N_RIGHT_BOX) <= nRightLimit
                        IF ::_get_stiffed() != STIFFED_HORIZONTALLY
                            ::_increment(N_RIGHT_BOX)
                        ENDIF
                    ENDIF
                ENDIF
            CASE nKey == K_ENTER
                ::_form_fast_edit(nTop, nLeft, nBottom, nRight, cScreen, xFormCode, xGetPos)
            CASE nKey == K_ALT_ENTER
                IF YesNo(Config():get_config('DoReadOrder'))
                    ReadModal(GETLIST)
                ENDIF
            CASE nKey == K_ALT_S
                ::_next_stiffed()
            CASE nKey == K_LBUTTONDOWN

                nKey := 0

                DO WHILE nKey == K_MOUSEMOVE .OR. nKey == 0

                    nKey := Inkey()

                    nMouseRow := MRow()
                    nMouseCol := MCol()
                    lRefresh := .F.

                    IF lActiveUpperLeftCorner
                        IF nMouseRow > ::_get_value(N_TOP_BOX)
                            IF ::_get_value(N_TOP_BOX) + 1 <= ::_get_value(N_BOTTOM_BOX) .AND. ::_get_value(N_TOP_BOX) <= nBottomLimit
                                IF ::_get_stiffed() != STIFFED_VERTICALLY
                                    ::_increment(N_TOP_BOX)
                                    lRefresh := .T.
                                ENDIF
                            ENDIF
                        ELSEIF nMouseRow < ::_get_value(N_TOP_BOX)
                            IF ::_get_value(N_TOP_BOX) - 1 <= ::_get_value(N_BOTTOM_BOX).AND. ::_get_value(N_TOP_BOX) >= nTopLimit
                                IF ::_get_stiffed() != STIFFED_VERTICALLY
                                    ::_decrement(N_TOP_BOX)
                                    lRefresh := .T.
                                ENDIF
                            ENDIF
                        ENDIF

                        IF nMouseCol > ::_get_value(N_LEFT_BOX)
                            IF ::_get_value(N_LEFT_BOX) + 1 <= ::_get_value(N_RIGHT_BOX) .AND. ::_get_value(N_LEFT_BOX) <= nRightLimit
                                IF ::_get_stiffed() != STIFFED_HORIZONTALLY
                                    ::_increment(N_LEFT_BOX)
                                    lRefresh := .T.
                                ENDIF
                            ENDIF
                        ELSEIF nMouseCol < ::_get_value(N_LEFT_BOX)
                            IF ::_get_value(N_LEFT_BOX) - 1 <= ::_get_value(N_RIGHT_BOX) .AND. ::_get_value(N_LEFT_BOX) >= nLeftLimit
                                IF ::_get_stiffed() != STIFFED_HORIZONTALLY
                                    ::_decrement(N_LEFT_BOX)
                                    lRefresh := .T.
                                ENDIF
                            ENDIF
                        ENDIF
                    ELSE
                        IF nMouseRow > ::_get_value(N_BOTTOM_BOX)
                            IF ::_get_value(N_TOP_BOX) <= ::_get_value(N_BOTTOM_BOX) + 1 .AND. ::_get_value(N_BOTTOM_BOX) <= nBottomLimit
                                IF ::_get_stiffed() != STIFFED_VERTICALLY
                                    ::_increment(N_BOTTOM_BOX)
                                    lRefresh := .T.
                                ENDIF
                            ENDIF
                        ELSEIF nMouseRow < ::_get_value(N_BOTTOM_BOX)
                            IF ::_get_value(N_TOP_BOX) <= ::_get_value(N_BOTTOM_BOX) - 1 .AND. ::_get_value(N_BOTTOM_BOX) >= nTopLimit
                                IF ::_get_stiffed() != STIFFED_VERTICALLY
                                    ::_decrement(N_BOTTOM_BOX)
                                    lRefresh := .T.
                                ENDIF
                            ENDIF
                        ENDIF

                        IF nMouseCol > ::_get_value(N_RIGHT_BOX)
                            IF ::_get_value(N_LEFT_BOX) <= ::_get_value(N_RIGHT_BOX) + 1 .AND. ::_get_value(N_RIGHT_BOX) <= nRightLimit
                                IF ::_get_stiffed() != STIFFED_HORIZONTALLY
                                    ::_increment(N_RIGHT_BOX)
                                    lRefresh := .T.
                                ENDIF
                            ENDIF
                        ELSEIF nMouseCol < ::_get_value(N_RIGHT_BOX)
                            IF ::_get_value(N_LEFT_BOX) <= ::_get_value(N_RIGHT_BOX) - 1 .AND. ::_get_value(N_RIGHT_BOX) >= nLeftLimit
                                IF ::_get_stiffed() != STIFFED_HORIZONTALLY
                                    ::_decrement(N_RIGHT_BOX)
                                    lRefresh := .T.
                                ENDIF
                            ENDIF
                        ENDIF
                    ENDIF

                    IF lRefresh
                        ::__refresh_and_display(nTop, nLeft, nBottom, nRight, nOldWindow, xFormCode, cScreen)
                    ELSE
                        ::_mouse_sleep()
                    ENDIF
                ENDDO
            CASE nKey == K_RBUTTONUP
                ::_display_menu(nTop, nLeft, nBottom, nRight, cScreen, xFormCode, xGetPos, @lActiveUpperLeftCorner, @lFinish, @lSave)
            CASE nKey == K_ESC
                IF YesNo(Config():get_config('YesNoBreakEdition'))
                    IF YesNo(Config():get_config('YesNoSave')) 
                        IF ::_save_form()
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
